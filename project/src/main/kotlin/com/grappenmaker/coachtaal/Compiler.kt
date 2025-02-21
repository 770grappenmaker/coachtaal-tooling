package com.grappenmaker.coachtaal

import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.tree.*
import kotlin.math.ceil

inline fun <reified T : Any> internalNameOf() = T::class.java.name.replace('.', '/')

inline fun buildInsnList(block: MethodVisitor.() -> Unit): InsnList {
    val target = InsnList()
    val visitor = InsnListVisitor(target)
    block(visitor)
    return target
}

class InsnListVisitor(private val output: InsnList) : MethodVisitor(ASM9) {
    override fun visitInsn(opcode: Int) = output.add(InsnNode(opcode))
    override fun visitIntInsn(opcode: Int, operand: Int) = output.add(IntInsnNode(opcode, operand))
    override fun visitVarInsn(opcode: Int, varIndex: Int) = output.add(VarInsnNode(opcode, varIndex))
    override fun visitTypeInsn(opcode: Int, type: String?) = output.add(TypeInsnNode(opcode, type))
    override fun visitFieldInsn(opcode: Int, owner: String?, name: String?, descriptor: String?) =
        output.add(FieldInsnNode(opcode, owner, name, descriptor))

    override fun visitMethodInsn(
        opcode: Int,
        owner: String?,
        name: String?,
        descriptor: String?,
        isInterface: Boolean
    ) = output.add(MethodInsnNode(opcode, owner, name, descriptor, isInterface))

    override fun visitInvokeDynamicInsn(
        name: String?,
        descriptor: String?,
        bootstrapMethodHandle: Handle?,
        vararg bootstrapMethodArguments: Any?
    ) = output.add(InvokeDynamicInsnNode(name, descriptor, bootstrapMethodHandle, *bootstrapMethodArguments))

    private fun Label.node() = info as? LabelNode ?: LabelNode(this).also { info = it }
    override fun visitJumpInsn(opcode: Int, label: Label) = output.add(JumpInsnNode(opcode, label.node()))
    override fun visitLabel(label: Label) = output.add(label.node())
    override fun visitLdcInsn(value: Any?) = output.add(LdcInsnNode(value))
    override fun visitIincInsn(varIndex: Int, increment: Int) = output.add(IincInsnNode(varIndex, increment))
    override fun visitTableSwitchInsn(min: Int, max: Int, dflt: Label, vararg labels: Label) =
        output.add(TableSwitchInsnNode(min, max, dflt.node(), *labels.map { it.node() }.toTypedArray()))

    override fun visitLookupSwitchInsn(dflt: Label, keys: IntArray?, labels: Array<Label>) =
        output.add(LookupSwitchInsnNode(dflt.node(), keys, labels.map { it.node() }.toTypedArray()))

    override fun visitMultiANewArrayInsn(descriptor: String?, numDimensions: Int) =
        output.add(MultiANewArrayInsnNode(descriptor, numDimensions))
}

inline fun <reified T : ModelRunner> createCompiledModel(
    compiledName: String,
    iter: ParsedProgram,
    init: ParsedProgram,
    maxIterations: Int = -1,
    language: Language = DutchLanguage,
) = loadCompiledModel<T>(
    compiledName.replace('/', '.'),
    compileModel(compiledName, iter, init, language, maxIterations, listOf(internalNameOf<T>()))
)

inline fun <reified T : ModelRunner> loadCompiledModel(name: String, bytes: ByteArray): T {
    val loader = object : ClassLoader() {
        fun createClass(name: String, bytes: ByteArray) =
            defineClass(name, bytes, 0, bytes.size)
    }

    return loader.createClass(name, addModelType<T>(bytes)).getConstructor().newInstance() as T
}

inline fun <reified T : ModelRunner> addModelType(bytes: ByteArray): ByteArray {
    val reader = ClassReader(bytes)
    val internal = internalNameOf<T>()
    if (internal in reader.interfaces) return bytes

    val writer = ClassWriter(reader, 0)

    reader.accept(object : ClassVisitor(ASM9, writer) {
        override fun visit(
            version: Int,
            access: Int,
            name: String,
            signature: String?,
            superName: String?,
            interfaces: Array<String>?
        ) {
            super.visit(version, access, name, signature, superName, (interfaces ?: emptyArray()) + internal)
        }
    }, 0)

    return writer.toByteArray()
}

inline fun ClassVisitor.generateMethod(
    name: String,
    desc: String,
    access: Int = ACC_PUBLIC,
    block: MethodVisitor.() -> Unit
) = with(visitMethod(access, name, desc, null, null)) {
    visitCode()
    block()
    visitMaxs(-1, -1)
    visitEnd()
}

fun compileModel(
    compiledName: String,
    iter: ParsedProgram,
    init: ParsedProgram,
    language: Language = DutchLanguage,
    maxIterations: Int = -1,
    implements: List<String> = emptyList(),
    runnable: Boolean = false,
): ByteArray = ClassWriter(ClassWriter.COMPUTE_FRAMES).apply {
    val itf = buildSet {
        add(internalNameOf<ModelRunner>())
        if (!runnable) addAll(implements)
    }.toTypedArray()

    visit(V1_8, ACC_PUBLIC, compiledName, null, "java/lang/Object", itf)

    val variables = iter.extractVariables() + init.extractVariables()
    variables.forEach {
        visitField(ACC_PUBLIC, it.value, "F", null, null)
        generateMethod("get${it.value.replaceFirstChar { c -> c.uppercaseChar() }}", "()F") {
            visitVarInsn(ALOAD, 0)
            visitFieldInsn(GETFIELD, compiledName, it.value, "F")
            visitInsn(FRETURN)
        }
    }

    if (maxIterations > 0) visitField(ACC_PUBLIC, "\$\$iters", "I", null, null)

    val allFunctions = iter.functions + init.functions
    val functionNames = allFunctions.mapTo(hashSetOf()) { it.name }
    val terminableFunctions = (iter.terminableFunctions() + init.terminableFunctions()).mapTo(hashSetOf()) { it.name }

    with(CompilationContext(compiledName, language, variables, functionNames, terminableFunctions)) {
        allFunctions.forEach {
            generateMethod(it.name.value, "(${"F".repeat(it.parameters.size)})F") {
                visitInsn(FCONST_0)
                visitVarInsn(FSTORE, it.parameters.size + 1)
                compile(it.body, it)
                visitVarInsn(FLOAD, it.parameters.size + 1)
                visitInsn(FRETURN)
            }
        }

        generateMethod("memoryByName", "(Ljava/lang/String;)F") {
            visitVarInsn(ALOAD, 0)
            visitVarInsn(ALOAD, 1)
            visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "hashCode", "()I", false)

            val end = Label()
            val default = Label()
            val sorted = variables.map { it to it.hashCode() }.sortedBy { (_, a) -> a }
            val labels = sorted.map { Label() }

            visitLookupSwitchInsn(default, sorted.map { (_, a) -> a }.toIntArray(), labels.toTypedArray())
            sorted.forEachIndexed { idx, (v) ->
                visitLabel(labels[idx])
                visitFieldInsn(GETFIELD, compiledName, v.value, "F")
                visitJumpInsn(GOTO, end)
            }

            visitLabel(default)
            visitInsn(POP)
            visitInsn(FCONST_0)
            visitLabel(end)
            visitInsn(FRETURN)
        }

        visitField(ACC_PUBLIC, "_stopped", "Z", null, null)
        generateMethod("getStopped", "()Z") {
            visitVarInsn(ALOAD, 0)
            visitFieldInsn(GETFIELD, compiledName, "_stopped", "Z")
            visitInsn(IRETURN)
        }

        val compiledInit = buildInsnList { compile(init.lines) }

        generateMethod("<init>", "()V") {
            visitVarInsn(ALOAD, 0)
            visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)

            compiledInit.accept(this)

            if (maxIterations == 0) {
                visitVarInsn(ALOAD, 0)
                visitInsn(ICONST_1)
                visitFieldInsn(PUTFIELD, compiledName, "_stopped", "Z")
            }

            visitInsn(RETURN)
        }

        generateMethod("iteration", "()V") {
            val label = Label()

            visitVarInsn(ALOAD, 0)
            visitFieldInsn(GETFIELD, compiledName, "_stopped", "Z")
            visitJumpInsn(IFNE, label)

            compile(iter.lines)

            if (maxIterations > 0) {
                visitVarInsn(ALOAD, 0)
                visitFieldInsn(GETFIELD, compiledName, "\$\$iters", "I")
                visitInsn(ICONST_1)
                visitInsn(IADD)
                visitInsn(DUP)
                visitVarInsn(ALOAD, 0)
                visitInsn(SWAP)
                visitFieldInsn(PUTFIELD, compiledName, "\$\$iters", "I")
                visitLdcInsn(maxIterations)
                visitJumpInsn(IF_ICMPLT, label)

                visitVarInsn(ALOAD, 0)
                visitInsn(ICONST_1)
                visitFieldInsn(PUTFIELD, compiledName, "_stopped", "Z")
            }

            visitLabel(label)
            visitInsn(RETURN)
        }

        generateMethod("reset", "()V") {
            if (maxIterations != 0) {
                visitVarInsn(ALOAD, 0)
                visitInsn(ICONST_0)
                visitFieldInsn(PUTFIELD, compiledName, "_stopped", "Z")
            }

            compiledInit.accept(this)
            visitInsn(RETURN)
        }

        generateMethod("main", "([Ljava/lang/String;)V", access = ACC_PUBLIC or ACC_STATIC) {
            visitVarInsn(ALOAD, 0)
            visitInsn(ICONST_0)
            visitInsn(AALOAD)
            visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "parseInt", "(Ljava/lang/String;)I", false)
            visitInsn(DUP)
            visitVarInsn(ISTORE, 4)
            visitVarInsn(ISTORE, 5)

            visitTypeInsn(NEW, compiledName)
            visitInsn(DUP)
            visitMethodInsn(INVOKESPECIAL, compiledName, "<init>", "()V", false)
            visitVarInsn(ASTORE, 1)

            visitMethodInsn(INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J", false)
            visitVarInsn(LSTORE, 2)

            val repeat = Label().also { visitLabel(it) }
            visitIincInsn(4, -1)
            visitVarInsn(ALOAD, 1)
            visitMethodInsn(INVOKEVIRTUAL, compiledName, "reset", "()V", false)

            // <single iteration>
            val label = Label().also { visitLabel(it) }
            val end = Label()

            visitVarInsn(ALOAD, 1)
            visitFieldInsn(GETFIELD, compiledName, "_stopped", "Z")
            visitJumpInsn(IFNE, end)

            visitVarInsn(ALOAD, 1)
            visitMethodInsn(INVOKEVIRTUAL, compiledName, "iteration", "()V", false)

            visitJumpInsn(GOTO, label)
            visitLabel(end)
            // </single iteration>

            visitVarInsn(ILOAD, 4)
            visitJumpInsn(IFGT, repeat)

            visitMethodInsn(INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J", false)
            visitVarInsn(LLOAD, 2)
            visitInsn(LSUB)
            visitVarInsn(LSTORE, 2)

            visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

            visitTypeInsn(NEW, "java/lang/StringBuilder")
            visitInsn(DUP)
            visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false)

            fun appendString(cst: String) {
                visitLdcInsn(cst)
                visitMethodInsn(
                    INVOKEVIRTUAL,
                    "java/lang/StringBuilder",
                    "append",
                    "(Ljava/lang/String;)Ljava/lang/StringBuilder;",
                    false
                )
            }

            appendString("Took ")
            visitVarInsn(LLOAD, 2)
            visitMethodInsn(
                INVOKEVIRTUAL,
                "java/lang/StringBuilder",
                "append",
                "(J)Ljava/lang/StringBuilder;",
                false
            )

            appendString("ms for ")
            visitVarInsn(ILOAD, 5)
            visitMethodInsn(
                INVOKEVIRTUAL,
                "java/lang/StringBuilder",
                "append",
                "(I)Ljava/lang/StringBuilder;",
                false
            )

            appendString(" iterations.")
            visitMethodInsn(
                INVOKEVIRTUAL,
                "java/lang/StringBuilder",
                "toString",
                "()Ljava/lang/String;",
                false
            )

            visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
            visitInsn(RETURN)
        }
    }

    visitEnd()
}.toByteArray()

private fun Expr.remainingStackHeight(language: Language) = when (this) {
    is CallExpr -> if (name.value == language.stop) 0 else 1
    is BinaryOperatorExpr, is IdentifierExpr, is LiteralExpr, is NotExpr, is UnaryMinusExpr -> 1
    // body gets ironed out anyway, should be 0
    is AssignmentExpr, is ConditionalExpr, is RepeatUntilExpr,
    is RepeatingExpr, is WhileExpr, is FunctionExpr, is EOLExpr -> 0
}

private fun MethodVisitor.popRemaining(expr: Expr, language: Language) =
    repeat(expr.remainingStackHeight(language)) { visitInsn(POP) }

context(CompilationContext)
private fun MethodVisitor.compile(
    program: List<Expr>,
    function: FunctionExpr? = null,
    topLevel: Boolean = true
) = program.forEach {
    compile(it, function)
    if (topLevel) popRemaining(it, language)
}

fun MethodVisitor.returnFunctionSafe(function: FunctionExpr? = null) {
    if (function != null) {
        visitVarInsn(FLOAD, function.parameters.size + 1)
        visitInsn(FRETURN)
    } else visitInsn(RETURN)
}

context(CompilationContext)
private fun MethodVisitor.callFunction(id: Identifier, parameters: List<Expr>, function: FunctionExpr?) {
    visitVarInsn(ALOAD, 0)
    compile(parameters, function, false)
    visitMethodInsn(INVOKEVIRTUAL, thisName, id.value, "(${"F".repeat(parameters.size)})F", false)

    if (id in terminableFunctions) {
        visitVarInsn(ALOAD, 0)
        visitFieldInsn(GETFIELD, thisName, "_stopped", "Z")
        val label = Label()
        visitJumpInsn(IFEQ, label)
        returnFunctionSafe(function)
        visitLabel(label)
    }
}

context(CompilationContext)
private fun MethodVisitor.reference(id: Identifier, function: FunctionExpr?) {
    if (function != null) {
        val idx = function.parameters.indexOf(id) + 1
        if (idx > 0) {
            visitVarInsn(FLOAD, idx)
            return
        }
    }

    if (id in functions) {
        callFunction(id, emptyList(), function)
        return
    }

    visitVarInsn(ALOAD, 0)
    visitFieldInsn(GETFIELD, thisName, id.value, "F")
}

private inline fun MethodVisitor.update(id: Identifier, thisName: String, value: MethodVisitor.() -> Unit) {
    visitVarInsn(ALOAD, 0)
    value()
    visitFieldInsn(PUTFIELD, thisName, id.value, "F")
}

private fun MethodVisitor.convertBoolean(opcode: Int, compareWithZero: Boolean = true) {
    val label = Label()
    val end = Label()

    if (compareWithZero) visitInsn(FCONST_0)
    visitInsn(FCMPG)
    visitJumpInsn(opcode, label)

    visitInsn(FCONST_0)
    visitJumpInsn(GOTO, end)

    visitLabel(label)
    visitLdcInsn(255f)
    visitLabel(end)
}

context(CompilationContext)
fun MethodVisitor.compileConditionDefault(
    condition: Expr,
    function: FunctionExpr?,
    label: Label,
) {
    compile(condition, function)
    visitInsn(FCONST_0)
    visitInsn(FCMPG)
    visitJumpInsn(IFLE, label)
}

context(CompilationContext)
fun MethodVisitor.compileConditionOptimized(
    condition: BinaryOperatorExpr,
    function: FunctionExpr?,
    opcode: Int,
    label: Label,
) {
    compile(condition.left, function)
    compile(condition.right, function)
    visitInsn(FCMPG)
    visitJumpInsn(opcode, label)
}

context(CompilationContext)
fun MethodVisitor.compileCondition(
    condition: Expr,
    function: FunctionExpr?,
    label: Label
) = when (condition) {
    is BinaryOperatorExpr -> when (condition.operatorToken) {
        "<=" -> compileConditionOptimized(condition, function, IFGT, label)
        ">=" -> compileConditionOptimized(condition, function, IFLT, label)
        ">" -> compileConditionOptimized(condition, function, IFLE, label)
        "<" -> compileConditionOptimized(condition, function, IFGE, label)
        "=" -> compileConditionOptimized(condition, function, IFNE, label)
        "<>" -> compileConditionOptimized(condition, function, IFEQ, label)
        else -> compileConditionDefault(condition, function, label)
    }

    else -> compileConditionDefault(condition, function, label)
}

private fun MethodVisitor.comparisonOperator(opcode: Int) = convertBoolean(opcode, false)

context(CompilationContext)
private fun MethodVisitor.compile(
    expr: Expr,
    function: FunctionExpr? = null,
) {
    when (expr) {
        is AssignmentExpr -> {
            if (function?.name == expr.left) {
                compile(expr.right, function)
                visitVarInsn(FSTORE, function.parameters.size + 1)
                return
            }

            update(expr.left, thisName) { compile(expr.right, function) }
        }

        is BinaryOperatorExpr -> {
            compile(expr.left, function)
            if (expr.operatorToken == "^") visitInsn(F2D)

            compile(expr.right, function)
            if (expr.operatorToken == "^") visitInsn(F2D)

            when (expr.operatorToken) {
                "+" -> visitInsn(FADD)
                "-" -> visitInsn(FSUB)
                "/" -> visitInsn(FDIV)
                "*" -> visitInsn(FMUL)
                "<=" -> comparisonOperator(IFLE)
                ">=" -> comparisonOperator(IFGE)
                ">" -> comparisonOperator(IFGT)
                "<" -> comparisonOperator(IFLT)
                "=" -> comparisonOperator(IFEQ)
                "<>" -> comparisonOperator(IFNE)
                "^" -> {
                    visitMethodInsn(INVOKESTATIC, "java/lang/Math", "pow", "(DD)D", false)
                    visitInsn(D2F)
                }

                // Optimization (might be incorrect)
                "&&" -> visitInsn(FMUL)
                "||" -> visitInsn(FADD)

                else -> error("Operator ${expr.operatorToken} not implemented")
            }
        }

        is CallExpr -> {
            val name = expr.name.value
            if (name in functionNames) {
                callFunction(expr.name, expr.arguments, function)
                return
            }

            compile(expr.arguments, topLevel = false)

            when (name) {
                !in language.allBuiltins -> error("Unresolved function call $name")

                language.stop -> {
                    visitVarInsn(ALOAD, 0)
                    visitInsn(ICONST_1)
                    visitFieldInsn(PUTFIELD, thisName, "_stopped", "Z")
                    returnFunctionSafe(function)
                }

                language.sqr -> {
                    visitInsn(DUP)
                    visitInsn(FMUL)
                }

                language.factorial -> visitMethodInsn(
                    INVOKESTATIC,
                    "com/grappenmaker/coachtaal/UtilKt",
                    "roundFactorial",
                    "(F)F",
                    false
                )

                language.step -> {
                    val label = Label()
                    val end = Label()
                    visitInsn(FCMPG)
                    visitJumpInsn(IFLT, label)

                    visitInsn(FCONST_1)
                    visitJumpInsn(GOTO, end)

                    visitLabel(label)
                    visitInsn(FCONST_0)
                    visitLabel(end)
                }

                language.random -> {
                    visitMethodInsn(INVOKESTATIC, "java/lang/Math", "random", "()D", false)
                    visitInsn(D2F)
                }

                language.min, language.max -> {
                    val actual = if (name == language.min) "min" else "max"
                    repeat(expr.arguments.size - 1) {
                        visitMethodInsn(INVOKESTATIC, "java/lang/Math", actual, "(FF)F", false)
                    }
                }

                else -> {
                    val desc = "D".repeat(expr.arguments.size)
                    val actualName = when (name) {
                        language.arcsin -> "asin"
                        language.arccos -> "acos"
                        language.arctan -> "atan"
                        language.floor -> "floor"
                        language.round -> "round"
                        language.sign -> "signum"
                        language.log -> "log10"
                        language.ln -> "log"
                        else -> name
                    }

                    visitInsn(F2D)
                    visitMethodInsn(INVOKESTATIC, "java/lang/Math", actualName, "($desc)D", false)
                    visitInsn(D2F)
                }
            }
        }

        is ConditionalExpr -> {
            val case = Label()
            val end = Label()

            compileCondition(expr.condition, function, case)
            compile(expr.whenTrue, function)
            visitJumpInsn(GOTO, end)

            visitLabel(case)
            expr.whenFalse?.let { compile(it, function) }

            visitLabel(end)
        }

        is IdentifierExpr -> when (expr.value.value.lowercase()) {
            language.pi -> visitLdcInsn(Math.PI.toFloat())
            language.on -> visitLdcInsn(255.0f)
            language.off -> visitInsn(FCONST_0)
            else -> reference(expr.value, function)
        }

        is LiteralExpr -> visitLdcInsn(expr.value) // potentially slightly inefficient, but who cares?
        is NotExpr -> convertBoolean(IFEQ)
        is UnaryMinusExpr -> {
            compile(expr.on, function)
            visitInsn(FNEG)
        }

        is WhileExpr -> {
            val start = Label()
            val end = Label()

            visitLabel(start)
            compileCondition(expr.condition, function, end)

            compile(expr.body)
            visitJumpInsn(GOTO, start)

            visitLabel(end)
        }

        is RepeatUntilExpr -> {
            val loop = Label().also { visitLabel(it) }
            compile(expr.body)

            compileCondition(expr.condition, function, loop)
        }

        is RepeatingExpr -> {
            val rep = expr.repetitions
            if (rep is LiteralExpr && rep.value <= 0.0f) return

            visitInsn(ICONST_0)
            visitVarInsn(ISTORE, 1)

            val start = Label().also { visitLabel(it) }
            compile(expr.body)

            visitIincInsn(1, 1)
            visitVarInsn(ILOAD, 1)

            when (rep) {
                is LiteralExpr -> {
                    visitLdcInsn(ceil(rep.value).toInt())
                    visitJumpInsn(IF_ICMPLT, start)
                }

                else -> {
                    visitInsn(I2F)
                    compile(rep, function)
                    visitInsn(FCMPG)
                    visitJumpInsn(IFLT, start)
                }
            }
        }

        is FunctionExpr -> error("Function expression should not have propagated down to the compiler within a body!")
        is EOLExpr -> {}
    }
}

data class CompilationContext(
    val thisName: String,
    val language: Language,
    val variables: Set<Identifier>,
    val functions: Set<Identifier>,
    val terminableFunctions: Set<Identifier>,
) {
    val variableNames: Set<String> = variables.mapTo(hashSetOf()) { it.value }
    val functionNames: Set<String> = functions.mapTo(hashSetOf()) { it.value }
}