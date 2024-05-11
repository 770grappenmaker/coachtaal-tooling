package com.grappenmaker.coachtaal

import com.grappenmaker.jvmutil.*
import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.tree.*
import kotlin.math.ceil

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
    language: Language = DutchLanguage,
) = loadCompiledModel<T>(
    compiledName.replace('/', '.'),
    compileModel(compiledName, iter, init, language, listOf(internalNameOf<T>()))
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

fun compileModel(
    compiledName: String,
    iter: ParsedProgram,
    init: ParsedProgram,
    language: Language = DutchLanguage,
    implements: List<String> = emptyList(),
    runnable: Boolean = false,
) = generateClassBytes(
    name = compiledName,
    // or emptylist but extra allocation grr :upside_down:
    implements = (if (runnable) implements else listOf(internalNameOf<ModelRunner>()) + implements).distinct(),
    defaultConstructor = false
) {
    val variables = iter.extractVariables() + init.extractVariables()
    variables.forEach {
        visitField(ACC_PUBLIC, it.value, "F", null, null)
        generateMethod("get${it.value.replaceFirstChar { c -> c.uppercaseChar() }}", "()F") {
            loadThis()
            visitFieldInsn(GETFIELD, compiledName, it.value, "F")
            returnMethod(FRETURN)
        }
    }

    val allFunctions = iter.functions + init.functions
    val functionNames = allFunctions.mapTo(hashSetOf()) { it.name }
    val terminableFunctions = (iter.terminableFunctions() + init.terminableFunctions()).mapTo(hashSetOf()) { it.name }

    with(CompilationContext(compiledName, language, variables, functionNames, terminableFunctions)) {
        allFunctions.forEach {
            generateMethod(it.name.value, "(${"F".repeat(it.parameters.size)})F") {
                loadConstant(0.0f)
                store(it.parameters.size + 1, FSTORE)
                compile(it.body, it)
                load(it.parameters.size + 1, FLOAD)
                returnMethod(FRETURN)
            }
        }

        generateMethod("memoryByName", "(Ljava/lang/String;)F") {
            loadThis()
            load(1)
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
            pop()
            loadConstant(0f)
            visitLabel(end)
            returnMethod(FRETURN)
        }

        visitField(ACC_PUBLIC, "_stopped", "Z", null, null)
        generateMethod("getStopped", "()Z") {
            loadThis()
            visitFieldInsn(GETFIELD, compiledName, "_stopped", "Z")
            returnMethod(IRETURN)
        }

        val compiledInit = buildInsnList { compile(init.lines) }

        generateMethod("<init>", "()V") {
            loadThis()
            visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)

            compiledInit.accept(this)
            returnMethod()
        }

        generateMethod("iteration", "()V") {
            val label = Label()

            loadThis()
            visitFieldInsn(GETFIELD, compiledName, "_stopped", "Z")
            visitJumpInsn(IFNE, label)

            compile(iter.lines)
            visitLabel(label)
            returnMethod()
        }

        generateMethod("reset", "()V") {
            loadThis()
            loadConstant(false)
            visitFieldInsn(PUTFIELD, compiledName, "_stopped", "Z")

            compiledInit.accept(this)
            returnMethod()
        }

        generateMethod("main", "([Ljava/lang/String;)V", access = ACC_PUBLIC or ACC_STATIC) {
            load(0)
            loadConstant(0)
            visitInsn(AALOAD)
            visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "parseInt", "(Ljava/lang/String;)I", false)
            dup()
            store(4, ISTORE)
            store(5, ISTORE)

            construct(compiledName, "()V")
            store(1)

            visitMethodInsn(INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J", false)
            store(2, LSTORE)

            val repeat = Label().also { visitLabel(it) }
            visitIincInsn(4, -1)
            load(1)
            visitMethodInsn(INVOKEVIRTUAL, compiledName, "reset", "()V", false)

            // <single iteration>
            val label = Label().also { visitLabel(it) }
            val end = Label()

            load(1)
            visitFieldInsn(GETFIELD, compiledName, "_stopped", "Z")
            visitJumpInsn(IFNE, end)

            load(1)
            visitMethodInsn(INVOKEVIRTUAL, compiledName, "iteration", "()V", false)

            visitJumpInsn(GOTO, label)
            visitLabel(end)
            // </single iteration>

            load(4, ILOAD)
            visitJumpInsn(IFGT, repeat)

            visitMethodInsn(INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J", false)
            load(2, LLOAD)
            visitInsn(LSUB)
            store(2, LSTORE)

            visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
            concat {
                appendString("Took ")
                appendPrimitive(Type.LONG_TYPE) { load(2, LLOAD) }
                appendString("ms for ")
                appendPrimitive(Type.INT_TYPE) { load(5, ILOAD) }
                appendString(" iterations.")
            }

            visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
            returnMethod()
        }
    }
}

private fun Expr.remainingStackHeight(language: Language) = when (this) {
    is CallExpr -> if (name.value == language.stop) 0 else 1
    is BinaryOperatorExpr, is IdentifierExpr, is LiteralExpr, is NotExpr, is UnaryMinusExpr -> 1
    // body gets ironed out anyway, should be 0
    is AssignmentExpr, is ConditionalExpr, is RepeatUntilExpr, is RepeatingExpr, is WhileExpr, is FunctionExpr -> 0
}

private fun MethodVisitor.popRemaining(expr: Expr, language: Language) {
    val pops = expr.remainingStackHeight(language)
    if (pops > 0) pop(pops)
}

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
        load(function.parameters.size + 1, FLOAD)
        returnMethod(FRETURN)
    } else returnMethod()
}

context(CompilationContext)
private fun MethodVisitor.callFunction(id: Identifier, parameters: List<Expr>, function: FunctionExpr?) {
    loadThis()
    compile(parameters, function, false)
    visitMethodInsn(INVOKEVIRTUAL, thisName, id.value, "(${"F".repeat(parameters.size)})F", false)

    if (id in terminableFunctions) {
        loadThis()
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
            load(idx, FLOAD)
            return
        }
    }

    if (id in functions) {
        callFunction(id, emptyList(), function)
        return
    }

    loadThis()
    visitFieldInsn(GETFIELD, thisName, id.value, "F")
}

private inline fun MethodVisitor.update(id: Identifier, thisName: String, value: MethodVisitor.() -> Unit) {
    loadThis()
    value()
    visitFieldInsn(PUTFIELD, thisName, id.value, "F")
}

private fun MethodVisitor.convertBoolean(opcode: Int, compareWithZero: Boolean = true) {
    val label = Label()
    val end = Label()

    if (compareWithZero) loadConstant(0f)
    visitInsn(FCMPG)
    visitJumpInsn(opcode, label)

    loadConstant(0f)
    visitJumpInsn(GOTO, end)

    visitLabel(label)
    loadConstant(255f)
    visitLabel(end)
}

context(CompilationContext)
fun MethodVisitor.compileConditionDefault(
    condition: Expr,
    function: FunctionExpr?,
    label: Label,
) {
    compile(condition, function)
    loadConstant(0f)
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
                store(function.parameters.size + 1, FSTORE)
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
                    loadThis()
                    loadConstant(true)
                    visitFieldInsn(PUTFIELD, thisName, "_stopped", "Z")
                    returnFunctionSafe(function)
                }

                language.sqr -> {
                    dup()
                    visitInsn(FMUL)
                }

                language.factorial -> invokeMethod(Float::roundFactorial)
                language.step -> {
                    val label = Label()
                    val end = Label()
                    visitInsn(FCMPG)
                    visitJumpInsn(IFLT, label)

                    loadConstant(1f)
                    visitJumpInsn(GOTO, end)

                    visitLabel(label)
                    loadConstant(0f)
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
            language.pi -> loadConstant(Math.PI.toFloat())
            language.on -> loadConstant(255.0f)
            language.off -> loadConstant(0.0f)
            else -> reference(expr.value, function)
        }

        is LiteralExpr -> loadConstant(expr.value)
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

            loadConstant(0)
            store(1, ISTORE)

            val start = Label().also { visitLabel(it) }
            compile(expr.body)

            visitIincInsn(1, 1)
            load(1, ILOAD)

            when (rep) {
                is LiteralExpr -> {
                    loadConstant(ceil(rep.value).toInt())
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