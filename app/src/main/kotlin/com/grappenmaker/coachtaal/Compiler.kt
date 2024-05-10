package com.grappenmaker.coachtaal

import com.grappenmaker.jvmutil.*
import org.objectweb.asm.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.tree.*
import java.io.PrintStream
import kotlin.math.ceil
import kotlin.system.measureTimeMillis

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
    iter: List<Expr>,
    init: List<Expr>,
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
    iter: List<Expr>,
    init: List<Expr>,
    language: Language = DutchLanguage,
    implements: List<String> = emptyList(),
    runnable: Boolean = false,
) = generateClassBytes(
    name = compiledName,
    // or emptylist but extra allocation grr :upside_down:
    implements = (if (runnable) implements else listOf(internalNameOf<ModelRunner>()) + implements).distinct(),
    defaultConstructor = false
) {
    val variables = iter.extractVariables(language) + init.extractVariables(language)
    variables.forEach {
        visitField(ACC_PUBLIC, it, "F", null, null)
        generateMethod("get${it.replaceFirstChar { c -> c.uppercaseChar() }}", "()F") {
            loadThis()
            visitFieldInsn(GETFIELD, compiledName, it, "F")
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
            visitFieldInsn(GETFIELD, compiledName, v, "F")
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

    val compiledInit = buildInsnList { compile(init, compiledName, language) }

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

        compile(iter, compiledName, language)
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

private fun Expr.remainingStackHeight(language: Language) = when (this) {
    is CallExpr -> if (name.value == language.stop) 0 else 1
    is BinaryOperatorExpr, is IdentifierExpr, is LiteralExpr, is NotExpr, is UnaryMinusExpr -> 1
    // body gets ironed out anyway, should be 0
    is AssignmentExpr, is ConditionalExpr, is RepeatUntilExpr, is RepeatingExpr, is WhileExpr -> 0
}

private fun MethodVisitor.popRemaining(expr: Expr, language: Language) {
    val pops = expr.remainingStackHeight(language)
    if (pops > 0) pop(pops)
}

private fun MethodVisitor.compile(
    program: List<Expr>,
    thisName: String,
    language: Language,
    topLevel: Boolean = true
) = program.forEach {
    compile(it, thisName, language)
    if (topLevel) popRemaining(it, language)
}

private fun MethodVisitor.reference(id: Identifier, thisName: String) {
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

fun MethodVisitor.compileConditionDefault(
    condition: Expr,
    thisName: String,
    language: Language,
    label: Label,
) {
    compile(condition, thisName, language)
    loadConstant(0f)
    visitInsn(FCMPG)
    visitJumpInsn(IFLE, label)
}

fun MethodVisitor.compileConditionOptimized(
    condition: BinaryOperatorExpr,
    thisName: String,
    language: Language,
    opcode: Int,
    label: Label,
) {
    compile(condition.left, thisName, language)
    compile(condition.right, thisName, language)
    visitInsn(FCMPG)
    visitJumpInsn(opcode, label)
}

fun MethodVisitor.compileCondition(
    condition: Expr,
    thisName: String,
    language: Language,
    label: Label
) = when (condition) {
    is BinaryOperatorExpr -> when (condition.operatorToken) {
        "<=" -> compileConditionOptimized(condition, thisName, language, IFGT, label)
        ">=" -> compileConditionOptimized(condition, thisName, language, IFLT, label)
        ">" -> compileConditionOptimized(condition, thisName, language, IFLE, label)
        "<" -> compileConditionOptimized(condition, thisName, language, IFGE, label)
        "=" -> compileConditionOptimized(condition, thisName, language, IFNE, label)
        "<>" -> compileConditionOptimized(condition, thisName, language, IFEQ, label)
        else -> compileConditionDefault(condition, thisName, language, label)
    }

    else -> compileConditionDefault(condition, thisName, language, label)
}

private fun MethodVisitor.comparisonOperator(opcode: Int) = convertBoolean(opcode, false)
private fun MethodVisitor.compile(expr: Expr, thisName: String, language: Language) {
    when (expr) {
        is AssignmentExpr -> update(expr.left, thisName) { compile(expr.right, thisName, language) }
        is BinaryOperatorExpr -> {
            compile(expr.left, thisName, language)
            if (expr.operatorToken == "^") visitInsn(I2D)

            compile(expr.right, thisName, language)
            if (expr.operatorToken == "^") visitInsn(I2D)

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
            compile(expr.arguments, thisName, language, false)

            when (val name = expr.name.value) {
                !in language.allBuiltins -> error("$name is not supported")
                language.stop -> {
                    loadThis()
                    loadConstant(true)
                    visitFieldInsn(PUTFIELD, thisName, "_stopped", "Z")
                    returnMethod()
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

            compileCondition(expr.condition, thisName, language, case)
            compile(expr.whenTrue, thisName, language)
            visitJumpInsn(GOTO, end)

            visitLabel(case)
            expr.whenFalse?.let { compile(it, thisName, language) }

            visitLabel(end)
        }

        is IdentifierExpr -> when (expr.value.value.lowercase()) {
            language.pi -> loadConstant(Math.PI.toFloat())
            language.on -> loadConstant(255.0f)
            language.off -> loadConstant(0.0f)
            else -> reference(expr.value, thisName)
        }

        is LiteralExpr -> loadConstant(expr.value)
        is NotExpr -> convertBoolean(IFEQ)
        is UnaryMinusExpr -> {
            compile(expr.on, thisName, language)
            visitInsn(FNEG)
        }

        is WhileExpr -> {
            val start = Label()
            val end = Label()

            visitLabel(start)
            compileCondition(expr.condition, thisName, language, end)

            compile(expr.body, thisName, language)
            visitJumpInsn(GOTO, start)

            visitLabel(end)
        }

        is RepeatUntilExpr -> {
            val loop = Label().also { visitLabel(it) }
            compile(expr.body, thisName, language)

            compileCondition(expr.condition, thisName, language, loop)
        }

        is RepeatingExpr -> {
            val rep = expr.repetitions
            if (rep is LiteralExpr && rep.value <= 0.0f) return

            loadConstant(0)
            store(1, ISTORE)

            val start = Label().also { visitLabel(it) }
            compile(expr.body, thisName, language)

            visitIincInsn(1, 1)
            load(1, ILOAD)

            when (rep) {
                is LiteralExpr -> {
                    loadConstant(ceil(rep.value).toInt())
                    visitJumpInsn(IF_ICMPLT, start)
                }

                else -> {
                    visitInsn(I2F)
                    compile(rep, thisName, language)
                    visitInsn(FCMPG)
                    visitJumpInsn(IFLT, start)
                }
            }
        }
    }
}