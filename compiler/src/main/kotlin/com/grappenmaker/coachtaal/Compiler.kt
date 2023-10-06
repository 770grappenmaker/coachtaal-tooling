package com.grappenmaker.coachtaal

import com.grappenmaker.jvmutil.*
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes.*

private var unnamedCounter = 0
    get() = field++

inline fun <reified T : CompiledModel> loadCompiledModel(name: String, bytes: ByteArray): T {
    val loader = object : ClassLoader() {
        fun createClass(name: String, bytes: ByteArray) =
            defineClass(name, bytes, 0, bytes.size)
    }

    return loader.createClass(name, bytes).getConstructor().newInstance() as T
}

fun compileModel(
    iter: List<Expr>,
    init: List<Expr>,
    language: Language = DutchLanguage,
    compiledName: String = "Unnamed$unnamedCounter",
    implements: List<String> = emptyList(),
) = generateClassBytes(
    name = compiledName,
    implements = listOf(internalNameOf<CompiledModel>()) + implements,
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
        invokeMethod(Object::hashCode)

        val end = Label()
        val default = Label()
        val sorted = variables.sortedBy { it.hashCode() }
        val labels = sorted.map { Label() }

        visitLookupSwitchInsn(default, sorted.map { it.hashCode() }.toIntArray(), labels.toTypedArray())
        sorted.forEachIndexed { idx, v ->
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

    generateMethod("<init>", "()V") {
        loadThis()
        visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)

        compile(init, compiledName)
        returnMethod()
    }

    generateMethod("iteration", "()V") {
        val label = Label()

        loadThis()
        visitFieldInsn(GETFIELD, compiledName, "_stopped", "Z")
        visitJumpInsn(IFNE, label)

        compile(iter, compiledName)
        visitLabel(label)
        returnMethod()
    }
}

private fun MethodVisitor.compile(program: List<Expr>, thisName: String) = program.forEach { compile(it, thisName) }

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

private fun MethodVisitor.comparisonOperator(opcode: Int) = convertBoolean(opcode, false)
private fun MethodVisitor.compile(expr: Expr, thisName: String) {
    when (expr) {
        is AssignmentExpr -> update(expr.left, thisName) { compile(expr.right, thisName) }
        is BinaryOperatorExpr -> {
            compile(expr.left, thisName)
            if (expr.operatorToken == "^") visitInsn(I2D)

            compile(expr.right, thisName)
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
            compile(expr.arguments, thisName)

            when (val name = expr.name.value) {
                "stop" -> {
                    loadThis()
                    loadConstant(true)
                    visitFieldInsn(PUTFIELD, thisName, "_stopped", "Z")
                    returnMethod()
                }

                else -> {
                    val desc = "D".repeat(expr.arguments.size)
                    visitInsn(F2D)
                    visitMethodInsn(INVOKESTATIC, "java/lang/Math", name, "($desc)D", false)
                    visitInsn(D2F)
                }
            }
        }

        is ConditionalExpr -> {
            val case = Label()
            val end = Label()

            compile(expr.condition, thisName)
            loadConstant(0f)
            visitInsn(FCMPG)
            visitJumpInsn(IFGT, case)

            expr.whenFalse?.let { compile(it, thisName) }
            visitJumpInsn(GOTO, end)

            visitLabel(case)
            compile(expr.whenTrue, thisName)

            visitLabel(end)
        }

        is IdentifierExpr -> reference(expr.value, thisName)
        is LiteralExpr -> loadConstant(expr.value)
        is NotExpr -> convertBoolean(IFEQ)
        is UnaryMinusExpr -> {
            compile(expr.on, thisName)
            visitInsn(FNEG)
        }
    }
}

interface CompiledModel {
    val stopped: Boolean
    fun iteration()
    fun memoryByName(name: String): Float
}

fun CompiledModel.run(logVariables: Set<String> = emptySet()): List<List<LogbookEntry>> {
    var iter = 0
    val result = mutableListOf<List<LogbookEntry>>()

    while (!stopped) {
        iteration()
        result += logVariables.map { LogbookEntry(it, memoryByName(it), iter) }
        iter++
    }

    return result
}