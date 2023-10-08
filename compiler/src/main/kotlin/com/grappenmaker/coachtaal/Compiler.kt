package com.grappenmaker.coachtaal

import com.grappenmaker.jvmutil.*
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes.*
import kotlin.math.ceil
import kotlin.reflect.full.memberProperties

inline fun <reified T : CompiledModel> createCompiledModel(
    compiledName: String,
    iter: List<Expr>,
    init: List<Expr>,
    language: Language = DutchLanguage,
) = loadCompiledModel<T>(compiledName, compileModel(compiledName, iter, init, language, listOf(internalNameOf<T>())))

inline fun <reified T : CompiledModel> loadCompiledModel(name: String, bytes: ByteArray): T {
    val loader = object : ClassLoader() {
        fun createClass(name: String, bytes: ByteArray) =
            defineClass(name, bytes, 0, bytes.size)
    }

    return loader.createClass(name, bytes).getConstructor().newInstance() as T
}

fun compileModel(
    compiledName: String,
    iter: List<Expr>,
    init: List<Expr>,
    language: Language = DutchLanguage,
    implements: List<String> = emptyList(),
) = generateClassBytes(
    name = compiledName,
    implements = (listOf(internalNameOf<CompiledModel>()) + implements).distinct(),
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

        compile(init, compiledName, language)
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
}

private fun MethodVisitor.compile(program: List<Expr>, thisName: String, language: Language) =
    program.forEach { compile(it, thisName, language) }

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
            compile(expr.arguments, thisName, language)

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
                    invokeMethod(Math::random)
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

            compile(expr.condition, thisName, language)
            loadConstant(0f)
            visitInsn(FCMPG)
            visitJumpInsn(IFGT, case)

            expr.whenFalse?.let { compile(it, thisName, language) }
            visitJumpInsn(GOTO, end)

            visitLabel(case)
            compile(expr.whenTrue, thisName, language)

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
            compile(expr.condition, thisName, language)
            loadConstant(0f)
            visitInsn(FCMPG)
            visitJumpInsn(IFLE, end)

            compile(expr.body, thisName, language)
            visitJumpInsn(GOTO, start)

            visitLabel(end)
        }
        is RepeatUntilExpr -> {
            val loop = Label().also { visitLabel(it) }
            compile(expr.body, thisName, language)

            compile(expr.condition, thisName, language)
            loadConstant(0f)
            visitInsn(FCMPG)
            visitJumpInsn(IFLE, loop)
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
        if (logVariables.isNotEmpty()) result += logVariables.map { LogbookEntry(it, memoryByName(it), iter) }
        iter++
    }

    return result
}