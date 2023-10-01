package com.grappenmaker.coachtaal

import kotlin.math.pow

val binaryOperators = mapOf<String, (Float, Float) -> Float>(
    "=" to { a, b -> (a == b).asCoach },
    "<>" to { a, b -> (a != b).asCoach },
    "<=" to { a, b -> (a <= b).asCoach },
    ">=" to { a, b -> (a >= b).asCoach },
    "<" to { a, b -> (a < b).asCoach },
    ">" to { a, b -> (a > b).asCoach },
    "+" to Float::plus,
    "-" to Float::minus,
    "/" to Float::div,
    "*" to Float::times,
    "^" to Float::pow,
)

fun parser(tokens: List<Token>): List<Expr> {
    var ptr = 0
    var curr: Expr? = null
    val result = mutableListOf<Expr>()

    TODO()

    return result
}

sealed interface ExprResult {
    data object None : ExprResult
    data class Number(val value: Float) : ExprResult

    val number get() = (this as? Number)?.value ?: error("Expression $this does not yield a number!")
}

sealed interface Expr {
    fun eval(interpreter: Interpreter): ExprResult
}

data class BinaryOperatorExpr(
    val left: Expr,
    val right: Expr,
    val operator: (Float, Float) -> Float
) : Expr {
    override fun eval(interpreter: Interpreter) =
        ExprResult.Number(operator(left.eval(interpreter).number, right.eval(interpreter).number))
}

data class AssignmentExpr(val left: Identifier, val right: Expr) : Expr {
    override fun eval(interpreter: Interpreter): ExprResult {
        interpreter.memory[left.value] = right.eval(interpreter).number
        return ExprResult.None
    }
}

data class CallExpr(val name: Identifier, val isProcedure: Boolean, val arguments: List<Expr>) : Expr {
    override fun eval(interpreter: Interpreter): ExprResult {
        val result = interpreter.call(name.value, arguments.map { it.eval(interpreter).number })

        return if (isProcedure) ExprResult.None
        else ExprResult.Number(result ?: error("Function did not return number"))
    }
}

data class BlockExpr(val contents: List<Expr>) : Expr {
    override fun eval(interpreter: Interpreter): ExprResult {
        contents.forEach { it.eval(interpreter) }
        return ExprResult.None
    }
}