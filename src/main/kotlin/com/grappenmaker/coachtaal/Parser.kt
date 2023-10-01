package com.grappenmaker.coachtaal

import kotlin.math.pow

// Example
//fun main() {
//    val parsed = parseProgram(
//        """
//        b wordt 10
//        a wordt -b^-2 + 10 - 1 * 5
//        c wordt (-b^-2 + 10 - 1) * 5
//        d wordt (-b^(-2 + 10) - 1) * 5
//        e = PI
//        f = rand
//        g = rand(1;2;3)
//    """.trimIndent()
//    )
//
//    println(parsed)
//    println(Interpreter(parsed).apply { run() }.memory)
//}

fun parseProgram(contents: String): List<Expr> {
    val tokens = lexer(contents)
    if (tokens.isEmpty()) return emptyList()

    return Parser(tokens).parseFull()
}

fun parseExpression(tokens: List<Token>) = Parser(tokens, allowAssignment = false).compare()

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

fun findOperator(info: BinaryOperatorToken) = binaryOperators.getValue(info.operator)

private fun unexpected(token: Token): Nothing =
    error("Unexpected token ${token.info.javaClass.simpleName} at ${token.line}:${token.column}, \"${token.lexeme}\"")

class Parser(private val tokens: List<Token>, private val allowAssignment: Boolean = true) {
    private var ptr = 0

    // Can do ptr - 1 because of the empty check at the start
    private fun eofError() = require(ptr in tokens.indices) {
        val token = tokens[ptr - 1]
        "Unexpected EOF at ${token.line}:${token.column}"
    }

    // Returns the current token and goes to the next
    private fun take(skipNewLine: Boolean = true): Token {
        eofError()
        while (skipNewLine && tokens[ptr].info is NewLineToken && ptr + 1 in tokens.indices) advance()

        val returned = tokens[ptr]
        advance()
        return returned
    }

    // Returns the current token
    private fun peek(skipNewLine: Boolean = true): Token {
        eofError()

        // Do not want to alter state, use temporary variable
        var tempPtr = ptr
        while (skipNewLine && tokens[tempPtr].info is NewLineToken && tempPtr + 1 in tokens.indices) tempPtr++
        return tokens[tempPtr]
    }

    private fun advance() {
        ptr++
    }

    fun primary(): Expr {
        val curr = take()
        if (ptr in tokens.indices && peek().info is GroupToken)
            return call(curr.info as? Identifier ?: unexpected(curr))

        return when (val info = curr.info) {
            is Identifier -> IdentifierExpr(info)
            is NumberToken -> LiteralExpr(info.value)
            is GroupToken -> parseExpression(info.tokens)
            else -> unexpected(curr)
        }
    }

    fun binaryOperator(parseLeft: () -> Expr, parseRight: () -> Expr = parseLeft, operators: Set<String>): Expr {
        var first = parseLeft()

        while (ptr in tokens.indices) {
            val operator = peek()
            val info = if (operator.info is EqualsToken) BinaryOperatorToken("=")
            else operator.info as? BinaryOperatorToken ?: break

            if (info.operator !in operators) break
            advance()

            first = BinaryOperatorExpr(first, parseRight(), findOperator(info))
        }

        return first
    }

    fun power() = binaryOperator(parseLeft = ::primary, parseRight = ::unary, operators = setOf("^"))
    fun unary(): Expr {
        val curr = peek()
        val info = curr.info

        return when {
            info is BinaryOperatorToken && info.operator == "-" -> {
                advance()
                UnaryMinusExpr(unary())
            }

            info is NotToken -> {
                advance()
                NotExpr(unary())
            }

            else -> power()
        }
    }

    fun multiply() = binaryOperator(parseLeft = ::unary, operators = setOf("*", "/"))
    fun add() = binaryOperator(parseLeft = ::multiply, operators = setOf("+", "-"))
    fun compare() = binaryOperator(parseLeft = ::add, operators = setOf("=", "<>", "<", ">", "<=", ">="))

    fun assignment(id: Identifier) = AssignmentExpr(id, compare())

    fun call(id: Identifier): Expr {
        if (ptr !in tokens.indices) return CallExpr(id, emptyList())

        val curr = peek()
        val info = curr.info

        if (curr.info is GroupToken) advance()
        val arguments = (info as? GroupToken)?.verify()?.tokens
            ?.split { it.info is ParameterSeparatorToken } ?: emptyList()

        return CallExpr(id, arguments.map { parseExpression(it) })
    }

    private fun GroupToken.verify(): GroupToken {
        val last = tokens.lastOrNull()
        if (last?.info is ParameterSeparatorToken) unexpected(last)

        unexpected(
            (tokens.windowed(2).find { it.all { t -> t.info is ParameterSeparatorToken } } ?: return this).first()
        )
    }

    fun statement(): Expr {
        val currToken = take()
        val identifier = currToken.info as? Identifier ?: unexpected(currToken)

        return if (allowAssignment && ptr in tokens.indices) {
            val info = peek().info
            if (info is EqualsToken || info is AssignmentToken) {
                advance()
                assignment(identifier)
            } else call(identifier)
        } else call(identifier)
    }

    fun parseFull(): List<Expr> {
        val result = mutableListOf<Expr>()

        while (ptr in tokens.indices) {
            result += statement()
            if (ptr in tokens.indices && tokens[ptr].info !is NewLineToken) unexpected(tokens[ptr])
        }

        return result
    }
}

sealed interface ExprResult {
    data object None : ExprResult
    data class Number(val value: Float) : ExprResult

    val number get() = (this as? Number)?.value ?: error("Expression $this does not yield a number!")

    companion object {
        operator fun invoke(value: Float?) = if (value == null) None else Number(value)
    }
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

data class UnaryMinusExpr(val on: Expr) : Expr {
    override fun eval(interpreter: Interpreter) = ExprResult.Number(-on.eval(interpreter).number)
}

data class NotExpr(val on: Expr) : Expr {
    override fun eval(interpreter: Interpreter) =
        ExprResult.Number((!on.eval(interpreter).number.asCoachBoolean).asCoach)
}

data class AssignmentExpr(val left: Identifier, val right: Expr) : Expr {
    override fun eval(interpreter: Interpreter): ExprResult {
        interpreter[left.value] = right.eval(interpreter).number
        return ExprResult.None
    }
}

data class CallExpr(val name: Identifier, val arguments: List<Expr>) : Expr {
    override fun eval(interpreter: Interpreter) =
        ExprResult(interpreter.call(name.value, arguments.map { it.eval(interpreter).number }))
}

data class IdentifierExpr(val value: Identifier) : Expr {
    override fun eval(interpreter: Interpreter) = ExprResult.Number(interpreter[value.value])
}

data class LiteralExpr(val value: Float) : Expr {
    private val result = ExprResult.Number(value)
    override fun eval(interpreter: Interpreter) = result
}