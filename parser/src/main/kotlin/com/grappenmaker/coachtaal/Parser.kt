package com.grappenmaker.coachtaal

import kotlin.math.pow

fun parseProgram(contents: String) = parseProgram(lexer(contents))
fun parseProgram(tokens: List<Token>) = Parser(tokens).parseFull()

fun parseExpression(tokens: List<Token>) = parseSingle(tokens) { compare() }
fun parseStatement(tokens: List<Token>) = parseSingle(tokens) { statement() }
fun parseBlock(tokens: List<Token>) = parseSingle(tokens) { parseFull() }

private inline fun <T> parseSingle(tokens: List<Token>, method: Parser.() -> T): T {
    val parser = Parser(tokens)
    return parser.method().also {
        require(parser.isAtEnd) {
            "parsing statement had dangling tokens: ${parser.tokens.drop(parser.ptr)}" +
                    ", consumed ${parser.tokens.take(parser.ptr)}"
        }
    }
}

private fun unexpected(token: Token): Nothing =
    error("Unexpected token ${token.info.javaClass.simpleName} at ${token.line}:${token.column}, \"${token.lexeme}\"")

class Parser(val tokens: List<Token>) {
    var ptr = 0
    val isAtEnd get() = ptr !in tokens.indices

    // Can do ptr - 1 because of the empty check at the start
    private fun eofError() = require(!isAtEnd) {
        val token = tokens[ptr - 1]
        "Unexpected EOF at ${token.line}:${token.column}"
    }

    // Returns the current token and goes to the next
    private fun take(skipNewLine: Boolean = true): Token {
        eofError()
        while (skipNewLine && tokens[ptr].info is NewLineToken) {
            advance()
            eofError()
        }

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

    // Returns all next tokens satisfying [cond]
    fun takeWhile(skipNewLine: Boolean = true, cond: (TokenInfo) -> Boolean): List<Token> {
        val start = ptr
        while (ptr + 1 in tokens.indices &&
            (skipNewLine && tokens[ptr + 1].info is NewLineToken || cond(tokens[ptr + 1].info))
        ) advance()

        return tokens.slice(start..ptr)
    }

    // Skips all newlines
    fun skipNewLine() {
        while (!isAtEnd && tokens[ptr].info is NewLineToken) advance()
    }

    private fun advance() {
        ptr++
    }

    fun primary(): Expr {
        val curr = take()
        if (!isAtEnd && peek().info is GroupToken)
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

        while (!isAtEnd) {
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

    fun multiply() = binaryOperator(parseLeft = ::unary, operators = setOf("*", "/", "&&"))
    fun add() = binaryOperator(parseLeft = ::multiply, operators = setOf("+", "-", "||"))
    fun compare() = binaryOperator(parseLeft = ::add, operators = setOf("=", "<>", "<", ">", "<=", ">="))

    fun assignment(id: Identifier) = AssignmentExpr(id, compare())

    fun ifStatement(): Expr {
        val condition = takeWhile { it !is Identifier || it.value.lowercase() != "dan" }
        eofError()

        advance()
        advance()

        val whenTrue = takeWhile { it !is Identifier || it.value.lowercase() !in setOf("anders", "eindals") }
        eofError()

        val seenEnd = (peek().info as? Identifier)?.value == "eindals"
        advance()

        val curr = if (!seenEnd && !isAtEnd) peek().info else null
        val whenFalse = if (curr is Identifier && curr.value.lowercase() == "anders") {
            advance()
            takeWhile { it !is Identifier || it.value.lowercase() != "eindals" }
        } else null

        eofError()
        advance()
        if (curr != null) advance()

        return ConditionalExpr(parseExpression(condition), parseBlock(whenTrue), whenFalse?.let(::parseBlock))
    }

    fun call(id: Identifier): Expr {
        if (id.value.lowercase() == "als") return ifStatement()
        if (isAtEnd) return CallExpr(id, emptyList())

        val curr = peek()
        val info = curr.info

        if (info is GroupToken) advance()
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

        return if (!isAtEnd) {
            val info = peek().info
            if (info is EqualsToken || info is AssignmentToken) {
                advance()
                assignment(identifier)
            } else call(identifier)
        } else call(identifier)
    }

    fun parseFull() = if (tokens.isEmpty()) emptyList() else buildList {
        while (!isAtEnd) {
            add(statement())
            if (!isAtEnd && tokens[ptr].info !is NewLineToken) unexpected(tokens[ptr])
            skipNewLine()
        }
    }

    private val binaryOperators = mapOf<String, (Float, Float) -> Float>(
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
        "&&" to { a, b -> (a.asCoachBoolean && b.asCoachBoolean).asCoach },
        "||" to { a, b -> (a.asCoachBoolean || b.asCoachBoolean).asCoach },
    )

    private fun findOperator(info: BinaryOperatorToken) = binaryOperators.getValue(info.operator)
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

data class ConditionalExpr(val condition: Expr, val whenTrue: List<Expr>, val whenFalse: List<Expr>?) : Expr {
    override fun eval(interpreter: Interpreter): ExprResult {
        (if (condition.eval(interpreter).number.asCoachBoolean) whenTrue else whenFalse)?.eval(interpreter)
        return ExprResult.None
    }
}

fun List<Expr>.eval(interpreter: Interpreter) = forEach { it.eval(interpreter) }