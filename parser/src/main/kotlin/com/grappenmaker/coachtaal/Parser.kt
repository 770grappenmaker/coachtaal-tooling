package com.grappenmaker.coachtaal

import kotlin.math.pow

fun parseProgram(contents: String, language: Language = DutchLanguage) = parseProgram(lexer(contents), language)
fun parseProgram(tokens: List<Token>, language: Language = DutchLanguage) = Parser(tokens, language).parseFull()

fun parseExpression(tokens: List<Token>, language: Language) = parseSingle(tokens, language) { compare() }
fun parseStatement(tokens: List<Token>, language: Language) = parseSingle(tokens, language) { statement() }
fun parseBlock(tokens: List<Token>, language: Language) = parseSingle(tokens, language) { parseFull() }

private inline fun <T> parseSingle(tokens: List<Token>, language: Language, method: Parser.() -> T): T {
    val parser = Parser(tokens, language)
    return parser.method().also {
        require(parser.isAtEnd) {
            "parsing statement had dangling tokens: ${parser.tokens.drop(parser.ptr)}" +
                    ", consumed ${parser.tokens.take(parser.ptr)}"
        }
    }
}

private fun unexpected(token: Token, extra: String? = null): Nothing =
    error("Unexpected token ${token.info.javaClass.simpleName} " +
            "at line ${token.line}, column ${token.column}, \"${token.lexeme}\"" +
            if (extra != null)  ": $extra" else ""
    )

class Parser(val tokens: List<Token>, private val language: Language = DutchLanguage) {
    var ptr = 0
    val isAtEnd get() = ptr !in tokens.indices

    // Can do ptr - 1 because of the empty check at the start
    private fun eofError(message: String? = null) = require(!isAtEnd) {
        val token = tokens[ptr - 1]
        "Unexpected EOF at line ${token.line}, col ${token.column}: ${message ?: "this is a bug"}"
    }

    // Returns the current token and goes to the next
    private fun take(expectedMessage: String? = null, skipNewLine: Boolean = true): Token {
        eofError(expectedMessage)
        while (skipNewLine && tokens[ptr].info is NewLineToken) {
            advance()
            eofError(expectedMessage)
        }

        val returned = tokens[ptr]
        advance()
        return returned
    }

    // Returns the current token
    private fun peek(expectedMessage: String? = null, skipNewLine: Boolean = true): Token {
        eofError(expectedMessage)

        // Do not want to alter state, use temporary variable
        var tempPtr = ptr
        while (skipNewLine && tokens[tempPtr].info is NewLineToken && tempPtr + 1 in tokens.indices) tempPtr++
        return tokens[tempPtr]
    }

    // Returns all next tokens satisfying [cond]
    private fun takeWhile(skipNewLine: Boolean = true, cond: (TokenInfo) -> Boolean): List<Token> {
        if (isAtEnd) return emptyList()

        val start = ptr
        while (ptr + 1 in tokens.indices &&
            (skipNewLine && tokens[ptr + 1].info is NewLineToken || cond(tokens[ptr + 1].info))
        ) advance()

        advance()
        return tokens.slice(start until ptr)
    }

    // Skips all newlines
    private fun skipNewLine() {
        while (!isAtEnd && tokens[ptr].info is NewLineToken) advance()
    }

    private fun advance() {
        ptr++
    }

    private fun primary(): Expr {
        val curr = take("expected identifier, number or parenthesized expression")
        if (!isAtEnd && peek().info is GroupToken) {
            if (curr.info !is Identifier) unexpected(curr, "parenthesized expression after non-identifier")
            return call(curr)
        }

        return when (val info = curr.info) {
            is Identifier -> IdentifierExpr(info)
            is NumberToken -> LiteralExpr(info.value)
            is GroupToken -> parseExpression(info.tokens, language)
            else -> unexpected(curr, "expected identifier, number or parenthesized expression")
        }
    }

    private fun binaryOperator(
        parseLeft: () -> Expr,
        parseRight: () -> Expr = parseLeft,
        operators: Set<String>
    ): Expr {
        var first = parseLeft()

        while (!isAtEnd) {
            val operator = peek()
            val info = if (operator.info is EqualsToken) BinaryOperatorToken("=")
            else operator.info as? BinaryOperatorToken ?: break

            if (info.operator !in operators) break
            advance()

            first = BinaryOperatorExpr(first, parseRight(), findOperator(info), info.operator)
        }

        return first
    }

    private fun power() = binaryOperator(parseLeft = ::primary, parseRight = ::unary, operators = setOf("^"))
    private fun unary(): Expr {
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

    private fun multiply() = binaryOperator(parseLeft = ::unary, operators = setOf("*", "/", "&&"))
    private fun add() = binaryOperator(parseLeft = ::multiply, operators = setOf("+", "-", "||"))
    fun compare() = binaryOperator(parseLeft = ::add, operators = setOf("=", "<>", "<", ">", "<=", ">="))

    private fun assignment(id: Identifier) = AssignmentExpr(id, compare())

    private fun ifStatement(): Expr {
        val condition = takeWhile { it !is Identifier || it.value.lowercase() != language.ifThen }

        eofError("""no "${language.ifThen}" after "als"""")
        advance()

        val expectedConditionals = setOf(language.elseStatement, language.endIfStatement)
        val whenTrue = takeWhile { it !is Identifier || it.value.lowercase() !in expectedConditionals }
        eofError("""no "${language.elseStatement}" or "${language.endIfStatement}" after "${language.ifThen}"""")

        val seenEnd = (peek().info as? Identifier)?.value == language.endIfStatement
        advance()

        val expectElse = !seenEnd && !isAtEnd
        val whenFalse = if (expectElse) takeWhile {
            it !is Identifier || it.value.lowercase() != language.endIfStatement
        } else null

        if (expectElse) {
            eofError("""expected "${language.endIfStatement}" after "${language.elseStatement}"""")
            advance()
        }

        return ConditionalExpr(
            condition = parseExpression(condition, language),
            whenTrue = parseBlock(whenTrue, language),
            whenFalse = whenFalse?.let { parseBlock(it, language) }
        )
    }

    private fun call(idToken: Token): Expr {
        val id = idToken.info as? Identifier ?: unexpected(idToken, "expected an identifier in a call")
        if (id.value.lowercase() == language.ifStatement) return ifStatement()
        if (isAtEnd) return CallExpr(id, emptyList(), idToken)

        val curr = peek()
        val info = curr.info

        if (info is GroupToken) advance()
        val arguments = (info as? GroupToken)?.verify()?.tokens
            ?.split { it.info is ParameterSeparatorToken } ?: emptyList()

        return CallExpr(id, arguments.map { parseExpression(it, language) }, idToken)
    }

    private fun GroupToken.verify(): GroupToken {
        val last = tokens.lastOrNull()
        if (last?.info is ParameterSeparatorToken) unexpected(last, "trailing comma in function call")

        unexpected(
            (tokens.windowed(2).find { it.all { t -> t.info is ParameterSeparatorToken } } ?: return this).first(),
            "double comma in parameter list"
        )
    }

    fun statement(): Expr {
        val currToken = take()
        val identifier = currToken.info as? Identifier
            ?: unexpected(currToken, "statements should start with identifiers")

        return if (!isAtEnd) {
            val info = peek().info
            if (info is EqualsToken || info is AssignmentToken) {
                advance()
                assignment(identifier)
            } else call(currToken)
        } else call(currToken)
    }

    fun parseFull() = if (tokens.isEmpty()) emptyList() else buildList {
        skipNewLine()

        while (!isAtEnd) {
            val newStatement = statement()
            add(newStatement)

            if (!isAtEnd && tokens[ptr].info !is NewLineToken) unexpected(tokens[ptr], "after statement $newStatement")
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
    val operator: (Float, Float) -> Float,
    val operatorToken: String,
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

data class CallExpr(val name: Identifier, val arguments: List<Expr>, val debug: Token) : Expr {
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

data class ParsedProgram(val lines: List<Expr>, val language: Language)

fun ParsedProgram.eval(interpreter: Interpreter) = lines.eval(interpreter)
fun List<Expr>.eval(interpreter: Interpreter) = forEach { it.eval(interpreter) }

fun List<Expr>.extractVariables(language: Language = DutchLanguage): Set<String> =
    flatMapTo(hashSetOf()) { it.extractVariables(language) }
        .filterTo(hashSetOf()) { it.lowercase() !in language.allBuiltins }

fun Expr.extractVariables(language: Language = DutchLanguage): Set<String> = when (this) {
    is AssignmentExpr -> setOf(left.value) + right.extractVariables(language)
    is BinaryOperatorExpr -> left.extractVariables(language) + right.extractVariables(language)
    is CallExpr -> arguments.extractVariables(language)
    is ConditionalExpr -> condition.extractVariables(language) + whenTrue.extractVariables(language) +
            (whenFalse?.extractVariables(language) ?: emptySet())
    is IdentifierExpr -> setOf(value.value)
    else -> emptySet()
}