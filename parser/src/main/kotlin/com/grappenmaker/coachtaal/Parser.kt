package com.grappenmaker.coachtaal

import kotlin.math.ceil
import kotlin.math.pow

class ParseException(
    message: String,
    val token: Token,
    cause: Throwable? = null,
) : IllegalStateException(message, cause)

class ParseFailedException(message: String) : IllegalStateException(message)

inline fun <reified V : Any, T> Iterable<T>.partitionIs(): Pair<List<V>, List<T>> {
    val resA = mutableListOf<V>()
    val resB = mutableListOf<T>()

    forEach { if (it is V) resA += it else resB += it }
    return resA to resB
}

fun parseProgram(contents: String, language: Language): ParsedProgram {
    val parser = Parser(lexer(contents), language, contents)
    val (functions, lines) = parser.parseFull().partitionIs<FunctionExpr, _>()
    parser.validateFunctions(functions)
    return ParsedProgram(lines, functions, language)
}

fun parseExpression(
    tokens: List<Token>,
    language: Language,
    originalCode: String? = null,
) = parseSingle(tokens, language, originalCode) { compare().also { it.assertNoFunctions() } }

fun parseBlock(
    tokens: List<Token>,
    language: Language,
    originalCode: String? = null
) = if (tokens.isEmpty()) emptyList() else parseSingle(tokens, language, originalCode) {
    parseFull().also { it.assertNoFunctions() }
}

private inline fun <T> parseSingle(
    tokens: List<Token>,
    language: Language,
    originalCode: String? = null,
    method: Parser.() -> T
): T {
    val parser = Parser(tokens, language, originalCode)
    return parser.method().also {
        require(parser.isAtEnd) {
            "parsing statement had dangling tokens: ${parser.tokens.drop(parser.ptr)}" +
                    ", consumed ${parser.tokens.take(parser.ptr)}"
        }
    }
}

fun Expr.humanFriendly() = when (this) {
    is AssignmentExpr -> "assignment of ${left.value}"
    is BinaryOperatorExpr -> "binary operator $operatorToken"
    is CallExpr -> "function call ${name.value}"
    is ConditionalExpr -> "if-statement"
    is IdentifierExpr -> "identifier ${value.value}"
    is LiteralExpr -> "float literal $value"
    is NotExpr -> "unary not expression"
    is RepeatUntilExpr -> "repeat until expression"
    is RepeatingExpr -> "repeated expression"
    is UnaryMinusExpr -> "unary minus expression"
    is WhileExpr -> "while expression"
    is FunctionExpr -> "function \"${name.value}\" expression"
    is NewLineExpr -> "new line"
}

class Parser(
    val tokens: List<Token>,
    private val language: Language = DutchLanguage,
    private val originalCode: String? = null,
) {
    private val caughtErrors = mutableListOf<ParseException>()
    var ptr = 0
    val isAtEnd get() = ptr !in tokens.indices

    // Can do ptr - 1 because of the empty check at the start
    private fun eofError(message: String? = null) = check(!isAtEnd) {
        unexpected(
            tokens.getOrNull(ptr - 1) ?: error("Unexpected EOF in empty file"),
            "unexpected EOF: ${message ?: "this is a bug"}"
        )
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

    private fun takeStacking(open: String, close: String): List<Token> {
        var count = 1
        val res = mutableListOf<Token>()

        while (!isAtEnd) {
            val candidate = take(skipNewLine = false)
            val info = candidate.info
            if (info is Identifier) {
                if (info.value == open) count++
                if (info.value == close) count--
            }

            if (count == 0) return res
            res += candidate
        }

        eofError("expected $close after $open")
        error("Should be impossible???")
    }

    // just for if-else-endif :)
    private fun takeStackingSpecial(): Pair<List<Token>, List<Token>> {
        var count = 1
        var seenElse = false
        val resA = mutableListOf<Token>()
        val resB = mutableListOf<Token>()

        while (!isAtEnd) {
            val candidate = take(skipNewLine = false)
            val info = candidate.info
            if (info is Identifier) {
                if (info.value == language.ifStatement) count++
                if (info.value == language.endIfStatement) count--
                if (count == 1 && info.value == language.elseStatement) seenElse = true
            }

            if (count == 0) return resA to resB
            (if (seenElse) resB else resA) += candidate
        }

        eofError(
            if (seenElse) "expected ${language.endIfStatement} after ${language.elseStatement}"
            else "expected ${language.endIfStatement} or ${language.elseStatement} after ${language.ifStatement}"
        )
        error("Should be impossible???")
    }

    // Returns all next tokens satisfying [cond]
    private fun takeWhile(skipNewLine: Boolean = false, cond: (TokenInfo) -> Boolean) = generateSequence {
        if (isAtEnd) null else peek(skipNewLine = skipNewLine)
            .takeIf { cond(it.info) }
            ?.also { take(skipNewLine = skipNewLine) }
    }.toList()

    // Skips all newlines
    private inline fun skipNewLine(onEach: () -> Unit = {}) {
        while (!isAtEnd && tokens[ptr].info is NewLineToken) {
            advance()
            onEach()
        }
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
            is GroupToken -> parseExpressionOrError(curr, info.tokens)
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
            val operator = peek("one of the following operators expected: $operators")
            val info = if (operator.info is EqualsToken) BinaryOperatorToken("=")
            else operator.info as? BinaryOperatorToken ?: break

            if (info.operator !in operators) break
            take()

            first = BinaryOperatorExpr(first, parseRight(), findOperator(info), info.operator)
        }

        return first
    }

    private fun power() = binaryOperator(parseLeft = ::primary, parseRight = ::unary, operators = setOf("^"))
    private fun unary(): Expr {
        val curr = peek("expected an expression")
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

    private fun parseExpressionOrError(token: Token, block: List<Token>): Expr {
        if (block.isEmpty()) unexpected(token, "expected an expression after it")
        return parseExpression(block, language, originalCode)
    }

    private fun ifStatement(token: Token): Expr {
        val condition = takeWhile { it !is Identifier || it.value.lowercase() != language.ifThen }
        take("""no "${language.ifThen}" after "${language.ifStatement}"""")
        val (whenTrue, whenFalse) = takeStackingSpecial()
//
//        val expectedConditionals = setOf(language.elseStatement, language.endIfStatement)
//        val whenTrue = takeWhile { it !is Identifier || it.value.lowercase() !in expectedConditionals }
//        val tentativeEnd =
//            take("""no "${language.elseStatement}" or "${language.endIfStatement}" after "${language.ifThen}"""")
//
//        val seenEnd = (tentativeEnd.info as? Identifier)?.value == language.endIfStatement
//        val expectElse = !seenEnd && !isAtEnd
//        val whenFalse = if (expectElse) takeWhile {
//            it !is Identifier || it.value.lowercase() != language.endIfStatement
//        } else null
//
//        if (expectElse) take("""expected "${language.endIfStatement}" after "${language.elseStatement}"""")

        return ConditionalExpr(
            condition = parseExpressionOrError(token, condition),
            whenTrue = parseBlock(whenTrue, language, originalCode),
            whenFalse = whenFalse.takeIf { it.isNotEmpty() }?.let { parseBlock(it, language, originalCode) }
        )
    }

    private fun redoStatement(): Expr {
        // Weirdly enough, *any* expression is valid, not only literals
        val amount = compare()
        val body = takeStacking(language.redoStatement, language.endRedo)
        return RepeatingExpr(parseBlock(body, language, originalCode), amount)
    }

    private fun whileStatement(token: Token): Expr {
        val condition = takeWhile { it !is Identifier || it.value.lowercase() != language.startDo }
        take("""no "${language.startDo}" after "${language.whileStatement}"""")

        val body = takeStacking(language.startDo, language.endDo)
        return WhileExpr(
            condition = parseExpressionOrError(token, condition),
            body = parseBlock(body, language, originalCode),
        )
    }

    private fun repeatStatement(): Expr {
        val body = takeStacking(language.doWhileStatement, language.doWhileUntil)
        return RepeatUntilExpr(compare(), parseBlock(body, language, originalCode))
    }

    private fun function(openName: String, endName: String): Expr {
        val nameToken = take("function declaration without name")
        val name = nameToken.info as? Identifier ?: unexpected(nameToken, "expected an identifier for a function name")
        if (name.value in language.allBuiltins) error("Illegal function name ${name.value}")

        // Could technically not be CallExpr but we are smarter than the compiler here
        val arguments = (call(nameToken) as CallExpr).arguments
        val argumentNames = arguments.filterIsInstance<IdentifierExpr>().map { it.value }
        if (arguments.size != argumentNames.size)
            unexpected(nameToken, "Invalid parameter declaration for function ${name.value}")

        val body = takeStacking(openName, endName)
        return FunctionExpr(name, argumentNames, parseBlock(body, language, originalCode), nameToken)
    }

    private fun call(idToken: Token): Expr {
        val id = idToken.info as? Identifier ?: unexpected(idToken, "expected an identifier in a call")
        when (id.value.lowercase()) {
            language.startProcedure -> {
                println(
                    "Warning: procedures behave like functions in this implementation of Coach. " +
                            "Use ${language.startFunction} instead of ${language.startProcedure}."
                )

                return function(language.startProcedure, language.endProcedure)
            }

            language.startFunction -> return function(language.startFunction, language.endFunction)
            language.ifStatement -> return ifStatement(idToken)
            language.redoStatement -> return redoStatement()
            language.whileStatement -> return whileStatement(idToken)
            language.doWhileStatement -> return repeatStatement()
            language.onceInvalidStatement -> unexpected(
                idToken, "\"${language.onceInvalidStatement}\" is disabled, " +
                        "since it produces side effects that are not accessible from within the model."
            )
        }

        if (isAtEnd) return CallExpr(id, emptyList(), idToken)

        val curr = peek()
        val info = curr.info

        if (info is GroupToken) take()
        val arguments = (info as? GroupToken)?.verify()?.tokens
            ?.split { it.info is ParameterSeparatorToken } ?: emptyList()

        return CallExpr(id, arguments.map { parseExpressionOrError(curr, it) }, idToken)
    }

    private fun GroupToken.verify(): GroupToken {
        val last = tokens.lastOrNull()
        if (last?.info is ParameterSeparatorToken) unexpected(last, "trailing separator in function call")

        unexpected(
            (tokens.windowed(2).find { it.all { t -> t.info is ParameterSeparatorToken } } ?: return this).first(),
            "double separator in parameter list"
        )
    }

    fun statement(): Expr {
        val currToken = take()
        val identifier = currToken.info as? Identifier
            ?: unexpected(currToken, "lone expressions are not allowed; statements should start with identifiers")

        return if (!isAtEnd) {
            val info = peek().info
            if (info is EqualsToken || info is AssignmentToken) {
                advance()
                assignment(identifier)
            } else call(currToken)
        } else call(currToken)
    }

    private fun recover() {
        takeWhile(skipNewLine = false) { it !is NewLineToken }
    }

    fun parseFull() = if (tokens.isEmpty()) emptyList() else buildList {
        skipNewLine()

        while (!isAtEnd) {
            try {
                val newStatement = statement()
                add(newStatement)

                if (!isAtEnd && tokens[ptr].info !is NewLineToken) {
                    unexpected(tokens[ptr], "after statement \"${newStatement.humanFriendly()}\", expected newline")
                }

                advance()
                skipNewLine { add(NewLineExpr) }
            } catch (e: ParseException) {
                caughtErrors += e
                recover()
            } catch (e: ParseFailedException) {
                caughtErrors += e.suppressedExceptions.filterIsInstance<ParseException>()
                recover()
            }
        }

        if (caughtErrors.isNotEmpty()) {
            val target = ParseFailedException("Parsing failed: ${caughtErrors.size} errors were found")
            caughtErrors.forEach { target.addSuppressed(it) }
            throw target
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

    private fun unexpected(token: Token, extra: String? = null): Nothing {
        val header = "Unexpected token ${token.info.javaClass.simpleName} " +
                "at line ${token.line}, column ${token.column}, \"${token.lexeme}\"" +
                if (extra != null) ": $extra" else ""

        val context = originalCode?.let {
            val targetLine = it.lines()[token.line - 1]

            "\n\n" + """
                $targetLine
                ${" ".repeat(token.column - 1)}^ here
            """.trimIndent()
        } ?: ""

        throw ParseException(header + context, token)
    }

    fun List<Expr>.assertNoFunctions() = forEach { it.assertNoFunctions() }
    fun Expr.assertNoFunctions(): Unit = when (this) {
        is ConditionalExpr -> {
            whenFalse?.assertNoFunctions()
            whenTrue.assertNoFunctions()
        }

        is FunctionExpr -> unexpected(debug, "function not allowed in code block")
        is RepeatUntilExpr -> body.assertNoFunctions()
        is RepeatingExpr -> body.assertNoFunctions()
        is WhileExpr -> body.assertNoFunctions()
        else -> {}
    }

    fun validateFunctions(functions: List<FunctionExpr>) {
        functions.groupBy { it.name.value }.values.find { it.size > 1 }?.let { illegalFunctions ->
            val culprit = illegalFunctions[1]
            unexpected(culprit.debug, "duplicate function definition with name \"${culprit.name.value}\"")
        }
    }
}

sealed interface ExprResult {
    data object None : ExprResult
    data class Number(val value: Float) : ExprResult

    val number get() = (this as? Number)?.value ?: error("Expression $this does not yield a number!")
    val boolean get() = number.asCoachBoolean

    companion object {
        operator fun invoke(value: Float?) = if (value == null) None else Number(value)
    }
}

sealed interface Expr {
    fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>): ExprResult
}

data class BinaryOperatorExpr(
    val left: Expr,
    val right: Expr,
    val operator: (Float, Float) -> Float,
    val operatorToken: String,
) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) =
        ExprResult.Number(operator(left.eval(interpreter, scope).number, right.eval(interpreter, scope).number))
}

data class UnaryMinusExpr(val on: Expr) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) =
        ExprResult.Number(-on.eval(interpreter, scope).number)
}

data class NotExpr(val on: Expr) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) =
        ExprResult.Number((!on.eval(interpreter, scope).boolean).asCoach)
}

data class AssignmentExpr(val left: Identifier, val right: Expr) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>): ExprResult {
        val lhs = left.value
        val rhs = right.eval(interpreter, scope).number

        // coach logic
        if (lhs in scope) scope[lhs] = rhs else interpreter[lhs] = rhs

        return ExprResult.None
    }
}

data class CallExpr(val name: Identifier, val arguments: List<Expr>, val debug: Token) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) =
        ExprResult(interpreter.call(name.value, arguments.map { it.eval(interpreter, scope).number }))
}

data class IdentifierExpr(val value: Identifier) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) =
        ExprResult.Number(scope[value.value] ?: interpreter[value.value])
}

data class LiteralExpr(val value: Float) : Expr {
    private val result = ExprResult.Number(value)
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) = result
}

data class ConditionalExpr(val condition: Expr, val whenTrue: List<Expr>, val whenFalse: List<Expr>?) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>): ExprResult {
        (if (condition.eval(interpreter, scope).boolean) whenTrue else whenFalse)?.eval(interpreter, scope)
        return ExprResult.None
    }
}

data class RepeatingExpr(val body: List<Expr>, val repetitions: Expr) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>): ExprResult {
        repeat(ceil(repetitions.eval(interpreter, scope).number).toInt()) { body.eval(interpreter, scope) }
        return ExprResult.None
    }
}

data class WhileExpr(val condition: Expr, val body: List<Expr>) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>): ExprResult {
        while (condition.eval(interpreter, scope).boolean) body.eval(interpreter, scope)
        return ExprResult.None
    }
}

data class RepeatUntilExpr(val condition: Expr, val body: List<Expr>) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>): ExprResult {
        do body.eval(interpreter, scope) while (!condition.eval(interpreter, scope).boolean)
        return ExprResult.None
    }
}

data class FunctionExpr(
    val name: Identifier,
    val parameters: List<Identifier>,
    val body: List<Expr>,
    val debug: Token,
) : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) =
        error("Function expressions should never be evaluated! This is a bug")
}

// Fake expr lol
data object NewLineExpr : Expr {
    override fun eval(interpreter: Interpreter, scope: MutableMap<String, Float>) = ExprResult.None
}

data class ParsedProgram(
    val lines: List<Expr>,
    val functions: List<FunctionExpr>,
    val language: Language
)

fun ParsedProgram.eval(interpreter: Interpreter) = lines.eval(interpreter, mutableMapOf())

fun List<Expr>.eval(interpreter: Interpreter, scope: MutableMap<String, Float>) =
    forEach { it.eval(interpreter, scope) }

fun ParsedProgram.findNonConstant(): Set<Identifier> {
    val analysis = analyzeVariables()
    return analysis.assignments - analysis.constants.keys
}

fun ParsedProgram.extractVariables() = lines.extractVariables(language) - functions.mapTo(hashSetOf()) { it.name }
fun List<Expr>.extractVariables(language: Language = DutchLanguage): Set<Identifier> =
    flatMapTo(hashSetOf()) { it.extractVariables(language) }
        .filterTo(hashSetOf()) { it.value.lowercase() !in language.allBuiltins }

fun Expr.extractVariables(language: Language = DutchLanguage): Set<Identifier> = when (this) {
    is AssignmentExpr -> setOf(left) + right.extractVariables(language)
    is BinaryOperatorExpr -> left.extractVariables(language) + right.extractVariables(language)
    is CallExpr -> arguments.extractVariables(language)
    is ConditionalExpr -> condition.extractVariables(language) + whenTrue.extractVariables(language) +
            (whenFalse?.extractVariables(language) ?: emptySet())

    is IdentifierExpr -> setOf(value)
    is NotExpr -> on.extractVariables(language)
    is RepeatUntilExpr -> body.extractVariables(language) + condition.extractVariables(language)
    is RepeatingExpr -> body.extractVariables(language) + repetitions.extractVariables(language)
    is UnaryMinusExpr -> on.extractVariables(language)
    is WhileExpr -> condition.extractVariables(language) + body.extractVariables(language)
    else -> emptySet()
}