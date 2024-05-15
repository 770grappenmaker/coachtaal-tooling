package com.grappenmaker.coachtaal

fun lexer(input: String, language: Language): List<Token> {
    val nameTokens = mapOf(
        language.assignmentLiteral to AssignmentToken,
        language.notOperator to NotToken,
        language.orOperator to BinaryOperatorToken("||"),
        language.andOperator to BinaryOperatorToken("&&"),
    )

    // oh no parser != lexer
    // TODO: we need to clean this shit up
    val lexingErrors = mutableListOf<ParseException>()

    var ptr = 0
    var lineStart = 0
    var line = 1

    data class GroupStart(val ptr: Int, val size: Int, val line: Int, val col: Int)
    val groupStack = ArrayDeque<GroupStart>()

    val result = mutableListOf<Token>()

    fun takeWhile(cond: (Char) -> Boolean): String {
        val start = ptr
        while (ptr + 1 in input.indices && cond(input[ptr + 1])) ptr++
        return input.slice(start..ptr)
    }

    fun takeDigits() = takeWhile { it.isDigit() }

    fun match(n: Int = 1, cond: (Char) -> Boolean): Boolean {
        if (ptr + n !in input.indices || !cond(input[ptr + n])) return false
        ptr += n
        return true
    }

    fun match(next: Char, n: Int = 1) = match(n) { it == next }
    fun matchDigit(n: Int = 1): Boolean = match(n) { it.isDigit() }

    while (ptr in input.indices) {
        val oldPtr = ptr

        fun createToken(info: TokenInfo) = Token(info, input.slice(oldPtr..ptr), line, oldPtr - lineStart + 1)
        fun token(info: TokenInfo) {
            result += createToken(info)
        }

        fun conditionalToken(info: TokenInfo, condition: Boolean, message: String) {
            val token = createToken(info)
            if (condition) result += token else {
                val header = "Illegal character (as $info) while lexing at line ${token.line}, " +
                        "column ${token.column}, \"${token.lexeme}\": $message"

                val targetLine = input.lines()[token.line - 1]
                val context = "\n\n" + """
                    $targetLine
                    ${" ".repeat(token.column - 1)}^ here
                """.trimIndent()

                lexingErrors += ParseException(header + context, token)
            }
        }

        val c = input[ptr]
        when {
            c.isDigit() -> {
                val firstPart = takeDigits()
                val fullNumber =
                    if ((match(',') || match('.')) && matchDigit())"$firstPart.${takeDigits()}" else firstPart

                val withExponent = if ((match('e') || match('E')) && matchDigit()) "${fullNumber}e${takeDigits()}"
                else fullNumber

                token(NumberToken(withExponent.toFloat()))
            }
            c.isValidIdentifier() -> {
                val fullIdentifier = takeWhile { it.isValidIdentifier() }
                token(nameTokens[fullIdentifier.lowercase()] ?: Identifier(fullIdentifier))
            }
            else -> when (c) {
                '\t', '\r', ' ' -> { /* ignore whitespace */ }
                '\'' -> {
                    val comment = takeWhile { it != '\n' }.removePrefix("'").trim()
                    token(EOLToken(comment))
                    ptr++
                    line++
                    lineStart = ptr + 1
                }
                '\n' -> {
                    token(EOLToken())
                    line++
                    lineStart = ptr + 1
                }

                '+', '-', '*', '/', '^' -> token(BinaryOperatorToken(c.toString()))
                '!', '>' -> token(BinaryOperatorToken(if (match('=')) "$c=" else c.toString()))
                '<' -> token(BinaryOperatorToken(
                    when {
                        match('=') -> "<="
                        match('>') -> "<>"
                        else -> "<"
                    }
                ))
                '=' -> token(EqualsToken)
                '(' -> groupStack.addLast(GroupStart(ptr, result.size, line, ptr - lineStart + 1))
                ')' -> {
                    val start = groupStack.removeLastOrNull() ?: error("Unmatched )")
                    val grouped = result.removeLastN(result.size - start.size)
                    result += Token(GroupToken(grouped), input.slice(start.ptr..ptr), start.line, start.col)
                }
                ';' -> conditionalToken(ParameterSeparatorToken, groupStack.isNotEmpty(), "; outside of parentheses")
                ':' -> conditionalToken(AssignmentToken, match('='), ": without = (assignment operator)")
                else -> conditionalToken(InvalidToken, false, "illegal identifier")
            }
        }

        ptr++
    }

    if (lexingErrors.isNotEmpty()) {
        val target = ParseFailedException("Lexing failed: ${lexingErrors.size} errors were found")
        lexingErrors.forEach { target.addSuppressed(it) }
        throw target
    }

    return result
}

data class Token(
    val info: TokenInfo,
    val lexeme: String,
    val line: Int,
    val column: Int
)

sealed interface TokenInfo
val TokenInfo.humanReadable get() = when (this) {
    AssignmentToken -> "assignment"
    is BinaryOperatorToken -> "binary operator"
    is EOLToken -> "end-of-line"
    EqualsToken -> "equals"
    is GroupToken -> "group"
    is Identifier -> "identifier"
    InvalidToken -> "invalid token"
    NotToken -> "unary not"
    is NumberToken -> "number literal"
    ParameterSeparatorToken -> "separator"
}

data class EOLToken(val comment: String? = null) : TokenInfo
data object AssignmentToken : TokenInfo
data object InvalidToken : TokenInfo
data object NotToken : TokenInfo
data object ParameterSeparatorToken : TokenInfo

// We don't know what the equals token means until parsing
data object EqualsToken : TokenInfo

data class NumberToken(val value: Float) : TokenInfo
data class BinaryOperatorToken(val operator: String) : TokenInfo
data class GroupToken(val tokens: List<Token>) : TokenInfo

fun List<Token>.find(line: Int, char: Int): Token? {
    var min = 0
    var max = lastIndex

    while (min <= max) {
        val pivot = (min + max) / 2
        val target = this[pivot]

        when {
            target.line == line -> when {
                target.column > char -> max = pivot - 1
                char in target.column..<target.column + target.lexeme.length -> return target
                else -> min = pivot + 1
            }

            target.line < line -> min = pivot + 1
            else -> max = pivot - 1
        }
    }

    return null
}