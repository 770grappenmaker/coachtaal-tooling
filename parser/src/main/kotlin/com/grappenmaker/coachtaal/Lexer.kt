package com.grappenmaker.coachtaal

fun lexer(input: String, language: Language = EnglishLanguage): List<Token> {
    val nameTokens = mapOf(
        language.assignmentLiteral to AssignmentToken,
        language.notOperator to NotToken,
        language.orOperator to BinaryOperatorToken("||"),
        language.andOperator to BinaryOperatorToken("&&"),
    )

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

    fun skip(target: Char) {
        while (ptr + 1 in input.indices && input[ptr + 1] != target) ptr++
    }

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
                '\'' -> skip('\n')
                '\n' -> {
                    token(NewLineToken)
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
                ';' -> {
                    require(groupStack.isNotEmpty()) { "; outside of parentheses" }
                    token(ParameterSeparatorToken)
                }
                ':' -> {
                    require(match('=')) { ": without = (assignment operator)" }
                    token(AssignmentToken)
                }
                else -> {
                    val debugInfo = createToken(InvalidToken)
                    error("Don't know what to do with \"${debugInfo.lexeme}\" at ${debugInfo.line}:${debugInfo.column}")
                }
            }
        }

        ptr++
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

data object NewLineToken : TokenInfo
data object AssignmentToken : TokenInfo
data object InvalidToken : TokenInfo
data object NotToken : TokenInfo
data object ParameterSeparatorToken : TokenInfo

// We don't know what the equals token means until parsing
data object EqualsToken : TokenInfo

data class NumberToken(val value: Float) : TokenInfo
data class BinaryOperatorToken(val operator: String) : TokenInfo
data class GroupToken(val tokens: List<Token>) : TokenInfo