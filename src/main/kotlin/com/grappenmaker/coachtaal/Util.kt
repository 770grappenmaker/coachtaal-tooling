package com.grappenmaker.coachtaal

val validSymbols = setOf('£', '_', '&', '~', '!', '|', '{', '}', '[', ']')

@JvmInline
value class Identifier(val value: String) : TokenInfo {
    init {
        require(value.isValidIdentifier()) { "Invalid identifier \"$value\"!" }
    }
}

fun Char.isValidIdentifier() = isLetterOrDigit() || this in validSymbols
fun String.isValidIdentifier() = all { it.isValidIdentifier() }

val Boolean.asCoach get() = if (this) 255f else 0f
val Float.asCoachBoolean get() = this > 0f

fun <T> MutableList<T>.removeLastN(n: Int) = List(n) { removeLast() }.asReversed()