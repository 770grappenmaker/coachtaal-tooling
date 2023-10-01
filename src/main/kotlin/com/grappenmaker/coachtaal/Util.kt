package com.grappenmaker.coachtaal

import java.math.BigInteger
import kotlin.math.roundToInt

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
inline fun <T> Iterable<T>.split(cond: (T) -> Boolean): List<List<T>> {
    val result = mutableListOf<List<T>>()
    var soFar = mutableListOf<T>()

    for (v in this) if (cond(v)) {
        result += soFar
        soFar = mutableListOf()
    } else soFar += v

    return result
}

fun Float.roundFactorial() = roundToInt().toBigInteger().factorial().toFloat()
fun BigInteger.factorial(): BigInteger {
    var result = BigInteger.ONE
    for (i in 2..intValueExact()) result *= i.toBigInteger()
    return result
}