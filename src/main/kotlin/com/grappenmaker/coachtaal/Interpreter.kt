package com.grappenmaker.coachtaal

import kotlin.math.*
import kotlin.random.Random

val coachConstants = mapOf(
    "aan" to 255f,
    "uit" to 0f,
    "pi" to Math.PI.toFloat()
)

val coachBuiltins: Map<String, Interpreter.(args: List<Float>) -> Float?> = mapOf(
    "abs" to { args -> args.first().absoluteValue },
    "arcsin" to { (a) -> asin(a) },
    "arccos" to { (a) -> acos(a) },
    "arctan" to { (a) -> atan(a) },
    "sin" to { (a) -> sin(a) },
    "cos" to { (a) -> cos(a) },
    "tan" to { (a) -> tan(a) },
    "exp" to { (a) -> exp(a) },
    "ln" to { (a) -> ln(a) },
    "log" to { (a) -> log10(a) },
    "sqr" to { (a) -> a * a },
    "sqrt" to { (a) -> sqrt(a) },
    "entier" to { (a) -> floor(a) },
    "round" to { (a) -> round(a) },
    "fac" to { (a) -> a.roundFactorial() },
    "max" to { it.max() },
    "min" to { it.min() },
    "rand" to { Random.nextFloat() },
    "teken" to { (a) -> a.sign },
    "eenheidstap" to { (x, b) -> if (x < b) 0f else 1f },
    "stop" to { stopped = true ; null }
)

class Interpreter(val parsed: List<Expr>) {
    var ptr = 0
    var stopped = false
    val memory = mutableMapOf<String, Float>()

    val isRunning get() = ptr in parsed.indices && !stopped

    fun call(name: String, arguments: List<Float>) =
        (coachBuiltins[name.lowercase()] ?: TODO("Calling $name"))(arguments)

    fun step() {
        if (!isRunning) return

        parsed[ptr].eval(this)
        ptr++
    }

    fun run() {
        while (isRunning) step()
    }

    operator fun get(name: String): Float {
        val lower = name.lowercase()
        if (lower in coachBuiltins) return coachBuiltins.getValue(lower)(emptyList())
            ?: error("Built-in procedure $name was referenced as identifier?")

        return coachConstants[lower] ?: memory[name] ?: 0f
    }

    operator fun set(name: String, value: Float) {
        val lower = name.lowercase()
        if (lower in coachConstants || lower in coachBuiltins) error("Cannot assign value to constant \"$name\"!")

        memory[name] = value
    }
}