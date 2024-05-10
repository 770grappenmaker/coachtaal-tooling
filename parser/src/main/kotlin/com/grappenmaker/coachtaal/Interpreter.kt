package com.grappenmaker.coachtaal

import kotlin.math.*
import kotlin.random.Random

data class LogbookEntry(val variable: String, val value: Float, val iteration: Int)

class Interpreter(
    private val iteration: ParsedProgram,
    private val initial: ParsedProgram,
    language: Language = iteration.language,
    private val maxIterations: Int = -1,
): ModelRunner {
    private val coachConstants = language.createConstants()
    private data class BuiltinData(val args: Int, val eval: (List<Float>) -> Float?)

    private val coachBuiltins: Map<String, BuiltinData> = with(language) {
        buildMap {
            operator fun String.invoke(args: Int = 1, f: (List<Float>) -> Float?) = put(this, BuiltinData(args, f))

            abs { (a) -> a.absoluteValue }
            arcsin { (a) -> asin(a) }
            arccos { (a) -> acos(a) }
            arctan { (a) -> atan(a) }
            sin { (a) -> sin(a) }
            cos { (a) -> cos(a) }
            tan { (a) -> tan(a) }
            exp { (a) -> exp(a) }
            ln { (a) -> ln(a) }
            log { (a) -> log10(a) }
            sqr { (a) -> a * a }
            sqrt { (a) -> sqrt(a) }
            floor { (a) -> floor(a) }
            round { (a) -> round(a) }
            factorial { (a) -> a.roundFactorial() }
            max(-1) { it.max() }
            min(-1) { it.min() }
            random { Random.nextFloat() }
            sign { (a) -> a.sign }
            step(2) { (a, b) -> if (a < b) 0f else 1f }
            stop(0) { stop(); null }
        }
    }

    var iterations = 0
        private set

    override var stopped = false
    val memory = mutableMapOf<String, Float>()

    fun call(name: String, args: List<Float>): Float? {
        val entry = coachBuiltins[name.lowercase()] ?: TODO("Calling $name")
        check(entry.args == args.size || entry.args < 0) { "$name: Expected ${entry.args} args, got ${args.size}" }
        return entry.eval(args)
    }

    init {
        reset()
    }

    override fun iteration() {
        if (stopped) return

        iteration.lines.run()
        if (++iterations >= maxIterations && maxIterations != -1) stopped = true
    }

    private fun List<Expr>.run() {
        for (expr in this) {
            if (stopped) return
            expr.eval(this@Interpreter)
        }
    }

    fun run() {
        while (!stopped) iteration()
    }

    operator fun get(name: String): Float {
        val lower = name.lowercase()
        return coachConstants[lower] ?: memory[name] ?: 0f
    }

    operator fun set(name: String, value: Float) {
        val lower = name.lowercase()
        if (lower in coachConstants || lower in coachBuiltins) error("Cannot assign value to constant \"$name\"!")

        memory[name] = value
    }

    fun stop() {
        stopped = true
    }

    override fun memoryByName(name: String) = get(name)

    override fun reset() {
        memory.clear()
        stopped = false
        iterations = 0
        initial.lines.run()
    }
}

interface SelfIteratingModelRunner : ModelRunner {
    val iterations: Int
}

interface ModelRunner {
    val stopped: Boolean
    fun iteration()
    fun memoryByName(name: String): Float
    fun reset()
}

fun ModelRunner.run(logVariables: Set<String> = emptySet()): List<List<LogbookEntry>> {
    var iter = 0
    val result = mutableListOf<List<LogbookEntry>>()

    while (!stopped) {
        iteration()

        val actualIter = if (this is SelfIteratingModelRunner) iterations else iter
        if (logVariables.isNotEmpty()) result += logVariables.map { LogbookEntry(it, memoryByName(it), actualIter) }
        iter++
    }

    return result
}