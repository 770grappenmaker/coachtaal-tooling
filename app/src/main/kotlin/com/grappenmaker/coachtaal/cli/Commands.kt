package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import org.objectweb.asm.ClassReader
import kotlin.io.path.*
import kotlin.random.Random
import kotlin.random.nextUInt
import kotlin.system.measureTimeMillis
import kotlin.time.measureTimedValue

object Format : Command() {
    override val name = "format"
    override val aliases = setOf("f")

    private val program by string()
    private val language by enum(CliLanguage.DUTCH)

    override fun invoke(args: List<String>) =
        println(parseProgram(Path(program[args]).readText(), language[args].underlying).asText())
}

object Parse : Command() {
    override val name = "parse"
    override val aliases = setOf("p")

    private val program by string()
    private val language by enum(CliLanguage.DUTCH)

    override fun invoke(args: List<String>) =
        parseProgram(Path(program[args]).readText(), language[args].underlying).lines.forEach { println(it) }
}

object Tokenize : Command() {
    override val name = "tokenize"
    override val aliases = setOf("t", "l")

    private val program by string()
    private val language by enum(CliLanguage.DUTCH)

    override fun invoke(args: List<String>) =
        lexer(Path(program[args]).readText(), language[args].underlying).forEach { println(it) }
}

object Compile : Command() {
    override val name = "compile"
    override val aliases = setOf("c")

    private val program by string()
    private val initial by string()
    private val language by enum(CliLanguage.DUTCH)
    private val output by optionalString("compiled${Random.nextUInt()}.class")

    override fun invoke(args: List<String>) {
        val actualLanguage = language[args].underlying
        val program = parseProgram(Path(program[args]).readText(), actualLanguage)
        val initial = parseProgram(Path(initial[args]).readText(), actualLanguage)
        val (popt, iopt) = program.optimizeWithInit(initial)

        val outPath = Path(output[args])
        outPath.createParentDirectories()

        val compiledName = outPath.joinToString("/").removeSuffix(".class")
        val bytes = compileModel(compiledName, popt.lines, iopt.lines, actualLanguage, runnable = true)
        outPath.writeBytes(bytes)
    }
}

object RunCompiled : Command() {
    override val name = "runcompiled"
    override val aliases = setOf("rc")

    private val path by string()
    private val runs by optionalInt(1)

    override fun invoke(args: List<String>) {
        val bytes = Path(path[args]).readBytes()
        val reader = ClassReader(bytes)
        val model = loadCompiledModel<ModelRunner>(reader.className.replace('/', '.'), bytes)
        val time = measureTimeMillis {
            repeat(runs[args]) {
                model.run()
                model.reset()
            }
        }

        println("Took ${time}ms")
    }
}

object Help : Command() {
    override val name = "help"
    override val aliases = setOf("h")
    override fun invoke(args: List<String>) = printHelp(0)
}

object Visualize : Command() {
    override val name = "visualize"
    override val aliases = setOf("v")

    private val program by string()
    private val initial by string()
    private val xVariable by string()
    private val yVariable by string()
    private val language by enum(CliLanguage.DUTCH)
    private val compile by optionalBoolean(true)
    private val lineThickness by optionalFloat(0f)
    private val margin by optionalFloat(10f)

    override fun invoke(args: List<String>) {
        val runner = measureTimedValue {
            loadCliModel(program[args], initial[args], language[args].underlying, compile[args])
        }.also { println("Took ${it.duration.inWholeMilliseconds}ms to load model") }.value.runner

        val result = measureTimedValue {
            runner.run(setOf(xVariable[args], yVariable[args]))
        }.also { println("Took ${it.duration.inWholeMilliseconds}ms to evaluate model") }.value

        result.visualize(xVariable[args], yVariable[args], lineThickness[args], margin[args])
    }
}

object CSV : Command() {
    override val name = "csv"
    override val aliases = setOf("export", "excel")

    private val program by string()
    private val initial by string()
    private val language by enum(CliLanguage.DUTCH)
    private val compile by optionalBoolean(true)

    override fun invoke(args: List<String>) {
        val cliProgram = loadCliModel(program[args], initial[args], language[args].underlying, compile[args])
        val variables = cliProgram.program.extractVariables() + cliProgram.initial.extractVariables()
        val result = cliProgram.runner.run(variables)

        println(variables.joinToString(","))
        result.forEach { step ->
            println(step.joinToString(",") { it.value.toString() })
        }
    }
}