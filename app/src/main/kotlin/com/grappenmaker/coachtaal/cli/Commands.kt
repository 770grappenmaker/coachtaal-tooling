package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import java.nio.file.Path
import kotlin.io.path.*
import kotlin.random.Random
import kotlin.random.nextUInt
import kotlin.system.measureTimeMillis
import kotlin.time.measureTimedValue

object Format : Command() {
    override val name = "format"
    override val aliases = setOf("f")

    private val program by string()
    private val language by enum<EnumLanguage>()

    override fun invoke(args: List<String>) =
        println(parseProgram(Path(program[args]).readText(), language[args].underlying).asText())
}

object Translate : Command() {
    override val name = "translate"
    override val aliases = setOf<String>()

    private val program by string()
    private val from by enum<EnumLanguage>()
    private val to by enum<EnumLanguage>()

    override fun invoke(args: List<String>) {
        val parsed = parseProgram(Path(program[args]).readText(), from[args].underlying)
        println(parsed.copy(language = to[args].underlying).asText())
    }
}

object Parse : Command() {
    override val name = "parse"
    override val aliases = setOf("p")

    private val program by string()
    private val language by enum<EnumLanguage>()

    override fun invoke(args: List<String>) =
        println(parseProgram(Path(program[args]).readText(), language[args].underlying))
}

object Tokenize : Command() {
    override val name = "tokenize"
    override val aliases = setOf("t", "l")

    private val program by string()
    private val language by enum<EnumLanguage>()

    override fun invoke(args: List<String>) =
        lexer(Path(program[args]).readText(), language[args].underlying).forEach { println(it) }
}

object Visualize : Command() {
    override val name = "visualize"
    override val aliases = setOf("v")

    private val program by string()
    private val initial by string()
    private val language by enum<EnumLanguage>()
    private val xVariable by string()
    private val yVariable by string()
    private val compile by optionalBoolean(true)
    private val lineThickness by optionalFloat(0f)
    private val margin by optionalFloat(10f)

    override fun invoke(args: List<String>) {
        genericVisualize(
            loadCliModel(Path(program[args]), Path(initial[args]), language[args].underlying).runner,
            xVariable[args],
            yVariable[args],
            lineThickness[args],
            margin[args]
        )
    }
}

// What a mess
fun genericVisualize(
    runner: ModelRunner,
    xVariable: String,
    yVariable: String,
    lineThickness: Float,
    margin: Float,
    screenshotOutput: Path? = null
) {
    val result = measureTimedValue {
        runner.run(setOf(xVariable, yVariable))
    }.also { println("Took ${it.duration.inWholeMilliseconds}ms to evaluate model") }.value

    result.visualize(xVariable, yVariable, lineThickness, margin, screenshotOutput)
}

fun genericCSV(cliProgram: CliModel): String {
    val variables = (cliProgram.program.findNonConstant() + cliProgram.initial.findNonConstant())
        .mapTo(hashSetOf()) { it.value }

    val result = cliProgram.runner.run(variables)

    return buildString {
        appendLine(variables.joinToString(","))
        result.forEach { step -> appendLine(step.joinToString(",") { it.value.toString() }) }
    }
}


object CSV : Command() {
    override val name = "csv"
    override val aliases = setOf("export", "excel")

    private val program by string()
    private val initial by string()
    private val language by enum<EnumLanguage>()
    private val compile by optionalBoolean(true)

    override fun invoke(args: List<String>) {
        println(genericCSV(loadCliModel(
            Path(program[args]),
            Path(initial[args]),
            language[args].underlying
        )))
    }
}

object Init : Command() {
    override val name = "init"
    override val aliases = setOf("i")

    private val i by switch()
    private val language by optionalEnum(EnumLanguage.DUTCH)
    private val compile by optionalBoolean(true)

    override fun CommandContext.invoke() {
        val config = if (i[this]) {
            val byLowercase = enumValues<EnumLanguage>().associateBy { it.name.lowercase() }
            val langChoices = byLowercase.keys.joinToString("/")

            val lang = generateSequence { question("What language will the project be written in? ($langChoices)") }
                .firstNotNullOf { byLowercase[it.trim().lowercase()] }

            val comp = question("Should this project use JIT compilation? (Y/n)")
                .lowercase().trim().firstOrNull() != 'n'

            println("The project will be written in $lang and will ${if (comp) "" else "not "}use JIT compilation.")

            if (question("Is this information correct? (Y/n)").lowercase().trim().firstOrNull() != 'y') {
                println("Cancelling init process")
                return
            }

            ProjectConfig(lang, comp)
        } else ProjectConfig(language[args], compile[args])

        Project(cwd, config).init()
    }
}
