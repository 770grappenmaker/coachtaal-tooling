package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import java.nio.file.Path
import kotlin.io.path.readText
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty
import kotlin.system.exitProcess

object RootCommand : CommandHolding() {
    override val subCommands = listOf(
        Init, Project, Visualize, CSV, Format,
        Translate, Compile, RunCompiled, Parse, Tokenize,
        ExerciseCommand
    )

    override val name get() = error("The root command does not have a name!")
    override val aliases = emptySet<String>()
}

fun printHelp(code: Int): Nothing {
    println("Usage:")
    println(RootCommand.usage)
    exitProcess(code)
}

fun CommandHolding.printCommandHelp(): Nothing {
    println("Usage:")
    println(usage)
    exitProcess(0)
}

fun main(args: Array<String>) {
    val (switches, toCommand) = args.partition { it.length == 2 && it.first() == '-' }
    val ctx = CommandContext(toCommand, switches.mapTo(hashSetOf()) { it[1] })

    runCatching { with (RootCommand) { ctx() } }.onFailure { ex ->
        when (ex) {
            is ParseFailedException -> {
                println(ex.message)
                ex.suppressedExceptions.forEach { println(it.message) }
            }
            is IllegalStateException -> println(ex.message)
            else -> ex.printStackTrace()
        }
    }
}

var cliModelCounter = 0
    get() = field++

const val cliCompiledPrefix = "com/grappenmaker/coachtaal/CliCompiledModel"

data class CliModel(val program: ParsedProgram, val initial: ParsedProgram, val runner: ModelRunner)

fun loadCliModel(
    programPath: Path,
    initialPath: Path,
    language: Language,
    compile: Boolean,
    optimize: Boolean = true
) = loadCliModel(programPath.readText(), initialPath.readText(), language, compile, optimize)

fun loadCliModel(
    programCode: String,
    initialCode: String,
    language: Language,
    compile: Boolean,
    optimize: Boolean = true
): CliModel {
    val program = parseProgram(programCode, language)
    val initial = parseProgram(initialCode, language)
    val (popt, iopt) = if (optimize) program.optimizeWithInit(initial) else program to initial

    return CliModel(
        program, initial, when {
            compile -> createCompiledModel<ModelRunner>(
                compiledName = "$cliCompiledPrefix$cliModelCounter",
                iter = popt,
                init = iopt,
                language = language
            )

            else -> Interpreter(popt, iopt, language)
        }
    )
}

fun String.surround(lr: Pair<String, String>) = lr.first + this + lr.second

abstract class Command {
    abstract val name: String
    abstract val aliases: Set<String>

    open val usage by lazy {
        (switches.map { "[-${it.name}]" } +
                parameters.map { it.usage.surround(if (it.optional) "[" to "]" else "<" to ">") }).joinToString(" ")
    }

    private val switches = mutableListOf<Switch>()
    private val parameters = mutableListOf<Parameter<*>>()
    private var seenOptional = false

    fun string() = ParameterDelegate(::StringParameter)
    fun optionalString(default: String) = ParameterDelegate(::StringParameter, default)

    fun int() = ParameterDelegate(::IntParameter)
    fun optionalInt(default: Int) = ParameterDelegate(::IntParameter, default.toString())

    fun float() = ParameterDelegate(::FloatParameter)
    fun optionalFloat(default: Float) = ParameterDelegate(::FloatParameter, default.toString())

    fun boolean() = ParameterDelegate(::BooleanParameter)
    fun optionalBoolean(default: Boolean) = ParameterDelegate(::BooleanParameter, default.toString())

    inline fun <reified T : Enum<T>> enum(crossinline extra: (EnumParameter<T>) -> Parameter<T> = { it }) =
        ParameterDelegate({ idx, name ->
            val byLowercase = enumValues<T>().associateBy { it.name.lowercase() }
            EnumParameter(idx, name, enumValues<T>().joinToString("|") { it.name.lowercase() }) {
                byLowercase[it.lowercase()] ?: printHelp(1)
            }.let(extra)
        })

    inline fun <reified T : Enum<T>> optionalEnum(default: T) = enum<T> { it.withDefault(default.name.lowercase()) }

    inner class ParameterDelegate<T>(
        private val ctor: (Int, String) -> Parameter<T>,
        private val default: String? = null,
    ) {
        operator fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, Parameter<T>> {
            val impl = ctor(parameters.size, property.name)
            val actual = if (default != null) {
                seenOptional = true
                impl.withDefault(default)
            } else {
                require(!seenOptional) { "Required parameters are not allowed after optional parameters!" }
                impl
            }

            parameters += actual
            return ReadOnlyProperty { _, _ -> actual }
        }
    }

    inner class SwitchDelegate {
        operator fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, Switch> {
            val switch = Switch(property.name.single())
            switches += switch
            return ReadOnlyProperty { _, _ -> switch }
        }
    }

    fun switch() = SwitchDelegate()

    open operator fun CommandContext.invoke() = invoke(args)
    open operator fun invoke(args: List<String>) {
        error("At least one overload of Command#invoke has to be implemented!")
    }

    fun prepareArguments(args: List<String>): List<String> =
        ParameterList(parameters.filterIsInstance<DefaultParameter<*>>().associate { it.index to it.default }, args)
}

abstract class CommandHolding : Command() {
    abstract val subCommands: List<Command>

    private val help = Help()
    private val actualCommands get() = listOf(help) + subCommands

    override val usage get() = actualCommands.joinToString(System.lineSeparator()) { c ->
        val lines = c.usage.lines()
        val prefix = if (lines.size == 1) "" else System.lineSeparator()
        c.name + lines.joinToString(separator = System.lineSeparator(), prefix = prefix) { " $it" }
    }

    override fun CommandContext.invoke() {
        val sub = args.firstOrNull() ?: printCommandHelp()
        val target = actualCommands.find { it.match(sub) } ?: printHelp(1)
        val ctx = CommandContext(target.prepareArguments(args.drop(1)), switches)
        with (target) { ctx() }
    }

    inner class Help : Command() {
        override val name = "help"
        override val aliases = setOf("h")
        override fun invoke(args: List<String>) = printCommandHelp()
    }
}

fun Command.match(sub: String) = sub == name || sub in aliases

data class CommandContext(val args: List<String>, val switches: Set<Char>)

data class Switch(val name: Char) {
    operator fun get(on: Set<Char>) = name in on
    operator fun get(on: CommandContext) = get(on.switches)
}

sealed interface Parameter<T> {
    val index: Int
    val name: String
    val optional: Boolean get() = false
    val usage: String get() = name

    operator fun get(on: List<String>): T
    operator fun get(on: CommandContext): T = get(on.args)
}

class ParameterList<T>(private val entries: Map<Int, T>, private val backing: List<T>) : List<T> by backing {
    override fun get(index: Int) = backing.getOrNull(index) ?: entries[index] ?: printHelp(1)
}

fun question(msg: String): String {
    print("$msg ")
    return readlnOrNull() ?: error("Standard input was closed unexpectedly")
}