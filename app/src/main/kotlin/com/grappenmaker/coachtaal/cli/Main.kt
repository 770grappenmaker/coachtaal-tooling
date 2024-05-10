package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import kotlinx.serialization.Serializable
import java.nio.file.Path
import kotlin.io.path.Path
import kotlin.io.path.readText
import kotlin.properties.PropertyDelegateProvider
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty
import kotlin.system.exitProcess

object RootCommand : CommandHolding() {
    override val subCommands = listOf(Init, Project, Visualize, CSV, Format, Compile, RunCompiled, Parse, Tokenize)
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
    runCatching { RootCommand(args.toList()) }.onFailure {
        if (it is IllegalStateException) println(it.message)
        else it.printStackTrace()
    }
}

var cliModelCounter = 0
    get() = field++

const val cliCompiledPrefix = "com/grappenmaker/coachtaal/CliCompiledModel"

data class CliModel(val program: ParsedProgram, val initial: ParsedProgram, val runner: ModelRunner)

fun loadCliModel(programPath: Path, initialPath: Path, language: Language, compile: Boolean): CliModel {
    val program = parseProgram(programPath.readText(), language)
    val initial = parseProgram(initialPath.readText(), language)
    val (popt, iopt) = program.optimizeWithInit(initial)

    return CliModel(
        program, initial, when {
            compile -> createCompiledModel<ModelRunner>(
                compiledName = "$cliCompiledPrefix$cliModelCounter",
                iter = popt.lines,
                init = iopt.lines,
                language = language
            )

            else -> Interpreter(popt, iopt, language)
        }
    )
}

@Serializable
enum class CliLanguage(val underlying: Language) {
    ENGLISH(EnglishLanguage), DUTCH(DutchLanguage)
}

fun String.surround(lr: Pair<String, String>) = lr.first + this + lr.second

abstract class Command {
    abstract val name: String
    abstract val aliases: Set<String>

    open val usage by lazy {
        parameters.joinToString(separator = " ") {
            it.usage.surround(if (it.optional) "[" to "]" else "<" to ">")
        }
    }

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
    ) : PropertyDelegateProvider<Any?, ReadOnlyProperty<Any?, Parameter<T>>> {
        override fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, Parameter<T>> {
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

    abstract operator fun invoke(args: List<String>)

    fun prepareArguments(args: List<String>): List<String> =
        ParameterList(parameters.filterIsInstance<DefaultParameter<*>>().associate { it.index to it.default }, args)
}

abstract class CommandHolding : Command() {
    abstract val subCommands: List<Command>

    private val help = Help()
    val actualCommands get() = subCommands + help

    override val usage get() = actualCommands.joinToString(System.lineSeparator()) { c ->
        val lines = c.usage.lines()
        val prefix = if (lines.size == 1) "" else System.lineSeparator()
        c.name + lines.joinToString(separator = System.lineSeparator(), prefix = prefix) { " $it" }
    }

    override fun invoke(args: List<String>) {
        val sub = args.firstOrNull() ?: printCommandHelp()
        val target = actualCommands.find { it.match(sub) } ?: printHelp(1)
        target(target.prepareArguments(args.drop(1)))
    }

    inner class Help : Command() {
        override val name = "help"
        override val aliases = setOf("h")
        override fun invoke(args: List<String>) = printCommandHelp()
    }
}

fun Command.match(sub: String) = sub == name || sub in aliases

sealed interface Parameter<T> {
    val index: Int
    val name: String
    val optional: Boolean get() = false
    val usage: String get() = name
    operator fun get(on: List<String>): T
}

class ParameterList<T>(private val entries: Map<Int, T>, private val backing: List<T>) : List<T> by backing {
    override fun get(index: Int) = backing.getOrNull(index) ?: entries[index] ?: printHelp(1)
}