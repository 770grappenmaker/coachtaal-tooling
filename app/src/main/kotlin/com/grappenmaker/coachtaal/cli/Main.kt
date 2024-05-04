package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import kotlin.io.path.Path
import kotlin.io.path.readText
import kotlin.properties.PropertyDelegateProvider
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty
import kotlin.system.exitProcess

val commands = listOf(Help, Visualize, CSV, Format, Compile, RunCompiled, Parse, Tokenize)

fun printHelp(code: Int): Nothing {
    println("Available commands:")
    commands.forEach { println(it.usage) }
    exitProcess(code)
}

fun main(args: Array<String>) {
    val sub = args.firstOrNull() ?: printHelp(0)
    val target = commands.find { it.match(sub) } ?: printHelp(1)
    target(target.prepareArguments(args.drop(1)))
}

var cliModelCounter = 0
    get() = field++

const val cliCompiledPrefix = "com/grappenmaker/coachtaal/CliCompiledModel"

data class CliModel(val program: ParsedProgram, val initial: ParsedProgram, val runner: ModelRunner)

fun loadCliModel(programPath: String, initialPath: String, language: Language, compile: Boolean): CliModel {
    val program = parseProgram(Path(programPath).readText(), language)
    val initial = parseProgram(Path(initialPath).readText(), language)
    val (popt, iopt) = program.optimizeWithInit(initial)

    return CliModel(program, initial, when {
        compile -> createCompiledModel<ModelRunner>(
            compiledName = "$cliCompiledPrefix$cliModelCounter",
            iter = popt.lines,
            init = iopt.lines,
            language = language
        )
        else -> Interpreter(popt, iopt, language)
    })
}

enum class CliLanguage(val underlying: Language) {
    ENGLISH(EnglishLanguage), DUTCH(DutchLanguage)
}

fun String.surround(lr: Pair<String, String>) = lr.first + this + lr.second

abstract class Command {
    abstract val name: String
    abstract val aliases: Set<String>

    open val usage by lazy {
        parameters.joinToString(separator = " ", prefix = "$name ") {
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

    inline fun <reified T : Enum<T>> enum(default: T) = ParameterDelegate({ idx, name ->
        val byLowercase = enumValues<T>().associateBy { it.name.lowercase() }
        EnumParameter(idx, name, enumValues<T>().joinToString("|") { it.name.lowercase() }) {
            byLowercase[it.lowercase()] ?: default
        }
    })

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