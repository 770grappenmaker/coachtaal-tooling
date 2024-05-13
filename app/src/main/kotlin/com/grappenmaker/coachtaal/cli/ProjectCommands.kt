package com.grappenmaker.coachtaal.cli

import com.grappenmaker.coachtaal.*
import java.nio.file.Path
import kotlin.io.path.*
import kotlin.random.Random
import kotlin.random.nextUInt

object Project : CommandHolding() {
    override val name = "project"
    override val aliases = setOf("proj")
    override val subCommands = listOf(
        FormatProject, TranslateProject, CompileProject, CleanProject, VisualizeProject, CSVProject, RunCompiledProject
    )
}

object FormatProject : Command() {
    override val name = "format"
    override val aliases = setOf("f")

    private fun format(path: Path, language: Language) =
        path.writeText(parseProgram(path.readText(), language).asText())

    override fun invoke(args: List<String>) {
        val proj = cwd.loadProject()
        val lang = proj.config.language.underlying
        format(proj.initScriptPath, lang)
        format(proj.iterScriptPath, lang)
    }
}

object TranslateProject : Command() {
    override val name = "translate"
    override val aliases = setOf<String>()

    private val to by enum<EnumLanguage>()

    override fun invoke(args: List<String>) {
        val proj = cwd.loadProject()
        val from = proj.config.language.underlying

        val toLang = to[args]
        fun translate(path: Path) {
            val parsed = parseProgram(path.readText(), from)
            path.writeText(parsed.copy(language = toLang.underlying).asText())
        }

        translate(proj.initScriptPath)
        translate(proj.iterScriptPath)
        proj.copy(config = proj.config.copy(language = toLang)).storeConfig()
    }
}

object CompileProject : Command() {
    override val name = "compile"
    override val aliases = setOf("c")

    override fun invoke(args: List<String>) = cwd.loadProject().compile()
}

object VisualizeProject : Command() {
    override val name = "visualize"
    override val aliases = setOf("v")

    private val xVariable by string()
    private val yVariable by string()
    private val lineThickness by optionalFloat(0f)
    private val margin by optionalFloat(10f)

    override fun invoke(args: List<String>) = with(cwd.loadProject()) {
        genericVisualize(
            runner(),
            xVariable[args],
            yVariable[args],
            lineThickness[args],
            margin[args],
            screenshotOutput = resultsDir
        )
    }
}

object CSVProject : Command() {
    override val name = "csv"
    override val aliases = setOf("export", "excel")

    private val save by optionalBoolean(false)

    override fun invoke(args: List<String>) = with(cwd.loadProject()) {
        val csv = genericCSV(
            loadCliModel(
                iterScriptPath,
                initScriptPath,
                config.language.underlying,
                config.compiled,
                config.optimize
            )
        )

        if (save[args]) resultsDir.resolve("result-${Random.nextUInt()}.csv").writeText(csv)
        else println(csv)
    }
}

@OptIn(ExperimentalPathApi::class)
fun Path.deleteChildrenRecursively() = useDirectoryEntries { e -> e.forEach { it.deleteRecursively() } }

object CleanProject : Command() {
    override val name = "clean"
    override val aliases = emptySet<String>()

    override fun invoke(args: List<String>) = with(cwd.loadProject()) {
        buildDir.deleteChildrenRecursively()
        resultsDir.deleteChildrenRecursively()
    }
}

object RunCompiledProject : Command() {
    override val name = "runcompiled"
    override val aliases = setOf("rc")

    private val runs by optionalInt(1)

    override fun invoke(args: List<String>) = genericRunCompiled(
        cwd.loadProject().cachedCompilation() ?: error("Cannot find appropriate build to run!"),
        runs[args]
    )
}