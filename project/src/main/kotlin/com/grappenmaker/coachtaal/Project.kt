package com.grappenmaker.coachtaal

import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import org.objectweb.asm.ClassReader
import java.nio.file.Path
import java.security.MessageDigest
import kotlin.io.path.*

const val coachExtension = "coach"
const val coachInitFileName = "init.$coachExtension"
const val coachIterFileName = "iter.$coachExtension"
const val coachProjectFileName = "project.coach.json"
const val coachFormatterFileName = "formatter.coach.json"

private val json = Json {
    encodeDefaults = true
    ignoreUnknownKeys = true
    prettyPrint = true
}

fun String.decodeProjectConfig() = json.decodeFromString<ProjectConfig>(this)
fun String.decodeFormatterConfig() = json.decodeFromString<FormatterConfig>(this)

fun Path.projectConfig() = resolve(coachProjectFileName)
fun Path.formatterConfig() = resolve(coachFormatterFileName)

private inline fun <reified T : Any> Path.jsonOrEmpty() =
    json.decodeFromString<T>(runCatching { readText() }.getOrElse { "{}" })

fun Path.loadProject() = Project(
    this,
    projectConfig().jsonOrEmpty(),
    formatterConfig().jsonOrEmpty()
).also { it.createDirectories() }

inline fun <reified T : ModelRunner> Path.loadCompiledModel(): ModelRunner {
    val bytes = readBytes()
    val reader = ClassReader(bytes)
    return loadCompiledModel<T>(reader.className.replace('/', '.'), bytes)
}

data class Project(
    val dir: Path,
    val config: ProjectConfig,
    val formatter: FormatterConfig = FormatterConfig()
)

val Project.initScriptPath get() = dir.resolve(coachInitFileName)
val Project.iterScriptPath get() = dir.resolve(coachIterFileName)
val Project.resultsDir get() = dir.resolve("results")
val Project.buildDir get() = dir.resolve("build")
val Project.configPath get() = dir.projectConfig()
val Project.formatterConfigPath get() = dir.formatterConfig()
val Project.compilationOutput get() = buildDir.resolve("${dir.name}.class")
val Project.compilationHash get() = buildDir.resolve(".hash")

private val compilationDigest = MessageDigest.getInstance("SHA-512")

fun ByteArray.formatHex() = joinToString("") { it.toUByte().toString(16).padStart(2, '0') }
private fun String.computeHash() = compilationDigest.digest(encodeToByteArray()).formatHex()
fun Project.computeHash() = initScriptPath.readText().computeHash() + "\n" + iterScriptPath.readText().computeHash()
fun Project.storeHash() = compilationHash.writeText(computeHash())

fun Project.isHashValid() = compilationHash.exists() && compilationHash.readText() == computeHash()
fun Project.cachedCompilation() =
    compilationOutput.takeIf { config.compiled && config.useCompilationCache && it.exists() && isHashValid() }

fun Project.createDirectories() {
    dir.createDirectories()
    resultsDir.createDirectories()
    buildDir.createDirectories()
}

fun Project.init() {
    if (configPath.exists()) error("Directory $dir has already been initialized!")

    createDirectories()
    initScriptPath.writeText(
        """
        'Hier komt de code voor het initialiseren van het model.
        'Dit komt overeen met het rechterpaneel in Coach.
    """.trimIndent()
    )

    iterScriptPath.writeText(
        """
        'Hier komt de code voor het draaien van een iteratie van het model.
        'Dit komt overeen met het linkerpaneel in Coach.
    """.trimIndent()
    )

    storeConfig()
    dir.resolve(".gitignore").writeText(
        """
            build/
            results/
        """.trimIndent()
    )
}

fun Project.storeConfig() {
    configPath.writeText(json.encodeToString(config))
    formatterConfigPath.writeText(json.encodeToString(formatter))
}

@Serializable
data class ProjectConfig(
    val language: EnumLanguage = EnumLanguage.DUTCH,
    val compiled: Boolean = true,
    val useCompilationCache: Boolean = true,
    val optimize: Boolean = true,
    val maxIterations: Int = -1,
    // TODO
)

@Serializable
enum class EnumLanguage(val underlying: Language) {
    ENGLISH(EnglishLanguage), DUTCH(DutchLanguage), SKIBIDI(SkibidiLanguage)
}

fun Language.toEnum() = when (this) {
    DutchLanguage -> EnumLanguage.DUTCH
    EnglishLanguage -> EnumLanguage.ENGLISH
    SkibidiLanguage -> EnumLanguage.SKIBIDI
}