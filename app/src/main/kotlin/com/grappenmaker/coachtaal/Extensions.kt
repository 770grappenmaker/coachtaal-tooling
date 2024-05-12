package com.grappenmaker.coachtaal

import com.grappenmaker.coachtaal.cli.genericCompile
import com.grappenmaker.coachtaal.cli.loadCliModel
import kotlin.io.path.name
import kotlin.io.path.readText

// viezig
fun Project.runner(): ModelRunner {
    val cached = cachedCompilation()
    if (cached != null) {
        println("Reused compilation cache to run model")
        return cached.loadCompiledModel<ModelRunner>()
    }

    if (config.compiled) {
        compile()
        cachedCompilation()?.let { return it.loadCompiledModel<ModelRunner>() }
    }

    return loadCliModel(iterScriptPath, initScriptPath, config.language.underlying, config.compiled).runner
}

fun Project.compile() {
    if (!config.compiled) error("Compilation has been disabled for this project!")

    val actualLanguage = config.language.underlying
    val output = compilationOutput

    storeHash()
    genericCompile(
        actualLanguage,
        parseProgram(iterScriptPath.readText(), actualLanguage),
        parseProgram(initScriptPath.readText(), actualLanguage),
        output.name,
        output.parent
    )
}