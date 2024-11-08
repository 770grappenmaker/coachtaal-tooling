package com.grappenmaker.coachtaal

import com.grappenmaker.coachtaal.cli.loadCliModel
import kotlin.io.path.name
import kotlin.io.path.readText

// FIXME: viezig
fun Project.runner() = loadCliModel(iterScriptPath, initScriptPath, config.language.underlying).runner
