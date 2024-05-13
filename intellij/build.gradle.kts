plugins {
    id("kotlin")
    id("org.jetbrains.intellij") version "1.17.3"
}

intellij {
    pluginName.set("CoachTaal")
    version.set("2024.1.1")
    type.set("IU")
}

kotlin { jvmToolchain(17) }

dependencies {
    implementation(project(":project"))
}

tasks {
    processResources {
        val lspJar = project(":lsp").tasks.jar.get()
        dependsOn(lspJar)
        from(lspJar) { rename { "lsp.jar" } }
    }

    buildSearchableOptions {
        enabled = false
    }
}