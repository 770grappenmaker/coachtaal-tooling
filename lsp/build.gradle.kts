plugins {
	kotlin("jvm")
}

dependencies {
    implementation(project(":project"))
    implementation(libs.lsp4j)
    implementation(libs.coroutines.core)
}

tasks {
    processResources {
        // Hack in order to always update the version /shrug
        outputs.upToDateWhen { false }

        filesMatching("server-version") {
            expand("version" to version)
        }
    }

    jar {
        from(configurations.runtimeClasspath.map { conf ->
            conf.map { if (it.isDirectory) it else zipTree(it) }
        }) {
            exclude("META-INF/*.RSA", "META-INF/*.SF", "META-INF/*.DSA")
        }

        duplicatesStrategy = DuplicatesStrategy.EXCLUDE

        manifest {
            attributes("Main-Class" to "com.grappenmaker.coachtaal.lsp.CoachLanguageServerKt")
        }
    }

    val copyBuiltLsp by registering(Copy::class) {
        dependsOn(jar)
        from(jar.get().outputs.files.singleFile)
        into(projectDir.resolve("extension").resolve("build"))
        rename { "lsp.jar" }
    }

    clean { delete(copyBuiltLsp) }
    build { dependsOn(copyBuiltLsp) }
}
