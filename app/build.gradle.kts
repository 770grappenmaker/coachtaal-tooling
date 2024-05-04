plugins {
    id("kotlin")
}

// TODO: version catalog the crap out of this
dependencies {
    implementation(project(":parser"))
    implementation("com.grappenmaker:nasty-jvm-util") {
        capabilities {
            requireCapability("com.grappenmaker:nasty-jvm-util-reflect")
        }
    }

    val gdxVersion = "1.11.0"
    implementation("com.badlogicgames.gdx:gdx:$gdxVersion")
    implementation("com.badlogicgames.gdx:gdx-platform:$gdxVersion:natives-desktop")
    implementation("com.badlogicgames.gdx:gdx-backend-lwjgl3:$gdxVersion")
    ktxImplementation("app", "assets", "collections", "graphics")
}

fun DependencyHandlerScope.ktxImplementation(vararg names: String) =
    names.forEach { implementation("io.github.libktx:ktx-$it:1.11.0-rc3") }

tasks {
    jar {
        from(configurations.runtimeClasspath.map { conf ->
            conf.map { if (it.isDirectory) it else zipTree(it) }
        })

        duplicatesStrategy = DuplicatesStrategy.EXCLUDE

        manifest {
            attributes(
                "Main-Class" to "com.grappenmaker.coachtaal.cli.MainKt"
            )
        }
    }
}