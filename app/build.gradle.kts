plugins {
    id("kotlin")
    kotlin("plugin.serialization")
    application
}

application {
    mainClass = "com.grappenmaker.coachtaal.cli.MainKt"
    applicationDefaultJvmArgs = listOf("-Dfile.encoding=UTF-8")
    applicationName = "coach"
}

open class InstallDistTo : Sync() {
    @Option(option = "into", description = "Directory to copy the distribution into")
    override fun into(destDir: Any): AbstractCopyTask = super.into(destDir)
}

tasks {
    val installDist by getting(Sync::class)
    val installDistTo by registering(InstallDistTo::class) {
        dependsOn(installDist)
        from(installDist.outputs.files.singleFile)
    }
}

dependencies {
    implementation(project(":parser"))
    implementation("com.grappenmaker:nasty-jvm-util") {
        capabilities {
            requireCapability("com.grappenmaker:nasty-jvm-util-reflect")
        }
    }

    implementation(variantOf(libs.gdx.platform) { classifier("natives-desktop") })
    implementation(libs.bundles.dx)

    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.5.1")
}


//tasks {
//    jar {
//        from(configurations.runtimeClasspath.map { conf ->
//            conf.map { if (it.isDirectory) it else zipTree(it) }
//        })
//
//        duplicatesStrategy = DuplicatesStrategy.EXCLUDE
//
//        manifest {
//            attributes(
//                "Main-Class" to "com.grappenmaker.coachtaal.cli.MainKt"
//            )
//        }
//    }
//}