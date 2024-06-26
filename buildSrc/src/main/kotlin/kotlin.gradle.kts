import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
}

repositories {
    mavenCentral()
}

kotlin { jvmToolchain(8) }

tasks.withType<KotlinCompile>().configureEach {
    compilerOptions { freeCompilerArgs.add("-Xcontext-receivers") }
}