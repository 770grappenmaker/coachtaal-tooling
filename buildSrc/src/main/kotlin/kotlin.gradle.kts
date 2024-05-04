import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm")
}

repositories {
    mavenCentral()
}

kotlin { jvmToolchain(17) }

tasks.withType<KotlinCompile>().configureEach {
    kotlinOptions { freeCompilerArgs += "-Xcontext-receivers" }
}