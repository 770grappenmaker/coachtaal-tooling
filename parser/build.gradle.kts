import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
	kotlin("jvm")
    kotlin("plugin.serialization")
}

tasks.withType<KotlinCompile>().configureEach {
    compilerOptions { freeCompilerArgs.add("-Xcontext-receivers") }
}

dependencies {
    implementation(kotlin("reflect"))
    implementation(libs.ser.core)
}
