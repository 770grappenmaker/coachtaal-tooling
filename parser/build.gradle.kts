plugins {
    id("kotlin")
    kotlin("plugin.serialization")
}

dependencies {
    implementation(kotlin("reflect"))
    implementation(libs.ser.core)
}