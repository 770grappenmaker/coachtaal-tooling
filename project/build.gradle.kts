plugins {
    id("kotlin")
    kotlin("plugin.serialization")
}

dependencies {
    api(project(":parser"))
    implementation(libs.ser.json)
    implementation(libs.bundles.asm)
}