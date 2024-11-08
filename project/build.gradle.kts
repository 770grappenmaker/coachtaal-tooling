plugins {
    kotlin("jvm")
    kotlin("plugin.serialization")
}

dependencies {
    api(project(":parser"))

    implementation(libs.ser.json)
}
