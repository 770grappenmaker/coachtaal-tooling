plugins {
    id("kotlin")
    kotlin("plugin.serialization")
}

dependencies {
    api(project(":parser"))
    implementation("com.grappenmaker:nasty-jvm-util") {
        capabilities {
            requireCapability("com.grappenmaker:nasty-jvm-util-reflect")
        }
    }

    implementation(libs.ser.json)
}