plugins {
    id("kotlin")
}

dependencies {
    implementation(project(":parser"))
    implementation("com.grappenmaker:nasty-jvm-util") {
        capabilities {
            requireCapability("com.grappenmaker:nasty-jvm-util-reflect")
        }
    }
}