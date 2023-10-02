rootProject.name = "coachtaal-tooling"

pluginManagement {
    plugins {
        kotlin("jvm") version "1.9.10"
    }
}

plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.7.0"
}

include("parser", "visualizer")