rootProject.name = "coachtaal-tooling"

includeBuild("nasty-jvm-util") {
    dependencySubstitution {
        substitute(module("com.grappenmaker:nasty-jvm-util")).using(project(":"))
    }
}

plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.8.0"
}

include("parser", "app", "lsp", "project")