rootProject.name = "coachtaal-tooling"

plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.8.0"
}

include("parser", "app", "lsp", "project", "intellij")