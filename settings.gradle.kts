rootProject.name = "coachtaal-tooling"

includeBuild("nasty-jvm-util") {
    dependencySubstitution {
        substitute(module("com.grappenmaker:nasty-jvm-util")).using(project(":"))
    }
}

include("parser", "app", "lsp", "project")
