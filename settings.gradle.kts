rootProject.name = "coachtaal-tooling"

pluginManagement {
    repositories {
        System.getenv()["NIX_MAVEN_REPO"]?.let {
            mavenLocal {
                url = uri(it)
                metadataSources {
                    mavenPom()
                    gradleMetadata()
                }
            }
        }
            ?: run {
                mavenCentral()
                gradlePluginPortal()
            }
    }
}

include("parser", "app", "lsp", "project")

