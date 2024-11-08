allprojects {
    group = "com.grappenmaker"
    version = "0.1"
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
            ?: run { mavenCentral() }
    }
}

plugins {
	kotlin("jvm") version "2.0.0" apply false
	kotlin("plugin.serialization") version "2.0.0" apply false
}

task("listrepos") {
    doLast {
        println( "Repositories:")
        project.repositories.forEach { println((it as MavenArtifactRepository).url) }
   }
}
