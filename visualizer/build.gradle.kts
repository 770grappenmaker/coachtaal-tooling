plugins {
    id("kotlin")
}

// TODO: version catalog the crap out of this
dependencies {
    implementation(project(":parser"))

    val gdxVersion = "1.11.0"
    implementation("com.badlogicgames.gdx:gdx:$gdxVersion")
    implementation("com.badlogicgames.gdx:gdx-platform:$gdxVersion:natives-desktop")
    implementation("com.badlogicgames.gdx:gdx-backend-lwjgl3:$gdxVersion")
    ktxImplementation("app", "assets", "collections", "graphics")
}

fun DependencyHandlerScope.ktxImplementation(vararg names: String) =
    names.forEach { implementation("io.github.libktx:ktx-$it:1.11.0-rc3") }