plugins {
    id("kotlin")
    kotlin("plugin.serialization")
    application
}

application {
    mainClass = "com.grappenmaker.coachtaal.cli.MainKt"
    applicationDefaultJvmArgs = listOf("-Dfile.encoding=UTF-8")
    applicationName = "coach"
}

open class InstallDistTo : Sync() {
    @Option(option = "into", description = "Directory to copy the distribution into")
    override fun into(destDir: Any): AbstractCopyTask = super.into(destDir)
}

tasks {
    val installDist by getting(Sync::class)
    val installDistTo by registering(InstallDistTo::class) {
        dependsOn(installDist)
        from(installDist.outputs.files.singleFile)
    }
}

dependencies {
    implementation(project(":project"))
    implementation(variantOf(libs.gdx.platform) { classifier("natives-desktop") })
    implementation(libs.bundles.dx)
}