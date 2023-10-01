plugins {
    kotlin("jvm") version "1.9.10"
}

group = "com.grappenmaker"
version = "0.1"

repositories {
    mavenCentral()
}

kotlin { jvmToolchain(17) }