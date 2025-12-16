plugins {
    kotlin("jvm") version "2.2.0"
    application
}

repositories {
    mavenCentral()
}

application {}


fileTree("src/main/kotlin") {
    include("**/Day*.kt")
}.forEach { file ->
    val base = file.nameWithoutExtension
    val className = "${base}Kt"

    tasks.register<JavaExec>("run$base") {
        group = "application"
        mainClass.set("pl.sgorecki.${base.lowercase()}.$className")
        classpath = sourceSets["main"].runtimeClasspath
    }
}

