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

    tasks.register<JavaExec>("run$base") {
        group = "application"
        mainClass.set("pl.sgorecki.${base.lowercase()}.${base}Kt")
        classpath = sourceSets["main"].runtimeClasspath
    }
}

tasks.withType<JavaExec>().configureEach {
    systemProperty("projectDir", project.projectDir.absolutePath)
}