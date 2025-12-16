package pl.sgorecki.common

import java.nio.file.Path
import kotlin.io.path.readLines

fun readLines(name: String) =
    Path.of("${System.getProperty("projectDir")}/inputs/$name").readLines()