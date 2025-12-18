package pl.sgorecki.common

import java.nio.file.Path
import kotlin.io.path.readLines

fun readLines(name: String) =
    Path.of("${System.getProperty("projectDir")}/inputs/$name").readLines()

fun <E> solve(resultFn: (List<String>) -> E, input: String, expected: E) {
    val lines = readLines(input)
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! The answer $actual is incorrect.")
    }
}