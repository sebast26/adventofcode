package me.sgorecki

import java.io.File

fun main() {
    val filename = "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/04.txt"
    val input = File(filename).readLines()
    val matrix = input.toMatrix().also { println("$it") }
}

fun List<String>.toMatrix(): Array<CharArray> {
    val matrix = Array(count()) { CharArray(first().count()) }
    mapIndexed { index, s -> matrix[index] = s.toCharArray() }
    return matrix
}
