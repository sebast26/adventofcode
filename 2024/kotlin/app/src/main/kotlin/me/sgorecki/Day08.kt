package me.sgorecki

import java.io.File

fun main() {
    fun part1(input: List<String>) = 0

    fun part2(input: List<String>) = 0

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/08sample.txt", 14)
}

private fun <E> solve(resultFn: (List<String>) -> E, input: String, expected: E) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}