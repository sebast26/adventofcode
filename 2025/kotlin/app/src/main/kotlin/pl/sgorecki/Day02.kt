package pl.sgorecki

import java.io.File

fun main() {
    fun part1(input: List<String>): Int {
        val ranges = input.first().split(',').map { textRange ->
            val ids = textRange.split('-')
            ids.first().toLong()..ids.last().toLong()
        }
        println(ranges)
        return 0
    }

    solve(::part1, "/Users/seba/code/adventofcode/2025/kotlin/inputs/02.txt", 1)
}

private fun <E> solve(resultFn: (List<String>) -> E, input: String, expected: E) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! The answer $actual is incorrect.")
    }
}