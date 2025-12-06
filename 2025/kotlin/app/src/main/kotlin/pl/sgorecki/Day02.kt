package pl.sgorecki

import java.io.File

fun main() {
    fun part1(input: List<String>): Long {
        val ranges = input.first().split(',').map { textRange ->
            val ids = textRange.split('-')
            ids.first().toLong()..ids.last().toLong()
        }
        return ranges.map { range ->
            range.map { it.toString() }
                .filter { it.length % 2 == 0 }
                .filter { str ->
                    str.subSequence(0, str.length / 2) == str.subSequence(str.length / 2, str.length)
                }.sumOf { it.toLong() }

        }.sumOf { it.toLong() }
    }

    solve(::part1, "/Users/seba/code/adventofcode/2025/kotlin/inputs/02sample.txt", 1227775554)
    solve(::part1, "/Users/seba/code/adventofcode/2025/kotlin/inputs/02.txt", 29940924880)
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