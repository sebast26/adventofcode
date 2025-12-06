package pl.sgorecki

import java.io.File

fun main() {
    fun part1(input: List<String>): Long {
        return input.map { bank ->
            var biggestDigit = 0 to 0
            bank.mapIndexed { index, char ->
                if (char.digitToInt() > biggestDigit.first && index < bank.length - 1) {
                    biggestDigit = char.digitToInt() to index
                }
            }
            val second = bank.subSequence(biggestDigit.second + 1, bank.length)
                .fold(-1) { acc, ch -> if (ch.digitToInt() > acc) ch.digitToInt() else acc }
            biggestDigit.first * 10 + second
        }.sumOf { it.toLong() }
    }

    solve(::part1, "/Users/seba/code/adventofcode/2025/kotlin/inputs/03sample.txt", 357)
    solve(::part1, "/Users/seba/code/adventofcode/2025/kotlin/inputs/03.txt", 17524)
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