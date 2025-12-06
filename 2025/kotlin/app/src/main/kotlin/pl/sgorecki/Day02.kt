package pl.sgorecki

import java.io.File

fun main() {
    fun prepareRanges(input: List<String>): List<LongRange> = input.first().split(',').map { textRange ->
        val ids = textRange.split('-')
        ids.first().toLong()..ids.last().toLong()
    }

    fun part1(input: List<String>): Long {
        val ranges = prepareRanges(input)
        return ranges.map { range ->
            range.map { it.toString() }
                .filter { it.length % 2 == 0 }
                .filter { str ->
                    str.subSequence(0, str.length / 2) == str.subSequence(str.length / 2, str.length)
                }.sumOf { it.toLong() }

        }.sumOf { it }
    }

    fun part2(input: List<String>): Long {
        val ranges = prepareRanges(input)
        return ranges.map { range ->
            range.map { it.toString() }
                .filter { it.containsPattern() }
                .sumOf { it.toLong() }
        }.sumOf { it }
    }

    solve(::part1, "/Users/seba/code/adventofcode/2025/kotlin/inputs/02sample.txt", 1227775554)
    solve(::part1, "/Users/seba/code/adventofcode/2025/kotlin/inputs/02.txt", 29940924880)
    solve(::part2, "/Users/seba/code/adventofcode/2025/kotlin/inputs/02sample.txt", 4174379265)
    solve(::part2, "/Users/seba/code/adventofcode/2025/kotlin/inputs/02.txt", 48631958998)
}

private fun String.containsPattern(): Boolean {
    if (!this.containsCharsTwice()) return false
    if (this.length == 2) return true
    return this.mapIndexed { index, _ -> this.subSequence(0, index) }
        .any { seq -> this.replace(seq.toString(), "").isEmpty() }
}

private fun String.containsCharsTwice(): Boolean {
    return this.associate { it to 1 }.count() != this.length
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