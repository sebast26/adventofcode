package me.sgorecki

import java.io.File

val inputRegex = """(?<res>\d+):(?<nums>(?:\s+\d+)*)""".toRegex()

data class Equation(val result: Int, val numbers: List<Int>)

fun part1(input: List<String>): Int {
    val equations = input.map { line ->
        val (res, nums) = inputRegex.matchEntire(line)
            ?.destructured
            ?: throw IllegalArgumentException("Incorrect input line $line")
        Equation(
            result = res.toInt(),
            numbers = nums.split(" ")
                .filter { it != "" }
                .map { it.toInt() }
        )
    }

    return 0
}

fun main() {
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/07sample.txt", 3749)
}

private fun solve(resultFn: (List<String>) -> Int, input: String, expected: Int) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}