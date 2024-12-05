package me.sgorecki

import java.io.File

fun main() {

    fun part1(input: List<String>): Int {
        return 0;
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/05test.txt", 143)
}

fun solve(resultFn: (List<String>) -> Int, input: String, expected: Int) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}