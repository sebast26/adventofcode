package pl.sgorecki

import java.io.File

fun main() {
    fun part01(input: List<String>): Int {
        var current = 50
        return input
            .map { rotation ->
                if (rotation.startsWith('L')) {
                    val n = rotation.substringAfter('L').toInt()
                    var newCurrent = (current - n) % 100
                    if (newCurrent < 0) newCurrent += 100
                    current = newCurrent
                } else {
                    val n = rotation.substringAfter('R').toInt()
                    current = (current + n) % 100
                }
                current
            }.count { it == 0 }
    }

    solve(::part01, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01sample.txt", 3)
    solve(::part01, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01.txt", 1102)
//    solve(::part02, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01sample.txt", 6)
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