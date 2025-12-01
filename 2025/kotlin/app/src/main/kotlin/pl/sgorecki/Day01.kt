package pl.sgorecki

import java.io.File
import kotlin.math.abs
import kotlin.math.floor

sealed interface Rotator {
    fun rotate(amount: Int): Int
}

data class LeftRotator(val initial: Int): Rotator {
    override fun rotate(amount: Int): Int {
        TODO("Not yet implemented")
    }
}

data class RightRotator(val initial: Int): Rotator {
    override fun rotate(amount: Int): Int {
        TODO("Not yet implemented")
    }

}
fun main() {
    fun part01(input: List<String>): Int {
        var current = 50
        input.map { command ->
            if (command.startsWith('L')) {
                val n = command.substringAfter('L').toInt()
                current = LeftRotator(current).rotate(n)
            } else {
                val n = command.substringAfter('R').toInt()
                current = RightRotator(current).rotate(n)
            }
            current
        }.count { it == 0 }
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
    solve(::day01part02, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01sample.txt", 6)
    solve(::day01part02, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01.txt", 6)
}

fun day01part02(input: List<String>): Int {
    var current = 50
    var zeroPoint = 0
    input
        .map { rotation ->
            if (rotation.startsWith('L')) {
                val n = rotation.substringAfter('L').toInt()
                if ((current - n) % 100 == 0) zeroPoint++
                val temp = abs(floor((current - n) / 100.0).toInt())
                zeroPoint += temp.toInt()
                var newCurrent = (current - n) % 100
                if (newCurrent < 0) {
                    newCurrent += 100
                }
                current = newCurrent
            } else {
                val n = rotation.substringAfter('R').toInt()
                val temp = floor((current + n) / 100.0)
                zeroPoint += temp.toInt()
                current = (current + n) % 100
            }
//                if (current == 0) zeroPoint++
        }
    return zeroPoint
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