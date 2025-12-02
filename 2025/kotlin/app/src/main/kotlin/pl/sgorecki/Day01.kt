package pl.sgorecki

import java.io.File
import kotlin.math.abs
import kotlin.math.floor


sealed interface Rotator {
    fun rotateFrom(initialValue: Int): Int
}

data class LeftRotator(val value: Int) : Rotator {
    override fun rotateFrom(initialValue: Int): Int {
        val temp = (initialValue - value) % 100
        return if (temp < 0) temp + 100 else temp
    }
}

data class RightRotator(val value: Int) : Rotator {
    override fun rotateFrom(initialValue: Int): Int {
        return (initialValue + value) % 100
    }
}


fun main() {
    fun part01(input: List<String>): Int {
        var current = 50
        return input.map { command ->
            val n = command.substring(1).toInt()
            if (command.startsWith('L')) {
                current = LeftRotator(n).rotateFrom(current)
            } else {
                current = RightRotator(n).rotateFrom(current)
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