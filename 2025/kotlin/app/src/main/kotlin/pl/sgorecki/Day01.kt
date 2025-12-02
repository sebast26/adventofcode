package pl.sgorecki

import java.io.File
import kotlin.math.abs

sealed interface Rotator {
    fun rotateFrom(initialValue: Int): Int
    fun zeroPassesFrom(initialValue: Int): Int
}

data class LeftRotator(val value: Int) : Rotator {
    override fun rotateFrom(initialValue: Int): Int {
        return (initialValue - value).mod(100)
    }

    override fun zeroPassesFrom(initialValue: Int): Int {
        val temp = (initialValue - value)
        var count = 0
        if (temp <= 0 && initialValue != 0) count++
        return count + abs((initialValue - value) / 100)
    }
}

data class RightRotator(val value: Int) : Rotator {
    override fun rotateFrom(initialValue: Int): Int {
        return (initialValue + value).mod(100)
    }

    override fun zeroPassesFrom(initialValue: Int): Int {
        return (initialValue + value) / 100
    }
}


fun main() {
    fun part01(input: List<String>): Int {
        var current = 50
        return input.map { command ->
            val n = command.substring(1).toInt()
            current = if (command.startsWith('L')) {
                LeftRotator(n).rotateFrom(current)
            } else {
                RightRotator(n).rotateFrom(current)
            }
            current
        }.count { it == 0 }
    }

    fun part02(input: List<String>): Int {
        var current = 50
        var zeroPoint = 0
        input.map { command ->
            val n = command.substring(1).toInt()
            if (command.startsWith('L')) {
                zeroPoint += LeftRotator(n).zeroPassesFrom(current)
                current = LeftRotator(n).rotateFrom(current)
            } else {
                zeroPoint += RightRotator(n).zeroPassesFrom(current)
                current = RightRotator(n).rotateFrom(current)
            }
        }
        return zeroPoint
    }

    solve(::part01, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01sample.txt", 3)
    solve(::part01, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01.txt", 1102)
    solve(::part02, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01sample.txt", 6)
    solve(::part02, "/Users/seba/code/adventofcode/2025/kotlin/inputs/01.txt", 6175)
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