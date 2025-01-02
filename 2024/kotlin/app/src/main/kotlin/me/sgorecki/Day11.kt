package me.sgorecki

import java.io.File
import java.math.BigInteger
import java.util.*

fun BigInteger.engravedWithEvenDigits() = this.toString().length % 2 == 0

fun BigInteger.split(): Pair<BigInteger, BigInteger> {
    val str = toString()
    val first = str.substring(0, str.length / 2)
    val second = str.substring(str.length / 2, str.length)
    return first.toBigInteger() to second.toBigInteger()
}

fun blink(list: LinkedList<BigInteger>): LinkedList<BigInteger> {
    val out = LinkedList(list)
    var outIndex = 0
    list.forEachIndexed { origIndex, stone ->
        if (stone == BigInteger.ZERO) {
            out[outIndex] = BigInteger.ONE
        } else if (stone.engravedWithEvenDigits()) {
            val (first, second) = stone.split()
            out.removeAt(outIndex)
            out.add(outIndex, first)
            out.add(++outIndex, second)
        } else {
            out[outIndex] = stone.multiply(2024.toBigInteger())
        }
        outIndex++
    }
    return out
}

fun main() {
    fun part1(input: List<String>): Int {
        val initial = input[0].split(" ").map { it.toBigInteger() }

        var res = initial
        for (i in 0..24) {
            res = blink(LinkedList(res))
        }

        return res.size
    }

    fun part2(input: List<String>): Int {
        return 0
    }

//    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/11sample.txt", 55312)
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/11.txt", 199982)
}

private fun <E> solve(resultFn: (List<String>) -> E, input: String, expected: E) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}