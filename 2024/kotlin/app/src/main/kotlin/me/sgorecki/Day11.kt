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

fun Long.split(): Pair<Long, Long> {
    val str = toString()
    val first = str.substring(0, str.length / 2)
    val second = str.substring(str.length / 2, str.length)
    return first.toLong() to second.toLong()
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
        for (i in 1..25) {
            res = blink(LinkedList(res))
            println("$i: $res")
        }

        return res.size
    }
//    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/11sample.txt", 55312)
//    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/11.txt", 199982)
    solve(::day11part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/11.txt", 237149922829154)
}

// inspired by Advent of Code 2024 in Kotlin. Day 11. (https://www.youtube.com/watch?v=u9BSjRzS-gw)
fun countStones(stone: Long, cache: Map<Int, MutableMap<Long, Long>>, iteration: Int): Long {
    if (iteration == 0) {
        return 1L
    } else {
        val iterationCache = cache[iteration] ?: throw IllegalStateException("Empty iteration cache")
        return iterationCache.getOrPut(stone) {
            val str = stone.toString()
            if (stone == 0L) {
                countStones(1, cache, iteration - 1)
            } else if (str.length % 2 == 0) {
                val (first, second) = stone.split()
                countStones(first, cache, iteration - 1) + countStones(second, cache, iteration - 1)
            } else {
                countStones(stone * 2024, cache, iteration - 1)
            }
        }
    }
}

fun part2Test(input: List<String>, iterations: Int): Long {
    val initial = input[0].split(" ").map { it.toLong() }
    val cache = (1..iterations).associateWith { mutableMapOf<Long, Long>() }

    return initial.sumOf { countStones(it, cache, iterations) }
}

fun day11part2(input: List<String>): Long {
    val initial = input[0].split(" ").map { it.toLong() }
    val cache = (1..75).associateWith { mutableMapOf<Long, Long>() }

    return initial.sumOf { countStones(it, cache, 75) }
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