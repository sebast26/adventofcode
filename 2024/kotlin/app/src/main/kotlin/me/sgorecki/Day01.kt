package me.sgorecki

import java.io.File
import kotlin.math.abs

fun main() {
    val filename = "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/01a.txt"
    val pairs = File(filename).readLines()
        .map { line -> line.split("   ") }
        .map { strings: List<String> -> strings.first() to strings.last() }
        .map { pair -> pair.first.toInt() to pair.second.toInt() }

    val leftList = pairs.map { it.first }.sorted()
    val rightList = pairs.map { it.second }.sorted()

    leftList.zip(rightList)
        .sumOf { (left, right) -> abs(left - right) }
        .also { println("Result: $it") }

    leftList.sumOf { num ->
        val times = rightList.count { it == num }
        num * times
    }.also { println("Result: $it") }
}