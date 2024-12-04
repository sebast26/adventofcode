package me.sgorecki

import java.io.File
import java.util.LinkedList

fun main() {
    val filename = "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/03.txt"
    val input = File(filename).readLines().joinToString()
    val mulPattern = """mul\((?<a>[0-9]{1,3}),(?<b>[0-9]{1,3})\)""".toRegex()

    mulPattern.sumOfMultiplications(input).also { println("Result: $it") }

    val dos = input.indexesOf("do()").map { it to 'd' }
    val donts = input.indexesOf("don't()").map { it to 'n' }
    (dos + donts).sortedBy { it.first }.also { println("$it") }
    val fragments = squash(LinkedList((dos + donts).sortedBy { it.first }))
        .also { println("$it") }
    // adding initial 'enable'
    val (enable, disable) = fragments.apply { addFirst(0 to 'd') }
        .partition { it.second == 'd' }
    val sb = StringBuilder()
    enable.zip(disable).map { range: Pair<Pair<Int, Char>, Pair<Int, Char>> ->
        val start = range.first.first
        val end = range.second.first
        sb.append(input.substring(start .. end))
    }

    mulPattern.sumOfMultiplications(sb.toString()).also { println("Result: $it") }
}

private fun Regex.sumOfMultiplications(input: String) = findAll(input)
    .map {
        val (n, m) = it.destructured
        n.toBigInteger() to m.toBigInteger()
    }.map { (n, m) -> n.times(m) }
    .sumOf { it }

fun squash(queue: LinkedList<Pair<Int, Char>>): LinkedList<Pair<Int, Char>> {
    val sqashed: LinkedList<Pair<Int, Char>> = LinkedList()
    sqashed.add(queue.poll())
    while (!queue.isEmpty()) {
        val curr = queue.poll()
        if (curr.second != sqashed.peekLast().second) {
            sqashed.add(curr)
        }
    }
    return sqashed
}

private fun String.indexesOf(pattern: String) =
    Regex.escape(pattern)
        .toRegex()
        .findAll(this)
        .map { it.range.first }
        .toList()