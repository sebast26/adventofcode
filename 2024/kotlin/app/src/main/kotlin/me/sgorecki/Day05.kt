package me.sgorecki

import java.io.File
import java.util.*
import kotlin.math.floor

data class PageUpdate(val rules: List<Pair<Int, Int>>, val update: List<Int>) {
    fun inRightOrder(): Boolean {
        return update.mapIndexed { index, page ->
            val after = pagesAfter(page)
            val before = update.slice(0..<index)
            before.intersect(after).none()
        }.all { it }
    }

    fun middlePageNumber(): Int {
        return update[floor(update.count() / 2.0).toInt()]
    }

    fun withFixedOrdering(): PageUpdate {
        val newList = LinkedList(update)
        update.forEachIndexed { index, page ->
            val after = pagesAfter(page)
            val before = update.slice(0..<index)
            val misplaced = before.intersect(after)
            if (misplaced.isNotEmpty()) {
                newList.swap(page, misplaced.first())
                return this.copy(update = newList).withFixedOrdering()
            }
        }
        return this.copy(update = newList)
    }

    private fun LinkedList<Int>.swap(page: Int, other: Int) {
        val index1 = indexOf(page)
        val index2 = indexOf(other)
        set(index1, other)
        set(index2, page)
    }

    private fun pagesAfter(num: Int) =
        rules.filter { it.first == num }.map { it.second }
}

fun main() {
    fun readPageUpdates(input: List<String>): List<PageUpdate> {
        val (rulesSection, updatesSection) = input.partition { it.contains("|") }
        val rules = rulesSection.map { ruleLine ->
            val pages = ruleLine.split("|")
            pages.first().toInt() to pages.last().toInt()
        }
        val updates = updatesSection
            .filterNot { it == "" }
            .map { updateLine ->
                updateLine.split(",").map { it.toInt() }
            }

        val pds = updates.map { PageUpdate(rules, it) }
        return pds
    }

    fun part1(input: List<String>): Int {
        val pds = readPageUpdates(input)
        return pds.filter { it.inRightOrder() }
            .sumOf { it.middlePageNumber() }
    }

    fun part2(input: List<String>): Int {
        val pds = readPageUpdates(input)
        return pds.filter { !it.inRightOrder() }
            .map { it.withFixedOrdering() }
            .sumOf { it.middlePageNumber() }
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/05test.txt", 143)
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/05.txt", 5762)
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/05test.txt", 123)
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/05.txt", 4130)
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