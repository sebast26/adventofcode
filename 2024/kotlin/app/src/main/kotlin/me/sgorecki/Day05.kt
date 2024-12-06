package me.sgorecki

import java.io.File
import kotlin.math.floor

data class PageUpdate(val rules: List<Pair<Int, Int>>, val update: List<Int>) {
    fun inRightOrder(): Boolean {
        return update.mapIndexed { index, page ->
            val after = pagesAfter(page)
            val before = update.slice(0 ..<index)
            before.intersect(after).none()
        }.all { it }
    }

    fun middlePageNumber(): Int {
        return update[floor(update.count() / 2.0).toInt()]
    }

    private fun pagesAfter(num: Int) =
        rules.filter { it.first == num }.map { it.second }
}

fun main() {
    fun part1(input: List<String>): Int {
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
        return pds.filter { it.inRightOrder() }
            .sumOf { it.middlePageNumber() }
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/05test.txt", 143)
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/05.txt", 5762)
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