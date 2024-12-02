package me.sgorecki

import java.io.File
import kotlin.math.abs

fun main() {
    val filename = "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/02.txt"
    val reports = File(filename).readLines()
        .map { line ->
            Report(line.split(" ").map { it.toInt() })
        }
    reports.count { it.isSafe() }.also { println("Result: $it") }
    reports.count { it.isSafeWithToleration() }.also { println("Result: $it") }
}

private data class Report(val levels: List<Int>)

private fun Report.isSafe(): Boolean {
    val diffs = levels.zip(levels.drop(1)).map { (l1, l2) -> l2 - l1 }
    return (diffs.allIncreasing() || diffs.allDecreasing()) && diffs.maxDiffer(1, 3)
}

private fun List<Int>.allIncreasing() = all { it > 0 }
private fun List<Int>.countIncreasing() = count { it > 0 }
private fun List<Int>.allDecreasing() = all { it < 0 }
private fun List<Int>.countDecreasing() = count { it < 0 }
private fun List<Int>.maxDiffer(min: Int, max: Int) = all { abs(it) in min..max }
private fun List<Int>.countDiffer(min: Int, max: Int) = count { abs(it) in min..max }

// TODO
private fun Report.isSafeWithToleration(): Boolean {
    val diffs = levels.zip(levels.drop(1)).map { (l1, l2) -> l2 - l1 }
    val increasingWithToleration = diffs.countIncreasing() in diffs.count() - 1 .. diffs.count()
    val decreasingWithToleration = diffs.countDecreasing() in diffs.count() - 1 .. diffs.count()
    val maxDiffsWithToleration = diffs.countDiffer(1, 3) in diffs.count() - 1 .. diffs.count()
    return ((increasingWithToleration || decreasingWithToleration) && diffs.maxDiffer(1, 3)
            || (diffs.allIncreasing() || diffs.allDecreasing()) && maxDiffsWithToleration)
}