package pl.sgorecki.day01

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "01.txt", 74)
    solve(::part2, "01.txt", 74)
}

fun part1(lines: List<String>): Int {
    var line = lines.first()
    while (line.contains("()")) {
        line = line.replace("()", "")
    }
    return line.sumOf {
        when (it) {
            '(' -> 1
            ')' -> -1
            else -> 0
        }
    }
}

fun part2(lines: List<String>): Int {
    val line = lines.first()
    return line.foldIndexed(0 to 0) { idx, acc, ch ->
        if (acc.first == -1) return@foldIndexed acc
        val move = when (ch) {
            '(' -> 1
            ')' -> -1
            else -> 0
        }
        acc.first + move to idx + 1
    }.second
}