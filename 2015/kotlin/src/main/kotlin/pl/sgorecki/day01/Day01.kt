package pl.sgorecki.day01

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "01.txt", 74)
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
