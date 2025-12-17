package pl.sgorecki.day02

import pl.sgorecki.common.solve

fun main() {
    println("Hello, from day 02!")
    solve(::part1, "02_sample.txt", 2)
    solve(::part1, "02.txt", 542)
    solve(::part2, "02_sample.txt", 1)
    solve(::part2, "02.txt", 360)
}

data class Row(val char: Char, val range: IntRange, val password: String)

private fun String.toRow(): Row {
    val (policy, password) = this.split(':')
    val (range, char) = policy.split(' ')
    val (low, high) = range.split('-')
    return Row(char.first(), low.toInt()..high.toInt(), password.trim())
}

fun part1(lines: List<String>): Int {
    return lines.map { it.toRow() }
        .count { row ->
            val policyChars = row.password.toCharArray().filter { ch -> ch == row.char }
            policyChars.count() in row.range
        }
}

fun part2(lines: List<String>): Int {
    return lines.map { it.toRow() }
        .count { row ->
            val first = row.password.elementAt(row.range.first - 1)
            val second = row.password.elementAt(row.range.last - 1)
            (first == row.char) != (second == row.char)
        }
}