package pl.sgorecki.day02

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "02.txt", 1598415)
    solve(::part2, "02.txt", 3812909)
}

fun part1(lines: List<String>): Long {
    return lines.sumOf { line ->
        val (l, w, h) = line.split('x').map { it.toInt() }
        val (min1, min2) = listOf(l, w, h).sorted().take(2)
        2L * l * w + 2L * w * h + 2L * h * l + min1 * min2
    }
}

fun part2(lines: List<String>): Long {
    return lines.sumOf { line ->
        val (l, w, h) = line.split('x').map { it.toInt() }
        val (a, b) = listOf(l, w, h).sorted().take(2)
        2L * a + 2L * b + l * w * h
    }
}