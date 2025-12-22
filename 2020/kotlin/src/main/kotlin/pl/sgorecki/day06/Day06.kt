package pl.sgorecki.day06

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "06.txt", 6778)
    solve(::part2, "06.txt", 3406)
}

fun part1(lines: List<String>): Int {
    val groups = lines.joinToString("\n").split("\n\n")
    return groups
        .map { it.split("\n").joinToString("") }
        .map { it.toCharArray().toSet() }
        .sumOf { it.count() }
}

val allChars = ('a'..'z').map { it }.toSet()

fun part2(lines: List<String>): Int {
    val groups = lines.joinToString("\n").split("\n\n")
    return groups
        .map { it.split("\n") }
        .map {
            it.fold(allChars) { acc, chars ->
                acc.intersect(chars.toCharArray().toSet())
            }
        }
        .sumOf { it.count() }
}