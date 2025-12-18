package pl.sgorecki.day03

import pl.sgorecki.common.solve

fun main() {
    println("Hello from Day03!")
    solve(::part1, "03.txt", 225)
    solve(::part2, "03.txt", 225)
}

fun part1(lines: List<String>): Int {
    return countTrees(lines, 1, 3)
}

fun part2(lines: List<String>): Long {
    return countTrees(lines, 1, 1).toLong() *
            countTrees(lines, 1, 3) *
            countTrees(lines, 1, 5) *
            countTrees(lines, 1, 7) *
            countTrees(lines, 2, 1)
}

fun countTrees(lines: List<String>, dy: Int, dx: Int): Int {
    val height = lines.count()
    val width = lines[0].length

    var y = dy
    var x = dx % width
    var result = 0

    while (y < height) {
        if (lines[y].elementAt(x) == '#') result++
        y += dy
        x = (x + dx) % width
    }

    return result
}