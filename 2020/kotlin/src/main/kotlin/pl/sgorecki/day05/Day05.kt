package pl.sgorecki.day05

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "05.txt", 953)
    solve(::part2, "05.txt", 615)
}

fun part1(lines: List<String>) = lines.maxOf { it.seat() }

fun part2(lines: List<String>): Int {
    val seatIds = lines.map { it.seat() }
    val min = seatIds.min()
    val max = seatIds.max()
    (min..max).forEach { num ->
        if (num !in seatIds) return num
    }
    return 0
}

private fun String.row(): Int {
    val mask = take(7).map { ch ->
        when (ch) {
            'F' -> '0'
            'B' -> '1'
            else -> error("Unknown character for row")
        }
    }.joinToString("")
    return mask.toInt(2)
}

private fun String.column(): Int {
    val mask = takeLast(3).map { ch ->
        when (ch) {
            'L' -> '0'
            'R' -> '1'
            else -> error("Unknown character for column")
        }
    }.joinToString("")
    return mask.toInt(2)
}

private fun String.seat() = (row() shl 3) + column()