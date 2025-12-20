package pl.sgorecki.day03

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "03.txt", 2081)
    solve(::part2, "03.txt", 2341)
}

data class Result<T>(val path: List<Pair<T, T>>, val lastPosition: Pair<T, T>)

fun part1(lines: List<String>): Int {
    return lines.first().fold(Result(listOf(0 to 0), 0 to 0), moveSanta()).path.toSet().count()
}

fun part2(lines: List<String>): Int {
    val realSanta = lines.first().filterIndexed { idx, _ -> idx % 2 == 0 }
    val realSantaResult = realSanta.fold(Result(listOf(0 to 0), 0 to 0), moveSanta())

    val roboSanta = lines.first().filterIndexed { idx, _ -> idx % 2 == 1 }
    val roboSantaResult = roboSanta.fold(Result(listOf(0 to 0), 0 to 0), moveSanta())

    return (realSantaResult.path.toSet() + roboSantaResult.path.toSet()).count()
}

private fun moveSanta(): (Result<Int>, Char) -> Result<Int> = { acc, ch ->
    val (_, lastPosition) = acc
    val newPosition = when (ch) {
        '>' -> lastPosition.copy(second = lastPosition.second + 1)
        '<' -> lastPosition.copy(second = lastPosition.second - 1)
        '^' -> lastPosition.copy(first = lastPosition.first - 1)
        'v' -> lastPosition.copy(first = lastPosition.first + 1)
        else -> error("Unpredicted move")
    }
    acc.copy(path = acc.path + newPosition, lastPosition = newPosition)
}