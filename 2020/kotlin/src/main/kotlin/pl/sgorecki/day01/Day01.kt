package pl.sgorecki.day01

import pl.sgorecki.common.readLines

fun main() {
    println("Hello, from day 01!")
    println("Part1: ${part01(readLines("01.txt"))}")
    println("Part2: ${part02(readLines("01.txt"))}")
}

fun part01(lines: List<String>): Long {
    val numbers = lines.map { it.toInt() }.sorted()
    return numbers.mapIndexed { index, number ->
        numbers.binarySearch(2020 - number)
            .takeIf { it > 0 }
            ?.let { foundIndex ->
                return@mapIndexed (numbers[index] * numbers[foundIndex]).toLong()
            }
    }.first() ?: 0
}

fun part02(lines: List<String>): Long {
    val numbers = lines.map { it.toLong() }.sorted()
    (0..numbers.count() - 2).forEach { i ->
        (i..<numbers.count()).forEach { j ->
            numbers.binarySearch(2020 - numbers[i] - numbers[j])
                .takeIf { it > 0 }
                ?.let { k ->
                    return numbers[i] * numbers[j] * numbers[k]
                }
        }
    }
    return 0
}