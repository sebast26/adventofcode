package me.sgorecki

import java.io.File

typealias Frequency = Char

data class Distance(val x: Int, val y: Int)
data class Location(val x: Int, val y: Int) {
    fun distance(other: Location) = Distance(other.x - x, other.y - y)
}

data class Antenna(val freq: Frequency, val location: Location)
data class ResonantAntennas(val freq: Frequency, val locations: Set<Location>) {
    fun locationCombinations(): Set<Pair<Location, Location>> {
        val combinations = mutableSetOf<Pair<Location, Location>>()
        locations.forEach { one ->
            locations.forEach { two ->
                if (one != two && !combinations.contains(one to two) && !combinations.contains(two to one)) {
                    combinations.add(one to two)
                }
            }
        }
        return combinations
    }

    fun antinodePositions() = 0
}


fun main() {
    fun parseAntennas(input: List<String>): MutableSet<Antenna> {
        val antennas = mutableSetOf<Antenna>()
        input.forEachIndexed { idxY, row ->
            row.forEachIndexed { idxX, c ->
                if (c != '.') {
                    antennas.add(Antenna(c, Location(idxX, idxY)))
                }
            }
        }
        return antennas
    }

    fun part1(input: List<String>): Int {
        val antennas = parseAntennas(input)
            .groupBy { it.freq }
            .map { ResonantAntennas(it.key, it.value.map { it.location }.toSet()) }
//            .sumOf { it.antinodePositions() }

        return 0
    }

    fun part2(input: List<String>) = 0

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/08sample.txt", 14)
}

private fun <E> solve(resultFn: (List<String>) -> E, input: String, expected: E) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}