package me.sgorecki

import java.io.File

typealias Frequency = Char

data class Distance(val x: Int, val y: Int)
data class Location(val x: Int, val y: Int) {
    fun distance(other: Location) = Distance(other.x - x, other.y - y)
    fun addDistance(distance: Distance): Location {
        return Location(distance.x + x, distance.y + y)
    }

    fun withinBounds(maxX: Int, maxY: Int): Boolean {
        return x < maxX && y < maxY && x >= 0 && y >= 0
    }
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

    fun antinodeLocations() = locationCombinations().flatMap { (one, two) ->
        val antinodeOne = two.addDistance(one.distance(two))
        val antinodeTwo = one.addDistance(two.distance(one))
        setOf(antinodeOne, antinodeTwo)
    }.toSet()

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
        val boundX = input[0].length
        val boundY = input.count()
        return parseAntennas(input)
            .groupBy { it.freq }
            .map { ResonantAntennas(it.key, it.value.map { it.location }.toSet()) }
            .flatMap { it.antinodeLocations() }
            .filter { it.withinBounds(boundX, boundY) }
            .toSet().count()
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