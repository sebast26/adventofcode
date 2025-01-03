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

    // TODO
    fun harmonicsAntinodeLocations(maxX: Int, maxY: Int) =
        locationCombinations().flatMap { (one, two) ->
            val locs = mutableSetOf<Location>()
            val distOne = one.distance(two)
            val distTwo = two.distance(one)
            var temp = one.addDistance(distOne)
            while (temp.withinBounds(maxX, maxY)) {
                locs.add(temp)
                temp = temp.addDistance(distOne)
            }
            temp = two.addDistance(distTwo)
            while (temp.withinBounds(maxX, maxY)) {
                locs.add(temp)
                temp = temp.addDistance(distTwo)
            }
            setOf(locs)
        }
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
            .asSequence()
            .map { ResonantAntennas(it.key, it.value.map { it.location }.toSet()) }
            .flatMap { it.antinodeLocations() }
            .filter { it.withinBounds(boundX, boundY) }
            .toSet().count()
    }

    fun part2(input: List<String>): Int {
        val boundX = input[0].length
        val boundY = input.count()
        return parseAntennas(input)
            .groupBy { it.freq }
            .map { ResonantAntennas(it.key, it.value.map { it.location }.toSet()) }
            .flatMap { it.harmonicsAntinodeLocations(boundX, boundY) }
            .flatten().toSet().count()
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/08sample.txt", 14)
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/08.txt", 244)
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/08sample.txt", 34)
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/08.txt", 912)
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