package me.sgorecki

import java.io.File
import java.math.BigInteger

enum class DirectionDay10(val x: Int, val y: Int) {
    UP(0, -1),
    DOWN(0, 1),
    LEFT(-1, 0),
    RIGHT(1, 0)
}

data class Coord(val x: Int, val y: Int)

private operator fun Coord.plus(direction: DirectionDay10) = Coord(this.x + direction.x, this.y + direction.y)

data class High(val value: Int, val pos: Coord)
data class TrailHead(val pos: Coord)
data class Trail(val steps: Set<Coord>)
data class GridHeighs(val highs: Array<Array<High>>, val trailHeads: List<TrailHead>) {

    fun score() = trailHeads.sumOf { findTops(it).count() }
    fun scoreTrails() = trailHeads.sumOf { findTopTrails(it).count() }

    fun findTops(head: TrailHead): Set<Coord> {
        return findTop(High(0, head.pos))
    }

    fun findTopTrails(head: TrailHead): Set<Trail> {
        return findTopTrail(High(0, head.pos), Trail(emptySet()))
    }

    private fun findTop(high: High): Set<Coord> {
        if (high.value == 9) {
            return setOf(high.pos)
        }

        val possibleTop = getHigh(high.pos + DirectionDay10.UP)
        val possibleBottom = getHigh(high.pos + DirectionDay10.DOWN)
        val possibleLeft = getHigh(high.pos + DirectionDay10.LEFT)
        val possibleRight = getHigh(high.pos + DirectionDay10.RIGHT)

        val tops = mutableSetOf<Coord>()
        if (possibleTop != null && possibleTop.value == high.value + 1) tops += findTop(possibleTop)
        if (possibleBottom != null && possibleBottom.value == high.value + 1) tops += findTop(possibleBottom)
        if (possibleLeft != null && possibleLeft.value == high.value + 1) tops += findTop(possibleLeft)
        if (possibleRight != null && possibleRight.value == high.value + 1) tops += findTop(possibleRight)
        return tops
    }

    private fun findTopTrail(high: High, trail: Trail): Set<Trail> {
        val current = trail.copy(steps = trail.steps + high.pos)
        if (high.value == 9) {
            return setOf(current)
        }

        val possibleTop = getHigh(high.pos + DirectionDay10.UP)
        val possibleBottom = getHigh(high.pos + DirectionDay10.DOWN)
        val possibleLeft = getHigh(high.pos + DirectionDay10.LEFT)
        val possibleRight = getHigh(high.pos + DirectionDay10.RIGHT)

        val trails = mutableSetOf<Trail>()
        if (possibleTop != null && possibleTop.value == high.value + 1) trails += findTopTrail(possibleTop, current)
        if (possibleBottom != null && possibleBottom.value == high.value + 1) trails += findTopTrail(possibleBottom, current)
        if (possibleLeft != null && possibleLeft.value == high.value + 1) trails += findTopTrail(possibleLeft, current)
        if (possibleRight != null && possibleRight.value == high.value + 1) trails += findTopTrail(possibleRight, current)

        return trails
    }

    private fun inGrid(coord: Coord): Boolean {
        if (coord.x < 0 || coord.x >= highs.first().size) return false
        if (coord.y < 0 || coord.y >= highs.size) return false
        return true
    }

    private fun getHigh(coord: Coord): High? {
        if (!inGrid(coord)) return null
        return highs[coord.y][coord.x]
    }

}

fun main() {
    fun parseGrid(input: List<String>): GridHeighs {
        val trailHeads = mutableListOf<TrailHead>()
        val grid = Array(input.size) { Array(0) { High(-1, Coord(-1, -1)) } }
        input.forEachIndexed { idx, row ->
            val rowA = Array(row.length) { High(-1, Coord(-1, -1)) }
            row.forEachIndexed { rowIdx, c ->
                val coord = Coord(rowIdx, idx)
                val value = c.digitToInt()
                if (c == '0') {
                    trailHeads.add(TrailHead(coord))
                }
                rowA[rowIdx] = High(value, coord)
            }
            grid[idx] = rowA
        }
        return GridHeighs(grid, trailHeads)
    }

    fun part1(input: List<String>): BigInteger {
        val grid = parseGrid(input)
        return grid.score().toBigInteger()
    }

    fun part2(input: List<String>): BigInteger {
        val grid = parseGrid(input)
        return grid.scoreTrails().toBigInteger()
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/10sample.txt", 36.toBigInteger())
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/10.txt", 472.toBigInteger())
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/10sample.txt", 81.toBigInteger())
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/10.txt", 969.toBigInteger())
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