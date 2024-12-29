package me.sgorecki

import java.io.File
import java.math.BigInteger

enum class DirectionDay10 {}

fun main() {
    data class Position(val x: Int, val y: Int)
    data class Heigh(val value: Int, val pos: Position)
    data class Grid(val heighs: Array<Array<Heigh>>)

    fun parseGrid(input: List<String>): Pair<Grid, List<Position>> {
        val tailHeads = mutableListOf<Position>()
        val grid = Array(input.size) { Array(0) { Heigh(-1, Position(-1, -1)) } }
        input.forEachIndexed { idx, row ->
            val rowA = Array(row.length) { Heigh(-1, Position(-1, -1)) }
            row.forEachIndexed { rowIdx, c ->
                val pos = Position(rowIdx, idx)
                val value = c.digitToInt()
                if (c == '0') {
                    tailHeads.add(pos)
                }
                rowA[rowIdx] = Heigh(value, pos)
            }
            grid[idx] = rowA
        }
        return Grid(grid) to tailHeads
    }

    fun part1(input: List<String>): BigInteger? {
        val (grid, tailHeads) = parseGrid(input)

        return BigInteger.ZERO
    }

    fun part2(input: List<String>) = BigInteger.ZERO

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/10sample.txt", 36.toBigInteger())
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