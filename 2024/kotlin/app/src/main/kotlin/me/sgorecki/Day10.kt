package me.sgorecki

import java.io.File
import java.math.BigInteger

enum class DirectionDay10 {}

data class Coord(val x: Int, val y: Int)
data class High(val value: Int, val pos: Coord)
data class GridHeighs(val highs: Array<Array<High>>, val tailHeads: List<Coord>)

fun main() {


    fun parseGrid(input: List<String>): GridHeighs {
        val tailHeads = mutableListOf<Coord>()
        val grid = Array(input.size) { Array(0) { High(-1, Coord(-1, -1)) } }
        input.forEachIndexed { idx, row ->
            val rowA = Array(row.length) { High(-1, Coord(-1, -1)) }
            row.forEachIndexed { rowIdx, c ->
                val coord = Coord(rowIdx, idx)
                val value = c.digitToInt()
                if (c == '0') {
                    tailHeads.add(coord)
                }
                rowA[rowIdx] = High(value, coord)
            }
            grid[idx] = rowA
        }
        return GridHeighs(grid, tailHeads)
    }

    fun part1(input: List<String>): BigInteger? {
        val grid = parseGrid(input)

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