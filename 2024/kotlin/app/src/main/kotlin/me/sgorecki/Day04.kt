package me.sgorecki

import java.io.File

typealias Matrix = Array<CharArray>

data class Vec2(val x: Int, val y: Int)

data class Grid(private val elems: List<List<Char>>) {

    val allowedDirections = listOf(
        Vec2(1, 0),   // east
        Vec2(1, 1),   // southeast
        Vec2(0, 1),   // south
        Vec2(-1, 1),  // southwest
        Vec2(-1, 0),  // west
        Vec2(-1, -1), // northwest
        Vec2(0, -1),  // north
        Vec2(1, -1), // northeast
    )

    val indecies: Sequence<Pair<Int, Int>> = sequence {
        for (y in elems[0].indices) {
            for (x in elems.indices)
                yield(x to y)
        }
    }

    fun getAtPos(x: Int, y: Int): Char? = elems.getOrNull(y)?.getOrNull(x)

    fun countXmasWordForPosition(startX: Int, startY: Int): Int {
        return allowedDirections.count { direction ->
            checkXmasWordForDirection(startX, startY, direction)
        }
    }

    fun checkXmasWordForDirection(startX: Int, startY: Int, direction: Vec2): Boolean {
        var runningX = startX
        var runningY = startY

        for (letter in listOf('X', 'M', 'A', 'S')) {
            if (getAtPos(runningX, runningY) != letter) {
                return false
            }
            runningX += direction.x
            runningY += direction.y
        }
        return true
    }

    fun isMasWordAtPosition(startX: Int, startY: Int): Boolean {
        if (getAtPos(startX, startY) != 'A') {
            return false
        }

        val isRaisingMAS =
            getAtPos(startX - 1, startY -1) == 'M' && getAtPos(startX + 1, startY + 1) == 'S'
        val isRaisingSAM =
            getAtPos(startX - 1, startY -1) == 'S' && getAtPos(startX + 1, startY + 1) == 'M'
        val isFailingMAS =
            getAtPos(startX - 1, startY + 1) == 'M' && getAtPos(startX + 1, startY - 1) == 'S'
        val isFailingSAM =
            getAtPos(startX - 1, startY + 1) == 'S' && getAtPos(startX + 1, startY - 1) == 'M'

        return (isRaisingMAS || isRaisingSAM) && (isFailingMAS || isFailingSAM)
    }
}

fun main() {
    val filename = "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/04.txt"
    val input = File(filename).readLines().map { it.toList() }
    val grid = Grid(input)
    println(grid.indecies.take(20).toList())
    var xmasWordCount = 0
    for ((x, y) in grid.indecies) {
        xmasWordCount += grid.countXmasWordForPosition(x, y)
    }
    println("Result: $xmasWordCount")

    var masWordCount = 0
    for ((x, y) in grid.indecies) {
        masWordCount += if (grid.isMasWordAtPosition(x, y)) 1 else 0
    }
    println("Result: $masWordCount")
}
