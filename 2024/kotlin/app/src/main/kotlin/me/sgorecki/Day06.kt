package me.sgorecki

import me.sgorecki.Direction.Up
import java.io.File

private enum class Direction(val x: Int, val y: Int) {
    Up(0, -1),
    Right(1, 0),
    Down(0, 1),
    Left(-1, 0)
}

private data class Position(val x: Int, val y: Int)

private data class Part1Result(val result: Int, val visited: MutableSet<Position>)

fun main() {
    data class GuardPosition(val position: Position, val direction: Direction) {
        fun nextPosition() = Position(position.x + direction.x, position.y + direction.y)
        fun turnRight() = this.copy(
            direction = Direction.entries[(direction.ordinal + 1) % Direction.entries.size]
        )

        fun forward() = this.copy(
            position = nextPosition()
        )
    }

    fun part1(input: List<String>): Part1Result {
        lateinit var initialPos: GuardPosition
        val obstacles = mutableSetOf<Position>()

        input.forEachIndexed { idx, row ->
            row.forEachIndexed { rowIdx, c ->
                when (c) {
                    '^' -> initialPos = GuardPosition(Position(rowIdx, idx), Up)
                    '#' -> obstacles += Position(rowIdx, idx)
                }
            }
        }

        var currentPos = initialPos
        val visited = mutableSetOf<Position>()
        while (currentPos.position.x in 0..<input[0].length && currentPos.position.y in input.indices) {
            visited += currentPos.position
            val nextPos = currentPos.nextPosition()
            if (nextPos in obstacles) {
                currentPos = currentPos.turnRight()
            } else {
                currentPos = currentPos.forward()
            }
        }
        return Part1Result(visited.size, visited)
    }

    fun part2(input: List<String>, visited: MutableSet<Position>): Int {
        lateinit var initialPos: GuardPosition
        val obstacles = mutableSetOf<Position>()

        input.forEachIndexed { idx, row ->
            row.forEachIndexed { rowIdx, c ->
                when (c) {
                    '^' -> initialPos = GuardPosition(Position(rowIdx, idx), Up)
                    '#' -> obstacles += Position(rowIdx, idx)
                }
            }
        }

        var inLoop = 0
        visited.forEach { visit ->
            val newObstacles = obstacles + visit

            var currentPos = initialPos
            val guardVisited = mutableSetOf<GuardPosition>()
            while (currentPos.position.x in 0..<input[0].length && currentPos.position.y in input.indices) {
                if (currentPos in guardVisited) {
                    inLoop++
                    break
                }

                guardVisited += currentPos
                val nextPos = currentPos.nextPosition()
                if (nextPos in newObstacles) {
                    currentPos = currentPos.turnRight()
                } else {
                    currentPos = currentPos.forward()
                }
            }
        }

        return inLoop
    }

    val input = File("/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/06.txt").readLines()
    val part1 = part1(input)
    val result2 = part2(input, part1.visited)
    println("Result 2: $result2")
}
