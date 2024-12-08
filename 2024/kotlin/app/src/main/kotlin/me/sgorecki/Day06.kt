package me.sgorecki

import me.sgorecki.Direction.Up
import java.io.File

enum class Direction(val x: Int, val y: Int) {
    Up(0, -1),
    Right(1, 0),
    Down(0, 1),
    Left(-1, 0)
}

fun main() {
    data class Position(val x: Int, val y: Int)
    data class GuardPosition(val position: Position, val direction: Direction) {
        fun nextPosition() = Position(position.x + direction.x, position.y + direction.y)
        fun turnRight() = this.copy(
            direction = Direction.entries[(direction.ordinal + 1) % Direction.entries.size]
        )
        fun forward() = this.copy(
            position = nextPosition()
        )
    }

    fun part1(input: List<String>): Int {
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
        return visited.size
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/06sample.txt", 41)
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/06.txt", 4665)
}

private fun solve(resultFn: (List<String>) -> Int, input: String, expected: Int) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}
