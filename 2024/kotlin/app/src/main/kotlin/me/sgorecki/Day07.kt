package me.sgorecki

import me.sgorecki.Operation.*
import java.io.File
import java.math.BigInteger
import kotlin.math.pow

val inputRegex = """(?<res>\d+):(?<nums>(?:\s+\d+)*)""".toRegex()

enum class Operation {
    ADD, MUL, CON
}

data class Equation(val expectedResult: BigInteger, val numbers: List<Int>) {
    private val numberOfOperations = numbers.count() - 1

    fun operationPermute(availableOps: List<Operation>): List<List<Operation>> {
        fun permute(temp: List<List<Operation>>, ops: List<Operation>): List<List<Operation>> {
            if (ops.count() == numberOfOperations) {
                return temp + listOf(ops)
            }
            return availableOps.flatMap {
                val newOps = ops + it
                permute(temp, newOps)
            }
        }
        return permute(emptyList(), emptyList())
    }

    fun calculateResult(ops: List<Operation>): BigInteger {
        return numbers.foldIndexed(numbers[0].toBigInteger()) { index, acc, num ->
            if (index == 0) {
                return@foldIndexed acc
            }
            return@foldIndexed when (ops[index - 1]) {
                ADD -> acc + num.toBigInteger()
                MUL -> acc * num.toBigInteger()
                CON -> acc * (10.toDouble().pow(num.digits())).toInt().toBigInteger() + num.toBigInteger()
            }
        }
    }

    private fun Int.digits() = this.toString().length

    fun canBeCalculated(availableOps: List<Operation>) =
        operationPermute(availableOps).any { calculateResult(it) == expectedResult }
}

fun part1(input: List<String>) = parseEquations(input)
    .filter { it.canBeCalculated(listOf(ADD, MUL)) }
    .sumOf { it.expectedResult }

fun part2(input: List<String>) = parseEquations(input)
    .filter { it.canBeCalculated(listOf(ADD, MUL, CON)) }
    .sumOf { it.expectedResult }

private fun parseEquations(input: List<String>) = input.map { line ->
    val (res, nums) = inputRegex.matchEntire(line)
        ?.destructured
        ?: throw IllegalArgumentException("Incorrect input line $line")
    Equation(
        expectedResult = res.toBigInteger(),
        numbers = nums.split(" ")
            .filter { it != "" }
            .map { it.toInt() }
    )
}

fun main() {
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/07sample.txt", 3749.toBigInteger())
    solve(
        ::part1,
        "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/07.txt",
        2437272016585.toBigInteger()
    )
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/07sample.txt", 11387.toBigInteger())
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/07.txt", 162987117690649.toBigInteger())
}

private fun solve(resultFn: (List<String>) -> BigInteger, input: String, expected: BigInteger) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}