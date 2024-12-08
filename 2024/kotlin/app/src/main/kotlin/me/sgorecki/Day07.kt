package me.sgorecki

import me.sgorecki.Operation.ADD
import me.sgorecki.Operation.MUL
import java.io.File
import java.math.BigInteger

val inputRegex = """(?<res>\d+):(?<nums>(?:\s+\d+)*)""".toRegex()

enum class Operation {
    ADD, MUL
}

data class Equation(val expectedResult: BigInteger, val numbers: List<Int>) {
    private val numberOfOperations = numbers.count() - 1

    fun operationPermute(): List<List<Operation>> {
        fun permute(temp: List<List<Operation>>, ops: List<Operation>): List<List<Operation>> {
            if (ops.count() == numberOfOperations) {
                return temp + listOf(ops)
            }
            val opsA = ops + ADD
            val opsM = ops + MUL
            val listA = permute(temp, opsA)
            val listB = permute(temp, opsM)
            return listA + listB
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
            }
        }
    }

    fun canBeCalculated() = operationPermute().any { calculateResult(it) == expectedResult }
}

fun part1(input: List<String>) = parseEquations(input)
    .filter { it.canBeCalculated() }
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
    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/07.txt", 2437272016585.toBigInteger())
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