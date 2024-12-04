package me.sgorecki

import org.junit.jupiter.api.Test
import strikt.api.expectThat
import strikt.assertions.isEqualTo

class Day04Test {

    private val sampleInput = """
        MMMSXXMASM
        MSAMXMSMSA
        AMXSXMAAMM
        MSAMASMSMX
        XMASAMXAMM
        XXAMMXXAMA
        SMSMSASXSS
        SAXAMASAAA
        MAMMMXMMMM
        MXMXAXMASX
    """.trimIndent().split("\n")

    @Test
    fun `can convert to matrix`() {
        val matrix = sampleInput.toMatrix()
        expectThat(matrix[0][0]).isEqualTo('M')
        expectThat(matrix[9][9]).isEqualTo('X')
    }

}