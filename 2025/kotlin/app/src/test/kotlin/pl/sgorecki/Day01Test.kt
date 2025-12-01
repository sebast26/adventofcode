package pl.sgorecki

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day01Test {

    @Test
    fun `rotate right - finish on zero`() {
        assertEquals(1, day01part02(listOf("R50")))
    }

    @Test
    fun `R150 - finish on zero`() {
        assertEquals(2, day01part02(listOf("R150")))
    }

    @Test
    fun `L50 - finish on zero`() {
        assertEquals(1, day01part02(listOf("L50")))
    }

    @Test
    fun `L150 - finish on zero`() {
        assertEquals(2, day01part02(listOf("L150")))
    }
}