package pl.sgorecki

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day01Test {

    @Test
    fun `passZero for right rotator`() {
        assertEquals(0, RightRotator(40).zeroPassesFrom(40))
        assertEquals(1, RightRotator(50).zeroPassesFrom(50))
        assertEquals(1, RightRotator(40).zeroPassesFrom(80))
        assertEquals(2, RightRotator(230).zeroPassesFrom(50))
        assertEquals(0, RightRotator(50).zeroPassesFrom(49))
        assertEquals(1, RightRotator(149).zeroPassesFrom(50))
        assertEquals(4, RightRotator(400).zeroPassesFrom(99))
    }

    @Test
    fun `passZero for left rotator`() {
        assertEquals(0, LeftRotator(40).zeroPassesFrom(50))
        assertEquals(1, LeftRotator(60).zeroPassesFrom(50))
        assertEquals(1, LeftRotator(50).zeroPassesFrom(50))
        assertEquals(2, LeftRotator(230).zeroPassesFrom(50))
        assertEquals(0, LeftRotator(49).zeroPassesFrom(50))
        assertEquals(1, LeftRotator(50).zeroPassesFrom(49))
        assertEquals(4, LeftRotator(400).zeroPassesFrom(99))
        assertEquals(0, LeftRotator(5).zeroPassesFrom(0))
    }
}