package me.sgorecki

import org.junit.jupiter.api.Test
import strikt.api.expectThat
import strikt.assertions.hasSize
import strikt.assertions.isEqualTo
import java.math.BigInteger
import java.util.*

class Day11Test {
    @Test
    fun `initial step test`() {
        val list = LinkedList(
            listOf(
                125.toBigInteger(),
                17.toBigInteger(),
            )
        )

        val actual = blink(list)

        expectThat(actual).isEqualTo(LinkedList(listOf(253000.toBigInteger(), BigInteger.ONE, 7.toBigInteger())))
    }

    @Test
    fun `after 2nd blink test`() {
        val list = LinkedList(
            listOf(
                253000.toBigInteger(),
                BigInteger.ONE,
                7.toBigInteger(),
            )
        )

        val actual = blink(list)

        expectThat(actual).isEqualTo(LinkedList(listOf(
            253.toBigInteger(),
            BigInteger.ZERO,
            2024.toBigInteger(),
            14168.toBigInteger()
        )))
    }

    @Test
    fun `after 3rd blink test`() {
        val list = LinkedList(
            listOf(
                253.toBigInteger(),
                BigInteger.ZERO,
                2024.toBigInteger(),
                14168.toBigInteger()
            )
        )

        val actual = blink(list)

        expectThat(actual).isEqualTo(LinkedList(listOf(
            512072.toBigInteger(),
            BigInteger.ONE,
            20.toBigInteger(),
            24.toBigInteger(),
            28676032.toBigInteger(),
        )))
    }

    @Test
    fun `after 4th blink test`() {
        val list = LinkedList(
            listOf(
                512072.toBigInteger(),
                BigInteger.ONE,
                20.toBigInteger(),
                24.toBigInteger(),
                28676032.toBigInteger(),
            )
        )

        val actual = blink(list)

        expectThat(actual).isEqualTo(LinkedList(listOf(
            512.toBigInteger(),
            72.toBigInteger(),
            2024.toBigInteger(),
            2.toBigInteger(),
            BigInteger.ZERO,
            2.toBigInteger(),
            4.toBigInteger(),
            2867.toBigInteger(),
            6032.toBigInteger(),
        )))
    }

    @Test
    fun `after 5th blink test`() {
        val list = LinkedList(
            listOf(
                512.toBigInteger(),
                72.toBigInteger(),
                2024.toBigInteger(),
                2.toBigInteger(),
                BigInteger.ZERO,
                2.toBigInteger(),
                4.toBigInteger(),
                2867.toBigInteger(),
                6032.toBigInteger(),
            )
        )

        val actual = blink(list)

        expectThat(actual).isEqualTo(LinkedList(listOf(
            1036288.toBigInteger(),
            7.toBigInteger(),
            2.toBigInteger(),
            20.toBigInteger(),
            24.toBigInteger(),
            4048.toBigInteger(),
            BigInteger.ONE,
            4048.toBigInteger(),
            8096.toBigInteger(),
            28.toBigInteger(),
            67.toBigInteger(),
            60.toBigInteger(),
            32.toBigInteger(),
        )))
    }

    @Test
    fun `after 6th blink test`() {
        val list = LinkedList(
            listOf(
                1036288.toBigInteger(),
                7.toBigInteger(),
                2.toBigInteger(),
                20.toBigInteger(),
                24.toBigInteger(),
                4048.toBigInteger(),
                BigInteger.ONE,
                4048.toBigInteger(),
                8096.toBigInteger(),
                28.toBigInteger(),
                67.toBigInteger(),
                60.toBigInteger(),
                32.toBigInteger(),
            )
        )

        val actual = blink(list)

        expectThat(actual).hasSize(22)
    }

}