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

    @Test
    fun `part2 - works for 1st blink`() {
        expectThat(part2Test(listOf("125 17"), 1)).isEqualTo(3L)
    }

    @Test
    fun `part2 - works for 2nd blink`() {
        expectThat(part2Test(listOf("253000 1 7"), 1)).isEqualTo(4L)
    }

    @Test
    fun `part2 - works for 6th blink`() {
        expectThat(part2Test(listOf("1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32"), 1)).isEqualTo(22L)
    }

    @Test
    fun `part2 - works for 6th blink from 1st iteration`() {
        expectThat(part2Test(listOf("125 17"), 6)).isEqualTo(22L)
    }
}