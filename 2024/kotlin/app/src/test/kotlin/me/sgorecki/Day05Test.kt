package me.sgorecki

import org.junit.jupiter.api.Test
import strikt.api.expectThat
import strikt.assertions.isTrue

class Day05Test {

    val rules = listOf(
        47 to 53,
        97 to 13,
        97 to 61,
        97 to 47,
        75 to 29,
        61 to 13,
        75 to 53,
        29 to 13,
        97 to 29,
        53 to 29,
        61 to 53,
        97 to 53,
        61 to 29,
        47 to 13,
        75 to 47,
        97 to 75,
        47 to 61,
        75 to 61,
        47 to 29,
        75 to 13,
        53 to 13
    )

    @Test
    fun `should return true`() {
        expectThat(PageUpdate(rules, listOf(75, 47, 61, 53, 29)).obeyRules()).isTrue()
    }
}