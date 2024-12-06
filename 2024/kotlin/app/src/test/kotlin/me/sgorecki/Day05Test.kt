package me.sgorecki

import org.junit.jupiter.api.Test
import strikt.api.expectThat
import strikt.assertions.isFalse
import strikt.assertions.isTrue

class Day05Test {

    // before / after
    val rules = listOf(
        47 to 53, // 47 needs to be before 53
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
        expectThat(PageUpdate(rules, listOf(75, 47, 61, 53, 29)).inRightOrder()).isTrue()
    }

    @Test
    fun `should return true 2`() {
        expectThat(PageUpdate(rules, listOf(97, 61, 53, 29, 13)).inRightOrder()).isTrue()
    }

    @Test
    fun `should return true 3`() {
        expectThat(PageUpdate(rules, listOf(75, 29, 13)).inRightOrder()).isTrue()
    }

    @Test
    fun `should return false`() {
        expectThat(PageUpdate(rules, listOf(75, 97, 47, 61, 53)).inRightOrder()).isFalse()
    }

    @Test
    fun `should return false 2`() {
        expectThat(PageUpdate(rules, listOf(61, 13, 29)).inRightOrder()).isFalse()
    }

    @Test
    fun `should return false 3`() {
        expectThat(PageUpdate(rules, listOf(97, 13, 75, 29, 47)).inRightOrder()).isFalse()
    }
}