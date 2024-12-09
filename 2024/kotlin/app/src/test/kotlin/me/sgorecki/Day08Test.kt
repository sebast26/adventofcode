package me.sgorecki

import org.junit.jupiter.api.Test
import strikt.api.expectThat
import strikt.assertions.containsExactlyInAnyOrder
import strikt.assertions.hasSize
import strikt.assertions.isEqualTo

class Day08Test {
    @Test
    fun `location distance`() {
        // given
        val one = Location(5, 5)
        val two = Location(7, 10)

        // when
        val actual = one.distance(two)

        // then
        expectThat(actual).isEqualTo(Distance(2, 5))
        expectThat(two.distance(one)).isEqualTo(Distance(-2, -5))
    }

    @Test
    fun `location combinations`() {
        // given
        val ra = ResonantAntennas(
            'C', setOf(
                Location(1, 1),
                Location(2, 2),
                Location(3, 3)
            )
        )

        // when
        val actual = ra.locationCombinations()

        // then
        expectThat(actual).hasSize(3)
        expectThat(actual).containsExactlyInAnyOrder(
            Location(1, 1) to Location(2, 2),
            Location(1, 1) to Location(3, 3),
            Location(2, 2) to Location(3, 3)
        )
    }
}