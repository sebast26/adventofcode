package me.sgorecki

import org.junit.jupiter.api.Test
import strikt.api.expectThat
import strikt.assertions.isEqualTo

class Day10Test {
    //    0123
    //    1234
    //    8765
    //    9876
    val oneTrailGrid = GridHeighs(
        arrayOf(
            arrayOf(
                High(0, Coord(0, 0)),
                High(1, Coord(1, 0)),
                High(2, Coord(2, 0)),
                High(3, Coord(3, 0)),
            ),
            arrayOf(
                High(1, Coord(0, 1)),
                High(2, Coord(1, 1)),
                High(3, Coord(2, 1)),
                High(4, Coord(3, 1)),
            ),
            arrayOf(
                High(8, Coord(0, 2)),
                High(7, Coord(1, 2)),
                High(6, Coord(2, 2)),
                High(5, Coord(3, 2)),
            ),
            arrayOf(
                High(9, Coord(0, 3)),
                High(8, Coord(1, 3)),
                High(7, Coord(2, 3)),
                High(6, Coord(3, 3)),
            ),
        ), listOf(TrailHead(Coord(0, 0)))
    )

    @Test
    fun `should find single top for trailhead`() {
        val actual = oneTrailGrid.findTops(TrailHead(Coord(0, 0)))

        expectThat(actual).isEqualTo(setOf(Coord(0, 3)))
    }

    //    ...0...
    //    ...1...
    //    ...2...
    //    6543456
    //    7.....7
    //    8.....8
    //    9.....9
    val twoTrailGrid = GridHeighs(
        arrayOf(
            arrayOf(
                High(-1, Coord(0, 0)),
                High(-1, Coord(1, 0)),
                High(-1, Coord(2, 0)),
                High(0, Coord(3, 0)),
                High(-1, Coord(4, 0)),
                High(-1, Coord(5, 0)),
                High(-1, Coord(6, 0)),
            ),
            arrayOf(
                High(-1, Coord(0, 1)),
                High(-1, Coord(1, 1)),
                High(-1, Coord(2, 1)),
                High(1, Coord(3, 1)),
                High(-1, Coord(4, 1)),
                High(-1, Coord(5, 1)),
                High(-1, Coord(6, 1)),
            ),
            arrayOf(
                High(-1, Coord(0, 2)),
                High(-1, Coord(1, 2)),
                High(-1, Coord(2, 2)),
                High(2, Coord(3, 2)),
                High(-1, Coord(4, 2)),
                High(-1, Coord(5, 2)),
                High(-1, Coord(6, 2)),
            ),
            arrayOf(
                High(6, Coord(0, 3)),
                High(5, Coord(1, 3)),
                High(4, Coord(2, 3)),
                High(3, Coord(3, 3)),
                High(4, Coord(4, 3)),
                High(5, Coord(5, 3)),
                High(6, Coord(6, 3)),
            ),
            arrayOf(
                High(7, Coord(0, 4)),
                High(-1, Coord(1, 4)),
                High(-1, Coord(2, 4)),
                High(-1, Coord(3, 4)),
                High(-1, Coord(4, 4)),
                High(-1, Coord(5, 4)),
                High(7, Coord(6, 4)),
            ),
            arrayOf(
                High(8, Coord(0, 5)),
                High(-1, Coord(1, 5)),
                High(-1, Coord(2, 5)),
                High(-1, Coord(3, 5)),
                High(-1, Coord(4, 5)),
                High(-1, Coord(5, 5)),
                High(8, Coord(6, 5)),
            ),
            arrayOf(
                High(9, Coord(0, 6)),
                High(-1, Coord(1, 6)),
                High(-1, Coord(2, 6)),
                High(-1, Coord(3, 6)),
                High(-1, Coord(4, 6)),
                High(-1, Coord(5, 6)),
                High(9, Coord(6, 6)),
            ),
        ), listOf(TrailHead(Coord(3, 0)))
    )

    @Test
    fun `should return two tops`() {
        val actual = twoTrailGrid.findTops(TrailHead(Coord(3, 0)))

        expectThat(actual).isEqualTo(setOf(Coord(0, 6), Coord(6, 6)))
    }
}