package me.sgorecki

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
        ), listOf(Coord(0, 0))
    )

}