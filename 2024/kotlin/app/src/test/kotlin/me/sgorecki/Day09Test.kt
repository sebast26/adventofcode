package me.sgorecki

import org.junit.jupiter.api.Test
import strikt.api.expectThat
import strikt.assertions.isEqualTo

class Day09Test {

    @Test
    fun `to blocks`() {
        // given
        val diskMap = DiskMap(listOf(2, 3, 3, 3, 1, 3, 3, 1, 2, 1, 4, 1, 4, 1, 3, 1, 4, 0, 2))

        // when
        val actual = diskMap.toBlocks()

        // then
        expectThat(actual).isEqualTo(listOf(
            DiskFile(0, 2),
            FreeSpace(3),
            DiskFile(1, 3),
            FreeSpace(3),
            DiskFile(2, 1),
            FreeSpace(3),
            DiskFile(3, 3),
            FreeSpace(1),
            DiskFile(4, 2),
            FreeSpace(1),
            DiskFile(5, 4),
            FreeSpace(1),
            DiskFile(6, 4),
            FreeSpace(1),
            DiskFile(7, 3),
            FreeSpace(1),
            DiskFile(8, 4),
            FreeSpace(0),
            DiskFile(9, 2)
        ))
    }
}