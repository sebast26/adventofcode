package me.sgorecki

import me.sgorecki.BlockType.FileBlock
import me.sgorecki.BlockType.FreeSpaceBlock
import java.io.File
import java.math.BigInteger

enum class BlockType {
    FileBlock, FreeSpaceBlock
}

sealed interface Block {
    val type: BlockType
    val size: Int
    val wasMoved: Boolean

    fun move(): Block
}

data class DiskFile(val id: Int, override val size: Int, override val wasMoved: Boolean = false) : Block {
    override val type = FileBlock
    override fun move() = if (size > 0) copy(size = size - 1) else FreeSpace(0)
}

data class FreeSpace(override val size: Int, override val wasMoved: Boolean = false) : Block {
    override val type = FreeSpaceBlock
    override fun move() = if (size > 0) copy(size = size - 1) else FreeSpace(0)
    fun fit(block: DiskFile): List<Block> {
        val blocks = mutableListOf<Block>(block.copy(wasMoved = true))
        if (size > block.size) {
            blocks.add(FreeSpace(size = size - block.size))
        }
        return blocks
    }
}

data class DiskMap(val numbers: List<Int>) {
    private val blocks: List<Block>

    init {
        blocks = toBlocks()
    }

    fun toBlocks() = numbers.mapIndexed { index, number ->
        if (index % 2 == 0) {
            DiskFile(index / 2, number)
        } else {
            FreeSpace(number)
        }
    }

    fun defrag(): List<DiskFile> {
        var idxForward = 0
        var idxBackward = blocks.size - 1
        val copyBlocks = blocks.toMutableList()
        val defragBlocks = mutableListOf<DiskFile>()
        var buffer: Array<DiskFile?>? = null
        while (idxBackward >= idxForward) {

            when (val currentBlock = copyBlocks[idxForward]) {
                is DiskFile -> {
                    idxForward++
                    defragBlocks.add(currentBlock)
                    continue
                }

                is FreeSpace -> {
                    if (currentBlock.size == 0) {
                        idxForward++
                        continue
                    }
                }
            }

            when (val currentBlock = copyBlocks[idxBackward]) {
                is DiskFile -> {
                    if (currentBlock.size == 0) {
                        idxBackward--
                        continue
                    }
                }

                is FreeSpace -> {
                    idxBackward--
                    continue
                }
            }

            // forward is non-empty free space, backward is non-empty file block
            if (buffer == null) {
                val freeSpace = copyBlocks[idxForward] as FreeSpace
                buffer = Array(freeSpace.size) { null }
                val fileBlock = copyBlocks[idxBackward].move() as DiskFile
                buffer[0] = DiskFile(id = fileBlock.id, size = 1)
                copyBlocks[idxBackward] = fileBlock
            } else {
                val bufferIdx = buffer.indexOf(null)
                val fileBlock = copyBlocks[idxBackward].move() as DiskFile
                buffer[bufferIdx] = DiskFile(id = fileBlock.id, size = 1)
                copyBlocks[idxBackward] = fileBlock
            }

            if (buffer.indexOf(null) == -1) { // buffer is full
                val elements = buffer.compress()
                defragBlocks.addAll(elements)
                idxForward++
                buffer = null
            }
        }

        return defragBlocks
    }

    fun defrag2(): List<Block> {
        val defragBlocks = blocks.toMutableList()
        for (position in defragBlocks.size - 1 downTo 0) {
            val block = defragBlocks[position]
            if (block is DiskFile && !block.wasMoved) {
                val fitIdx = defragBlocks.findFirstFreeBlockThatFits(block.size)
                if (fitIdx == -1 || position <= fitIdx) {
                    continue
                }

                // relocate to new position
                val space = defragBlocks[fitIdx] as FreeSpace
                val newBlocks = space.fit(block)
                defragBlocks.removeAt(fitIdx)
                defragBlocks.addAll(fitIdx, newBlocks)

                // replace original block with free space
                val i = defragBlocks.indexOf(block)
                defragBlocks.removeAt(i)
                defragBlocks.add(i, FreeSpace(size = block.size))
            }
        }
        return defragBlocks
    }
}

private fun List<Block>.findFirstFreeBlockThatFits(size: Int) =
    indexOfFirst { it.type == FreeSpaceBlock && it.size >= size }

private fun Array<DiskFile?>.compress() = fold(mutableListOf<DiskFile>()) { acc, block ->
    if (acc.isEmpty()) {
        acc.add(block ?: throw IllegalStateException("Block cannot be null!"))
    } else {
        val last = acc.last()
        if (last.id == block?.id) {
            acc.removeLast()
            acc.add(last.copy(size = last.size + 1))
        } else {
            acc.add(block ?: throw IllegalStateException("Block cannot be null!"))
        }
    }
    acc
}

fun main() {
    fun part1(input: List<String>): BigInteger {
        val numbers = input[0].map { it.digitToIntOrNull() ?: throw IllegalArgumentException("Unknown digit!") }
        val diskMap = DiskMap(numbers)
        val defrag = diskMap.defrag()
        return defrag.checksum2().first
    }

    fun part2(input: List<String>): BigInteger {
        val numbers = input[0].map { it.digitToIntOrNull() ?: throw IllegalArgumentException("Unknown digit!") }
        val diskMap = DiskMap(numbers)
        val defrag = diskMap.defrag2()
        return defrag.checksum2().first
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/09sample.txt", 1928.toBigInteger())
    solve(
        ::part1,
        "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/09.txt",
        6200294120911.toBigInteger()
    )
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/09sample.txt", 2858.toBigInteger())
    solve(::part2, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/09.txt", 6230529137840.toBigInteger())
}

private fun List<Block>.checksum2() =
    fold(BigInteger.ZERO to 0) { (sum, idx), block ->
        if (block is DiskFile) {
            var temp = BigInteger.ZERO
            for (pos in idx..<idx + block.size) {
                temp = temp.plus(block.id.toBigInteger().multiply(pos.toBigInteger()))
            }
            sum.plus(temp) to idx + block.size
        } else {
            sum to idx + block.size
        }
    }


private fun <E> solve(resultFn: (List<String>) -> E, input: String, expected: E) {
    val lines = File(input).readLines()
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! Your answer $actual is incorrect.")
    }
}