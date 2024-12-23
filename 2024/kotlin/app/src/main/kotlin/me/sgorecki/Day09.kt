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

    fun move(): Block
}

data class DiskFile(val id: Int, override val size: Int) : Block {
    override val type = FileBlock
    override fun move() = if (size > 0) copy(size = size - 1) else FreeSpace(0)
}

data class FreeSpace(override val size: Int) : Block {
    override val type = FreeSpaceBlock
    override fun move() = if (size > 0) copy(size = size - 1) else FreeSpace(0)
}

data class DiskMap(val numbers: List<Int>) {
    private val blocks: List<Block>

    init {
        blocks = toBlocks().also(::println)
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
}

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
        val defrag = diskMap.defrag().also(::println)
        return defrag.checksum().first
    }

    fun part2(input: List<String>): BigInteger {
        return BigInteger.ZERO
    }

    solve(::part1, "/Users/seba/projects/priv/code/adventofcode/2024/kotlin/inputs/09sample.txt", 1928.toBigInteger())
}

private fun List<DiskFile>.checksum() =
    fold(BigInteger.ZERO to 0) { (sum, idx), diskFile ->
        var temp = BigInteger.ZERO
        for (pos in idx..<idx + diskFile.size) {
            temp = temp.plus(diskFile.id.toBigInteger().multiply(pos.toBigInteger()))
        }
        sum.plus(temp) to idx + diskFile.size
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