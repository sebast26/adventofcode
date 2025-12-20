package pl.sgorecki.day04

import java.security.MessageDigest

fun main() {
    val md = MessageDigest.getInstance("MD5")
    println("Part 1: ${part(md, "iwrupvqb", "00000")}")
    println("Part 2: ${part(md, "iwrupvqb", "000000")}")
}

fun part(md: MessageDigest, input: String, pattern: String): Long {
    (0..Long.MAX_VALUE).forEach { num ->
        val hex = md.digest("$input$num".toByteArray()).toHexString()
        if (hex.startsWith(pattern)) {
            return num
        }
    }
    return 0
}