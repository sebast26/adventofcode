package pl.sgorecki.day05

import pl.sgorecki.common.solve

val doubleLetters = ('a'..'z').map { ch -> "$ch$ch" }
val vowels = listOf('a', 'e', 'i', 'o', 'u')
val allPairs = ('a'..'z').flatMap { ch ->
    ('a'..'z').map { other -> "$ch$other" }
}
val repeatingLetters = ('a'..'z').flatMap { ch ->
    ('a'..'z').map { other -> "$ch$other$ch" }
}

fun main() {
    solve(::part1, "05.txt", 255)
    println("qjhvhtzxzqqjkmpb is ${if ("qjhvhtzxzqqjkmpb".isNicePart2()) "nice" else "naughty"}")
    println("xxyxx is ${if ("xxyxx".isNicePart2()) "nice" else "naughty"}")
    println("uurcxstgmygtbstg is ${if ("uurcxstgmygtbstg".isNicePart2()) "nice" else "naughty"}")
    println("ieodomkazucvgmuy is ${if ("ieodomkazucvgmuy".isNicePart2()) "nice" else "naughty"}")
    solve(::part2, "05.txt", 255)
}

fun part1(lines: List<String>) = lines.count { it.isNice() }

fun part2(lines: List<String>) = lines.count { it.isNicePart2() }

private fun String.isNice() =
    !containsSpecialStrings() && containsVowels(3) && containsDoubleLetter()

private fun String.containsSpecialStrings() =
    contains("ab") || contains("cd") || contains("pq") || contains("xy")

private fun String.containsDoubleLetter(): Boolean {
    doubleLetters.forEach { doubleLetters ->
        if (this.contains(doubleLetters)) return true
    }
    return false
}

private fun String.containsVowels(count: Int) = count { it in vowels } >= count

private fun String.isNicePart2() =
    containsPairTwice() && hasRepeatingLetter()

private fun String.containsPairTwice(): Boolean {
    allPairs.forEach { pair ->
        if (this.replace(pair, "").length <= this.length - 4) return true
    }
    return false
}

private fun String.hasRepeatingLetter(): Boolean {
    repeatingLetters.forEach { repeat ->
        if (this.contains(repeat)) return true
    }
    return false
}