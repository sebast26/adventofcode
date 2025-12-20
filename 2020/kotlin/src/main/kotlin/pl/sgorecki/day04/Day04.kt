package pl.sgorecki.day04

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "04.txt", 228)
}

typealias Passport = List<Field>

data class Field(val name: String, val value: String)

val allNames = listOf("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

fun part1(lines: List<String>): Int {
    val passports = mutableListOf<List<Field>>()
    var fields = mutableListOf<Field>()
    lines.forEach { line ->
        if (line.isEmpty()) {
            passports += fields
            fields = mutableListOf()
            return@forEach
        }
        val fieldsStr = line.split(" ")
        fieldsStr.forEach { str ->
            val (name, value) = str.split(':')
            fields += Field(name, value)
        }
    }
    passports += fields

    return passports.count { it.isValid() }
}

private fun Passport.isValid(): Boolean {
    val passportNames = map { it.name }
    return passportNames.containsAll(allNames)
}