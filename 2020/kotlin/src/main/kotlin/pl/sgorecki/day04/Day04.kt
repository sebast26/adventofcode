package pl.sgorecki.day04

import pl.sgorecki.common.solve

fun main() {
    solve(::part1, "04.txt", 228)
    solve(::part2, "04.txt", 175)
}

typealias Passport = List<Field>

data class Field(val name: String, val value: String)

val allNames = listOf("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

fun part1(lines: List<String>): Int {
    val passports = parsePassports(lines)
    return passports.count { it.allRequiredPresent() }
}

fun part2(lines: List<String>): Int {
    val passports = parsePassports(lines)
    return passports.count { it.allRequiredPresent() && it.allValid() }
}

private fun parsePassports(lines: List<String>): List<Passport> {
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
    return passports
}

private fun Passport.allRequiredPresent(): Boolean {
    val passportNames = map { it.name }
    return passportNames.containsAll(allNames)
}

private fun Passport.allValid(): Boolean {
    return first { it.name == "byr" }.validBirthYear() &&
            first { it.name == "iyr" }.validIssueYear() &&
            first { it.name == "eyr" }.validExpirationYear() &&
            first { it.name == "hgt" }.validHeight() &&
            first { it.name == "hcl" }.validHairColor() &&
            first { it.name == "ecl" }.validEye() &&
            first { it.name == "pid" }.validPassport()
}

val height = "(\\d+)(cm|in)".toRegex()
val hair = "#[0-9a-f]{6}".toRegex()
val eye = "amb|blu|brn|gry|grn|hzl|oth".toRegex()
val passport = "[0-9]{9}".toRegex()

private fun Int.isBetween(low: Int, high: Int) = this in low..high
private fun Field.validBirthYear() = value.toInt().isBetween(1920, 2002)
private fun Field.validIssueYear() = value.toInt().isBetween(2010, 2020)
private fun Field.validExpirationYear() = value.toInt().isBetween(2020, 2030)
private fun Field.validHeight(): Boolean {
    val (hValue, hFormat) = height.find(this.value)?.destructured ?: return false
    return when (hFormat) {
        "cm" -> hValue.toInt().isBetween(150, 193)
        "in" -> hValue.toInt().isBetween(59, 76)
        else -> error("Invalid height format")
    }
}

private fun Field.validHairColor() = value.matches(hair)
private fun Field.validEye() = value.matches(eye)
private fun Field.validPassport() = value.matches(passport)

