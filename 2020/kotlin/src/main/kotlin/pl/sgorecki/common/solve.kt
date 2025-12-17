package pl.sgorecki.common

fun <E> solve(resultFn: (List<String>) -> E, input: String, expected: E) {
    val lines = readLines(input)
    val actual = resultFn(lines)
    if (actual == expected) {
        println("Correct! The answer is $expected.")
    } else {
        println("Wrong! The answer $actual is incorrect.")
    }
}