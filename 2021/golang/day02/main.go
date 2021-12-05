package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func readInput(filename string) (numbers []string, err error) {
	f, err := os.Open(filename)
	if err != nil {
		return
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		numbers = append(numbers, scanner.Text())
	}
	err = scanner.Err()
	return
}

func parseInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		log.Fatalf("cannot parse int from %s\n", s)
	}
	return i
}

func readCommand(command string) (horizontal int, depth int) {
	if strings.HasPrefix(command, "forward") {
		n := parseInt(strings.ReplaceAll(command, "forward ", ""))
		return n, 0
	}
	if strings.HasPrefix(command, "down") {
		n := parseInt(strings.ReplaceAll(command, "down ", ""))
		return 0, n
	}
	if strings.HasPrefix(command, "up") {
		n := parseInt(strings.ReplaceAll(command, "up ", ""))
		return 0, -n
	}
	log.Fatalf("unknown command")
	return 0, 0
}

func findFinalPosition(commands []string) (horizontal int, depth int) {
	for _, c := range commands {
		h, d := readCommand(c)
		horizontal += h
		depth += d
	}
	return
}

func readCommandWithAim(command string, aim int) (horizontal int, depth int, newAim int) {
	if strings.HasPrefix(command, "forward") {
		n := parseInt(strings.ReplaceAll(command, "forward ", ""))
		return n, aim * n, 0
	}
	if strings.HasPrefix(command, "down") {
		n := parseInt(strings.ReplaceAll(command, "down ", ""))
		return 0, 0, n
	}
	if strings.HasPrefix(command, "up") {
		n := parseInt(strings.ReplaceAll(command, "up ", ""))
		return 0, 0, -n
	}
	log.Fatalf("unknown command")
	return 0, 0, 0
}

func findFinalPositionWithAim(commands []string) (horizontal int, depth int) {
	aim := 0
	for _, c := range commands {
		h, d, a := readCommandWithAim(c, aim)
		horizontal += h
		depth += d
		aim += a
	}
	return
}

func main() {
	commands, err := readInput("02a.input")
	if err != nil {
		log.Fatalln("error reading file 02a.input")
	}

	h, d := findFinalPosition(commands)
	fmt.Printf("multiply: %d\n", h*d)

	h, d = findFinalPositionWithAim(commands)
	fmt.Printf("multiply: %d\n", h*d)
}
