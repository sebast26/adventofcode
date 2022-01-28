package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
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

func findMostCommonBits(lines []string) uint64 {
	numLen := len(lines[0])
	bits := ""
	for i := 0; i < numLen; i++ {
		countOnes := 0
		for _, line := range lines {
			if string(line[i]) == "1" {
				countOnes++
			}
		}
		log.Printf("countOnes: %d", countOnes)
		if countOnes > len(lines) / 2 {
			bits += "1"
		} else {
			bits += "0"
		}
	}

	ret, err := strconv.ParseInt(bits, 2, 64)
	if err != nil {
		log.Fatalf("error when converting string %s\n", bits)
	}
	return uint64(ret)
}

func main() {
	lines, err := readInput("03a.input")
	if err != nil {
		log.Fatalf("error reading input file")
	}

	gamma := findMostCommonBits(lines)
	fmt.Printf("gamma: %b\n", gamma)
	epsilon := ^gamma
	fmt.Printf("epsilon: %b\n", epsilon)
	fmt.Printf("power cosumption: %d\n", gamma * epsilon)
}
