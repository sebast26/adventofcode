package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func readInput(filename string) (numbers []int, err error) {
	f, err := os.Open(filename)
	if err != nil {
		return
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		number, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		numbers = append(numbers, number)
	}
	err = scanner.Err()
	return
}

func computeNumberOfIncreases(measurements []int) int {
	count := 0
	for i, m := range measurements {
		if i == 0 {
			continue
		}
		if measurements[i-1] < m {
			count++
		}
	}
	return count
}

func main() {
	measurements, err := readInput("01a.input")
	if err != nil {
		log.Fatalln("error when reading file")
	}
	increases := computeNumberOfIncreases(measurements)
	fmt.Printf("the number of times a depth measurement increases: %d", increases)
}
