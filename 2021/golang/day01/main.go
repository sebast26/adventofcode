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

func computeSlidingWindowIncreases(measurements []int, slidingWindowSize int) int {
	prevSum := 0
	for i := 0; i < slidingWindowSize; i++ {
		prevSum += measurements[i]
	}

	tempSum := prevSum
	count := 0
	for i := slidingWindowSize; i < len(measurements); i++ {
		tempSum = tempSum - measurements[i-slidingWindowSize] + measurements[i]
		if tempSum > prevSum {
			count++
		}
		prevSum = tempSum
	}
	return count
}

func main() {
	measurements, err := readInput("01a.input")
	if err != nil {
		log.Fatalln("error when reading file")
	}
	increases := computeNumberOfIncreases(measurements)
	fmt.Printf("the number of times a depth measurement increases: %d\n", increases)

	slidingIncreases := computeSlidingWindowIncreases(measurements, 3)
	fmt.Printf("the number of times a depth measurement increases in 3: %d\n", slidingIncreases)
}
