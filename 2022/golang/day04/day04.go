package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	in, err := os.Open("day04/input.txt")
	if err != nil {
		panic("error reading file")
	}
	defer in.Close()

	var fully, overlap int
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		line := scanner.Text()
		sec := strings.Split(line, ",")
		if len(sec) != 2 {
			panic("section must contain 2 elements")
		}
		s1 := NewSection(sec[0])
		s2 := NewSection(sec[1])
		if s1.FullyOverlap(s2) || s2.FullyOverlap(s1) {
			fully++
		}
		if s1.Overlap(s2) || s2.Overlap(s1) {
			overlap++
		}
	}
	fmt.Println(fully)
	fmt.Println(overlap)
}
