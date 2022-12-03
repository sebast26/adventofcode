package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	in, err := os.Open("day03/input.txt")
	if err != nil {
		panic(err)
	}
	defer in.Close()

	scanner := bufio.NewScanner(in)
	var total int
	for scanner.Scan() {
		rs := NewRucksack(scanner.Text())
		total += Priority(rs.CommonItem())
	}
	fmt.Println(total)
}
