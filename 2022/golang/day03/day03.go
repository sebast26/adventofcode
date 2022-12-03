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
	var total, totalGroup int
	var group []string
	for scanner.Scan() {
		line := scanner.Text()
		rs := NewRucksack(line)
		total += Priority(rs.CommonItem())
		group = append(group, line)
		if len(group) == 3 {
			gr := NewGroup(group)
			totalGroup += Priority(gr.CommonItem())
			group = make([]string, 0)
		}
	}
	fmt.Println(total)
	fmt.Println(totalGroup)
}
