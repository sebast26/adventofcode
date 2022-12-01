package main

import (
	"bufio"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	in, err := os.Open("day01/input.txt")
	if err != nil {
		panic(err)
	}
	defer in.Close()

	scanner := bufio.NewScanner(in)

	var cal []int
	var current int
	for scanner.Scan() {
		line := strings.Trim(scanner.Text(), " \n\r\t")
		if line == "" {
			cal = append(cal, current)
			current = 0
			continue
		}
		num, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		current += num
	}

	sort.Ints(cal)

	println(cal[len(cal)-1])
	println(cal[len(cal)-1] + cal[len(cal)-2] + cal[len(cal)-3])
}
