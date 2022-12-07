package main

import (
	"bufio"
	"fmt"
	"os"
	"sgorecki.me/adventofcode/2022/common"
)

func main() {
	in, err := os.Open("day06/input.txt")
	if err != nil {
		panic(err)
	}
	defer in.Close()

	scanner := bufio.NewScanner(in)
	scanner.Scan()
	chars := scanner.Bytes()
	fmt.Println(StartOfPacket(chars))
	fmt.Println(StartOfMessage(chars))
}

func StartOfPacket(chars []byte) int {
	for i := 4; i < len(chars); i++ {
		sl := chars[i-4 : i]
		sl2 := common.EliminateDuplicates(sl)
		if len(sl) == len(sl2) {
			return i
		}
	}
	return -1
}

func StartOfMessage(chars []byte) int {
	for i := 14; i < len(chars); i++ {
		sl := chars[i-14 : i]
		sl2 := common.EliminateDuplicates(sl)
		if len(sl) == len(sl2) {
			return i
		}
	}
	return -1
}
