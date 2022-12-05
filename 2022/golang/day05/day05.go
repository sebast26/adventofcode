package main

import (
	"bufio"
	"fmt"
	"github.com/golang-collections/collections/stack"
	"os"
	"strings"
)

func main() {
	in, err := os.Open("day05/input.txt")
	if err != nil {
		panic(err)
	}
	defer in.Close()
	scanner := bufio.NewScanner(in)

	stacks := initStacks()
	stacks2 := initStacks()
	for scanner.Scan() {
		line := scanner.Text()
		if strings.Index(line, "move") != -1 {
			instr := NewInstruction(line)
			stacks.Apply(instr)
			stacks2.Apply9001(instr)
		}
	}
	fmt.Println(stacks.PeekAll())
	fmt.Println(stacks2.PeekAll())
}

func initStacks() Stacks {
	stacks := make([]stack.Stack, 10)
	stacks[1].Push('R')
	stacks[1].Push('N')
	stacks[1].Push('P')
	stacks[1].Push('G')
	stacks[2].Push('T')
	stacks[2].Push('J')
	stacks[2].Push('B')
	stacks[2].Push('L')
	stacks[2].Push('C')
	stacks[2].Push('S')
	stacks[2].Push('V')
	stacks[2].Push('H')
	stacks[3].Push('T')
	stacks[3].Push('D')
	stacks[3].Push('B')
	stacks[3].Push('M')
	stacks[3].Push('N')
	stacks[3].Push('L')
	stacks[4].Push('R')
	stacks[4].Push('V')
	stacks[4].Push('P')
	stacks[4].Push('S')
	stacks[4].Push('B')
	stacks[5].Push('G')
	stacks[5].Push('C')
	stacks[5].Push('Q')
	stacks[5].Push('S')
	stacks[5].Push('W')
	stacks[5].Push('M')
	stacks[5].Push('V')
	stacks[5].Push('H')
	stacks[6].Push('W')
	stacks[6].Push('Q')
	stacks[6].Push('S')
	stacks[6].Push('C')
	stacks[6].Push('D')
	stacks[6].Push('B')
	stacks[6].Push('J')
	stacks[7].Push('F')
	stacks[7].Push('Q')
	stacks[7].Push('L')
	stacks[8].Push('W')
	stacks[8].Push('M')
	stacks[8].Push('H')
	stacks[8].Push('T')
	stacks[8].Push('D')
	stacks[8].Push('L')
	stacks[8].Push('F')
	stacks[8].Push('V')
	stacks[9].Push('L')
	stacks[9].Push('P')
	stacks[9].Push('B')
	stacks[9].Push('V')
	stacks[9].Push('M')
	stacks[9].Push('J')
	stacks[9].Push('F')
	return stacks
}
