package main

import (
	"github.com/golang-collections/collections/stack"
	"regexp"
	"sgorecki.me/adventofcode/2022/common"
	"strconv"
	"strings"
)

type Stacks []stack.Stack

func (s Stacks) Apply(instr Instruction) {
	for i := 0; i < instr.times; i++ {
		val := s[instr.from].Pop()
		s[instr.to].Push(val)
	}
}

func (s Stacks) Apply9001(instr Instruction) {
	temp := stack.New()
	for i := 0; i < instr.times; i++ {
		val := s[instr.from].Pop()
		temp.Push(val)
	}
	for i := 0; i < instr.times; i++ {
		val := temp.Pop()
		s[instr.to].Push(val)
	}
}

func (s Stacks) PeekAll() string {
	var sb strings.Builder
	for i := 1; i < len(s); i++ {
		sb.WriteRune(s[i].Peek().(rune))
	}
	return sb.String()
}

type Instruction struct {
	times, from, to int
}

func NewInstruction(l string) Instruction {
	matches := re.FindStringSubmatch(l)
	ti := re.SubexpIndex("times")
	fi := re.SubexpIndex("from")
	toi := re.SubexpIndex("to")
	times, err1 := strconv.Atoi(matches[ti])
	from, err2 := strconv.Atoi(matches[fi])
	to, err3 := strconv.Atoi(matches[toi])
	common.NoErrors(err1, err2, err3)
	return Instruction{times: times, from: from, to: to}
}

var (
	re = regexp.MustCompile(`move (?P<times>\d+) from (?P<from>\d+) to (?P<to>\d+)`)
)
