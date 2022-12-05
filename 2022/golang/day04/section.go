package main

import (
	"strconv"
	"strings"
)

type Section struct {
	start, end int
}

func NewSection(sec string) Section {
	pos := strings.Split(sec, "-")
	if len(pos) != 2 {
		panic("section must have start and end")
	}
	start, err := strconv.Atoi(pos[0])
	if err != nil {
		panic("start is NaN")
	}
	end, err := strconv.Atoi(pos[1])
	if err != nil {
		panic("end is NaN")
	}
	return Section{start: start, end: end}
}

func (s Section) FullyOverlap(otherSec Section) bool {
	return s.start <= otherSec.start && s.end >= otherSec.end
}

func (s Section) Overlap(otherSec Section) bool {
	return (s.start >= otherSec.start && s.start <= otherSec.end) ||
		(s.end >= otherSec.start && s.end <= otherSec.end)
}
