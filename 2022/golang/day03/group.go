package main

import (
	"sgorecki.me/adventofcode/2022/common"
)

type Group struct {
	items []string
}

func NewGroup(str []string) Group {
	if len(str) != 3 {
		panic("group consists of 3 rucksacks")
	}
	return Group{items: str}
}

func (g Group) CommonItem() rune {
	chars := common.CommonCharacters(g.items)
	return chars[0]
}
