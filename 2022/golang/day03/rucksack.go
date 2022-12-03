package main

import (
	"sgorecki.me/adventofcode/2022/common"
	"strings"
)

type Rucksack struct {
	items string
}

func NewRucksack(items string) Rucksack {
	return Rucksack{items: items}
}

func (r Rucksack) Compartment(num int) string {
	switch num {
	case 1:
		return r.items[:(len(r.items) / 2)]
	case 2:
		return r.items[(len(r.items) / 2):]
	default:
		panic("rucksack has only first or second compartment")
	}
}

func (r Rucksack) CommonItem() rune {
	c1 := r.Compartment(1)
	c2 := r.Compartment(2)
	chars := common.CommonCharacters([]string{c1, c2})
	return chars[0]
}

func Priority(r rune) int {
	return 1 + strings.IndexRune(letters, r)
}

const (
	letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
)
