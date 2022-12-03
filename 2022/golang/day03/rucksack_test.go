package main

import (
	"reflect"
	"testing"
)

func TestRucksack(t *testing.T) {
	t.Run("compartments", func(t *testing.T) {
		tests := []struct {
			name  string
			items string
			want  []string
		}{
			{
				"", "vJrwpWtwJgWrhcsFMMfFFhFp", []string{"vJrwpWtwJgWr", "hcsFMMfFFhFp"},
			},
			{
				"", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", []string{"jqHRNqRjqzjGDLGL", "rsFMfFZSrLrFZsSL"},
			},
			{
				"", "PmmdzqPrVvPwwTWBwg", []string{"PmmdzqPrV", "vPwwTWBwg"},
			},
		}
		for _, test := range tests {
			t.Run(test.name, func(t *testing.T) {
				rs := NewRucksack(test.items)
				if got := []string{rs.Compartment(1), rs.Compartment(2)}; !reflect.DeepEqual(got, test.want) {
					t.Errorf("Compartment(1), Compartment(2) = %v, want %v", got, test.want)
				}
			})
		}
	})

	t.Run("priority", func(t *testing.T) {
		tests := []struct {
			name  string
			items string
			want  rune
		}{
			{
				"p", "vJrwpWtwJgWrhcsFMMfFFhFp", 'p',
			},
			{
				"L", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", 'L',
			},
			{
				"P", "PmmdzqPrVvPwwTWBwg", 'P',
			},
			{
				"v", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", 'v',
			},
			{
				"t", "ttgJtRGJQctTZtZT", 't',
			},
			{
				"s", "CrZsJsPPZsGzwwsLwLmpwMDw", 's',
			},
		}
		for _, tt := range tests {
			t.Run(tt.name, func(t *testing.T) {
				rs := NewRucksack(tt.items)
				if got := rs.CommonItem(); !reflect.DeepEqual(got, tt.want) {
					t.Errorf("CommonItem() = %v, want %v", got, tt.want)
				}
			})
		}
	})
}

func TestPriority(t *testing.T) {
	tests := []struct {
		name string
		item rune
		want int
	}{
		{
			"p", 'p', 16,
		},
		{
			"L", 'L', 38,
		},
		{
			"P", 'P', 42,
		},
		{
			"v", 'v', 22,
		},
		{
			"t", 't', 20,
		},
		{
			"s", 's', 19,
		},
		{
			"a", 'a', 1,
		},
		{
			"z", 'z', 26,
		},
		{
			"A", 'A', 27,
		},
		{
			"Z", 'Z', 52,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := Priority(tt.item); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Priority() = %v, want %v", got, tt.want)
			}
		})
	}
}
