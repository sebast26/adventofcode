package common

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestCommonCharacters(t *testing.T) {

	t.Run("no common", func(t *testing.T) {
		actual := CommonCharacters([]string{"abc", "def", "ghi", "jkl"})
		assert.Empty(t, actual)
	})

	tests := []struct {
		name    string
		strings []string
		want    []rune
	}{
		{
			"test", []string{"geeksforgeeks", "gemkstones", "acknowledges", "aguelikes"}, []rune{'e', 'g', 'k', 's'},
		},
		{
			"test#2", []string{"vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"}, []rune{'r'},
		},
		{
			"test#3", []string{"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"}, []rune{'Z'},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			actual := CommonCharacters(tt.strings)
			assert.Equal(t, tt.want, actual)
		})
	}

}
