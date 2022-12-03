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
	t.Run("test", func(t *testing.T) {
		actual := CommonCharacters([]string{"geeksforgeeks", "gemkstones", "acknowledges", "aguelikes"})
		assert.Equal(t, []rune{'e', 'g', 'k', 's'}, actual)
	})

	t.Run("test#2", func(t *testing.T) {
		actual := CommonCharacters([]string{"vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"})
		assert.Equal(t, []rune{'r'}, actual)
	})

	t.Run("test#3", func(t *testing.T) {
		actual := CommonCharacters([]string{"wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"})
		assert.Equal(t, []rune{'Z'}, actual)
	})
}
