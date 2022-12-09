package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestReadNode(t *testing.T) {
	t.Run("read example.txt", func(t *testing.T) {
		root := ReadTree("example.txt")
		root.Parent()
	})
}

func TestTotalSizes(t *testing.T) {
	t.Run("TotalSizes", func(t *testing.T) {
		root := ReadTree("example.txt")
		sum := TotalSizes(root)
		assert.Equal(t, 48381165, sum)
	})
}

func TestSumMax(t *testing.T) {
	t.Run("summax", func(t *testing.T) {
		root := ReadTree("example.txt")
		TotalSizes(root)
		sum := filter(root, 100_000)

		assert.Equal(t, 95437, sum)
	})
}

func TestSmallestToDelete(t *testing.T) {
	t.Run("smallest to delete", func(t *testing.T) {
		root := ReadTree("example.txt")
		TotalSizes(root)
		size := SmallestToDelete(root, 8381165)
		assert.Equal(t, 24933642, size)
	})
}
