package common

import (
	"github.com/stretchr/testify/assert"
	"reflect"
	"testing"
)

func TestSliceFill(t *testing.T) {
	t.Run("bool", func(t *testing.T) {
		// given
		s := make([]bool, 255)
		// when
		SliceFill(&s, true)
		// then
		for i := range s {
			if s[i] == false {
				t.Errorf("SliceFill() = %v, want %v", s[i], true)
			}
		}
	})

	t.Run("ints", func(t *testing.T) {
		// given
		s := make([]int, 1024)
		val := 13
		// when
		SliceFill(&s, 13)
		// then
		for i := range s {
			if s[i] != val {
				t.Errorf("SliceFill() = %v, want %v", s[i], val)
			}
		}
	})
}

func TestEliminateDuplicates(t *testing.T) {
	t.Run("empty", func(t *testing.T) {
		got := EliminateDuplicates([]int{})
		assert.Empty(t, got)
	})

	t.Run("ints", func(t *testing.T) {
		tests := []struct {
			slice []int
			want  []int
		}{
			{[]int{1, 1}, []int{1}},
			{[]int{1, 2}, []int{1, 2}},
			{[]int{1, 2, 2}, []int{1, 2}},
			{[]int{1, 2, 8, 8, 9}, []int{1, 2, 8, 9}},
		}

		for _, tt := range tests {
			t.Run("", func(t *testing.T) {
				if got := EliminateDuplicates(tt.slice); !reflect.DeepEqual(got, tt.want) {
					t.Errorf("EliminateDuplicates = %v, but want %v", got, tt.want)
				}
			})
		}
	})
}
