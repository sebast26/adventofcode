package common

import "testing"

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
