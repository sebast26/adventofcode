package main

import "testing"

func TestStartOfPacket(t *testing.T) {
	tests := []struct {
		name  string
		input []byte
		want  int
	}{
		{"", []byte("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7},
		{"", []byte("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5},
		{"", []byte("nppdvjthqldpwncqszvftbrmjlhg"), 6},
		{"", []byte("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10},
		{"", []byte("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := StartOfPacket(tt.input); got != tt.want {
				t.Errorf("StartOfPacket() = %d, but expected %d", got, tt.want)
			}
		})
	}
}

func TestStartOfMessage(t *testing.T) {
	tests := []struct {
		name  string
		input []byte
		want  int
	}{
		{"", []byte("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19},
		{"", []byte("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23},
		{"", []byte("nppdvjthqldpwncqszvftbrmjlhg"), 23},
		{"", []byte("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29},
		{"", []byte("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := StartOfMessage(tt.input); got != tt.want {
				t.Errorf("StartOfMessage() = %d, but expected %d", got, tt.want)
			}
		})
	}
}
