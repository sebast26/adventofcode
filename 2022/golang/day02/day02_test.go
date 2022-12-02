package main

import (
	"reflect"
	"testing"
)

func TestRound_Score(t *testing.T) {
	tests := []struct {
		name string
		line string
		want int
	}{
		{"i won", "A Y", 8},
		{"i loss", "B X", 1},
		{"draw", "C Z", 6},
		{"rock, rock", "A X", 4},
		{"rock, paper", "A Y", 8},
		{"rock, scissors", "A Z", 3},
		{"paper, rock", "B X", 1},
		{"paper, paper", "B Y", 5},
		{"paper, scissors", "B Z", 9},
		{"scissors, rock", "C X", 7},
		{"scissors, paper", "C Y", 2},
		{"scissors, scissors", "C Z", 6},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newRoundWithMove(tt.line)
			if got := r.Score(); got != tt.want {
				t.Errorf("Score() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_newRoundWithHelp(t *testing.T) {
	tests := []struct {
		name string
		line string
		want int
	}{
		{"draw", "A Y", 4},
		{"lost", "B X", 1},
		{"win", "C Z", 7},
		{"rock, loose", "A X", 3},
		{"rock, draw", "A Y", 4},
		{"rock, win", "A Z", 8},
		{"paper, loose", "B X", 1},
		{"paper, draw", "B Y", 5},
		{"paper, win", "B Z", 9},
		{"scissors, loose", "C X", 2},
		{"scissors, draw", "C Y", 6},
		{"scissors, win", "C Z", 7},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := newRoundWithResults(tt.line)
			if got := r.Score(); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Score() = %v, want %v", got, tt.want)
			}
		})
	}
}
