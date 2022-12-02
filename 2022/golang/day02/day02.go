package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

type round struct {
	opp, me int
}

func newRoundWithMove(line string) round {
	return round{opp: int(line[0] - 64), me: int(line[2] - 87)}
}

func newRoundWithResults(line string) round {
	bOpp := int(line[0] - 64)
	result := int(line[2]) - 89
	me := bOpp + result
	if me == 0 {
		me = 3
	}
	if me == 4 {
		me = 1
	}
	return round{opp: bOpp, me: me}
}

func (r round) result() int {
	if r.opp == r.me {
		return 3
	}
	if math.Abs(float64(r.opp-r.me)) > 1 {
		r.opp, r.me = r.me, r.opp
	}
	if r.opp > r.me {
		return 0
	} else {
		return 6
	}

}

func (r round) Score() int {
	score := r.me
	score += r.result()
	return score
}

func main() {
	in, err := os.Open("day02/input.txt")
	if err != nil {
		panic(err)
	}
	defer in.Close()

	scanner := bufio.NewScanner(in)
	var total, hintTotal int
	for scanner.Scan() {
		line := scanner.Text()
		r := newRoundWithMove(line)
		rh := newRoundWithResults(line)
		total += r.Score()
		hintTotal += rh.Score()
	}
	fmt.Println(total)
	fmt.Println(hintTotal)
}
