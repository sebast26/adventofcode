package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"sort"
	"strconv"
)

func main() {
	root := ReadTree("day07/input.txt")
	TotalSizes(root)
	atMost := filter(root, 100_000)
	var total int
	for _, e := range atMost {
		total += e
	}
	fmt.Println(total)

	space := 70_000_000
	unused := space - root.data.TotalSize
	requiredSpace := 30_000_000 - unused

	size := SmallestToDelete(root, requiredSpace)
	fmt.Println(size)
}

func SmallestToDelete(node *Node[string, DirContent], minimumSpace int) int {
	sizes := allSizes(node)
	sort.Ints(sizes)
	for _, size := range sizes {
		if size >= minimumSpace {
			return size
		}
	}
	return 0
}

func allSizes(node *Node[string, DirContent]) []int {
	var out []int
	for _, n := range node.children {
		o := allSizes(n)
		out = append(out, o...)
	}
	out = append(out, node.data.TotalSize)
	return out
}

func filter(node *Node[string, DirContent], threshold int) []int {
	var out []int
	for _, n := range node.children {
		o := filter(n, threshold)
		out = append(out, o...)
	}
	if node.data.TotalSize <= threshold {
		out = append(out, node.data.TotalSize)
	}
	return out
}

func TotalSizes(node *Node[string, DirContent]) int {
	if len(node.children) == 0 {
		return node.data.TotalSize
	}
	var total int
	for _, n := range node.children {
		total += TotalSizes(n)
	}
	node.data.TotalSize += total
	return node.data.TotalSize
}

func ReadTree(file string) *Node[string, DirContent] {
	in, err := os.Open(file)
	if err != nil {
		panic(err)
	}
	defer in.Close()
	scanner := bufio.NewScanner(in)

	var root *Node[string, DirContent]
	var node *Node[string, DirContent]
	for scanner.Scan() {
		line := scanner.Text()
		if line == "$ cd /" {
			root = NewNode("/", DirContent{})
			node = root
			continue
		}
		if line == "$ cd .." {
			node = node.Parent()
			continue
		}
		if dirRe.MatchString(line) { // $ cd a
			dirName := dirName(dirRe, line)
			n := NewNode(dirName, DirContent{})
			node.AddChildren(n)
			node = n
		}
		if fileRe.MatchString(line) { // 14848514 b.txt
			file := buildFile(fileRe, line)
			appendFile(node, file)
		}

	}
	return root
}

func dirName(dirRe *regexp.Regexp, line string) string {
	matches := dirRe.FindStringSubmatch(line)
	dirIndex := dirRe.SubexpIndex("dir")
	return matches[dirIndex]
}

func buildFile(fileRe *regexp.Regexp, line string) File {
	matches := fileRe.FindStringSubmatch(line)
	sizeIndex := fileRe.SubexpIndex("size")
	nameIndex := fileRe.SubexpIndex("name")
	sizeStr := matches[sizeIndex]
	name := matches[nameIndex]
	size, err := strconv.Atoi(sizeStr)
	if err != nil {
		panic(err)
	}
	return NewFile(name, size)
}

func appendFile(node *Node[string, DirContent], file File) {
	node.data.AppendFile(file)
}

var (
	dirRe  = regexp.MustCompile(`\$ cd (?P<dir>\w+)`)
	fileRe = regexp.MustCompile(`(?P<size>\d+) (?P<name>[\w.]+)`)
)
