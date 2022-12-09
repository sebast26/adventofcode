package main

type Node[T comparable, S any] struct {
	label    T
	data     S
	parent   *Node[T, S]
	children []*Node[T, S]
}

func NewNode[T comparable, S any](label T, data S) *Node[T, S] {
	return &Node[T, S]{label: label, data: data}
}

func (n *Node[T, S]) Parent() *Node[T, S] {
	return n.parent
}

func (n *Node[T, S]) AddChildren(node *Node[T, S]) {
	node.parent = n
	n.children = append(n.children, node)
}
