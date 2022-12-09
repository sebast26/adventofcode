package main

type DirContent struct {
	Files     []File
	TotalSize int
}

func (d *DirContent) AppendFile(file File) {
	d.Files = append(d.Files, file)
	d.TotalSize += file.Size
}

type File struct {
	Name string
	Size int
}

func NewFile(name string, size int) File {
	return File{Name: name, Size: size}
}
