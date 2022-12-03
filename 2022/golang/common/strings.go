package common

// CommonCharacters finds the common characters in all the strings
func CommonCharacters(strings []string) []rune {
	// primary array for common characters; we assume all characters are seen before
	p := make([]bool, len(letters))
	SliceFill(&p, true)

	for _, str := range strings {
		// secondary array for common characters; initially marked false
		s := make([]bool, len(letters))
		SliceFill(&s, false)
		for _, ch := range str {
			// if character is present in all strings before, mark it
			if p[ch-'!'] {
				s[ch-'!'] = true
			}
		}
		// copy whole secondary array into primary
		copy(p, s)
	}

	var out []rune
	for i := 0; i < len(letters); i++ {
		if p[i] {
			out = append(out, rune(i+33))
		}
	}
	return out
}

const (
	letters = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
)
