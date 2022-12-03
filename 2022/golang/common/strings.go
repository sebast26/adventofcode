package common

func CommonCharacters(strings []string) []rune {
	p := make([]bool, len(letters))
	SliceFill(&p, true)

	for _, str := range strings {
		s := make([]bool, len(letters))
		SliceFill(&s, false)
		for _, ch := range str {
			if p[ch-'a'] {
				s[ch-'a'] = true
			}
		}
		copy(p, s)
	}

	var out []rune
	for i := 0; i < len(letters); i++ {
		if p[i] {
			out = append(out, rune(i+97))
		}
	}
	return out
}

const (
	// !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
	letters = `abcdefghijklmnopqrstuvwxyz`
)
