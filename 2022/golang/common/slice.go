package common

// SliceFill fills whole slice with value
// It modifies the slice in place
func SliceFill[T any](slice *[]T, val T) {
	for i := range *slice {
		(*slice)[i] = val
	}
}

// EliminateDuplicates returns slice without duplicates from original slice
func EliminateDuplicates[T comparable](slice []T) []T {
	var out []T
	set := map[T]struct{}{}
	for i := range slice {
		val := slice[i]
		_, ok := set[val]
		if !ok {
			set[val] = struct{}{}
			out = append(out, val)
		}
	}
	return out
}

func Contains[T comparable](slice []T, elem T) bool {
	for _, e := range slice {
		if e == elem {
			return true
		}
	}
	return false
}
