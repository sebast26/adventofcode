package common

// SliceFill fills whole slice with value
// It modifies the slice in place
func SliceFill[T any](slice *[]T, val T) {
	for i := range *slice {
		(*slice)[i] = val
	}
}
