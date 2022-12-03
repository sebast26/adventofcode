package common

func SliceFill[T any](slice *[]T, val T) {
	for i := range *slice {
		(*slice)[i] = val
	}
}
