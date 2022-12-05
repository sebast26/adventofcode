package common

// NoErrors panics if any of the errors given are not nil
func NoErrors(errs ...error) {
	for _, e := range errs {
		if e != nil {
			panic(e)
		}
	}
}
