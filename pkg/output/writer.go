package output

import (
    "bufio"
    "os"
)

// WriteLinesWithErr writes every string received on lines to the file at path, one per line.
// It returns a channel that receives write/flush errors (if any) and is closed when done.
// The function spawns a goroutine to perform the writes; callers can monitor the returned error channel.
func WriteLinesWithErr(path string, lines <-chan string) (<-chan error, error) {
    f, err := os.Create(path)
    if err != nil {
        return nil, err
    }

    errs := make(chan error, 1)

    go func() {
        defer close(errs)
        defer f.Close()

        w := bufio.NewWriter(f)
        for l := range lines {
            if _, werr := w.WriteString(l + "\n"); werr != nil {
                errs <- werr
                return
            }
        }
        if ferr := w.Flush(); ferr != nil {
            errs <- ferr
            return
        }
    }()

    return errs, nil
}

// WriteLines keeps the original synchronous API: it writes the lines and returns an error (if any).
// Internally it reuses WriteLinesWithErr and waits for the goroutine to finish, returning the first error.
func WriteLines(path string, lines <-chan string) error {
    errs, err := WriteLinesWithErr(path, lines)
    if err != nil {
        return err
    }

    var first error
    for e := range errs {
        if e != nil && first == nil {
            first = e
        }
    }
    return first
}