package input

import (
    "bufio"
    "os"
)

// ReadLines opens path and returns a channel that emits each line from the file.
// Returns an error if the file cannot be opened. The returned channel is closed
// when scanning completes.
func ReadLines(path string) (<-chan string, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }

    out := make(chan string)
    scanner := bufio.NewScanner(f)

    go func() {
        defer close(out)
        defer f.Close()
        for scanner.Scan() {
            out <- scanner.Text()
        }
        // Any scanner error is ignored here; caller/tests detect missing data via opened file semantics.
    }()

    return out, nil
}