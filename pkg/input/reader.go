package input

import (
    "bufio"
    "log"
    "os"
)

// ReadLinesWithErr opens path and returns two channels:
//  - lines: emits each line from the file
//  - errs: receives scanner errors (if any) and is closed when done
// Returns an error only if the file cannot be opened.
func ReadLinesWithErr(path string) (<-chan string, <-chan error, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, nil, err
    }

    out := make(chan string)
    errs := make(chan error, 1)
    scanner := bufio.NewScanner(f)

    go func() {
        defer close(out)
        defer close(errs)
        defer f.Close()
        for scanner.Scan() {
            out <- scanner.Text()
        }
        if scErr := scanner.Err(); scErr != nil {
            errs <- scErr
        }
    }()

    return out, errs, nil
}

// ReadLines preserves the original API: it returns a channel of lines and an open error (only on open failure).
// For compatibility it drains scanner errors in the background and logs them. Use ReadLinesWithErr if you
// want to observe scanner errors.
func ReadLines(path string) (<-chan string, error) {
    lines, errs, err := ReadLinesWithErr(path)
    if err != nil {
        return nil, err
    }

    // drain errors in background to preserve original behavior while not losing errors.
    go func() {
        for e := range errs {
            if e != nil {
                log.Printf("input.ReadLines scanner error: %v", e)
            }
        }
    }()

    return lines, nil
}