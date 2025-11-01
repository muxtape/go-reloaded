package output

import (
    "bufio"
    "os"
)

// WriteLines writes every string received on lines to the file at path, one per line.
// Returns an error if the file cannot be created or a write fails.
func WriteLines(path string, lines <-chan string) error {
    f, err := os.Create(path)
    if err != nil {
        return err
    }
    defer f.Close()

    w := bufio.NewWriter(f)
    for l := range lines {
        if _, err := w.WriteString(l + "\n"); err != nil {
            return err
        }
    }
    return w.Flush()
}