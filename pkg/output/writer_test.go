package output

import (
    "os"
    "path/filepath"
    "strings"
    "testing"
)

func TestWriteLines_Success(t *testing.T) {
    dir := t.TempDir()
    p := filepath.Join(dir, "out.txt")

    lines := make(chan string)
    go func() {
        defer close(lines)
        lines <- "alpha"
        lines <- "beta"
        lines <- "gamma"
    }()

    if err := WriteLines(p, lines); err != nil {
        t.Fatalf("WriteLines error: %v", err)
    }

    b, err := os.ReadFile(p)
    if err != nil {
        t.Fatalf("read file error: %v", err)
    }

    got := strings.Split(strings.TrimRight(string(b), "\n"), "\n")
    want := []string{"alpha", "beta", "gamma"}
    if len(got) != len(want) {
        t.Fatalf("line count mismatch: want %d got %d", len(want), len(got))
    }
    for i := range want {
        if got[i] != want[i] {
            t.Fatalf("line %d mismatch: want %q got %q", i, want[i], got[i])
        }
    }
}