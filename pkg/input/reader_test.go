package input

import (
    "os"
    "path/filepath"
    "reflect"
    "testing"
)

func TestReadLines_Success(t *testing.T) {
    dir := t.TempDir()
    p := filepath.Join(dir, "sample.txt")
    content := "line1\nline2\nlast line\n"
    if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
        t.Fatal(err)
    }

    ch, err := ReadLines(p)
    if err != nil {
        t.Fatalf("ReadLines returned error: %v", err)
    }

    var got []string
    for l := range ch {
        got = append(got, l)
    }

    want := []string{"line1", "line2", "last line"}
    if !reflect.DeepEqual(got, want) {
        t.Fatalf("lines mismatch\nwant: %#v\ngot:  %#v", want, got)
    }
}

func TestReadLines_InvalidPath(t *testing.T) {
    ch, err := ReadLines("/path/does/not/exist/hopefully.txt")
    if err == nil {
        t.Fatalf("expected error for invalid path, got nil and channel=%v", ch)
    }
}