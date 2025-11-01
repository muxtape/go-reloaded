package fsm

import (
    "testing"
)

func TestFSM_Integration_Golden(t *testing.T) {
    f := New()

    tokens := []string{
        "I", "saw", "a", "apple", "(a/an)", ",",
        "it", "was", "ff", "(hex)", "and", "1010", "(bin)", ".",
        "He", "said", "'", "hello", "world", "'", ".",
        "MAKE", "(low)", "and", "that", "(up)", "and", "multiple", "words", "(cap,2)", ".",
    }

    out, err := f.Process(tokens)
    if err != nil {
        t.Fatalf("Process returned error: %v", err)
    }

    got := FormatTokens(out)
    want := "I saw an apple, it was 255 and 10. He said 'hello world'. make and THAT and Multiple Words."
    if got != want {
        t.Fatalf("integration golden mismatch\nwant: %q\ngot:  %q", want, got)
    }

    if f.State() != END {
        t.Fatalf("FSM final state = %s, want END", f.State())
    }
}