package fsm

import "testing"

func TestFSM_Quote_SingleWord(t *testing.T) {
    f := New()
    input := []string{"He", "said", "'", "hello", "'", "."}
    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process error: %v", err)
    }
    got := FormatTokens(out)
    want := "He said 'hello'."
    if got != want {
        t.Fatalf("FormatTokens = %q, want %q", got, want)
    }
}

func TestFSM_Quote_MultiWord(t *testing.T) {
    f := New()
    input := []string{"He", "said", "'", "hello", "world", "'", "."}
    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process error: %v", err)
    }
    got := FormatTokens(out)
    want := "He said 'hello world'."
    if got != want {
        t.Fatalf("FormatTokens = %q, want %q", got, want)
    }
}

func TestFSM_Quote_Unmatched(t *testing.T) {
    f := New()
    input := []string{"This", "is", "'", "unterminated"}
    _, err := f.Process(input)
    if err == nil {
        t.Fatalf("expected error for unmatched quote, got nil")
    }
}