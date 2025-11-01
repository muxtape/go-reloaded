package fsm

import "testing"

func TestFormatTokens_BasicPunctuation(t *testing.T) {
    in := []string{"hello", ",", "world", "!"}
    got := FormatTokens(in)
    want := "hello, world!"
    if got != want {
        t.Fatalf("FormatTokens(%#v) = %q, want %q", in, got, want)
    }
}

func TestFormatTokens_Parentheses(t *testing.T) {
    in := []string{"see", "(", "note", ")"}
    got := FormatTokens(in)
    want := "see (note)"
    if got != want {
        t.Fatalf("FormatTokens(%#v) = %q, want %q", in, got, want)
    }
}

func TestFormatTokens_Ellipsis(t *testing.T) {
    in := []string{"Wait", "...", "what", "?"}
    got := FormatTokens(in)
    want := "Wait... what?"
    if got != want {
        t.Fatalf("FormatTokens(%#v) = %q, want %q", in, got, want)
    }
}

func TestFormatTokens_Mixed(t *testing.T) {
    in := []string{"Hello", ",", "(", "world", ")", ":" , "nice", "to", "meet", "you", "."}
    got := FormatTokens(in)
    want := "Hello, (world): nice to meet you."
    if got != want {
        t.Fatalf("FormatTokens(%#v) = %q, want %q", in, got, want)
    }
}