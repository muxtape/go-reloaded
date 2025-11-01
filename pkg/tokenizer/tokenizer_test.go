package tokenizer

import (
    "reflect"
    "testing"
)

func TestTokenize_SimplePunctuation(t *testing.T) {
    in := "hello, world!"
    want := []string{"hello", ",", "world", "!"}
    got := Tokenize(in)
    if !reflect.DeepEqual(got, want) {
        t.Fatalf("Tokenize(%q) = %#v, want %#v", in, got, want)
    }
}

func TestTokenize_CommandsAndWords(t *testing.T) {
    in := "convert (hex) to decimal"
    want := []string{"convert", "(hex)", "to", "decimal"}
    got := Tokenize(in)
    if !reflect.DeepEqual(got, want) {
        t.Fatalf("Tokenize(%q) = %#v, want %#v", in, got, want)
    }
}

func TestTokenize_ContractionAndHyphen(t *testing.T) {
    in := "don't stop state-of-art"
    want := []string{"don't", "stop", "state-of-art"}
    got := Tokenize(in)
    if !reflect.DeepEqual(got, want) {
        t.Fatalf("Tokenize(%q) = %#v, want %#v", in, got, want)
    }
}

func TestTokenize_ParenthesizedComplex(t *testing.T) {
    in := "apply (cap,2) to the previous word."
    want := []string{"apply", "(cap,2)", "to", "the", "previous", "word", "."}
    got := Tokenize(in)
    if !reflect.DeepEqual(got, want) {
        t.Fatalf("Tokenize(%q) = %#v, want %#v", in, got, want)
    }
}