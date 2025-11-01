package fsm

import (
    "reflect"
    "testing"
)

func TestFSM_Case_Up(t *testing.T) {
    f := New()
    input := []string{"hello", "(up)"}
    want := []string{"HELLO"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
}

func TestFSM_Case_Low(t *testing.T) {
    f := New()
    input := []string{"HELLO", "(low)"}
    want := []string{"hello"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
}

func TestFSM_Case_Cap_DefaultCount(t *testing.T) {
    f := New()
    input := []string{"word", "(cap)"}
    want := []string{"Word"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
}

func TestFSM_Case_Cap_WithCount(t *testing.T) {
    f := New()
    input := []string{"multiple", "words", "(cap,2)"}
    want := []string{"Multiple", "Words"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
}

func TestFSM_Case_Cap_SkipPunctuation(t *testing.T) {
    f := New()
    input := []string{"hello", ",", "world", "(cap,2)"}
    want := []string{"Hello", ",", "World"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
}

func TestFSM_Case_NotEnoughWords(t *testing.T) {
    f := New()
    input := []string{"only", "(cap,2)"}
    _, err := f.Process(input)
    if err == nil {
        t.Fatalf("expected error for insufficient words, got nil")
    }
}