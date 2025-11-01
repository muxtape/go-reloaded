package fsm

import (
    "reflect"
    "testing"
)

func TestFSM_Article_Vowel(t *testing.T) {
    f := New()
    input := []string{"I", "saw", "a", "apple", "(a/an)"}
    want := []string{"I", "saw", "an", "apple"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process returned error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
    if f.State() != END {
        t.Fatalf("FSM final state = %s, want END", f.State())
    }
}

func TestFSM_Article_H(t *testing.T) {
    f := New()
    input := []string{"I", "waited", "a", "hour", "(a/an)"}
    want := []string{"I", "waited", "an", "hour"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process returned error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
}

func TestFSM_Article_NoChange(t *testing.T) {
    f := New()
    input := []string{"He", "owns", "a", "dog", "(a/an)"}
    want := []string{"He", "owns", "a", "dog"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process returned error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("want %#v got %#v", want, out)
    }
}

func TestFSM_Article_NoPrevA_Error(t *testing.T) {
    f := New()
    input := []string{"This", "(a/an)"}
    _, err := f.Process(input)
    if err == nil {
        t.Fatalf("expected error for missing preceding article, got nil")
    }
}