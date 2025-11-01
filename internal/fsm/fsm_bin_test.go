package fsm

import (
    "reflect"
    "testing"
)

func TestFSM_BinRule_Success(t *testing.T) {
    f := New()
    input := []string{"value", "1010", "(bin)"}
    want := []string{"value", "10"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process returned error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("Process output mismatch\nwant: %#v\ngot:  %#v", want, out)
    }
    if f.State() != END {
        t.Fatalf("FSM final state = %s, want END", f.State())
    }
}

func TestFSM_BinRule_WithPrefix_Success(t *testing.T) {
    f := New()
    input := []string{"value", "0b110", "(bin)"}
    want := []string{"value", "6"}

    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process returned error: %v", err)
    }
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("Process output mismatch\nwant: %#v\ngot:  %#v", want, out)
    }
}

func TestFSM_BinRule_InvalidBin(t *testing.T) {
    f := New()
    input := []string{"value", "102", "(bin)"}
    _, err := f.Process(input)
    if err == nil {
        t.Fatalf("expected error for invalid binary, got nil")
    }
}