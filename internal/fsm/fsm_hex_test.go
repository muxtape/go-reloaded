package fsm

import (
    "reflect"
    "testing"
)

func TestFSM_HexRule_Success(t *testing.T) {
    f := New()
    input := []string{"convert", "ff", "(hex)", "."}
    want := []string{"convert", "255", "."}

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

func TestFSM_HexRule_InvalidHex(t *testing.T) {
    f := New()
    input := []string{"value", "gg", "(hex)"}
    _, err := f.Process(input)
    if err == nil {
        t.Fatalf("expected error for invalid hex, got nil")
    }
}