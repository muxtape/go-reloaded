package fsm

import (
    "reflect"
    "testing"
)

func TestFSM_ProcessIdentity(t *testing.T) {
    f := New()

    // Ensure (hex) follows the hex token so the rule applies to the preceding token.
    input := []string{"hello", ",", "convert", "ff", "(hex)", "."}
    out, err := f.Process(input)
    if err != nil {
        t.Fatalf("Process returned error: %v", err)
    }

    // Expect the (hex) rule to convert "ff" -> "255" and the rule token itself is not emitted.
    want := []string{"hello", ",", "convert", "255", "."}
    if !reflect.DeepEqual(out, want) {
        t.Fatalf("Process output mismatch\nwant: %#v\ngot:  %#v", want, out)
    }

    if f.State() != END {
        t.Fatalf("FSM final state = %s, want END", f.State())
    }
}