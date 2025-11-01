package fsm

import "testing"

func TestFSM_InitialState(t *testing.T) {
    f := New()
    if f.State() != READ {
        t.Fatalf("initial state = %s, want READ", f.State())
    }
}

func TestFSM_ValidTransitions(t *testing.T) {
    f := New()

    steps := []struct {
        ev   Event
        want State
    }{
        {EventToken, READ},      // still reading
        {EventToken, READ},      // still reading
        {EventRule, APPLY_RULE}, // rule encountered
        {EventEmit, OUTPUT},     // applied rule -> output
        {EventToken, READ},      // resume reading
        {EventEOF, END},         // finish
    }

    for i, s := range steps {
        if err := f.SendEvent(s.ev); err != nil {
            t.Fatalf("step %d: unexpected error sending event %s: %v", i, s.ev, err)
        }
        if f.State() != s.want {
            t.Fatalf("step %d: state = %s, want %s", i, f.State(), s.want)
        }
    }
}

func TestFSM_InvalidTransition(t *testing.T) {
    f := New()

    // move to APPLY_RULE
    if err := f.SendEvent(EventRule); err != nil {
        t.Fatalf("setup: cannot send rule event: %v", err)
    }

    // APPLY_RULE + EventToken is invalid per transition table
    if err := f.SendEvent(EventToken); err == nil {
        t.Fatalf("expected error for invalid transition APPLY_RULE + Token, got nil")
    }
}