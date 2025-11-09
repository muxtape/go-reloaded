package tokenizer

import (
    "reflect"
    "testing"
)

func TestTokenize_RefactorEdgeCases(t *testing.T) {
    cases := []struct {
        in   string
        want []string
    }{
        // parenthesized command with space normalized
        {"(low, 3)", []string{"(low,3)"}},
        {"(up)", []string{"(up)"}},
        // normal parenthetical phrase preserved as separate tokens including "(" and ")"
        {"(this is a test)", []string{"(", "this", "is", "a", "test", ")"}},
        {"hello (this is a test) (up)", []string{"hello", "(", "this", "is", "a", "test", ")", "(up)"}},
        // apostrophe inside word preserved
        {"don't stop", []string{"don't", "stop"}},
        // hyphenated word preserved as single token
        {"state-of-art", []string{"state-of-art"}},
    }

    for _, tc := range cases {
        got := Tokenize(tc.in)
        if !reflect.DeepEqual(got, tc.want) {
            t.Fatalf("Tokenize(%q) = %v, want %v", tc.in, got, tc.want)
        }
    }
}