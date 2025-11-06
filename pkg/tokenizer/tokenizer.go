package tokenizer

import (
	"strings"
	"unicode"
)

// Tokenize splits input text into tokens: words, punctuation, and command tokens like "(hex)" or "(cap,2)".
// Rules:
// - Sequences of letters/digits, apostrophes and hyphens are returned as single word tokens (e.g. don't, state-of-art).
// - Parenthesized command tokens are returned as single tokens including the parentheses, e.g. "(hex)".
// - Punctuation characters (.,!?;:) are emitted as separate tokens.
// - Whitespace separates tokens.
func Tokenize(s string) []string {
	var toks []string
	var b strings.Builder
	runes := []rune(s)
	n := len(runes)

	flushWord := func() {
		if b.Len() > 0 {
			toks = append(toks, b.String())
			b.Reset()
		}
	}

	isWordChar := func(r rune) bool {
		// Accept letters, digits, apostrophe and hyphen as part of word tokens.
		return unicode.IsLetter(r) || unicode.IsDigit(r) /*|| r == '\'' || r == '-'*/
	}

	for i := 0; i < n; i++ {
		r := runes[i]

		// Whitespace: flush current word and continue.
		if unicode.IsSpace(r) {
			flushWord()
			continue
		}

		// Parenthesized command: capture until the next ')' (inclusive).
		if r == '(' {
			flushWord()
			var cmd strings.Builder
			cmd.WriteRune(r)
			j := i + 1
			for ; j < n; j++ {
				cmd.WriteRune(runes[j])
				if runes[j] == ')' {
					break
				}
			}
			// advance i to j (if j reached n without ')', it will include rest)
			i = j
			toks = append(toks, cmd.String())
			continue
		}

		// Word characters
		if isWordChar(r) {
			b.WriteRune(r)
			continue
		}

		// Punctuation or other symbol: flush any word, then emit the symbol as a token.
		flushWord()
		// treat most single non-word, non-space characters as separate tokens
		toks = append(toks, string(r))
	}

	flushWord()
	return toks
}
