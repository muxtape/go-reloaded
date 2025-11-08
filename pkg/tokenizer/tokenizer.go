package tokenizer

import (
	"strings"
	"unicode"
)

// Tokenize splits input text into tokens: words, punctuation, and command tokens like "(hex)" or "(cap,2)".
// Rules:
//   - Sequences of letters/digits and hyphens are returned as single word tokens (e.g. state-of-art).
//   - Internal apostrophes that are surrounded by letters/digits are kept inside words (don't, O'Connor).
//   - Quoted spans (opening quote followed later by a matching closing quote) are emitted as:
//     " ' " , <inner tokens...>, " ' "
//   - Parenthesized command tokens are returned as single tokens including the parentheses, e.g. "(hex)".
//   - Punctuation characters (.,!?;:) are emitted as separate tokens.
//   - Whitespace separates tokens.
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
		// Accept letters, digits and hyphen as part of word tokens.
		return unicode.IsLetter(r) || unicode.IsDigit(r) || r == '-'
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
			i = j
			toks = append(toks, cmd.String())
			continue
		}

		// Handle ASCII apostrophe (') and typographic apostrophe (’) uniformly:
		// - If surrounded by letters/digits -> keep inside current word (contraction/O'Name).
		// - Else if there's a later matching same apostrophe in this fragment -> treat as quoted span.
		// - Otherwise emit as a standalone token.
		if r == '\'' || r == '’' {
			// check if contraction-like (inside a word)
			prevIsAlphaNum := i-1 >= 0 && (unicode.IsLetter(runes[i-1]) || unicode.IsDigit(runes[i-1]))
			nextIsAlphaNum := i+1 < n && (unicode.IsLetter(runes[i+1]) || unicode.IsDigit(runes[i+1]))
			if prevIsAlphaNum && nextIsAlphaNum {
				// internal apostrophe -> keep in word
				b.WriteRune(r)
				continue
			}

			// look for later matching same apostrophe to form a quoted span
			found := -1
			for j := i + 1; j < n; j++ {
				if runes[j] == r {
					found = j
					break
				}
			}
			if found != -1 {
				// quoted span: flush before, emit opening quote, tokenize interior, emit closing quote
				flushWord()
				toks = append(toks, string(r)) // opening quote
				inner := string(runes[i+1 : found])
				if inner != "" {
					innerToks := Tokenize(inner)
					toks = append(toks, innerToks...)
				}
				toks = append(toks, string(r)) // closing quote
				i = found
				continue
			}

			// otherwise standalone apostrophe token
			flushWord()
			toks = append(toks, string(r))
			continue
		}

		// Word characters (letters, digits, hyphen)
		if isWordChar(r) {
			b.WriteRune(r)
			continue
		}

		// Punctuation or other symbol: flush any word, then emit the symbol as a token.
		flushWord()
		toks = append(toks, string(r))
	}

	flushWord()
	return toks
}
