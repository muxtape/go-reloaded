package tokenizer

import (
	"strings"
	"unicode"
	"unicode/utf8"
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

		// Parenthesized command: capture until the next ')' (inclusive) only when
		// the inner part looks like a command (e.g. "hex", "cap,2", "low, 3").
		if r == '(' {
			// scan ahead for matching ')'
			j := i + 1
			for j < n && runes[j] != ')' {
				j++
			}
			if j < n && runes[j] == ')' {
				inner := string(runes[i+1 : j])
				// Normalize by removing spaces to allow "(low, 3)" -> "(low,3)"
				compact := strings.ReplaceAll(inner, " ", "")
				if compact != "" {
					// allowed character set for command payload
					allowed := true
					for _, rr := range compact {
						if !(unicode.IsLetter(rr) || unicode.IsDigit(rr) || rr == ',' || rr == '-' || rr == '.' || rr == ':' || rr == '%') {
							allowed = false
							break
						}
					}
					if allowed {
						// accept as command token only if compact either contains a comma (has args)
						// or matches a small whitelist of known single-word commands.
						lc := strings.ToLower(compact)
						whitelist := map[string]bool{
							"hex": true, "bin": true, "cap": true, "low": true, "up": true,
						}
						if strings.Contains(compact, ",") || whitelist[lc] {
							flushWord()
							// emit normalized token without internal spaces so FSM parsing is stable
							toks = append(toks, "("+compact+")")
							i = j // advance past the ')'
							continue
						}
					}
				}
				// otherwise fallthrough: treat '(' as standalone token and let loop handle inner text
			}
			// no matching ')' or inner not allowed: treat '(' as its own token
			flushWord()
			toks = append(toks, "(")
			continue
		}

		// Handle ASCII apostrophe (') and typographic apostrophe (’) uniformly:
		// - If surrounded by letters/digits -> keep inside current word (contraction/O'Name).
		// - Else if there's a later matching same apostrophe in this fragment -> treat as quoted span.
		// - Otherwise emit as a standalone token.
		if r == '\'' || r == '’' {
			// check if we are inside a word (previous and next are letters/digits)
			prevIs := false
			nextIs := false
			if b.Len() > 0 {
				pr, _ := utf8LastRune(b.String())
				prevIs = unicode.IsLetter(pr) || unicode.IsDigit(pr)
			}
			if i+1 < n {
				nextRune := runes[i+1]
				nextIs = unicode.IsLetter(nextRune) || unicode.IsDigit(nextRune)
			}
			if prevIs && nextIs {
				// keep apostrophe inside word
				b.WriteRune(r)
				continue
			}
			// otherwise treat as standalone quote token
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

// helper to get last rune of a string (used above)
func utf8LastRune(s string) (rune, int) {
	if s == "" {
		return 0, 0
	}
	r, size := rune(s[len(s)-1]), 1
	// rough fallback: iterate runes to find last one
	for i := len(s) - 1; i >= 0; {
		r, size = utf8.DecodeLastRuneInString(s[:i+1])
		return r, size
	}
	return r, size
}
