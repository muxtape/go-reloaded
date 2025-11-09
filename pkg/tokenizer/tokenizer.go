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

	// scan ahead to decide if this parenthesis is a command (emit "(compact)") and return
	// new index (j) if we consumed it; else return -1 to indicate don't skip inner content.
	scanParenCommand := func(i int) (token string, newIdx int) {
		j := i + 1
		for j < n && runes[j] != ')' {
			j++
		}
		if j >= n || runes[j] != ')' {
			return "", -1
		}
		inner := string(runes[i+1 : j])
		compact := strings.ReplaceAll(inner, " ", "")
		if compact == "" {
			return "", -1
		}
		// allowed command characters
		for _, rr := range compact {
			if !(unicode.IsLetter(rr) || unicode.IsDigit(rr) || rr == ',' || rr == '-' || rr == '.' || rr == ':' || rr == '%') {
				return "", -1
			}
		}
		lc := strings.ToLower(compact)
		whitelist := map[string]bool{"hex": true, "bin": true, "cap": true, "low": true, "up": true}
		if strings.Contains(compact, ",") || whitelist[lc] {
			return "(" + compact + ")", j
		}
		return "", -1
	}

	// get last rune from builder string (used to inspect prior char for apostrophe logic)
	utf8LastRune := func(s string) (rune, int) {
		if s == "" {
			return 0, 0
		}
		return utf8.DecodeLastRuneInString(s)
	}

	for i := 0; i < n; i++ {
		r := runes[i]

		// Whitespace: flush current word and continue.
		if unicode.IsSpace(r) {
			flushWord()
			continue
		}

		// Parenthesis handling: try to parse as a command "(...)" first.
		if r == '(' {
			if tok, j := scanParenCommand(i); j != -1 {
				flushWord()
				toks = append(toks, tok)
				i = j // advance past ')'
				continue
			}
			// otherwise treat '(' as its own token and continue (inner text will be tokenized normally)
			flushWord()
			toks = append(toks, "(")
			continue
		}

		// Handle ASCII apostrophe (') and typographic apostrophe (’) uniformly:
		// - If surrounded by letters/digits -> keep inside current word (contraction/O'Name).
		// - Otherwise emit as a standalone quote token.
		if r == '\'' || r == '’' {
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
				b.WriteRune(r)
				continue
			}
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
