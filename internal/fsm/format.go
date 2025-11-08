package fsm

import "strings"

// FormatTokens joins token slice into a single string applying punctuation spacing rules.
// Rules implemented:
//   - No space before closing punctuation: . , ! ? ; : ) ] }
//   - Space before opening punctuation like '(' but no space immediately after it: "word (note)"
//   - Single-quote handling: opening quote has a space before it (unless first token) and no space after it;
//     closing quote has no space before it and (if next token requires) a space after it.
//   - Treat other tokens normally (one space between words).
func FormatTokens(tokens []string) string {
	if len(tokens) == 0 {
		return ""
	}

	closing := map[string]bool{
		".": true, ",": true, "!": true, "?": true, ";": true, ":": true,
		")": true, "]": true, "}": true,
		"...": true,
	}

	opening := map[string]bool{
		"(": true, "[": true, "{": true,
	}

	var b strings.Builder
	quoteOpen := false

	for i, tok := range tokens {
		// Line break sentinel handling: emit newline and treat next token as line-start.
		if tok == LineBreakToken {
			b.WriteByte('\n')
			continue
		}

		// If this is the first token in the entire output, write it plainly.
		if i == 0 {
			b.WriteString(tok)
			if tok == "'" || tok == "’" {
				quoteOpen = !quoteOpen
			}
			continue
		}

		prevTok := tokens[i-1]

		// If previous token was a line break sentinel, treat current token as line-start.
		if prevTok == LineBreakToken {
			b.WriteString(tok)
			if tok == "'" || tok == "’" {
				quoteOpen = !quoteOpen
			}
			continue
		}

		// Handle single-quote tokens: opening vs closing (consider ASCII and Unicode apostrophe)
		if tok == "'" || tok == "’" {
			if !quoteOpen {
				// opening quote: add space before (normal), then no space after (handled by next token)
				b.WriteByte(' ')
				b.WriteString(tok)
				quoteOpen = true
				continue
			}
			// closing quote: no space before closing quote
			b.WriteString(tok)
			quoteOpen = false
			continue
		}

		// If previous token was a quote opening, no space between quote and next token
		if (prevTok == "'" || prevTok == "’") && quoteOpen {
			b.WriteString(tok)
			continue
		}

		// If current token is a closing punctuation, do not add a space before it.
		if closing[tok] {
			b.WriteString(tok)
			continue
		}

		// If previous token is an opening punctuation, do not add a space before current token.
		if opening[prevTok] {
			b.WriteString(tok)
			continue
		}

		// Otherwise, add a single space before current token.
		b.WriteByte(' ')
		b.WriteString(tok)
	}

	return b.String()
}
