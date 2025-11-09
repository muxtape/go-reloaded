package fsm

import "strings"

// helper: punctuation categories
var closingPunct = map[string]bool{
    ".":   true,
    ",":   true,
    "!":   true,
    "?":   true,
    ";":   true,
    ":":   true,
    ")":   true,
    "]":   true,
    "}":   true,
    "...": true,
}

var openingPunct = map[string]bool{
    "(": true,
    "[": true,
    "{": true,
}

func isClosing(tok string) bool { return closingPunct[tok] }
func isOpening(tok string) bool { return openingPunct[tok] }

// FormatTokens joins token slice into a single string applying punctuation spacing rules.
func FormatTokens(tokens []string) string {
    if len(tokens) == 0 {
        return ""
    }

    var b strings.Builder
    quoteOpen := false
    atLineStart := true
    var prev string
    prevWasOpeningQuote := false

    for _, tok := range tokens {
        // Line break sentinel handling: emit newline and treat next token as line-start.
        if tok == LineBreakToken {
            b.WriteString("\n")
            atLineStart = true
            prev = ""
            prevWasOpeningQuote = false
            continue
        }

        // If this is the first token in the entire output or right after a line break,
        // write it without a leading space but still respect quote/opening/closing semantics.
        if atLineStart {
            // Handle opening single-quote as opening (space not needed at start)
            if tok == "'" || tok == "’" {
                b.WriteString(tok)
                quoteOpen = true
                atLineStart = false
                prev = tok
                prevWasOpeningQuote = true
                continue
            }
            // For any other token, emit directly (this preserves leading "(")
            b.WriteString(tok)
            atLineStart = false
            prev = tok
            prevWasOpeningQuote = false
            continue
        }

        // Single-quote handling: opening vs closing
        if tok == "'" || tok == "’" {
            if quoteOpen {
                // closing quote: attach directly, and mark that the previous quote is NOT an opening one
                b.WriteString(tok)
                quoteOpen = false
                prev = tok
                prevWasOpeningQuote = false
                continue
            }
            // opening quote: add space before unless at line start (handled above)
            b.WriteByte(' ')
            b.WriteString(tok)
            quoteOpen = true
            prev = tok
            prevWasOpeningQuote = true
            continue
        }

        // Closing punctuation: no space before it.
        if isClosing(tok) {
            b.WriteString(tok)
            prev = tok
            prevWasOpeningQuote = false
            continue
        }

        // If previous token was an opening punctuation (or an opening single-quote), do not add a space before current token.
        if isOpening(prev) || prevWasOpeningQuote {
            b.WriteString(tok)
            prev = tok
            prevWasOpeningQuote = false
            continue
        }

        // Default: add a space before the token.
        b.WriteByte(' ')
        b.WriteString(tok)
        prev = tok
        prevWasOpeningQuote = false
    }

    return b.String()
}
