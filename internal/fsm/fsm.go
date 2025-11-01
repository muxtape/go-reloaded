package fsm

import (
    "fmt"
    "strconv"
    "strings"
    "unicode"
)

type State int

const (
    READ State = iota
    APPLY_RULE
    OUTPUT
    END
)

func (s State) String() string {
    switch s {
    case READ:
        return "READ"
    case APPLY_RULE:
        return "APPLY_RULE"
    case OUTPUT:
        return "OUTPUT"
    case END:
        return "END"
    default:
        return fmt.Sprintf("State(%d)", int(s))
    }
}

type Event int

const (
    EventToken Event = iota // regular token read
    EventRule               // encountered a rule token like "(hex)"
    EventEmit               // finished applying rule, emit output
    EventEOF                // end of input
)

func (e Event) String() string {
    switch e {
    case EventToken:
        return "Token"
    case EventRule:
        return "Rule"
    case EventEmit:
        return "Emit"
    case EventEOF:
        return "EOF"
    default:
        return fmt.Sprintf("Event(%d)", int(e))
    }
}

// FSM is a minimal finite-state machine used to coordinate token processing.
// Transitions are defined explicitly in a table to make behavior testable.
type FSM struct {
    state       State
    transitions map[State]map[Event]State
    quoteOpen   bool // track single-quote open/close state
}

// New creates an FSM with the default transition table.
func New() *FSM {
    t := map[State]map[Event]State{
        READ: {
            EventToken: READ,      // keep reading tokens
            EventRule:  APPLY_RULE, // when a rule token seen, move to APPLY_RULE
            EventEOF:   END,
        },
        APPLY_RULE: {
            EventEmit: OUTPUT, // after applying rule, emit output
            // allow EOF here to end if rule can't emit
            EventEOF: END,
        },
        OUTPUT: {
            EventToken: READ, // after output, continue reading tokens
            EventEOF:   END,
        },
        END: {},
    }

    return &FSM{
        state:       READ,
        transitions: t,
        quoteOpen:   false,
    }
}

// State returns the current FSM state.
func (f *FSM) State() State {
    return f.state
}

// SendEvent attempts to transition the FSM using the provided event.
// Returns an error if the transition is not allowed.
func (f *FSM) SendEvent(e Event) error {
    if f.state == END {
        return fmt.Errorf("cannot send event %s in END state", e)
    }
    if nextMap, ok := f.transitions[f.state]; ok {
        if ns, ok := nextMap[e]; ok {
            f.state = ns
            return nil
        }
    }
    return fmt.Errorf("invalid transition: state=%s event=%s", f.state, e)
}

// applyHexRule converts the last emitted token (assumed hex) to decimal.
// Modifies the out slice in-place. Returns an error if there is no preceding token
// or the value is not a valid hexadecimal number.
func (f *FSM) applyHexRule(out *[]string) error {
    if len(*out) == 0 {
        return fmt.Errorf("no preceding token for (hex) rule")
    }
    prev := (*out)[len(*out)-1]
    s := prev
    if strings.HasPrefix(s, "0x") || strings.HasPrefix(s, "0X") {
        s = s[2:]
    }
    if s == "" {
        return fmt.Errorf("invalid hex token %q", prev)
    }
    v, err := strconv.ParseInt(s, 16, 64)
    if err != nil {
        return fmt.Errorf("invalid hex token %q: %w", prev, err)
    }
    (*out)[len(*out)-1] = fmt.Sprintf("%d", v)
    return nil
}

// applyBinRule converts the last emitted token (assumed binary) to decimal.
// Accepts optional "0b" or "0B" prefix. Modifies out in-place.
func (f *FSM) applyBinRule(out *[]string) error {
    if len(*out) == 0 {
        return fmt.Errorf("no preceding token for (bin) rule")
    }
    prev := (*out)[len(*out)-1]
    s := prev
    if strings.HasPrefix(s, "0b") || strings.HasPrefix(s, "0B") {
        s = s[2:]
    }
    if s == "" {
        return fmt.Errorf("invalid bin token %q", prev)
    }
    // Validate that s contains only '0' or '1'
    for _, r := range s {
        if r != '0' && r != '1' {
            return fmt.Errorf("invalid bin token %q", prev)
        }
    }
    v, err := strconv.ParseInt(s, 2, 64)
    if err != nil {
        return fmt.Errorf("invalid bin token %q: %w", prev, err)
    }
    (*out)[len(*out)-1] = fmt.Sprintf("%d", v)
    return nil
}

// helper: token contains at least one letter/digit (counts as a word token)
func tokenHasLetter(s string) bool {
    for _, r := range s {
        if unicode.IsLetter(r) || unicode.IsDigit(r) {
            return true
        }
    }
    return false
}

// capitalizeWord capitalizes first rune and lowercases the rest.
func capitalizeWord(s string) string {
    if s == "" {
        return s
    }
    r := []rune(s)
    r[0] = unicode.ToUpper(r[0])
    for i := 1; i < len(r); i++ {
        r[i] = unicode.ToLower(r[i])
    }
    return string(r)
}

// applyCaseRule handles (up), (low), (cap) and (cap,N) rules.
// It applies the transformation to the preceding N word tokens (skipping punctuation).
func (f *FSM) applyCaseRule(ruleTok string, out *[]string) error {
    // strip parentheses and split by comma
    inner := strings.TrimSuffix(strings.TrimPrefix(ruleTok, "("), ")")
    parts := strings.Split(inner, ",")
    name := strings.ToLower(strings.TrimSpace(parts[0]))

    // default count = 1; allow optional numeric count for up/low/cap
    count := 1
    if len(parts) > 1 {
        nstr := strings.TrimSpace(parts[1])
        if nstr != "" {
            n, err := strconv.Atoi(nstr)
            if err != nil || n <= 0 {
                return fmt.Errorf("invalid count in %q", ruleTok)
            }
            count = n
        }
    }

    // find and transform previous 'count' word tokens (skip punctuation)
    i := len(*out) - 1
    transformed := 0
    for i >= 0 && transformed < count {
        if tokenHasLetter((*out)[i]) {
            switch name {
            case "up":
                (*out)[i] = strings.ToUpper((*out)[i])
            case "low":
                (*out)[i] = strings.ToLower((*out)[i])
            case "cap":
                (*out)[i] = capitalizeWord((*out)[i])
            default:
                // unknown case rule - no-op but keep count semantics
            }
            transformed++
        }
        i--
    }
    if transformed < count {
        return fmt.Errorf("not enough preceding words for %q", ruleTok)
    }
    return nil
}

// applyArticleRule inspects the preceding article ("a"/"A") in out and the next word token
// (from tokens at index i+1...). If the next word begins with a vowel or 'h' it replaces
// the preceding "a" with "an". Returns error if there is no preceding article token or no next word.
func (f *FSM) applyArticleRule(out *[]string, tokens []string, i int) error {
    // find the nearest preceding article token "a" (case-insensitive) in out
    j := len(*out) - 1
    found := -1
    for j >= 0 {
        if strings.ToLower((*out)[j]) == "a" {
            found = j
            break
        }
        j--
    }
    if found == -1 {
        return fmt.Errorf("no preceding article 'a' found")
    }

    // Try to find the next word token that follows the found article inside out first.
    k := found + 1
    for k < len(*out) && !tokenHasLetter((*out)[k]) {
        k++
    }
    var next string
    if k < len(*out) {
        next = (*out)[k]
    } else {
        // Fallback: look ahead in the original tokens after the rule index i.
        k = i + 1
        for k < len(tokens) && !tokenHasLetter(tokens[k]) {
            k++
        }
        if k >= len(tokens) {
            return fmt.Errorf("no following word to determine article")
        }
        next = tokens[k]
    }

    // find first letter/digit rune inside next token (skip leading punctuation like apostrophes)
    firstRune, ok := firstLetterOrDigit(next)
    if !ok {
        return fmt.Errorf("empty next token for article rule")
    }

    // vowels plus 'h' per requirement
    if firstRune == 'a' || firstRune == 'e' || firstRune == 'i' || firstRune == 'o' || firstRune == 'u' || firstRune == 'h' {
        // preserve capitalization of original article
        if (*out)[found] == "A" {
            (*out)[found] = "An"
        } else {
            (*out)[found] = "an"
        }
    }
    return nil
}

// helper: return first letter or digit rune (lowercased) from s, skipping leading non-letter/digit
func firstLetterOrDigit(s string) (rune, bool) {
    for _, r := range []rune(s) {
        if unicode.IsLetter(r) || unicode.IsDigit(r) {
            return unicode.ToLower(r), true
        }
    }
    return 0, false
}

// normalizeArticles scans out and replaces "a"/"A" with "an"/"An" when the next word
// (skipping punctuation) begins with a vowel or 'h'.
func normalizeArticles(out *[]string) {
    for i := 0; i < len(*out); i++ {
        if strings.ToLower((*out)[i]) != "a" {
            continue
        }
        // find next word token
        j := i + 1
        for j < len(*out) && !tokenHasLetter((*out)[j]) {
            j++
        }
        if j >= len(*out) {
            continue
        }
        first, ok := firstLetterOrDigit((*out)[j])
        if !ok {
            continue
        }
        if first == 'a' || first == 'e' || first == 'i' || first == 'o' || first == 'u' || first == 'h' {
            if (*out)[i] == "A" {
                (*out)[i] = "An"
            } else {
                (*out)[i] = "an"
            }
        }
    }
}

// Process runs a minimal FSM processing loop over the provided tokens.
// It returns the output tokens (with rules applied) and an error if any invalid transition occurs.
func (f *FSM) Process(tokens []string) ([]string, error) {
    var out []string
    for i := 0; i < len(tokens); i++ {
        tok := tokens[i]

        // Treat single-quote as quote delimiter: toggle quote state and emit token.
        if tok == "'" {
            // quote tokens do not trigger rule state; they are token-level.
            if err := f.SendEvent(EventToken); err != nil {
                return nil, fmt.Errorf("on token %q: %w", tok, err)
            }
            // toggle quote state
            f.quoteOpen = !f.quoteOpen
            out = append(out, tok)
            continue
        }

        // Rule tokens are parenthesized commands, e.g. "(hex)". Treat them specially:
        if len(tok) > 0 && tok[0] == '(' {
            // Only implement (hex), (bin), case rules, and article rule here.
            if err := f.SendEvent(EventRule); err != nil {
                return nil, fmt.Errorf("on token %q: %w", tok, err)
            }

            switch strings.ToLower(tok) {
            case "(hex)":
                if err := f.applyHexRule(&out); err != nil {
                    return nil, fmt.Errorf("on token %q: %w", tok, err)
                }
            case "(bin)":
                if err := f.applyBinRule(&out); err != nil {
                    return nil, fmt.Errorf("on token %q: %w", tok, err)
                }
            case "(a/an)":
                if err := f.applyArticleRule(&out, tokens, i); err != nil {
                    return nil, fmt.Errorf("on token %q: %w", tok, err)
                }
            default:
                // case rules: (up), (low), (cap) or (cap,N)
                lower := strings.ToLower(tok)
                if strings.HasPrefix(lower, "(up") || strings.HasPrefix(lower, "(low") || strings.HasPrefix(lower, "(cap") {
                    if err := f.applyCaseRule(tok, &out); err != nil {
                        return nil, fmt.Errorf("on token %q: %w", tok, err)
                    }
                }
                // unknown rules are no-op
            }

            if err := f.SendEvent(EventEmit); err != nil {
                return nil, fmt.Errorf("on token %q: %w", tok, err)
            }

            // After OUTPUT state, resume reading
            if err := f.SendEvent(EventToken); err != nil {
                return nil, fmt.Errorf("on token %q: %w", tok, err)
            }
            continue
        }

        // Regular token
        if err := f.SendEvent(EventToken); err != nil {
            return nil, fmt.Errorf("on token %q: %w", tok, err)
        }
        out = append(out, tok)
    }

    // Signal EOF to finish processing
    if err := f.SendEvent(EventEOF); err != nil {
        return nil, err
    }

    // Error if a single-quote was left open
    if f.quoteOpen {
        return nil, fmt.Errorf("unclosed single-quote at EOF")
    }

    // automatic article normalization (applies even without explicit rule token)
    normalizeArticles(&out)

    return out, nil
}