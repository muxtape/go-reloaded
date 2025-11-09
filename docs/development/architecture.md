# Go-Reloaded — Architecture & FSM Overview

This document describes the architecture of Go-Reloaded, the streaming FSM-based text transformer. It is intended for maintainers and new contributors: how components interact, where to look for behavior, and how to extend the system.

## High-level overview
Input → Tokenizer → FSM (streaming rules) → Formatter → Output

- Tokenizer: splits raw text into tokens (words, punctuation, parentheses commands).
- FSM: processes tokens sequentially, applies transformation rules (case, hex/bin conversion, article, quotes), and emits output tokens. The FSM supports streaming (ProcessToken / Finalize).
- Formatter: joins output tokens into human-readable text with correct spacing and punctuation.
- I/O: ReadLines / ReadLinesWithErr read input line-by-line; WriteLines / WriteLinesWithErr write output.

## Simple diagrams

Note: these use Mermaid syntax. Render them in a Markdown viewer that supports Mermaid to see the diagrams.

Component flow (high level):
```mermaid
flowchart LR
  Input[Input (file / STDIN)] --> Tokenizer[Tokenizer]
  Tokenizer --> FSM[FSM (streaming rules)]
  FSM --> Formatter[Formatter]
  Formatter --> Output[Output (file / STDOUT)]

  subgraph pkg [packages]
    Tokenizer
    FSM
    Formatter
    IO[(pkg/input & pkg/output)]
  end

  cmd[cmd/main.go] --> IO
  cmd --> Tokenizer
  cmd --> FSM
  cmd --> Formatter
  IO --> Output
```

Sequence: processing one line
```mermaid
sequenceDiagram
  participant CLI as cmd/main
  participant Reader as ReadLines
  participant Tok as Tokenizer
  participant FSM as FSM
  participant Formatter as FormatTokens
  participant Writer as WriteLines

  CLI->>Reader: request next line
  Reader-->>CLI: line text
  CLI->>Tok: Tokenize(line)
  Tok-->>CLI: tokens
  CLI->>FSM: ProcessToken(tokens...) (streaming)
  Note right of FSM: FSM updates state; may defer transforms until rules seen
  CLI->>FSM: Finalize() (end-of-input)
  FSM-->>CLI: outTokens
  CLI->>Formatter: FormatTokens(outTokens)
  Formatter-->>CLI: formatted text
  CLI->>Writer: write formatted text
  Writer-->>CLI: flush / result
```

## Components

- pkg/tokenizer
  - Tokenize(s string) []string
  - Command tokens: normalized `(low,3)`, `(hex)`, `(bin)`, `(cap, N)`, `(up)`, `(a/an)`.

- internal/fsm
  - FSM type: streaming API
    - New()
    - Process(tokens []string) ([]string, error)
    - ProcessToken(tok string) error  // streaming single token
    - Finalize() ([]string, error)
  - Rule implementations:
    - applyCaseRule
    - applyHexRule
    - applyBinRule
    - article handling
  - LineBreakToken sentinel marks original newlines (rendered by FormatTokens).

- internal/fsm/format.go
  - FormatTokens([]string) string — spacing and punctuation rules, quote handling.

- pkg/input, pkg/output
  - ReadLinesWithErr / ReadLines
  - WriteLinesWithErr / WriteLines

- cmd/main.go
  - CLI glue; builds FSM, feeds tokens, finalizes, writes output.

## Data flow example
Input line:
  "hello (this is a test) (up)"
Tokenizer:
  ["hello", "(", "this", "is", "a", "test", ")", "(up)"]
FSM streaming:
  - "hello" → append
  - "(" → pass through (non-command)
  - "this","is","a","test" → case rules may apply when "(up)" is processed
  - "(up)" → transform previous relevant token(s)
Formatter:
  - FormatTokens produces: "hello (this is a TEST)"

## Design decisions & invariants
- Tokenizer emits `(cmd)` tokens normalized (no inner spaces) for recognized commands; parentheses with spaces are emitted as "(" ... ")" tokens.
- FSM treats tokens starting with '(' and with length > 1 as rules; literal "(" is a regular token.
- Line break sentinel is a non-alphanumeric token (internal constant) so word-counting logic ignores it.
- Formatter handles spacing and quotes; FSM emits tokens only — formatting is a separate concern.

## Extension points
- Add new rule: add applyXRule in internal/fsm and a small parser branch in applyRule. Add unit tests in internal/fsm/*_test.go.
- Streaming output: adapt FSM to emit tokens incrementally (channel-based) if low-memory streaming is required.
- Add context-aware I/O (context.Context) to cancel long runs.

## Tests & validation
- Unit tests:
  - tokenizer edge cases (parenthesis commands, contractions, hyphens)
  - FSM rule unit tests (hex/bin/case/article)
  - FormatTokens tests (parentheses, quotes, punctuation)
- Integration:
  - Golden end-to-end tests using sample inputs in `docs/Golden_Test_Cases.md`.
- CI:
  - Run `go test ./... -v`, `go vet ./...`, and `staticcheck ./...`.

## Developer checklist for changes
1. Run full test-suite before refactor: `go test ./... -v`.
2. Make small, incremental changes and add targeted tests.
3. Run linters and staticcheck locally.
4. Open PR with description and link to failing/passing tests.

(End)