# Go-Reloaded

A small streaming text transformer written in Go. Input text is tokenized, processed by a finite-state machine (FSM) that applies transformation rules (case changes, hex/bin conversions, article fixes, quote normalization, etc.), then formatted and written back to output. The tool is designed for incremental/streaming processing and easy extension with new rules.

Badges

[![CI](https://github.com/muxtape/go-reloaded/actions/workflows/ci.yaml/badge.svg)](https://github.com/muxtape/go-reloaded/actions/workflows/ci.yaml)
- CI: .github/workflows/ci.yml runs tests, go vet and staticcheck.

Quick summary
- Tokenize → FSM (rules) → Format → Output
- Streaming-friendly API: ProcessToken / Finalize
- Extensible rule set: add applyXRule in internal/fsm

Highlights / Features
- Preserves hyphenated words and contractions (e.g., state-of-art, don't).
- Supports command tokens: `(up)`, `(low)`, `(cap,N)`, `(hex)`, `(bin)`, `(a/an)`.
- Parentheses with spaces are preserved as literal `( ... )`; recognized commands are normalized to `(cmd,args)`.
- Robust quote and punctuation spacing rules.
- Streaming I/O: ReadLinesWithErr / WriteLinesWithErr with observable errors.

Install / Requirements
- Go 1.24.x (go.mod: go 1.24.9)

Build
```bash
go build ./cmd
```

Run
```bash
# using flags
go run ./cmd -in sample.txt -out output.txt

# using positional args
go run ./cmd sample.txt output.txt
```

Examples

Input:
```
This is a ' tricky (up) ' sample text
```
Output:
```
This is a 'TRICKY' sample text
```

Rule examples:
- "hello (this is a test) (up)" → "hello (this is a TEST)"
- "Packed 1a (hex)" → "Packed 26"

Project layout
- cmd/ — CLI entrypoint
- pkg/tokenizer — tokenization logic
- internal/fsm — FSM implementation and formatting
- pkg/input — ReadLines / ReadLinesWithErr
- pkg/output — WriteLines / WriteLinesWithErr
- docs/ — architecture, tasks, golden tests

Developer notes
- Tokenizer and Formatter intentionally separated.
- LineBreakToken is a non-alphanumeric sentinel so it is ignored by word-count rules.
- For streaming usage: create f := fsm.New(); feed with f.ProcessToken(tok) per token; call f.Finalize() at EOF; format with fsm.FormatTokens(outTokens).

Testing & Linters
```bash
go test ./... -v
go vet ./...
go install honnef.co/go/tools/cmd/staticcheck@latest
staticcheck ./...
```

CI
- `.github/workflows/ci.yml` runs tests, vet, staticcheck on pushes/PRs to main.

Contributing
- Run tests and linters before PR.
- Add unit tests for tokenizer, FSM rules, and formatter when changing behavior.

License

This project is licensed under the MIT License — see the LICENSE file.

