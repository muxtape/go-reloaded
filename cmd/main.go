package main

import (
	"flag"
	"log"

	"platform.zone01.gr/git/atampour/go-reloaded/internal/fsm"
	inputpkg "platform.zone01.gr/git/atampour/go-reloaded/pkg/input"
	outputpkg "platform.zone01.gr/git/atampour/go-reloaded/pkg/output"
	"platform.zone01.gr/git/atampour/go-reloaded/pkg/tokenizer"
)

// use formatter implemented in format.go (avoid duplicating formatting logic)

func main() {
	inPath := flag.String("in", "input.txt", "input file path")
	outPath := flag.String("out", "output.txt", "output file path")
	flag.Parse()

	// allow positional args: prog [in-file [out-file]]
	args := flag.Args()
	if len(args) >= 1 {
		*inPath = args[0]
	}
	if len(args) >= 2 {
		*outPath = args[1]
	}

	linesCh, err := inputpkg.ReadLines(*inPath)
	if err != nil {
		log.Fatalf("open input: %v", err)
	}

	f := fsm.New()

	for line := range linesCh {
		toks := tokenizer.Tokenize(line)
		for _, tok := range toks {
			if err := f.ProcessToken(tok); err != nil {
				log.Fatalf("processing token %q: %v", tok, err)
			}
		}

		// preserve original line boundary in the token stream
		if err := f.ProcessToken(fsm.LineBreakToken); err != nil {
			log.Fatalf("processing line break token: %v", err)
		}
		// Note: this pipeline preserves token stream across lines.
		// If you need explicit line breaks preserved in output, add a special token here
		// and extend formatting accordingly.
	}

	outTokens, err := f.Finalize()
	if err != nil {
		log.Fatalf("finalize FSM: %v", err)
	}

	formatted := fsm.FormatTokens(outTokens)

	// use output writer (single-line)
	ch := make(chan string, 1)
	ch <- formatted
	close(ch)

	// Use WriteLinesWithErr to perform writes in a goroutine and observe any errors.
	errs, werr := outputpkg.WriteLinesWithErr(*outPath, ch)
	if werr != nil {
		log.Fatalf("create output writer: %v", werr)
	}
	for e := range errs {
		if e != nil {
			log.Fatalf("write output: %v", e)
		}
	}
}
