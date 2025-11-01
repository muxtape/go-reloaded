package main

import (
	"fmt"
	"os"

	fsm "platform.zone01.gr/git/atampour/go-reloaded/internal/fsm"
	modinput "platform.zone01.gr/git/atampour/go-reloaded/pkg/input"
	moutput "platform.zone01.gr/git/atampour/go-reloaded/pkg/output"
	tokenizer "platform.zone01.gr/git/atampour/go-reloaded/pkg/tokenizer"
)

func usage() {
	fmt.Fprintf(os.Stderr, "usage: %s <input-file> <output-file>\n", os.Args[0])
}

func main() {
	if len(os.Args) != 3 {
		usage()
		os.Exit(2)
	}
	inPath := os.Args[1]
	outPath := os.Args[2]

	linesCh, err := modinput.ReadLines(inPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening input %q: %v\n", inPath, err)
		os.Exit(1)
	}

	outCh := make(chan string)
	errW := make(chan error, 1)
	go func() {
		errW <- moutput.WriteLines(outPath, outCh)
	}()

	// Process each input line independently (new FSM per line).
	for line := range linesCh {
		// Tokenize the input line
		toks := tokenizer.Tokenize(line)
		// Process tokens with a fresh FSM
		f := fsm.New()
		outToks, err := f.Process(toks)
		if err != nil {
			// on processing error, close writer and exit
			close(outCh)
			<-errW
			fmt.Fprintf(os.Stderr, "processing error for line %q: %v\n", line, err)
			os.Exit(1)
		}
		// Format tokens back into a line
		outLine := fsm.FormatTokens(outToks)
		outCh <- outLine
	}

	// finished feeding writer
	close(outCh)

	// wait for writer to finish and check error
	if err := <-errW; err != nil {
		fmt.Fprintf(os.Stderr, "error writing output %q: %v\n", outPath, err)
		os.Exit(1)
	}
}
