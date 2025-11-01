package fsm

import (
	"strings"
	"testing"

	tokenizer "platform.zone01.gr/git/atampour/go-reloaded/pkg/tokenizer"
)

// helper to build a repetitive token slice
func buildTokens(pattern []string, repeat int) []string {
	var out []string
	out = make([]string, 0, len(pattern)*repeat)
	for i := 0; i < repeat; i++ {
		out = append(out, pattern...)
	}
	return out
}

func BenchmarkProcess_Small(b *testing.B) {
	pattern := []string{"I", "saw", "a", "apple", "(a/an)", ",", "ff", "(hex)", "1010", "(bin)", "hello", "(cap)"}
	toks := buildTokens(pattern, 8) // ~96 tokens

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		f := New()
		if _, err := f.Process(toks); err != nil {
			b.Fatalf("process error: %v", err)
		}
	}
}

func BenchmarkProcess_Large(b *testing.B) {
	para := []string{
		"If", "I", "make", "you", "BREAKFAST", "IN", "BED", "(low,3)", ",",
		"I", "have", "101", "(bin)", "outfits", ".", "Packed", "1a", "(hex)", ".",
		"Do", "not", "be", "sad", ",", "because", "sad", "backwards", "is", "das", ".",
	}
	toks := buildTokens(para, 500) // ~9k tokens

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		f := New()
		if _, err := f.Process(toks); err != nil {
			b.Fatalf("process error: %v", err)
		}
	}
}

func BenchmarkProcess_Parallel(b *testing.B) {
	pattern := []string{"He", "said", "'", "hello", "world", "'", ".", "MAKE", "(low)", "that", "(up,2)"}
	toks := buildTokens(pattern, 100) // ~800 tokens

	b.ReportAllocs()
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			f := New()
			if _, err := f.Process(toks); err != nil {
				b.Fatalf("process error: %v", err)
			}
		}
	})
}

func BenchmarkFormatTokens(b *testing.B) {
	pattern := []string{"Hello", ",", "(", "world", ")", ":", "nice", "to", "meet", "you", "."}
	toks := buildTokens(pattern, 1000) // ~11k tokens

	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = FormatTokens(toks)
	}
}

func BenchmarkTokenizer(b *testing.B) {
	line := strings.Repeat("If I saw a apple (a/an) ff (hex) 1010 (bin). ", 200)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = tokenizer.Tokenize(line)
	}
}
