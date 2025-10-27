# Agile + TDD Task Breakdown for AI Agents (Streaming FSM Architecture)

---

##  Purpose of This Document
This file provides a **machine-readable and actionable roadmap** for AI software agents (e.g., GPT-5 Codex, Claude Code, or Copilot) working on the Go-Reloaded project. It is derived from the human-authored `/docs/development/agile_tdd_task_breakdown.md` and extended with **detailed meta-prompts** for autonomous task execution under Agile + Test-Driven Development (TDD) principles.

###  How to Use This Document
- **Planner Agent** reads and assigns one task at a time.
- **Developer Agent** follows each task’s meta-prompt to execute TDD steps.
- **Reviewer Agent** verifies test coverage, code quality, and compliance.
- **Integrator Agent** merges approved work after validation.

All agents communicate using standardized messages defined at the end of this file.

---

#  Sprint 1: Project Initialization & Core I/O

### Task 1.1 — Initialize Project Structure
**Goal:** Set up the foundational Go module and folder layout.
- **TDD Step:** Write a dummy test to confirm `go test ./...` runs successfully.
- **Implementation:** Create module files, base folders, and placeholder tests.
- **Validation:** All tests execute successfully.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Initialize Go project structure for TDD workflow.
Steps:
  1. Create go.mod and directories: /cmd, /pkg, /internal, /docs.
  2. Add a dummy test file and confirm go test ./... runs successfully.
  3. Verify proper project bootstrapping.
Communication:
  - Report completion to Reviewer Agent.
  - Reviewer ensures project compiles and tests run.
  - Integrator merges initial commit.
```

### Task 1.2 — Implement Input Reader
**Goal:** Read text from an input file line by line for streaming.
- **TDD Step:** Write tests verifying correct reading and error handling.
- **Implementation:** Implement `ReadLines(path string) (<-chan string, error)` in `pkg/input/reader.go`.
- **Validation:** Tests confirm correct output and error states.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement streaming input reader with line-based channel output.
Steps:
  1. Write failing unit tests for valid/invalid file reads.
  2. Implement ReadLines(path string) using bufio.Scanner.
  3. Ensure proper error handling and channel closure.
  4. Run all tests and confirm success.
Communication:
  - Developer submits passing tests to Reviewer.
  - Reviewer verifies robustness and idiomatic Go.
  - Integrator merges when approved.
```

### Task 1.3 — Implement Output Writer
**Goal:** Write transformed output lines to a specified file.
- **TDD Step:** Write tests verifying written content matches expected output.
- **Implementation:** `WriteLines(path string, lines <-chan string) error`.
- **Validation:** Output file matches expected results.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement streaming output writer.
Steps:
  1. Write failing test to confirm file creation and line accuracy.
  2. Implement WriteLines(path, lines) using bufio.Writer.
  3. Run all tests, verify content correctness.
Communication:
  - Reviewer validates file handling and test coverage.
  - Integrator merges after review.
```

---

#  Sprint 2: FSM Foundation & Tokenization

### Task 2.1 — Define Tokenizer
**Goal:** Split input text into tokens (words, commands, punctuation).
- **TDD Step:** Write tokenization tests for edge cases.
- **Implementation:** Implement `pkg/tokenizer/tokenizer.go`.
- **Validation:** All tokenization tests pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement tokenizer that splits text into tokens respecting punctuation and rules.
Steps:
  1. Write tests verifying correct token output for diverse examples.
  2. Implement tokenizer to yield word, punctuation, and rule tokens.
  3. Verify punctuation and command recognition.
Communication:
  - Reviewer ensures correct token granularity.
  - Integrator merges when validated.
```

### Task 2.2 — Design FSM States & Transitions
**Goal:** Define FSM structure and transitions.
- **TDD Step:** Structural test ensuring valid states and transitions.
- **Implementation:** Implement `FSM` in `internal/fsm/fsm.go`.
- **Validation:** FSM transitions verified.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Create FSM skeleton for token-by-token processing.
Steps:
  1. Define FSM states (READ, APPLY_RULE, OUTPUT).
  2. Write tests for valid state transitions.
  3. Implement FSM type with transition table.
  4. Confirm all tests pass.
Communication:
  - Reviewer verifies FSM logic and coverage.
  - Integrator merges once validated.
```

### Task 2.3 — Implement Base Processing Loop
**Goal:** Core FSM loop processes tokens sequentially.
- **TDD Step:** Integration test simulating input/output round-trip.
- **Implementation:** Implement `FSM.Process(tokens []string)`.
- **Validation:** Test confirms identity transformation.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Establish base FSM loop to process tokens sequentially.
Steps:
  1. Write integration test for token pass-through.
  2. Implement minimal loop without transformations.
  3. Confirm round-trip test passes.
Communication:
  - Reviewer confirms architecture alignment.
  - Integrator merges after validation.
```

---

#  Sprint 3: Transformation Rules

Each transformation rule is implemented as an FSM action.

### Task 3.1 — Implement (hex) Rule
**Goal:** Convert preceding hex number to decimal.
- **TDD Step:** Failing tests for valid and invalid hex.
- **Implementation:** `applyHexRule()` in FSM.
- **Validation:** Tests pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement hexadecimal to decimal conversion rule.
Steps:
  1. Write tests verifying hex to decimal conversions.
  2. Implement applyHexRule(tokens, index) handling errors safely.
  3. Verify correct replacements and rerun tests.
Communication:
  - Reviewer validates conversion accuracy.
  - Integrator merges after verification.
```

### Task 3.2 — Implement (bin) Rule
**Goal:** Convert binary word to decimal.
- **TDD Step:** Tests for valid binary conversions.
- **Implementation:** `applyBinRule()`.
- **Validation:** All binary tests pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement binary to decimal conversion rule.
Steps:
  1. Write tests verifying correct binary conversion.
  2. Implement applyBinRule(tokens, index).
  3. Confirm all tests pass.
Communication:
  - Reviewer ensures correctness and test coverage.
  - Integrator merges once approved.
```

### Task 3.3 — Implement (up), (low), (cap) Rules
**Goal:** Change case of preceding word(s).
- **TDD Step:** Unit tests for all variations.
- **Implementation:** `applyCaseRule()`.
- **Validation:** All case transformation tests pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement case modification rules.
Steps:
  1. Write tests for (up), (low), (cap, N) variants.
  2. Implement applyCaseRule(tokens, index).
  3. Validate results against golden cases.
Communication:
  - Reviewer checks word count handling.
  - Integrator merges upon success.
```

### Task 3.4 — Implement Punctuation Spacing Rule
**Goal:** Adjust punctuation spacing.
- **TDD Step:** Edge case tests for commas, ellipses, etc.
- **Implementation:** Normalize punctuation spacing.
- **Validation:** Golden punctuation tests pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement punctuation spacing normalization.
Steps:
  1. Write tests for punctuation edge cases.
  2. Implement normalization logic.
  3. Rerun all tests.
Communication:
  - Reviewer validates spacing accuracy.
  - Integrator merges when approved.
```

### Task 3.5 — Implement Quotation Rule (' ')
**Goal:** Normalize single-quote spacing.
- **TDD Step:** Tests for single and multi-word quotes.
- **Implementation:** Quote open/close tracking in FSM.
- **Validation:** Golden quote cases pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement quotation normalization.
Steps:
  1. Write tests for quotes containing one or more words.
  2. Implement FSM logic to handle quotes.
  3. Verify test success.
Communication:
  - Reviewer validates text placement.
  - Integrator merges when validated.
```

### Task 3.6 — Implement Article Adjustment Rule (a/an)
**Goal:** Replace `a` with `an` before vowels or 'h'.
- **TDD Step:** Tests for correct substitutions.
- **Implementation:** `applyArticleRule()`.
- **Validation:** Golden test matches.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement article correction rule.
Steps:
  1. Write tests for vowel/h prefix detection.
  2. Implement applyArticleRule(tokens, index).
  3. Verify all tests pass.
Communication:
  - Reviewer checks correctness and edge cases.
  - Integrator merges on approval.
```

---

#  Sprint 4: Integration and End-to-End Testing

### Task 4.1 — Integrate All FSM Actions
**Goal:** Combine all transformation rules.
- **TDD Step:** Golden integration tests.
- **Implementation:** FSM dispatch system.
- **Validation:** Golden tests pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Integrate all FSM transformation rules.
Steps:
  1. Write integration test combining all rule behaviors.
  2. Implement rule dispatch logic in FSM.
  3. Verify complete golden tests suite.
Communication:
  - Reviewer ensures full coverage.
  - Integrator merges when validated.
```

### Task 4.2 — CLI Integration
**Goal:** Connect FSM to command-line arguments.
- **TDD Step:** Integration test simulating CLI.
- **Implementation:** `cmd/main.go` executes FSM pipeline.
- **Validation:** End-to-end test passes.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Implement CLI entrypoint.
Steps:
  1. Write test simulating go run . input.txt output.txt.
  2. Implement CLI argument handling.
  3. Execute end-to-end validation.
Communication:
  - Reviewer confirms proper I/O.
  - Integrator merges when successful.
```

### Task 4.3 — Performance and Memory Tests
**Goal:** Validate FSM efficiency.
- **TDD Step:** Benchmark tests.
- **Implementation:** Optimize FSM for throughput.
- **Validation:** Benchmarks within acceptable range.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Benchmark FSM for performance.
Steps:
  1. Write benchmark tests for large data.
  2. Identify performance bottlenecks.
  3. Apply optimizations safely.
  4. Re-run benchmarks.
Communication:
  - Reviewer evaluates metrics.
  - Integrator merges when stable.
```

---

#  Sprint 5: Maintenance, Documentation & Validation

### Task 5.1 — Code Cleanup & Refactoring
**Goal:** Improve maintainability.
- **TDD Step:** Ensure tests remain green.
- **Implementation:** Simplify code and improve modularity.
- **Validation:** All tests pass.

####  Meta-Prompt for AI Agents
```
Role: AI Software Developer
Goal: Refactor FSM code for clarity and maintainability.
Steps:
  1. Run all tests before refactor.
  2. Apply refactors incrementally.
  3. Re-run all tests after each change.
Communication:
  - Reviewer validates no regression.
  - Integrator merges final refactor.
```

### Task 5.2 — Add Developer Documentation
**Goal:** Document architecture and FSM logic.
- **Implementation:** Add `/docs/development/architecture.md`.
- **Validation:** Reviewer approves.

####  Meta-Prompt for AI Agents
```
Role: AI Documentation Agent
Goal: Document FSM and system design.
Steps:
  1. Summarize FSM workflow.
  2. Create diagrams for data flow.
  3. Write concise explanations of rule handling.
Communication:
  - Reviewer validates documentation quality.
  - Integrator merges after sign-off.
```

### Task 5.3 — Final Validation
**Goal:** Confirm all tests pass.
- **Implementation:** Run regression and benchmark suite.
- **Validation:** 100% test success.

####  Meta-Prompt for AI Agents
```
Role: AI QA Agent
Goal: Perform final validation and regression testing.
Steps:
  1. Execute full test suite.
  2. Confirm all golden cases pass.
  3. Report any issues to Planner.
Communication:
  - Reviewer reviews summary.
  - Integrator merges final release candidate.
```

---

#  Communication Protocol for AI Agents
- **Planner → Developer:** Assign next pending task.
- **Developer → Reviewer:** Submit passing code and tests.
- **Reviewer → Integrator:** Approve and request merge.
- **Integrator → Planner:** Confirm deployment or completion.

This ensures continuous, autonomous Agile + TDD development with transparent AI agent collaboration.

