## QuickLSP

A fast, lightweight, universal Language Server in Rust.

**One engine, one code path.** Every LSP operation — definitions, references, symbols, completions — goes through a single in-memory `Workspace`. An exact symbol table maps every definition name to its file and location. No approximations.

**Custom tokenizer.** A single-pass state machine extracts definitions across 8 language families (Rust, C/C++, Go, Python, JS/TS, Java/C#, Ruby) with full Unicode support. Median parse time: 16µs per file. No external grammars to install.

**Typo-tolerant search.** Precomputed deletion neighborhoods resolve transposition and insertion typos in O(1) hash lookups. 100% accuracy on synthetic typo benchmarks.

**8 runtime dependencies.** No grammar files, no compilation databases, no build system integration. Point it at a directory and it works.

**Target scale.** Repos up to ~50MB of source comfortably in memory.
