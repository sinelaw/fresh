# Dependency Security Status

## Syntect Unmaintained Dependencies

### Current Status
- **syntect version:** 5.3.0 (latest available on crates.io)  
- **Unmaintained transitive dependencies:**
  - `bincode 1.3.3` (RUSTSEC-2025-0141)
  - `yaml-rust 0.4.5` (RUSTSEC-2024-0320)

### Risk Assessment
- **Severity:** LOW-MEDIUM
- **Type:** Unmaintained dependencies (no active CVEs)
- **Impact:** Future security issues won't be patched upstream

###Mitigation
1. **Current:** syntect is an optional dependency behind the "runtime" feature
2. **Fallback:** Fresh has tree-sitter highlighter as a working alternative
3. **Monitoring:** Track https://github.com/trishume/syntect for updates

### Long-Term Options
1. **Wait for upstream:** Monitor syntect for dependency updates
2. **Full tree-sitter migration:** Make syntect truly optional, rely on tree-sitter
3. **Fork and patch:** Vendor syntect with updated dependencies

### Decision
Proceeding with documentation of the limitation. The risk is acceptable because:
- No active CVEs, just unmaintained status
- Alternative highlighter (tree-sitter) is available
- syntect is optional and can be disabled if needed
- Fresh is not exposing untrusted input to these libraries
