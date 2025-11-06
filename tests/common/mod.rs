// Common test utilities

pub mod fake_lsp;
pub mod fixtures;
pub mod git_test_helper;
pub mod harness;
pub mod visual_testing;

// Note: Visual regression tests write their own documentation files independently.
// No destructor needed - each test is self-contained and parallel-safe.
