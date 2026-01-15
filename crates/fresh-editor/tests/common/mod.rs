// Common test utilities

#[cfg(test)]
#[allow(dead_code)]
pub mod fake_lsp;
#[cfg(test)]
#[allow(dead_code)]
pub mod fixtures;
#[cfg(test)]
#[allow(dead_code)]
pub mod git_test_helper;
#[cfg(test)]
#[allow(dead_code)]
pub mod harness;
#[cfg(test)]
#[allow(dead_code)]
pub mod scrollbar;
#[cfg(test)]
#[allow(dead_code)]
pub mod tracing;
#[cfg(test)]
#[allow(dead_code)]
pub mod visual_testing;

// Note: Visual regression tests write their own documentation files independently.
// No destructor needed - each test is self-contained and parallel-safe.
