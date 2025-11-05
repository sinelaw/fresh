// Common test utilities

pub mod fake_lsp;
pub mod fixtures;
pub mod git_test_helper;
pub mod harness;
pub mod visual_testing;

// Test setup/teardown hooks for visual regression testing
#[cfg(test)]
#[ctor::dtor]
fn finalize_visual_testing() {
    // After all tests complete, generate the visual documentation
    // This runs at process exit, after all tests have completed
    if std::env::var("SKIP_VISUAL_DOCS").is_err() {
        visual_testing::generate_visual_documentation().ok();
    }
}
