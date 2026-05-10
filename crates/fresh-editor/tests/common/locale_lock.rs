//! Shared lock for tests that mutate process-global locale state.
//!
//! `rust_i18n::set_locale` is process-global, and `EditorTestHarness`
//! transitively calls `fresh::i18n::init_with_config` whenever it
//! constructs an editor with a non-default `config.locale`. Cargo runs
//! tests in parallel, so any harness setup that pins a non-English
//! locale races against any other test that asserts on translated UI
//! strings.
//!
//! Every test that either (a) creates a harness with a non-English
//! `config.locale`, or (b) directly calls `fresh::i18n::set_locale`,
//! must hold the lock returned by [`lock_locale`] for the duration of
//! the test. The returned guard resets the global locale to `"en"` on
//! drop, so the next test starts with a clean default — without that
//! reset, English-only assertions in unrelated tests would still flake
//! against a stale German / French / Spanish locale.

use std::sync::Mutex;

static LOCALE_LOCK: Mutex<()> = Mutex::new(());

/// Acquire the locale lock and reset to English on drop.
///
/// Use this whenever a test will pin the global locale (either by
/// creating a harness with `config.locale = LocaleName(Some(...))` or
/// by calling `fresh::i18n::set_locale` directly). The guard resets
/// the locale to `"en"` on drop so subsequent tests start clean.
pub fn lock_locale() -> impl Drop {
    struct Guard(#[allow(dead_code)] std::sync::MutexGuard<'static, ()>);
    impl Drop for Guard {
        fn drop(&mut self) {
            fresh::i18n::set_locale("en");
        }
    }
    let guard = LOCALE_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    fresh::i18n::set_locale("en");
    Guard(guard)
}
