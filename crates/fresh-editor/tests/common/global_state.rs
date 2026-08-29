//! One pin for every process-global the test harness derives from config.
//!
//! `EditorTestHarness::create` mutates two process-global registries while
//! building an editor, both keyed off the `Config` it was handed:
//!
//!   * the i18n locale — `fresh::i18n::init_with_config`, which ends in
//!     `fresh_i18n::set_locale`; and
//!   * the user indentation rules — `Editor::for_test` calls
//!     `config::reload_indent_overrides`, which *clears*
//!     `primitives::indent_rules::USER_RULES` and then re-registers only the
//!     `[languages.<id>.indent]` blocks of its own config.
//!
//! Cargo runs the test functions of one binary in parallel threads, so any
//! harness built by any other test overwrites both. A test that pinned `es`
//! observes an English screen a few keystrokes later; a test that registered
//! an `increase_indent_pattern` finds the rule gone by the time it presses
//! Enter and reads back an unindented line. Neither failure is deterministic
//! — it depends on when some unrelated test happens to build its editor —
//! which is exactly the shape of the flakes this module exists to remove.
//! (`indent_rules`' own unit tests already keep a single owner of that
//! global for the same reason, and `Editor::for_test` carries a TODO about
//! the locale half.)
//!
//! The discipline is a reader/writer split, so the cost lands only where it
//! has to:
//!
//!   * every harness construction takes the **read** side for the length of
//!     the construction ([`guard_harness_construction`]) — uncontended in
//!     the common case, so ordinary tests keep building editors in parallel;
//!   * a test that depends on one of these globals holding still takes the
//!     **write** side for its whole body ([`pin_config_globals`]), which
//!     keeps every other construction out until it drops.
//!
//! The pin is re-entrant for its own thread: a pinned test builds harnesses
//! of its own, and those constructions skip the read side rather than
//! deadlocking against the write guard they are already inside.
//!
//! Under `cargo nextest` (one process per test) the locks are uncontended
//! and this costs nothing; under `cargo test` it is what makes the pinning
//! tests independent of their neighbours.
//!
//! What this deliberately does *not* cover: a test that built its editor
//! before the pin was taken keeps rendering while the pin is held, and for
//! the locale half that means it renders in whatever language the pin
//! selected. The pin narrows that window — such a test now blocks at its
//! *next* construction rather than resetting the locale mid-pin — but it
//! cannot close it. Closing it properly means either giving the locale
//! tests a test binary of their own (nothing else in the process would then
//! read a pinned locale) or taking the locale off a process global
//! entirely; both are larger changes than the flakes here call for. The
//! indent half has no such gap: `USER_RULES` is read by config id, and no
//! test but the pinning one has a rule registered under its id.

use std::cell::Cell;
use std::sync::{RwLock, RwLockReadGuard};

static CONFIG_GLOBALS: RwLock<()> = RwLock::new(());

thread_local! {
    /// Set while this thread holds the write side, so its own harness
    /// constructions skip the (non-reentrant) read acquisition.
    static PINNED_BY_THIS_THREAD: Cell<bool> = const { Cell::new(false) };
}

/// Pin the config-derived process globals for the rest of the test.
///
/// Take this in any test that either
///
///   * pins a non-English locale (a harness built with
///     `config.locale = LocaleName(Some(..))`, or a direct
///     `fresh::i18n::set_locale`), or
///   * relies on its own `[languages.<id>.indent]` rules still being
///     registered when it exercises indentation.
///
/// The returned guard resets the locale to `"en"` on drop — and the pin
/// itself is taken with the locale already reset — so the next test starts
/// from the default regardless of what this one selected.
pub fn pin_config_globals() -> impl Drop {
    struct Guard(#[allow(dead_code)] std::sync::RwLockWriteGuard<'static, ()>);
    impl Drop for Guard {
        fn drop(&mut self) {
            fresh::i18n::set_locale("en");
            PINNED_BY_THIS_THREAD.with(|p| p.set(false));
        }
    }
    // A poisoned lock means some other test panicked while holding the pin.
    // The globals it left behind are re-initialized by whoever takes the pin
    // next, so the poison carries no information worth failing on.
    let guard = CONFIG_GLOBALS.write().unwrap_or_else(|e| e.into_inner());
    PINNED_BY_THIS_THREAD.with(|p| p.set(true));
    fresh::i18n::set_locale("en");
    Guard(guard)
}

/// Hold the read side for the length of one harness construction.
///
/// Call this around the part of `EditorTestHarness::create` that writes the
/// config-derived globals, and let the returned guard drop as soon as the
/// editor exists. Returns `None` — i.e. takes nothing — when this thread is
/// already inside its own [`pin_config_globals`], which is what lets a
/// pinned test build harnesses without deadlocking on its own write guard.
pub fn guard_harness_construction() -> Option<RwLockReadGuard<'static, ()>> {
    if PINNED_BY_THIS_THREAD.with(|p| p.get()) {
        return None;
    }
    Some(CONFIG_GLOBALS.read().unwrap_or_else(|e| e.into_inner()))
}
