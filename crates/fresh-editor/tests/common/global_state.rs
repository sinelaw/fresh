//! One owner for each process-global the integration tests share.
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
//!
//! Two more process-globals live here for the same reason, with their own
//! shapes: the `fresh` data directory ([`pin_data_dir`], a thread-local
//! redirection that needs no lock at all) and `$PATH`
//! ([`pin_path_with_dir_first`], which does need one, because the shim
//! directories under `tests/fixtures/` all provide a program called `ssh`).
//! See their docs below.

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

/// The other process-global the harness derives from: the `fresh` **data
/// directory**.
///
/// `Workspace::save`/`load`, `PersistedFileWorkspace` and the recovery store
/// resolve their files through `fresh::input::input_history::get_data_dir`,
/// which reads `$XDG_DATA_HOME` on every call — it is not part of the
/// per-test `DirectoryContext` the harness is built with. Tests that need a
/// private persistence tree (so a session-1 save is what session-2 boot
/// discovery finds) used to point that global at their temp dir with
/// `std::env::set_var("XDG_DATA_HOME", …)`, and each such test file said, in
/// its module docs, that it lived in a test binary of its own for exactly that
/// reason.
///
/// That reason expired when `tests/all_tests.rs` folded every root into one
/// binary: those `set_var`s now run as parallel threads of a single process.
/// A test that saved its workspaces and then read them back could find them
/// gone — an unrelated test had re-pointed the global in between — and a test
/// that never touched the global at all inherited whichever sandbox happened
/// to be installed, then lost it when that sandbox's `TempDir` was deleted.
///
/// [`pin_data_dir`] replaces the env mutation with a *thread-local*
/// redirection, so each test's persistence tree is private to it and no lock
/// is needed: tests keep running in parallel and none of them can move
/// another's store. The guard restores the previous value on drop, which
/// matters under `--test-threads=1` (where every test shares the main thread)
/// and for tests that pin more than once in sequence.
///
/// Hold the guard for as long as the editor it isolates is alive.
#[must_use = "the data dir reverts as soon as the guard is dropped"]
pub struct DataDirPin {
    previous: Option<std::path::PathBuf>,
}

impl Drop for DataDirPin {
    fn drop(&mut self) {
        let _ = fresh::input::input_history::set_data_dir_override(self.previous.take());
    }
}

/// Point this test's `fresh` data directory at `dir` until the guard drops.
///
/// `dir` is the `fresh` directory itself (the thing that used to be
/// `$XDG_DATA_HOME/fresh`), so `dir.join("workspaces")` is where
/// `Workspace::save` writes. Pass the same path as the `DirectoryContext`'s
/// `data_dir` and boot discovery agrees with what was saved.
pub fn pin_data_dir(dir: impl Into<std::path::PathBuf>) -> DataDirPin {
    let dir = dir.into();
    // Created eagerly: a save writes into `<dir>/workspaces`, but a *read*
    // (boot discovery) of a directory that was never created should look like
    // "no sessions", which it does either way. Creating it up front keeps the
    // pin's contract ("this directory is your data dir") true for callers that
    // plant fixtures into it before building an editor.
    std::fs::create_dir_all(&dir).expect("create pinned data dir");
    let previous = fresh::input::input_history::set_data_dir_override(Some(dir));
    DataDirPin { previous }
}

/// A `DirectoryContext` whose `data_dir` is the pinned data dir, both under
/// `base`.
///
/// This is the shape every persistence round-trip test needs: `Workspace::save`
/// resolves through [`pin_data_dir`] while boot discovery reads
/// `DirectoryContext::data_dir`, so the two must name one directory or a
/// session-1 save is not what session-2 discovery finds. Five test roots each
/// carried a private copy of this (all setting `$XDG_DATA_HOME`); it lives here
/// so there is one place that keeps the two halves in step.
///
/// Hold the returned pin for the whole test — it is what keeps the editor's
/// persistence inside `base`.
#[must_use = "the returned pin must outlive the editors it isolates"]
pub fn isolated_dir_context(
    base: &std::path::Path,
) -> (fresh::config_io::DirectoryContext, DataDirPin) {
    // `xdg-data/fresh` rather than `data/`: the layout these tests plant
    // fixtures into (`<data_dir>/orchestrator/windows.json`, `workspaces/`) is
    // unchanged from when this was `$XDG_DATA_HOME/fresh`, and keeping the
    // name keeps those paths readable next to the real ones.
    let data_dir = base.join("xdg-data").join("fresh");
    let pin = pin_data_dir(data_dir.clone());
    let dir_context = fresh::config_io::DirectoryContext {
        data_dir,
        config_dir: base.join("config"),
        home_dir: Some(base.join("home")),
        documents_dir: None,
        downloads_dir: None,
    };
    (dir_context, pin)
}

/// Serializes every test that shadows a binary on `$PATH`.
///
/// Held by [`PathPin`] for the body of such a test, so exactly one shim
/// directory is installed at a time.
fn path_env_lock() -> &'static std::sync::Mutex<()> {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    LOCK.get_or_init(|| std::sync::Mutex::new(()))
}

/// Restores `$PATH` (and any extra variables registered with
/// [`PathPin::set_env`]) when it drops, and releases [`path_env_lock`].
#[must_use = "the shim leaves $PATH as soon as the guard is dropped"]
pub struct PathPin {
    previous_path: Option<std::ffi::OsString>,
    extra: Vec<(&'static str, Option<std::ffi::OsString>)>,
    _lock: std::sync::MutexGuard<'static, ()>,
}

impl PathPin {
    /// Set an additional process-global variable for the life of the pin,
    /// restoring its previous value on drop.
    ///
    /// The fake-ssh shims are configured this way (`FAKE_SSH_SLOW_*`), and
    /// leaving those set is as damaging as leaving the shim on `$PATH`: they
    /// tell a *later* test's shim to stall on a gate file whose temp directory
    /// has already been deleted.
    pub fn set_env(&mut self, key: &'static str, value: impl AsRef<std::ffi::OsStr>) {
        self.extra.push((key, std::env::var_os(key)));
        std::env::set_var(key, value);
    }
}

impl Drop for PathPin {
    fn drop(&mut self) {
        for (key, previous) in self.extra.drain(..).rev() {
            match previous {
                Some(v) => std::env::set_var(key, v),
                None => std::env::remove_var(key),
            }
        }
        match self.previous_path.take() {
            Some(p) => std::env::set_var("PATH", p),
            None => std::env::remove_var("PATH"),
        }
    }
}

/// Put `dir` at the front of `$PATH` until the returned guard drops.
///
/// `$PATH` is process-global, and the shim directories under
/// `tests/fixtures/` deliberately collide: `fake-ssh` fails to connect,
/// `fake-ssh-hang` never completes the handshake, `fake-ssh-slow` connects and
/// then stalls — all of them provide a program called `ssh`. They used to be
/// prepended once each, permanently, under a `Once`; whichever ran last then
/// answered for *every* test in the process, including the ones that were
/// written against a different one. A test asking for the never-connecting
/// host and served the slow-but-working one waits on a screen that will never
/// appear, which is an unbounded `wait_until` — a hang, not a failure.
///
/// One lock and an exact restore make the shim mean what the test asked for:
/// only one is installed at a time, and `$PATH` goes back to what it was.
pub fn pin_path_with_dir_first(dir: &std::path::Path) -> PathPin {
    let lock = path_env_lock().lock().unwrap_or_else(|e| e.into_inner());
    let previous_path = std::env::var_os("PATH");
    let mut joined = std::ffi::OsString::from(dir);
    if let Some(existing) = &previous_path {
        joined.push(if cfg!(windows) { ";" } else { ":" });
        joined.push(existing);
    }
    std::env::set_var("PATH", joined);
    PathPin {
        previous_path,
        extra: Vec::new(),
        _lock: lock,
    }
}
