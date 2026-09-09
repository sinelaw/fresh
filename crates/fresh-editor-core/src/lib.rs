//! The data and presentation-primitives layer of the Fresh editor.
//!
//! Everything here is below the editor application: configuration, the text
//! model, the rendering primitives, the widget tree and the theme data. It
//! knows nothing about `app`, `view`, `services`, `input` or the servers —
//! the dependency edge runs one way, `fresh-editor` -> `fresh-editor-core`,
//! and that is what makes this a separate compilation unit.
//!
//! `fresh-editor` re-exports every module below under its old path, so
//! `fresh::config`, `fresh::model`, `fresh::primitives`, `fresh::widgets` and
//! `fresh::view::theme` keep resolving for downstream code and tests.

// Always available (needed for schema generation too).
pub mod config;
pub mod config_keys;
pub mod partial_config;
pub mod types;

#[cfg(feature = "runtime")]
pub mod config_io;

pub mod model;
pub mod primitives;
pub mod theme;
pub mod widgets;

// Pure leaves pulled down out of `view`/`services` so the modules above do
// not have to reach back up into `fresh-editor`.
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod color_support;
#[cfg(feature = "runtime")]
pub mod counters;
pub mod data_dir;
#[cfg(feature = "runtime")]
pub mod i18n_embedded;
#[cfg(feature = "runtime")]
pub mod language_detect;
#[cfg(feature = "runtime")]
pub mod markdown;
#[cfg(feature = "runtime")]
pub mod packages;
pub mod path_encode;
#[cfg(feature = "runtime")]
#[cfg(feature = "runtime")]
pub mod process_hidden;
#[cfg(feature = "runtime")]
pub mod process_limits;
#[cfg(feature = "runtime")]
pub mod recovery_types;
pub mod runtime_flags;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod wrap_machine;

#[cfg(feature = "runtime")]
pub mod log_dirs;
