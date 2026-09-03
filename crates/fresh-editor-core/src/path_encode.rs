//! Encoding a workspace path into a filesystem-safe filename.
//!
//! Used by `config_io` to name per-workspace config files and by
//! `fresh-editor`'s workspace store to name session files, so it lives here
//! rather than in either caller.

use std::path::Path;

/// The longest filename [`encode_path_for_filename`] will return: `NAME_MAX`
/// itself.
///
/// **The cap is what decides which existing names get re-keyed, so it is the
/// largest one that changes nothing.** This value is not a budget for the
/// callers that append to the encoding — it deliberately is not. Folding is a
/// rename, and every place the encoding is a *key* (a workspace snapshot, the
/// recovery scope, a terminal's scrollback directory, a daemon's socket) would
/// stop finding what the previous release wrote. So the only names that may
/// fold are the ones no caller could have created at all, which means the
/// filesystem's own limit and not a byte less.
///
/// Leaving headroom for a suffix would have been wrong in both directions.
/// Several callers append nothing — `terminal_dir_for`, `working_data_dir_for`,
/// `project_state_dir` and the recovery scope each join the encoding as a whole
/// directory name — so a 250-byte encoding is live today and must not move.
/// And the longest suffix is not a small constant either: `workspace_path_for`
/// builds `<encoded>.<stable_id>.json`, and a `ws-<hex>-<hex>` id makes that
/// roughly 28 bytes. A name that overflows *with* a suffix but fits without one
/// is a caller-specific limit, and it is no better or worse here than it was
/// before this cap existed.
const MAX_ENCODED_LEN: usize = 255;

/// Marks a name [`fold_to_max_len`] shortened.
///
/// A literal `~` can carry that meaning because the encoder can never emit
/// one: `~` is not in the pass-through set, so an actual tilde in a path
/// comes out as `%7E`. Its presence therefore means exactly one thing, which
/// is what lets `decode_filename_to_path` decline a folded name instead of
/// handing back a path that was never encoded.
const FOLD_MARK: char = '~';

/// FNV-1a over the path, spelled out rather than reached for.
///
/// The hash names a file that outlives the process, so it has to give the
/// same answer next week and in the next release. `DefaultHasher` documents
/// that it does not promise that across Rust versions; twelve lines here do.
fn fold_hash(bytes: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for b in bytes {
        h ^= *b as u64;
        h = h.wrapping_mul(0x0000_0100_0000_01b3);
    }
    h
}

/// Shorten an over-long encoding to `<head>~<hash>`, keeping it unique.
///
/// **Why an encoding can outgrow a filename at all.** A path is encoded whole
/// — every separator becomes an `_` — so the name grows with the depth of the
/// directory, and the orchestrator nests one encoded path inside another: a
/// workspace lives under `orchestrator/<slug of the repo root>/<name>`, and
/// naming *its* terminal directory encodes that path a second time. Two
/// encodings of one path, plus a long workspace name, is how a real repo
/// checked out under a long root reached `File name too long (os error 36)`
/// and left the workspace with nowhere to write its terminals (#1971).
///
/// The head keeps the tail of the path readable in a directory listing; the
/// hash is what keeps two paths sharing a 238-character prefix apart. It runs
/// over the whole encoding, not the truncated head, so the digest cannot be
/// truncated with it.
///
/// **Over the encoding rather than the path, because the encoding is the
/// normalised form.** `encode_path_for_filename` maps several spellings of one
/// path onto one name: both separators become `_`, runs of them collapse, and a
/// leading one is stripped, so `/a/b`, `/a//b`, `/a/b/` and `a/b` already share
/// a filename. Digesting the raw path would have broken that for exactly the
/// long paths that fold — same head, different hash, two directories for one
/// place — and neither `terminal_dir_for` nor `working_data_dir_for`
/// canonicalises what it is given (`spawn_terminal_session_impl` passes a
/// caller-supplied `cwd` straight through), so a trailing separator would have
/// been enough to strand a workspace's scrollback beside itself.
fn fold_to_max_len(encoded: String) -> String {
    if encoded.len() <= MAX_ENCODED_LEN {
        return encoded;
    }
    // 1 mark + 16 hex digits.
    const SUFFIX_LEN: usize = 17;
    let keep = MAX_ENCODED_LEN - SUFFIX_LEN;
    // The encoder emits ASCII and nothing else — every other byte is
    // percent-escaped — so cutting on a byte is cutting on a character. It
    // can still land *inside* a `%XX` triple, which would leave a stray `%`
    // or a half-escape that decodes as text, so back off to the escape's
    // start.
    let mut head = &encoded[..keep];
    if head.ends_with('%') {
        head = &head[..head.len() - 1];
    } else if head.len() >= 2 && head.as_bytes()[head.len() - 2] == b'%' {
        head = &head[..head.len() - 2];
    }
    format!("{head}{FOLD_MARK}{:016x}", fold_hash(encoded.as_bytes()))
}

/// Encode a path into a filesystem-safe filename using percent encoding
///
/// Keeps alphanumeric chars, `-`, `.`, `_` as-is.
/// Replaces `/` with `_` for readability.
/// Percent-encodes other special characters as %XX.
///
/// Example: `/home/user/my project` -> `home_user_my%20project`
///
/// A path whose encoding would exceed [`MAX_ENCODED_LEN`] is folded to
/// `<head>~<hash>` — see [`fold_to_max_len`]. That form is deliberately not
/// reversible, and `workspace::decode_filename_to_path` declines it rather
/// than inventing a path.
pub fn encode_path_for_filename(path: &Path) -> String {
    let path_str = path.to_string_lossy();
    let mut result = String::with_capacity(path_str.len() * 2);

    for c in path_str.chars() {
        match c {
            // Path separators become underscores for readability
            '/' | '\\' => result.push('_'),
            // Safe chars pass through
            c if c.is_ascii_alphanumeric() => result.push(c),
            '-' | '.' => result.push(c),
            // Underscore needs special handling to avoid collision with /
            '_' => result.push_str("%5F"),
            // Everything else gets percent-encoded
            c => {
                for byte in c.to_string().as_bytes() {
                    result.push_str(&format!("%{:02X}", byte));
                }
            }
        }
    }

    // Remove leading underscores (from leading /)
    let result = result.trim_start_matches('_').to_string();

    // Collapse multiple underscores
    let mut final_result = String::with_capacity(result.len());
    let mut last_was_underscore = false;
    for c in result.chars() {
        if c == '_' {
            if !last_was_underscore {
                final_result.push(c);
            }
            last_was_underscore = true;
        } else {
            final_result.push(c);
            last_was_underscore = false;
        }
    }

    if final_result.is_empty() {
        final_result = "root".to_string();
    }

    fold_to_max_len(final_result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_path_that_fits_is_encoded_whole() {
        let encoded = encode_path_for_filename(Path::new("/home/user/project"));
        assert_eq!(encoded, "home_user_project");
        assert!(!encoded.contains(FOLD_MARK));
    }

    /// The orchestrator nests one encoded path inside another, so a workspace
    /// under a long root ran past `NAME_MAX` and its terminal directory could
    /// not be created at all — `File name too long (os error 36)` (#1971).
    /// The cap re-keys nothing a previous release could have written: a name
    /// the filesystem accepts is returned exactly as before.
    #[test]
    fn a_name_the_filesystem_accepts_is_never_folded() {
        // Grown a component at a time until the encoder has to fold. Length
        // is not the signal here — a folded name is under the cap by
        // construction, so waiting for one to exceed it never returns — the
        // mark is.
        let mut deep = String::from("/a");
        while !encode_path_for_filename(Path::new(&deep)).contains(FOLD_MARK) {
            deep.push_str("/bbbbbbbbbb");
        }

        // One component shorter is the longest name that fits, and it is
        // untouched — a directory of exactly NAME_MAX bytes is live today.
        let fits = deep.rsplit_once('/').unwrap().0.to_string();
        let name = encode_path_for_filename(Path::new(&fits));
        assert!(name.len() <= MAX_ENCODED_LEN);
        assert!(
            !name.contains(FOLD_MARK),
            "a name that fits must be returned whole, or it stops finding what \
             the last release wrote under it: {name}"
        );
    }

    /// The encoder maps several spellings of one path onto one name, and a
    /// folded name has to keep doing that — otherwise the head matches, the
    /// digest does not, and one directory acquires two names.
    ///
    /// Only the equivalences the encoder actually provides are asserted. A
    /// *trailing* separator is not among them: it becomes a trailing `_` that
    /// nothing strips, so `/a/b/` and `/a/b` name different files here and
    /// always have. That is worth knowing and is not folding's to fix.
    #[test]
    fn folding_keeps_the_encoders_normalisation() {
        let long = format!("/{}", "a_directory_component/".repeat(30))
            .trim_end_matches('/')
            .to_string();
        let canonical = encode_path_for_filename(Path::new(&long));
        assert!(canonical.contains(FOLD_MARK), "long enough to fold");
        for spelling in [
            long.replace('/', "//"),             // doubled separators collapse
            long.trim_start_matches('/').into(), // a leading one is stripped
            long.replace('/', "\\"),             // either separator will do
        ] {
            assert_eq!(
                encode_path_for_filename(Path::new(&spelling)),
                canonical,
                "spelling {spelling:?} names the same directory"
            );
        }
    }

    #[test]
    fn an_over_long_path_folds_under_the_cap_and_stays_unique() {
        let deep = format!("/{}/workspace", "a_very_long_directory_name/".repeat(20));
        let sibling = format!("{deep}-two");
        let a = encode_path_for_filename(Path::new(&deep));
        let b = encode_path_for_filename(Path::new(&sibling));

        assert!(a.len() <= MAX_ENCODED_LEN, "{} bytes", a.len());
        assert!(b.len() <= MAX_ENCODED_LEN, "{} bytes", b.len());
        // Long enough to have been folded, and marked as such.
        assert!(a.contains(FOLD_MARK));
        // Two paths sharing every one of the kept characters still differ,
        // because the hash runs over the path and not over the head.
        assert_ne!(a, b, "a shared prefix must not collapse two workspaces");
        assert_eq!(a, encode_path_for_filename(Path::new(&deep)), "stable");
    }

    /// The cut is taken on the encoded form, where a `%XX` escape is three
    /// characters that mean one byte. Landing inside one would leave a stray
    /// `%` that reads back as literal text.
    #[test]
    fn folding_never_cuts_a_percent_escape_in_half() {
        // Spaces encode to `%20`, so escapes land at every offset in turn.
        for pad in 0..8 {
            let path = format!("/{}{} x", "b".repeat(pad), " x".repeat(200));
            let encoded = encode_path_for_filename(Path::new(&path));
            let head = encoded.split(FOLD_MARK).next().unwrap();
            assert!(!head.ends_with('%'), "{head}");
            assert!(
                !(head.len() >= 2 && head.as_bytes()[head.len() - 2] == b'%'),
                "{head}"
            );
        }
    }
}
