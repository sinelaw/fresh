//! Paths that belong to a [`FileSystem`](super::filesystem::FileSystem), not
//! to the client.
//!
//! `std::path::Path` applies the *running process's* rules: on Windows `\` is a
//! separator and [`std::path::MAIN_SEPARATOR`] is `\`, on Unix `\` is an
//! ordinary byte in a file name. That is correct for a local filesystem and
//! wrong for a remote one, because the editor and the host need not be the same
//! platform. A Windows client talking to a Linux host would split the host's
//! `a\b.txt` into two components, and `root.join("src/main.rs")` would hand the
//! host `"/home/user/proj\src/main.rs"` — a file it does not have.
//!
//! [`HostPath`] is the same bytes with the rules taken from the path instead of
//! from the client. Its [`PathStyle`] is read off the path's own spelling
//! ([`PathStyle::of`]), so `/home/user/proj` is POSIX and `C:\proj` is Windows
//! whichever platform is asking. Every [`FileSystem`](super::filesystem::FileSystem)
//! method takes `&HostPath`, so a `std::path::Path` cannot reach one without
//! [`AsHostPath::as_host`] saying so at the call site.
//!
//! What this type deliberately does *not* do is normalize separators. A POSIX
//! host may legitimately have a file called `a\b.txt`, and rewriting that to
//! `a/b.txt` names a different file.

use std::borrow::Borrow;
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

/// Which platform's rules a path follows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathStyle {
    /// `/` separates; every other byte, `\` included, is part of a name.
    Posix,
    /// Both `\` and `/` separate, and a path is rooted at a drive or a UNC share.
    Windows,
}

impl PathStyle {
    /// The style this process's own filesystem uses.
    pub const fn host() -> Self {
        if cfg!(windows) {
            PathStyle::Windows
        } else {
            PathStyle::Posix
        }
    }

    /// The style `bytes` is spelled in.
    ///
    /// A leading `/` is POSIX; a drive (`C:`) or a UNC/verbatim prefix (`\\`)
    /// is Windows. Nothing in a *relative* path says which platform it is for,
    /// so those fall back to [`PathStyle::host`] — callers that care join them
    /// onto an absolute base first, and the base decides.
    fn of_bytes(bytes: &[u8]) -> Self {
        match bytes {
            // `\\server\share`, `\\?\C:\…`, `\\.\device`.
            [b'\\', b'\\', ..] => PathStyle::Windows,
            // A drive letter, rooted (`C:\x`) or not (`C:x`).
            [drive, b':', ..] if drive.is_ascii_alphabetic() => PathStyle::Windows,
            [b'/', ..] => PathStyle::Posix,
            _ => PathStyle::host(),
        }
    }

    /// The style `path` is spelled in. See [`PathStyle::of_bytes`].
    pub fn of(path: &Path) -> Self {
        Self::of_bytes(path.as_os_str().as_encoded_bytes())
    }

    /// The separator to build a path with.
    pub const fn separator(self) -> char {
        match self {
            PathStyle::Posix => '/',
            PathStyle::Windows => '\\',
        }
    }

    /// Whether `b` separates components under these rules. This is the whole
    /// difference between the two styles: Windows accepts `/` as well as `\`,
    /// POSIX accepts only `/` and leaves `\` to file names.
    pub const fn is_separator(self, b: u8) -> bool {
        match self {
            PathStyle::Posix => b == b'/',
            PathStyle::Windows => b == b'/' || b == b'\\',
        }
    }
}

/// A borrowed path belonging to some filesystem, with that filesystem's rules.
///
/// The owned form is [`HostPathBuf`]. Build one from a `std::path::Path` with
/// [`AsHostPath::as_host`].
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct HostPath(OsStr);

impl HostPath {
    /// Borrow `s` as a host path.
    ///
    /// Prefer [`AsHostPath::as_host`] at call sites; this exists for the few
    /// places that hold an [`OsStr`] rather than a `Path`.
    pub fn from_os_str(s: &OsStr) -> &HostPath {
        // SAFETY: `HostPath` is a `#[repr(transparent)]` newtype around `OsStr`,
        // so the two have identical layout and this is the same cast
        // `std::path::Path::new` performs for `Path`. The reference's lifetime
        // is carried over unchanged.
        unsafe { &*(s as *const OsStr as *const HostPath) }
    }

    /// The bytes, in the platform's `OsStr` encoding.
    pub fn as_os_str(&self) -> &OsStr {
        &self.0
    }

    /// The same bytes as a `std::path::Path`.
    ///
    /// For a path whose style matches [`PathStyle::host`] this is the ordinary
    /// local path, which is why [`StdFileSystem`](super::filesystem::StdFileSystem)
    /// can hand it straight to `std::fs`. For any other style the `Path` is
    /// still the right *bytes* but its component-splitting is the client's, so
    /// use the inherent methods here instead of `Path`'s.
    pub fn as_path(&self) -> &Path {
        Path::new(&self.0)
    }

    /// The bytes, for the few callers that work below `OsStr`.
    fn bytes(&self) -> &[u8] {
        self.0.as_encoded_bytes()
    }

    /// Reconstruct a host path from a sub-slice of [`HostPath::bytes`].
    ///
    /// Only ever called with a slice whose ends fall on an ASCII separator
    /// boundary or an end of the original.
    fn from_bytes(bytes: &[u8]) -> &HostPath {
        // SAFETY: `bytes` is a sub-slice of an `OsStr`'s encoded bytes whose
        // boundaries are ASCII separators or the ends of that `OsStr`. Both
        // supported encodings (UTF-8 on unix, WTF-8 on Windows) are
        // self-synchronising at ASCII bytes: an ASCII byte is never part of a
        // multi-byte sequence, so splitting there cannot land inside one and
        // each side remains well-formed.
        Self::from_os_str(unsafe { OsStr::from_encoded_bytes_unchecked(bytes) })
    }

    /// The rules this path follows, read off its own spelling.
    pub fn style(&self) -> PathStyle {
        PathStyle::of_bytes(self.bytes())
    }

    /// Whether this path is rooted *under its own rules*.
    ///
    /// Not `Path::is_absolute`, which asks whether the path is absolute for the
    /// client: on Windows that wants a drive letter, so it calls the host path
    /// `/home/user/proj` relative and callers branch the wrong way.
    pub fn is_absolute(&self) -> bool {
        let bytes = self.bytes();
        match self.style() {
            PathStyle::Posix => bytes.first() == Some(&b'/'),
            PathStyle::Windows => match bytes {
                [b'\\', b'\\', ..] => true,
                // A drive alone (`C:x`) is relative to that drive's cwd.
                [drive, b':', sep, ..] if drive.is_ascii_alphabetic() => {
                    PathStyle::Windows.is_separator(*sep)
                }
                _ => false,
            },
        }
    }

    /// How many leading bytes are this path's root (`/`, `C:\`, `\\srv\share\`),
    /// which no operation may strip.
    fn root_len(&self) -> usize {
        let bytes = self.bytes();
        let style = self.style();
        match style {
            PathStyle::Posix => usize::from(bytes.first() == Some(&b'/')),
            PathStyle::Windows => {
                let after_prefix = match bytes {
                    // `\\?\C:\` / `\\server\share\` — keep it all; splitting a
                    // UNC prefix is not something this type needs to do.
                    [b'\\', b'\\', ..] => {
                        // Past the two leading separators, skip two more
                        // components (`?\C:` or `server\share`).
                        let mut seen = 0;
                        let mut i = 2;
                        while i < bytes.len() {
                            if style.is_separator(bytes[i]) {
                                seen += 1;
                                if seen == 2 {
                                    break;
                                }
                            }
                            i += 1;
                        }
                        i
                    }
                    [drive, b':', ..] if drive.is_ascii_alphabetic() => 2,
                    _ => 0,
                };
                // Then the root separator, when there is one.
                match bytes.get(after_prefix) {
                    Some(&b) if style.is_separator(b) => after_prefix + 1,
                    _ => after_prefix,
                }
            }
        }
    }

    /// This path with `rel` — a `/`-separated relative path, as a walk and
    /// `git ls-files` report — appended under this path's rules.
    ///
    /// This is the fix for the join that `Path::join` gets wrong: the result is
    /// spelled with *this* path's separator, not the client's, so a POSIX host
    /// path stays `/`-separated on a Windows client.
    ///
    /// An empty or `.` component is skipped; `..` is left in place for
    /// [`HostPath::normalize`] to resolve.
    pub fn join_rel(&self, rel: &str) -> HostPathBuf {
        let mut out = self.to_host_path_buf();
        for component in rel.split('/') {
            if component.is_empty() || component == "." {
                continue;
            }
            out.push_name(OsStr::new(component));
        }
        out
    }

    /// This path with one more component, taken whole.
    ///
    /// `name` is never split, so a POSIX host's `a\b.txt` stays one component
    /// rather than becoming two. Use this wherever the name came from a
    /// directory listing.
    pub fn join_name(&self, name: &OsStr) -> HostPathBuf {
        let mut out = self.to_host_path_buf();
        out.push_name(name);
        out
    }

    /// The path without its last component, or `None` for a root.
    ///
    /// Splits only on separators this path's style recognises, so a POSIX
    /// `/srv/a\b.txt` has parent `/srv` rather than `/srv/a`.
    pub fn parent(&self) -> Option<&HostPath> {
        let bytes = self.bytes();
        let root = self.root_len();
        let style = self.style();
        let body = bytes.get(root..)?;
        if body.is_empty() {
            return None;
        }
        // Ignore trailing separators, then cut at the last one.
        let end = body
            .iter()
            .rposition(|&b| !style.is_separator(b))
            .map_or(0, |i| i + 1);
        match body[..end].iter().rposition(|&b| style.is_separator(b)) {
            // Keep the root: the parent of `/a` is `/`, not ``.
            Some(cut) => Some(Self::from_bytes(&bytes[..root + cut])),
            None if root == 0 => None,
            None => Some(Self::from_bytes(&bytes[..root])),
        }
    }

    /// The last component, or `None` when there is none (a root, or empty).
    pub fn file_name(&self) -> Option<&OsStr> {
        let bytes = self.bytes();
        let root = self.root_len();
        let style = self.style();
        let body = bytes.get(root..)?;
        let end = body
            .iter()
            .rposition(|&b| !style.is_separator(b))
            .map_or(0, |i| i + 1);
        if end == 0 {
            return None;
        }
        let start = body[..end]
            .iter()
            .rposition(|&b| style.is_separator(b))
            .map_or(0, |i| i + 1);
        Some(Self::from_bytes(&body[start..end]).as_os_str())
    }

    /// The components under this path's rules, roots and separators dropped.
    pub fn components(&self) -> impl Iterator<Item = &OsStr> {
        let style = self.style();
        self.bytes()[self.root_len()..]
            .split(move |&b| style.is_separator(b))
            .filter(|c| !c.is_empty())
            .map(|c| Self::from_bytes(c).as_os_str())
    }

    /// This path relative to `root`, `/`-separated, or `None` when it is not
    /// under `root`.
    ///
    /// The `/` separators in the result are the shared spelling the rest of the
    /// editor uses for relative paths (globs, the file list, display). Because
    /// the *split* follows this path's own rules, a POSIX host's `a\b.txt`
    /// comes back as `a\b.txt` and not as `a/b.txt`, which would name a file
    /// that does not exist.
    ///
    /// `None` rather than a lossy string when either side is not UTF-8, since
    /// every consumer of a relative path handles it as a `str`.
    pub fn rel_to(&self, root: &HostPath) -> Option<String> {
        let style = self.style();
        let mut mine = self.components();
        for theirs in root.components() {
            // Compare under this path's rules; a case-insensitive filesystem is
            // a separate question this type does not try to answer.
            if mine.next()? != theirs {
                return None;
            }
        }
        let mut out = String::new();
        for component in mine {
            if !out.is_empty() {
                out.push('/');
            }
            out.push_str(component.to_str()?);
        }
        // Equal paths are "under" each other, as `strip_prefix` has it.
        let _ = style;
        Some(out)
    }

    /// Whether this path is `root` or lies under it.
    pub fn starts_with(&self, root: &HostPath) -> bool {
        self.rel_to(root).is_some()
    }

    /// `.` and `..` resolved without touching the filesystem, keeping this
    /// path's spelling.
    ///
    /// The replacement for the editor's old `normalize_path`, which rebuilt the
    /// path from `Path::components()` and so re-spelled every separator as the
    /// *client's*: on Windows that turned the host path `/home/user/x` into
    /// `\home\user\x`, which a POSIX host reads as one relative file name.
    pub fn normalize(&self) -> HostPathBuf {
        let root = &self.bytes()[..self.root_len()];
        let mut kept: Vec<&OsStr> = Vec::new();
        for component in self.components() {
            if component == "." {
                continue;
            }
            if component == ".." {
                // Past a root there is nowhere to go, so `..` is dropped; in a
                // relative path it has to be kept.
                match kept.last() {
                    Some(last) if *last != ".." => {
                        kept.pop();
                        continue;
                    }
                    _ if !root.is_empty() => continue,
                    _ => {}
                }
            }
            kept.push(component);
        }

        let mut out = HostPathBuf::from_os_string(OsString::from(
            HostPath::from_bytes(root).as_os_str(),
        ));
        for component in kept {
            out.push_name(component);
        }
        if out.as_host_path().as_os_str().is_empty() {
            out.push_name(OsStr::new("."));
        }
        out
    }

    /// The string to put on the wire to this path's filesystem.
    ///
    /// Lossy, like every other path-to-string in the editor; what matters is
    /// that by this point the separators are the *host's*, so the bytes name
    /// the file the host has.
    pub fn to_wire_string(&self) -> std::borrow::Cow<'_, str> {
        self.0.to_string_lossy()
    }

    /// An owned copy.
    pub fn to_host_path_buf(&self) -> HostPathBuf {
        HostPathBuf::from_os_string(self.0.to_os_string())
    }

    /// For display to a person. Same caveat as `Path::display`.
    pub fn display(&self) -> std::path::Display<'_> {
        self.as_path().display()
    }
}

impl ToOwned for HostPath {
    type Owned = HostPathBuf;
    fn to_owned(&self) -> HostPathBuf {
        self.to_host_path_buf()
    }
}

/// An owned path belonging to some filesystem. The borrowed form is [`HostPath`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct HostPathBuf(OsString);

impl HostPathBuf {
    /// Take ownership of `s` as a host path.
    pub fn from_os_string(s: OsString) -> Self {
        HostPathBuf(s)
    }

    /// Take ownership of a `std::path::PathBuf` as a host path.
    pub fn from_path_buf(p: PathBuf) -> Self {
        HostPathBuf(p.into_os_string())
    }

    /// Borrow as a [`HostPath`].
    pub fn as_host_path(&self) -> &HostPath {
        HostPath::from_os_str(&self.0)
    }

    /// Give up the host-path guarantee and go back to `std::path`.
    pub fn into_path_buf(self) -> PathBuf {
        PathBuf::from(self.0)
    }

    /// The owned bytes.
    pub fn into_os_string(self) -> OsString {
        self.0
    }

    /// Append one whole component, inserting this path's separator if needed.
    ///
    /// `name` is never split: the separator comes from the path's style, so a
    /// name containing the *other* platform's separator stays one component.
    pub fn push_name(&mut self, name: &OsStr) {
        let bytes = self.0.as_encoded_bytes();
        let style = self.as_host_path().style();
        let needs_sep = match bytes.last() {
            None => false,
            Some(&last) => !style.is_separator(last),
        };
        if needs_sep {
            self.0.push(OsStr::new(&style.separator().to_string()));
        }
        self.0.push(name);
    }

    /// Replace the last component, keeping the rest.
    pub fn set_file_name(&mut self, name: &OsStr) {
        if let Some(parent) = self.as_host_path().parent() {
            let mut out = parent.to_host_path_buf();
            out.push_name(name);
            *self = out;
        } else {
            self.0 = name.to_os_string();
        }
    }
}

impl Borrow<HostPath> for HostPathBuf {
    fn borrow(&self) -> &HostPath {
        self.as_host_path()
    }
}

impl std::ops::Deref for HostPathBuf {
    type Target = HostPath;
    fn deref(&self) -> &HostPath {
        self.as_host_path()
    }
}

impl AsRef<HostPath> for HostPathBuf {
    fn as_ref(&self) -> &HostPath {
        self.as_host_path()
    }
}

impl AsRef<HostPath> for HostPath {
    fn as_ref(&self) -> &HostPath {
        self
    }
}

/// Say, at a call site, that a client-side path is meant as a path on some
/// filesystem.
///
/// Every [`FileSystem`](super::filesystem::FileSystem) method takes
/// `&HostPath`, so this is the one step that turns "a path" into "a path on
/// that machine" — and the place to look when deciding whether the path really
/// does belong to the filesystem being called.
pub trait AsHostPath {
    /// Borrow as a host path, same bytes.
    fn as_host(&self) -> &HostPath;
}

impl AsHostPath for Path {
    fn as_host(&self) -> &HostPath {
        HostPath::from_os_str(self.as_os_str())
    }
}

impl AsHostPath for PathBuf {
    fn as_host(&self) -> &HostPath {
        HostPath::from_os_str(self.as_os_str())
    }
}

impl AsHostPath for OsStr {
    fn as_host(&self) -> &HostPath {
        HostPath::from_os_str(self)
    }
}

impl AsHostPath for str {
    fn as_host(&self) -> &HostPath {
        HostPath::from_os_str(OsStr::new(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn h(s: &str) -> &HostPath {
        s.as_host()
    }

    // ── Style, read off the path rather than the client ──────────────

    #[test]
    fn a_posix_root_is_posix_on_every_client() {
        assert_eq!(h("/home/user/proj").style(), PathStyle::Posix);
        assert_eq!(h("/").style(), PathStyle::Posix);
    }

    #[test]
    fn a_drive_or_unc_root_is_windows_on_every_client() {
        assert_eq!(h(r"C:\proj").style(), PathStyle::Windows);
        assert_eq!(h("C:/proj").style(), PathStyle::Windows);
        assert_eq!(h(r"\\server\share\proj").style(), PathStyle::Windows);
        assert_eq!(h(r"\\?\C:\proj").style(), PathStyle::Windows);
    }

    #[test]
    fn a_relative_path_has_nothing_to_read_so_it_follows_the_client() {
        assert_eq!(h("src/main.rs").style(), PathStyle::host());
    }

    /// The bug the whole type exists for: on a Windows client `Path::join`
    /// appends `\`, so the host is handed a name it does not have.
    #[test]
    fn joining_a_relative_path_keeps_the_hosts_separator() {
        assert_eq!(
            h("/home/user/proj").join_rel("src/main.rs").as_host_path(),
            h("/home/user/proj/src/main.rs")
        );
        assert_eq!(
            h(r"C:\proj").join_rel("src/main.rs").as_host_path(),
            h(r"C:\proj\src\main.rs")
        );
    }

    #[test]
    fn joining_skips_empty_and_dot_components() {
        assert_eq!(
            h("/srv").join_rel("a//./b").as_host_path(),
            h("/srv/a/b")
        );
    }

    #[test]
    fn joining_onto_a_root_does_not_double_the_separator() {
        assert_eq!(h("/").join_rel("etc/hosts").as_host_path(), h("/etc/hosts"));
        assert_eq!(h(r"C:\").join_rel("w").as_host_path(), h(r"C:\w"));
    }

    // ── A literal backslash is an ordinary byte on a POSIX host ──────

    #[test]
    fn a_posix_name_containing_a_backslash_stays_one_component() {
        let p = h("/srv").join_name(OsStr::new(r"a\b.txt"));
        assert_eq!(p.as_host_path(), h(r"/srv/a\b.txt"));
        assert_eq!(
            p.as_host_path().file_name(),
            Some(OsStr::new(r"a\b.txt")),
            "a POSIX host's backslash is part of the name, not a separator"
        );
        assert_eq!(p.as_host_path().parent(), Some(h("/srv")));
    }

    #[test]
    fn a_posix_backslash_name_is_not_rewritten_into_a_subdirectory() {
        // The old `replace('\\', "/")` turned this into `a/b.txt`, naming a
        // file that does not exist.
        let rel = h(r"/srv/proj/a\b.txt").rel_to(h("/srv/proj"));
        assert_eq!(rel.as_deref(), Some(r"a\b.txt"));
    }

    #[test]
    fn a_windows_path_does_split_on_its_backslashes() {
        assert_eq!(h(r"C:\proj\a\b.txt").file_name(), Some(OsStr::new("b.txt")));
        assert_eq!(h(r"C:\proj\a\b.txt").parent(), Some(h(r"C:\proj\a")));
        assert_eq!(
            h(r"C:\proj\a\b.txt").rel_to(h(r"C:\proj")).as_deref(),
            Some("a/b.txt"),
            "a relative path is `/`-separated whatever the style it came from"
        );
    }

    #[test]
    fn a_windows_path_also_splits_on_forward_slashes() {
        assert_eq!(h("C:/proj/a").file_name(), Some(OsStr::new("a")));
    }

    // ── Rooted-ness under the path's own rules ───────────────────────

    #[test]
    fn a_posix_host_path_is_absolute_even_though_windows_std_says_otherwise() {
        assert!(h("/home/user/proj").is_absolute());
        assert!(!h("src/main.rs").is_absolute());
    }

    #[test]
    fn a_windows_path_needs_a_rooted_drive_or_a_unc_share() {
        assert!(h(r"C:\proj").is_absolute());
        assert!(h("C:/proj").is_absolute());
        assert!(h(r"\\server\share").is_absolute());
        assert!(!h("C:proj").is_absolute(), "drive-relative, not absolute");
    }

    // ── parent / file_name edges ─────────────────────────────────────

    #[test]
    fn the_parent_of_a_root_is_nothing() {
        assert_eq!(h("/").parent(), None);
        assert_eq!(h("/").file_name(), None);
        assert_eq!(h(r"C:\").parent(), None);
    }

    #[test]
    fn the_parent_of_a_top_level_entry_is_the_root_itself() {
        assert_eq!(h("/etc").parent(), Some(h("/")));
        assert_eq!(h(r"C:\w").parent(), Some(h(r"C:\")));
    }

    #[test]
    fn a_trailing_separator_is_not_a_component() {
        assert_eq!(h("/srv/proj/").file_name(), Some(OsStr::new("proj")));
        assert_eq!(h("/srv/proj/").parent(), Some(h("/srv")));
    }

    #[test]
    fn a_relative_path_runs_out_of_parents() {
        assert_eq!(h("a/b").parent(), Some(h("a")));
        assert_eq!(h("a").parent(), None);
    }

    // ── rel_to ───────────────────────────────────────────────────────

    #[test]
    fn a_path_outside_the_root_has_no_relative_form() {
        assert_eq!(h("/other/x").rel_to(h("/srv/proj")), None);
        assert_eq!(h("/srv").rel_to(h("/srv/proj")), None);
    }

    #[test]
    fn the_root_itself_is_the_empty_relative_path() {
        assert_eq!(h("/srv/proj").rel_to(h("/srv/proj")).as_deref(), Some(""));
    }

    #[test]
    fn a_sibling_whose_name_merely_starts_the_same_is_not_under_the_root() {
        // A plain `starts_with` on the string would say `/srv/project` is under
        // `/srv/proj`. Components are what decide.
        assert_eq!(h("/srv/project/x").rel_to(h("/srv/proj")), None);
    }

    // ── normalize ────────────────────────────────────────────────────

    #[test]
    fn normalizing_resolves_dot_segments_without_re_spelling_separators() {
        // The old `normalize_path` returned `\home\user\baz` here on Windows.
        assert_eq!(
            h("/home/./user/bar/../baz").normalize().as_host_path(),
            h("/home/user/baz")
        );
    }

    #[test]
    fn normalizing_a_windows_path_keeps_backslashes() {
        assert_eq!(
            h(r"C:\a\.\b\..\c").normalize().as_host_path(),
            h(r"C:\a\c")
        );
    }

    #[test]
    fn normalizing_cannot_climb_out_of_a_root() {
        assert_eq!(h("/../../x").normalize().as_host_path(), h("/x"));
    }

    #[test]
    fn normalizing_keeps_a_relative_paths_leading_parent_segments() {
        assert_eq!(h("../../x").normalize().as_host_path(), h("../../x"));
        assert_eq!(h("a/../../x").normalize().as_host_path(), h("../x"));
    }

    #[test]
    fn normalizing_away_everything_leaves_the_current_directory() {
        assert_eq!(h("a/..").normalize().as_host_path(), h("."));
    }

    #[test]
    fn normalizing_does_not_treat_a_posix_backslash_as_a_segment() {
        assert_eq!(
            h(r"/srv/..\x").normalize().as_host_path(),
            h(r"/srv/..\x"),
            r"`..\x` is a file name on a POSIX host, not a parent segment"
        );
    }

    // ── components ───────────────────────────────────────────────────

    #[test]
    fn components_drop_the_root_and_the_separators() {
        let got: Vec<_> = h("/srv//proj/a")
            .components()
            .map(|c| c.to_str().unwrap().to_string())
            .collect();
        assert_eq!(got, vec!["srv", "proj", "a"]);
    }

    // ── the wire form ────────────────────────────────────────────────

    #[test]
    fn the_wire_string_is_the_hosts_own_spelling() {
        assert_eq!(
            h("/home/user/proj").join_rel("a/b.txt").to_wire_string(),
            "/home/user/proj/a/b.txt"
        );
    }

    // ── set_file_name, used for temp files beside a target ───────────

    #[test]
    fn replacing_a_file_name_keeps_the_directory_and_its_separator() {
        let mut p = h("/home/user/notes.txt").to_host_path_buf();
        p.set_file_name(OsStr::new(".notes.txt.tmp"));
        assert_eq!(p.as_host_path(), h("/home/user/.notes.txt.tmp"));
    }

    #[test]
    fn as_path_round_trips_the_bytes() {
        let p = h("/home/user/proj");
        assert_eq!(p.as_path().as_os_str(), p.as_os_str());
        assert_eq!(p.as_path().as_host(), p);
    }
}
