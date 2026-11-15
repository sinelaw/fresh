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
    /// Only ever called with a slice whose ends fall on an ASCII byte of the
    /// original or on one of its ends. Callers split on separators (`parent`,
    /// `file_name`, `components`, `normalize`) and on `.` (`extension`,
    /// `file_stem`); both are ASCII.
    fn from_bytes(bytes: &[u8]) -> &HostPath {
        // SAFETY: `bytes` is a sub-slice of an `OsStr`'s encoded bytes whose
        // boundaries are ASCII bytes of that `OsStr` or its ends. Both
        // supported encodings (UTF-8 on unix, WTF-8 on Windows) are
        // self-synchronising at ASCII bytes: an ASCII byte is never part of a
        // multi-byte sequence, so splitting there cannot land inside one and
        // each side remains well-formed. (std states the same contract for
        // `OsStr::as_encoded_bytes`: a split is valid immediately before or
        // after any valid non-empty UTF-8 substring.)
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

    /// How many leading bytes are this path's prefix — `C:` or
    /// `\\server\share` — with no root separator. Zero under POSIX rules.
    ///
    /// Kept apart from [`HostPath::root_len`] because `C:` alone is
    /// drive-relative: `std` joins `C:` with `a` as `C:a`, not `C:\a`
    /// (`library/std/src/path.rs`, the "special case of `C:`" in `push`).
    fn prefix_len(&self) -> usize {
        let bytes = self.bytes();
        if self.style() != PathStyle::Windows {
            return 0;
        }
        match bytes {
            [b'\\', b'\\', ..] => {
                let mut seen = 0;
                let mut i = 2;
                while i < bytes.len() {
                    if PathStyle::Windows.is_separator(bytes[i]) {
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
                let after_prefix = self.prefix_len();
                // The root is the prefix *plus* a separator. Without one the
                // path is relative to that prefix, so there is no root.
                match bytes.get(after_prefix) {
                    Some(&b) if style.is_separator(b) => after_prefix + 1,
                    _ => 0,
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
    /// [`HostPath::normalize`] to resolve. `rel` is treated as relative even
    /// when spelled with a leading `/` — unlike `Path::join`, which discards
    /// the base — because every caller holds a path that is relative to a
    /// root by construction.
    pub fn join_rel(&self, rel: &str) -> HostPathBuf {
        let style = self.style();
        let sep = self.separator_in_use();
        let mut out = self.to_host_path_buf();
        for component in rel.split('/') {
            if component.is_empty() || component == "." {
                continue;
            }
            out.push_name_with(OsStr::new(component), style, sep);
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
        if !self.has_components() {
            return None;
        }
        let bytes = self.bytes();
        let root = self.root_len().max(self.prefix_len());
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
        let last = self.components().next_back()?;
        // `..` is a traversal, not a name — as `Path::file_name` has it.
        (last != "..").then_some(last)
    }

    /// The extension of the last component, as `Path::extension` defines it
    /// but splitting components under this path's own rules.
    pub fn extension(&self) -> Option<&OsStr> {
        let name = self.file_name()?.as_encoded_bytes();
        // A leading dot is part of the name (`.gitignore` has no extension).
        let dot = name.iter().skip(1).rposition(|&b| b == b'.')? + 1;
        Some(HostPath::from_bytes(&name[dot + 1..]).as_os_str())
    }

    /// The separator this path is actually spelled with: the first one it
    /// contains, else its style's default.
    ///
    /// Used by the operations that rebuild a path, so they give back the
    /// spelling they were handed. A *relative* path has no root to read a
    /// style from, so without this a rebuild would reach for the client's
    /// separator — which is the whole confusion this type exists to remove.
    fn separator_in_use(&self) -> char {
        let style = self.style();
        self.bytes()
            .iter()
            .find(|&&b| style.is_separator(b))
            .map_or(style.separator(), |&b| b as char)
    }

    /// Whether this path has any component at all below its root.
    fn has_components(&self) -> bool {
        self.components().next().is_some()
    }

    /// The last component without its extension, as `Path::file_stem` defines
    /// it but splitting components under this path's own rules.
    pub fn file_stem(&self) -> Option<&OsStr> {
        let name = self.file_name()?.as_encoded_bytes();
        match name.iter().skip(1).rposition(|&b| b == b'.') {
            Some(dot) => Some(HostPath::from_bytes(&name[..dot + 1]).as_os_str()),
            None => self.file_name(),
        }
    }

    /// The components under this path's rules, roots and separators dropped.
    pub fn components(&self) -> impl DoubleEndedIterator<Item = &OsStr> {
        let style = self.style();
        let start = self.root_len().max(self.prefix_len());
        self.bytes()[start..]
            .split(move |&b| style.is_separator(b))
            .filter(|c| !c.is_empty() && c != b".")
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
    /// A component that is not UTF-8 is converted lossily rather than
    /// failing the whole call: a relative path is a `str` to every consumer,
    /// and dropping the entry instead would hide the file from a walk —
    /// including, for a directory, everything under it. `None` means only
    /// "not under `root`", so it is safe to treat as such.
    pub fn rel_to(&self, root: &HostPath) -> Option<String> {
        if !self.starts_with(root) {
            return None;
        }
        let mut mine = self.components();
        for _ in root.components() {
            mine.next();
        }
        let mut out = String::new();
        for component in mine {
            if !out.is_empty() {
                out.push('/');
            }
            out.push_str(&component.to_string_lossy());
        }
        // Equal paths are "under" each other, as `strip_prefix` has it.
        Some(out)
    }

    /// Whether this path is `root` or lies under it.
    ///
    /// Compares bytes, never a `String`, so a name that is not UTF-8 is
    /// answered as accurately as any other. Roots must match too: a rooted
    /// path does not lie under a relative one however their components line
    /// up.
    pub fn starts_with(&self, root: &HostPath) -> bool {
        if !self.roots_agree(root) {
            return false;
        }
        let mut mine = self.components();
        for theirs in root.components() {
            match mine.next() {
                Some(m) if self.same_component(m, theirs) => {}
                _ => return false,
            }
        }
        true
    }

    /// Whether two paths are rooted the same way.
    ///
    /// Not a byte comparison: on Windows `std` normalizes a drive letter's
    /// case (`windows_prefix.rs`, `drive.to_ascii_uppercase()`) and treats
    /// `/` and `\` alike, so `c:\proj` and `C:/proj` share a root.
    fn roots_agree(&self, other: &HostPath) -> bool {
        let (a, b) = (
            &self.bytes()[..self.root_len().max(self.prefix_len())],
            &other.bytes()[..other.root_len().max(other.prefix_len())],
        );
        if a.len() != b.len() {
            return false;
        }
        let style = self.style();
        if style != other.style() {
            return false;
        }
        a.iter().zip(b).all(|(x, y)| match style {
            PathStyle::Posix => x == y,
            PathStyle::Windows => {
                (style.is_separator(*x) && style.is_separator(*y)) || x.eq_ignore_ascii_case(y)
            }
        })
    }

    /// Whether two components name the same entry under this path's rules.
    /// Windows file names are compared case-insensitively for ASCII, as its
    /// filesystems do; POSIX names are compared exactly.
    fn same_component(&self, a: &OsStr, b: &OsStr) -> bool {
        match self.style() {
            PathStyle::Posix => a == b,
            PathStyle::Windows => {
                let (a, b) = (a.as_encoded_bytes(), b.as_encoded_bytes());
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.eq_ignore_ascii_case(y))
            }
        }
    }

    /// `.` and `..` resolved without touching the filesystem, keeping this
    /// path's spelling.
    ///
    /// The replacement for the editor's old `normalize_path`, which rebuilt the
    /// path from `Path::components()` and so re-spelled every separator as the
    /// *client's*: on Windows that turned the host path `/home/user/x` into
    /// `\home\user\x`, which a POSIX host reads as one relative file name.
    pub fn normalize(&self) -> HostPathBuf {
        // Prefix *and* root: a drive-relative `C:x` must keep its `C:`.
        let keep = self.root_len().max(self.prefix_len());
        let root = &self.bytes()[..keep];
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
                    _ if self.root_len() > 0 => continue,
                    _ => {}
                }
            }
            kept.push(component);
        }

        // The style is this path's, passed explicitly: read back off the
        // partial buffer a *relative* path has no root to spell it with, and
        // would pick up the client's separator instead.
        let style = self.style();
        let sep = self.separator_in_use();
        let mut out =
            HostPathBuf::from_os_string(OsString::from(HostPath::from_bytes(root).as_os_str()));
        for component in kept {
            out.push_name_with(component, style, sep);
        }
        if out.as_host_path().as_os_str().is_empty() {
            out.push_name_with(OsStr::new("."), style, sep);
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

    /// The path as UTF-8, or `None` when it is not UTF-8.
    pub fn to_str(&self) -> Option<&str> {
        self.0.to_str()
    }

    /// For display and for logs. [`HostPath::to_wire_string`] is the same
    /// bytes; prefer that name where the string is going to the host.
    pub fn to_string_lossy(&self) -> std::borrow::Cow<'_, str> {
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
        let style = self.as_host_path().style();
        self.push_name_styled(name, style);
    }

    /// [`HostPathBuf::push_name`] with the style given rather than read back
    /// off what has been pushed so far. A partially built path does not always
    /// spell its own style — a relative one has no root to read — so anything
    /// building a path component by component passes the style it started
    /// from.
    pub(crate) fn push_name_styled(&mut self, name: &OsStr, style: PathStyle) {
        self.push_name_with(name, style, style.separator());
    }

    /// [`HostPathBuf::push_name_styled`] with the separator to insert given
    /// too, so a rebuild keeps the spelling of the path it came from.
    pub(crate) fn push_name_with(&mut self, name: &OsStr, style: PathStyle, sep: char) {
        let bytes = self.0.as_encoded_bytes();
        let needs_sep = match bytes.last() {
            None => false,
            Some(&last) if style.is_separator(last) => false,
            // `C:` + `a` is `C:a`, not `C:\a`: the path is relative to that
            // drive's working directory. `std::path::PathBuf::push` carries
            // the same special case.
            _ => {
                !(style == PathStyle::Windows
                    && bytes.len() == 2
                    && bytes[1] == b':'
                    && bytes[0].is_ascii_alphabetic())
            }
        };
        if needs_sep {
            self.0.push(OsStr::new(&sep.to_string()));
        }
        self.0.push(name);
    }

    /// Replace the last component, keeping the rest.
    pub fn set_file_name(&mut self, name: &OsStr) {
        let style = self.as_host_path().style();
        let sep = self.as_host_path().separator_in_use();
        if let Some(parent) = self.as_host_path().parent() {
            let mut out = parent.to_host_path_buf();
            out.push_name_with(name, style, sep);
            *self = out;
        } else {
            // No parent: keep whatever root or prefix there is and put the
            // name under it, so a file at `/` gets a rooted sibling name
            // rather than a relative one.
            let keep = {
                let p = self.as_host_path();
                p.root_len().max(p.prefix_len())
            };
            let root = self.0.as_encoded_bytes()[..keep].to_vec();
            let mut out = HostPathBuf::from_os_string(OsString::from(
                HostPath::from_bytes(&root).as_os_str(),
            ));
            out.push_name_with(name, style, sep);
            *self = out;
        }
    }
}

impl From<HostPathBuf> for PathBuf {
    fn from(p: HostPathBuf) -> PathBuf {
        p.into_path_buf()
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
        assert_eq!(h("/srv").join_rel("a//./b").as_host_path(), h("/srv/a/b"));
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
    fn a_rooted_path_is_not_under_a_relative_one() {
        assert_eq!(h("/srv/proj").rel_to(h("srv")), None);
        assert_eq!(h("srv/proj").rel_to(h("/srv")), None);
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
        assert_eq!(h(r"C:\a\.\b\..\c").normalize().as_host_path(), h(r"C:\a\c"));
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

    // ── A name that is not UTF-8 is still a name ────────────────────

    /// Dropping it would hide the file from a walk — and for a directory,
    /// everything under it, since the walk skips the whole subtree.
    #[test]
    fn a_name_that_is_not_utf8_is_related_lossily_not_refused() {
        use std::os::unix::ffi::OsStrExt;
        let root = HostPath::from_os_str(OsStr::from_bytes(b"/srv/proj"));
        let path = HostPath::from_os_str(OsStr::from_bytes(b"/srv/proj/b\xFFad.rs"));
        assert!(path.starts_with(root), "it is plainly under the root");
        let rel = path
            .rel_to(root)
            .expect("a relative path, lossy if need be");
        assert!(rel.ends_with("ad.rs"), "got {rel:?}");
    }

    #[test]
    fn a_directory_whose_name_is_not_utf8_still_relates_to_its_root() {
        use std::os::unix::ffi::OsStrExt;
        let root = HostPath::from_os_str(OsStr::from_bytes(b"/srv"));
        let dir = HostPath::from_os_str(OsStr::from_bytes(b"/srv/d\xFFir"));
        assert!(dir.starts_with(root));
        assert!(dir.rel_to(root).is_some());
    }

    // ── Windows roots compare as Windows compares them ──────────────

    #[test]
    fn a_drive_letters_case_does_not_decide_whether_a_path_is_under_a_root() {
        // std normalizes a drive's case and treats `/` and `\` alike, so
        // these are the same root and `strip_prefix` succeeds on both.
        assert_eq!(h(r"c:\proj\a").rel_to(h(r"C:\proj")).as_deref(), Some("a"));
        assert_eq!(h("C:/proj/a").rel_to(h(r"C:\proj")).as_deref(), Some("a"));
        assert!(h(r"C:\proj\a").starts_with(h("c:/proj")));
    }

    #[test]
    fn a_posix_root_is_compared_exactly() {
        assert!(
            !h("/SRV/a").starts_with(h("/srv")),
            "POSIX is case-sensitive"
        );
    }

    // ── A rebuild keeps the spelling it was given ───────────────────

    /// The separator must come from the path, never from the client: a
    /// relative path has no root to read a style from, and re-spelling it
    /// with the client's separator is what this type exists to prevent.
    #[test]
    fn normalizing_a_relative_path_keeps_its_own_separators() {
        assert_eq!(h("../../x").normalize().as_host_path(), h("../../x"));
        assert_eq!(h("a/../b").normalize().as_host_path(), h("b"));
        assert_eq!(
            h(r"a\..\b").normalize().as_host_path(),
            h(r"a\..\b"),
            "off Windows a backslash is a name, so there is nothing to resolve"
        );
    }

    #[test]
    fn joining_keeps_the_bases_spelling() {
        assert_eq!(
            h("C:/proj").join_rel("src/a").as_host_path(),
            h("C:/proj/src/a")
        );
        assert_eq!(
            h(r"C:\proj").join_rel("src/a").as_host_path(),
            h(r"C:\proj\src\a")
        );
    }

    // ── A bare drive prefix is relative to that drive ───────────────

    #[test]
    fn a_bare_drive_prefix_is_not_promoted_to_the_drive_root() {
        // `std::path`: `Path::new("C:").join("a")` is `C:a`, not `C:\a`.
        assert_eq!(h("C:").join_rel("a").as_host_path(), h("C:a"));
        assert_eq!(h("C:x").normalize().as_host_path(), h("C:x"));
        assert!(!h("C:x").is_absolute());
        assert!(h(r"C:\x").is_absolute());
    }

    // ── Degenerate roots terminate a climb ─────────────────────────

    #[test]
    fn a_root_however_spelled_has_no_parent() {
        for r in ["/", "//", "///"] {
            assert_eq!(h(r).parent(), None, "parent of {r:?}");
            assert_eq!(h(r).file_name(), None, "file_name of {r:?}");
        }
        assert_eq!(h("C:").parent(), None);
        assert_eq!(h(r"C:\").parent(), None);
    }

    // ── `.` and `..` are not names ──────────────────────────────────

    #[test]
    fn a_dot_segment_is_not_a_file_name() {
        assert_eq!(h("/a/.").file_name(), Some(OsStr::new("a")));
        assert_eq!(h("/a/..").file_name(), None);
        assert_eq!(h("/a/..").file_stem(), None);
        assert_eq!(h("/a/..").extension(), None);
    }

    #[test]
    fn an_interior_dot_segment_does_not_break_the_relative_path() {
        assert_eq!(
            h("/srv/./proj/a").rel_to(h("/srv/proj")).as_deref(),
            Some("a")
        );
    }

    // ── A sibling temp name at the root stays rooted ────────────────

    #[test]
    fn replacing_the_file_name_of_a_path_at_the_root_keeps_the_root() {
        let mut p = h("/notes.txt").to_host_path_buf();
        p.set_file_name(OsStr::new(".notes.txt.tmp"));
        assert_eq!(
            p.as_host_path(),
            h("/.notes.txt.tmp"),
            "a relative temp name would be written to the process's cwd"
        );
    }

    #[test]
    fn as_path_round_trips_the_bytes() {
        let p = h("/home/user/proj");
        assert_eq!(p.as_path().as_os_str(), p.as_os_str());
        assert_eq!(p.as_path().as_host(), p);
    }
}
