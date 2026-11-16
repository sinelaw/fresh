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

    /// The style a filesystem reports by naming its separator.
    ///
    /// The only way a style is ever decided: the filesystem is the authority
    /// on how its own paths read, and the SSH agent runs *on the host*, so it
    /// says `os.sep` outright. Nothing here guesses a style from a path's
    /// spelling — a relative path has nothing to guess from, `:` and `\\` are
    /// legal in POSIX file names, and a rooted Windows path need not carry a
    /// drive letter.
    pub fn from_separator(sep: &str) -> Option<Self> {
        match sep {
            "/" => Some(PathStyle::Posix),
            "\\" => Some(PathStyle::Windows),
            _ => None,
        }
    }

    /// The separator to build a path with.
    pub const fn separator(self) -> char {
        match self {
            PathStyle::Posix => '/',
            PathStyle::Windows => '\\',
        }
    }

    /// A `/`-separated relative path — what [`Under::rel_to`] answers under
    /// either style — re-spelled with this style's separator.
    ///
    /// For a relative path that is *shown* to a person or saved as a path
    /// rather than consumed as a `/`-separated key: on a Windows host a tab
    /// or a recent-files entry has to read `src\main.rs`. The substitution is
    /// safe both ways, since `/` cannot appear inside a component under
    /// either style.
    pub fn respell_relative(self, rel: &str) -> String {
        match self {
            PathStyle::Posix => rel.to_string(),
            PathStyle::Windows => rel.replace('/', "\\"),
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

    /// Read this path under the rules `style` states.
    ///
    /// The style belongs to the filesystem, not to the spelling: get it from
    /// [`FileSystem::style`](super::filesystem::FileSystem::style), or name it
    /// outright where there is no filesystem (encoding a URI, a test).
    pub fn under(&self, style: PathStyle) -> Under<'_> {
        Under { path: self, style }
    }

    /// Whether this path is **fully qualified** under `style`'s rules — it
    /// names a file without reference to any current directory, so a caller
    /// holding a base does not join it onto one.
    ///
    /// Not `Path::is_absolute`, which asks the *client*: on Windows that
    /// wants a drive letter, so it calls the host path `/home/user/proj`
    /// relative and callers branch the wrong way. The *rules* are std's
    /// though, per style, and deliberately so — see below.
    ///
    /// This is a narrower question than [`HostPath::root_len`], and the two
    /// must not be merged. On Windows `\windows\system32` has a root (`\`)
    /// but is relative to the current drive, and `C:x` has a prefix but no
    /// root; `std` draws the same distinction between `has_root` and
    /// `is_absolute`. Merging them cost a round of CI: every call site asks
    /// this predicate "do I join this onto my base?", and answering "rooted"
    /// instead of "fully qualified" made a plugin's `/src/main.rs` and a
    /// code tour's step paths stop being joined onto the workspace root
    /// under Windows rules, so they resolved to files that were not there.
    pub fn is_absolute_in(&self, style: PathStyle) -> bool {
        let bytes = self.bytes();
        match style {
            PathStyle::Posix => bytes.first() == Some(&b'/'),
            PathStyle::Windows => match bytes {
                // A UNC share or a verbatim prefix, spelled with either
                // separator — `\\srv\share`, and the `//?/C:/…` that
                // `editor.pathJoin` emits.
                [a, b, ..] if style.is_separator(*a) && style.is_separator(*b) => true,
                // A drive alone (`C:x`) is relative to that drive's cwd.
                [drive, b':', sep, ..] if drive.is_ascii_alphabetic() => style.is_separator(*sep),
                _ => false,
            },
        }
    }

    /// Whether this path is rooted under *either* platform's rules.
    ///
    /// The question to ask when the filesystem a path belongs to is not known
    /// yet — a new window's root, say, before its authority exists. It is not
    /// a guess at the style: it accepts a path that is rooted on some
    /// platform and rejects one that is relative on every platform, which is
    /// all such a caller actually needs to know. Asking the *client* instead
    /// made a Windows client reject `/srv/proj` outright, since `std` wants a
    /// drive letter.
    pub fn is_rooted_anywhere(&self) -> bool {
        self.is_absolute_in(PathStyle::Posix) || self.is_absolute_in(PathStyle::Windows)
    }

    /// How many leading bytes are this path's prefix — `C:` or
    /// `\\server\share` — with no root separator. Zero under POSIX rules.
    ///
    /// Kept apart from [`HostPath::root_len`] because `C:` alone is
    /// drive-relative: `std` joins `C:` with `a` as `C:a`, not `C:\a`
    /// (`library/std/src/path.rs`, the "special case of `C:`" in `push`).
    fn prefix_len_in(&self, style: PathStyle) -> usize {
        let bytes = self.bytes();
        if style != PathStyle::Windows {
            return 0;
        }
        match bytes {
            // Either separator: a verbatim or UNC prefix is `\\?\C:\…` as
            // Windows spells it and `//?/C:/…` as a plugin's `pathJoin` does,
            // and both are the same path.
            [a, b, ..] if style.is_separator(*a) && style.is_separator(*b) => {
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
    fn root_len_in(&self, style: PathStyle) -> usize {
        let bytes = self.bytes();
        match style {
            PathStyle::Posix => usize::from(bytes.first() == Some(&b'/')),
            PathStyle::Windows => {
                let after_prefix = self.prefix_len_in(style);
                // The root is the prefix *plus* a separator. Without one the
                // path is relative to that prefix, so there is no root —
                // except for a UNC share, which is a root with nothing after
                // it: `\\srv\share` names the share's top directory, whereas
                // `C:` names whatever the current directory on C: happens to
                // be. Telling them apart is what the leading separators say.
                match bytes.get(after_prefix) {
                    Some(&b) if style.is_separator(b) => after_prefix + 1,
                    None if matches!(bytes.first(), Some(&b) if style.is_separator(b)) => {
                        after_prefix
                    }
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
    pub fn join_rel_in(&self, style: PathStyle, rel: &str) -> HostPathBuf {
        let sep = self.separator_in_use_in(style);
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
    pub fn join_name_in(&self, style: PathStyle, name: &OsStr) -> HostPathBuf {
        let mut out = self.to_host_path_buf();
        out.push_name_in(style, name);
        out
    }

    /// The path without its last component, or `None` for a root.
    ///
    /// Splits only on separators this path's style recognises, so a POSIX
    /// `/srv/a\b.txt` has parent `/srv` rather than `/srv/a`.
    pub fn parent_in(&self, style: PathStyle) -> Option<&HostPath> {
        if !self.has_components_in(style) {
            return None;
        }
        let bytes = self.bytes();
        let root = self.root_len_in(style).max(self.prefix_len_in(style));
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
    pub fn file_name_in(&self, style: PathStyle) -> Option<&OsStr> {
        let last = self.components_in(style).next_back()?;
        // `..` is a traversal, not a name — as `Path::file_name` has it.
        (last != "..").then_some(last)
    }

    /// The extension of the last component, as `Path::extension` defines it
    /// but splitting components under this path's own rules.
    pub fn extension_in(&self, style: PathStyle) -> Option<&OsStr> {
        let name = self.file_name_in(style)?.as_encoded_bytes();
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
    fn separator_in_use_in(&self, style: PathStyle) -> char {
        self.bytes()
            .iter()
            .find(|&&b| style.is_separator(b))
            .map_or(style.separator(), |&b| b as char)
    }

    /// Whether this path has any component at all below its root.
    fn has_components_in(&self, style: PathStyle) -> bool {
        self.components_in(style).next().is_some()
    }

    /// The last component without its extension, as `Path::file_stem` defines
    /// it but splitting components under this path's own rules.
    pub fn file_stem_in(&self, style: PathStyle) -> Option<&OsStr> {
        let name = self.file_name_in(style)?.as_encoded_bytes();
        match name.iter().skip(1).rposition(|&b| b == b'.') {
            Some(dot) => Some(HostPath::from_bytes(&name[..dot + 1]).as_os_str()),
            None => self.file_name_in(style),
        }
    }

    /// The components under this path's rules, roots and separators dropped.
    pub fn components_in(&self, style: PathStyle) -> impl DoubleEndedIterator<Item = &OsStr> {
        let start = self.root_len_in(style).max(self.prefix_len_in(style));
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
    pub fn rel_to_in(&self, style: PathStyle, root: &HostPath) -> Option<String> {
        if !self.starts_with_in(style, root) {
            return None;
        }
        let mut mine = self.components_in(style);
        for _ in root.components_in(style) {
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
    pub fn starts_with_in(&self, style: PathStyle, root: &HostPath) -> bool {
        if !self.roots_agree_in(style, root) {
            return false;
        }
        let mut mine = self.components_in(style);
        for theirs in root.components_in(style) {
            match mine.next() {
                Some(m) if self.same_component_in(style, m, theirs) => {}
                _ => return false,
            }
        }
        true
    }

    /// Whether two paths are rooted the same way.
    ///
    /// Two questions: are they both rooted (a rooted path is not under a
    /// relative one, or the other way round), and do they carry the same
    /// prefix. The prefix is compared rather than the root, because a root
    /// differs from a prefix only by a trailing separator and whether that
    /// separator is there is not a difference between paths:
    /// `\\srv\share\a` and the root `\\srv\share` are the same share.
    ///
    /// Not a byte comparison: on Windows `std` normalizes a drive letter's
    /// case (`windows_prefix.rs`, `drive.to_ascii_uppercase()`) and treats
    /// `/` and `\` alike, so `c:\proj` and `C:/proj` share a root.
    fn roots_agree_in(&self, style: PathStyle, other: &HostPath) -> bool {
        if (self.root_len_in(style) > 0) != (other.root_len_in(style) > 0) {
            return false;
        }
        let (a, b) = (
            &self.bytes()[..self.prefix_len_in(style)],
            &other.bytes()[..other.prefix_len_in(style)],
        );
        if a.len() != b.len() {
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
    fn same_component_in(&self, style: PathStyle, a: &OsStr, b: &OsStr) -> bool {
        match style {
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
    pub fn normalize_in(&self, style: PathStyle) -> HostPathBuf {
        // Prefix *and* root: a drive-relative `C:x` must keep its `C:`.
        let keep = self.root_len_in(style).max(self.prefix_len_in(style));
        let root = &self.bytes()[..keep];
        let mut kept: Vec<&OsStr> = Vec::new();
        for component in self.components_in(style) {
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
                    _ if self.root_len_in(style) > 0 => continue,
                    _ => {}
                }
            }
            kept.push(component);
        }

        // The style is this path's, passed explicitly: read back off the
        // partial buffer a *relative* path has no root to spell it with, and
        // would pick up the client's separator instead.
        let sep = self.separator_in_use_in(style);
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

/// A host path together with the rules for reading it.
///
/// This is the honest way to operate on a path: the style comes from the
/// filesystem the path belongs to ([`FileSystem::style`]), not from how the
/// path happens to be spelled. Get one with [`HostPath::under`], or from a
/// filesystem with `fs.at(path)`.
///
/// Both sides of a two-path operation (`rel_to`, `starts_with`) are read under
/// the *same* style, so there is no way to compare a path under one platform's
/// rules against a path under another's.
///
/// [`FileSystem::style`]: super::filesystem::FileSystem::style
#[derive(Debug, Clone, Copy)]
pub struct Under<'a> {
    path: &'a HostPath,
    style: PathStyle,
}

impl<'a> Under<'a> {
    /// The path itself, with no rules attached.
    pub fn path(self) -> &'a HostPath {
        self.path
    }

    /// The rules it is being read under.
    pub fn style(self) -> PathStyle {
        self.style
    }

    /// Whether this path is rooted under these rules.
    pub fn is_absolute(self) -> bool {
        self.path.is_absolute_in(self.style)
    }

    /// This path with `rel` — a `/`-separated relative path — appended,
    /// spelled with this path's own separator.
    pub fn join_rel(self, rel: &str) -> HostPathBuf {
        self.path.join_rel_in(self.style, rel)
    }

    /// This path with one more component, which may contain any byte a name
    /// may contain, separators included.
    pub fn join_name(self, name: &OsStr) -> HostPathBuf {
        self.path.join_name_in(self.style, name)
    }

    /// Everything above the last component, or `None` at a root.
    pub fn parent(self) -> Option<&'a HostPath> {
        self.path.parent_in(self.style)
    }

    /// The last component.
    pub fn file_name(self) -> Option<&'a OsStr> {
        self.path.file_name_in(self.style)
    }

    /// The last component's extension.
    pub fn extension(self) -> Option<&'a OsStr> {
        self.path.extension_in(self.style)
    }

    /// The last component without its extension.
    pub fn file_stem(self) -> Option<&'a OsStr> {
        self.path.file_stem_in(self.style)
    }

    /// The components below the root.
    pub fn components(self) -> impl DoubleEndedIterator<Item = &'a OsStr> {
        self.path.components_in(self.style)
    }

    /// This path relative to `root`, `/`-separated, or `None` when it does
    /// not lie under it. `root` is read under the same rules.
    pub fn rel_to(self, root: &HostPath) -> Option<String> {
        self.path.rel_to_in(self.style, root)
    }

    /// Whether this path is `root` or lies under it, both read under the same
    /// rules.
    pub fn starts_with(self, root: &HostPath) -> bool {
        self.path.starts_with_in(self.style, root)
    }

    /// `.` and `..` resolved as far as they can be without touching the
    /// filesystem, keeping the separators this path was given.
    pub fn normalize(self) -> HostPathBuf {
        self.path.normalize_in(self.style)
    }

    /// This path with a Windows verbatim prefix taken off: `\\?\C:\x` becomes
    /// `C:\x`, and `\\?\UNC\srv\share\x` becomes `\\srv\share\x`. Unchanged
    /// under POSIX rules, and for a path with no such prefix.
    ///
    /// `canonicalize` hands back the verbatim form on Windows while the paths
    /// the editor holds are spelled without it. They are the same path, so
    /// one has to be reduced to the other before the two can be compared.
    /// Either separator counts, since a plugin's `//?/C:/x` is the same
    /// prefix spelled the way `editor.pathJoin` emits it.
    pub fn strip_verbatim_prefix(self) -> HostPathBuf {
        let owned = |bytes: &[u8]| {
            HostPathBuf::from_os_string(OsString::from(HostPath::from_bytes(bytes).as_os_str()))
        };
        if self.style != PathStyle::Windows {
            return self.path.to_host_path_buf();
        }
        let bytes = self.path.bytes();
        let verbatim = bytes.len() >= 4
            && self.style.is_separator(bytes[0])
            && self.style.is_separator(bytes[1])
            && bytes[2] == b'?'
            && self.style.is_separator(bytes[3]);
        if !verbatim {
            return self.path.to_host_path_buf();
        }
        let rest = &bytes[4..];
        // A UNC share's verbatim form spells the `\\` as `UNC\`.
        if rest.len() > 4
            && rest[..3].eq_ignore_ascii_case(b"UNC")
            && self.style.is_separator(rest[3])
        {
            // The leading separators as this path spells them, then the
            // share: `\\?\UNC\srv\share` → `\\srv\share`.
            let mut out = OsString::from(HostPath::from_bytes(&bytes[..2]).as_os_str());
            out.push(HostPath::from_bytes(&rest[4..]).as_os_str());
            return HostPathBuf::from_os_string(out);
        }
        owned(rest)
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
    /// Append one component under the rules `style` states.
    ///
    /// The style is given rather than read back off what has been pushed so
    /// far: a partially built path does not always spell its own style — a
    /// relative one has no root to read — so anything building a path
    /// component by component passes the style it started from.
    pub fn push_name_in(&mut self, style: PathStyle, name: &OsStr) {
        self.push_name_with(name, style, style.separator());
    }

    /// [`HostPathBuf::push_name_in`] with the separator to insert given
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

    /// Replace the last component, keeping the rest, under the rules `style`
    /// states.
    pub fn set_file_name_in(&mut self, style: PathStyle, name: &OsStr) {
        let sep = self.as_host_path().separator_in_use_in(style);
        if let Some(parent) = self.as_host_path().parent_in(style) {
            let mut out = parent.to_host_path_buf();
            out.push_name_with(name, style, sep);
            *self = out;
        } else {
            // No parent: keep whatever root or prefix there is and put the
            // name under it, so a file at `/` gets a rooted sibling name
            // rather than a relative one.
            let keep = {
                let p = self.as_host_path();
                p.root_len_in(style).max(p.prefix_len_in(style))
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

    /// A path read under POSIX rules — what a Linux or macOS host's
    /// filesystem reports.
    ///
    /// Every test names the rules it is about. Nothing here infers them from
    /// the path's spelling, because nothing in the editor does either: the
    /// filesystem states its style and the path is read under it.
    fn posix(s: &str) -> Under<'_> {
        s.as_host().under(PathStyle::Posix)
    }

    /// A path read under Windows rules.
    fn win(s: &str) -> Under<'_> {
        s.as_host().under(PathStyle::Windows)
    }

    /// A bare path, for the second argument of `rel_to` and `starts_with`.
    /// One style governs both sides of those, and it comes from the side
    /// being read, so the root carries none of its own.
    fn h(s: &str) -> &HostPath {
        s.as_host()
    }

    // ── The style comes from the filesystem, not the path ────────────

    #[test]
    fn a_filesystem_states_its_style_by_naming_its_separator() {
        assert_eq!(PathStyle::from_separator("/"), Some(PathStyle::Posix));
        assert_eq!(PathStyle::from_separator(r"\"), Some(PathStyle::Windows));
        assert_eq!(PathStyle::from_separator(""), None);
        assert_eq!(PathStyle::from_separator(":"), None);
    }

    /// The same bytes under either set of rules, which is the case no
    /// inference can get right: `a\b.txt` is one file name on a POSIX host
    /// and two components on a Windows one, and only the filesystem knows
    /// which host it is.
    #[test]
    fn the_same_bytes_read_two_ways_give_two_answers() {
        assert_eq!(posix(r"a\b.txt").file_name(), Some(OsStr::new(r"a\b.txt")));
        assert_eq!(win(r"a\b.txt").file_name(), Some(OsStr::new("b.txt")));
    }

    // ── Joining keeps the host's separator ──────────────────────────

    /// The bug the whole type exists for: on a Windows client `Path::join`
    /// appends `\`, so the host is handed a name it does not have.
    #[test]
    fn joining_a_relative_path_keeps_the_hosts_separator() {
        assert_eq!(
            posix("/home/user/proj")
                .join_rel("src/main.rs")
                .as_host_path(),
            h("/home/user/proj/src/main.rs")
        );
        assert_eq!(
            win(r"C:\proj").join_rel("src/main.rs").as_host_path(),
            h(r"C:\proj\src\main.rs")
        );
    }

    #[test]
    fn joining_skips_empty_and_dot_components() {
        assert_eq!(
            posix("/srv").join_rel("a//./b").as_host_path(),
            h("/srv/a/b")
        );
    }

    #[test]
    fn joining_onto_a_root_does_not_double_the_separator() {
        assert_eq!(
            posix("/").join_rel("etc/hosts").as_host_path(),
            h("/etc/hosts")
        );
        assert_eq!(win(r"C:\").join_rel("w").as_host_path(), h(r"C:\w"));
    }

    #[test]
    fn joining_keeps_the_bases_spelling() {
        assert_eq!(
            win("C:/proj").join_rel("src/a").as_host_path(),
            h("C:/proj/src/a")
        );
        assert_eq!(
            win(r"C:\proj").join_rel("src/a").as_host_path(),
            h(r"C:\proj\src\a")
        );
    }

    // ── A literal backslash is an ordinary byte on a POSIX host ──────

    #[test]
    fn a_posix_name_containing_a_backslash_stays_one_component() {
        let p = posix("/srv").join_name(OsStr::new(r"a\b.txt"));
        assert_eq!(p.as_host_path(), h(r"/srv/a\b.txt"));
        assert_eq!(
            p.under(PathStyle::Posix).file_name(),
            Some(OsStr::new(r"a\b.txt")),
            "a POSIX host's backslash is part of the name, not a separator"
        );
        assert_eq!(p.under(PathStyle::Posix).parent(), Some(h("/srv")));
    }

    #[test]
    fn a_posix_backslash_name_is_not_rewritten_into_a_subdirectory() {
        // The old `replace('\\', "/")` turned this into `a/b.txt`, naming a
        // file that does not exist.
        let rel = posix(r"/srv/proj/a\b.txt").rel_to(h("/srv/proj"));
        assert_eq!(rel.as_deref(), Some(r"a\b.txt"));
    }

    #[test]
    fn a_windows_path_does_split_on_its_backslashes() {
        assert_eq!(
            win(r"C:\proj\a\b.txt").file_name(),
            Some(OsStr::new("b.txt"))
        );
        assert_eq!(win(r"C:\proj\a\b.txt").parent(), Some(h(r"C:\proj\a")));
        assert_eq!(
            win(r"C:\proj\a\b.txt").rel_to(h(r"C:\proj")).as_deref(),
            Some("a/b.txt"),
            "a relative path is `/`-separated whatever the style it came from"
        );
    }

    #[test]
    fn a_windows_path_also_splits_on_forward_slashes() {
        assert_eq!(win("C:/proj/a").file_name(), Some(OsStr::new("a")));
    }

    // ── Rooted-ness under the stated rules ───────────────────────────

    #[test]
    fn a_posix_host_path_is_absolute_even_though_windows_std_says_otherwise() {
        assert!(posix("/home/user/proj").is_absolute());
        assert!(!posix("src/main.rs").is_absolute());
    }

    #[test]
    fn a_windows_path_needs_a_rooted_drive_or_a_unc_share() {
        assert!(win(r"C:\proj").is_absolute());
        assert!(win("C:/proj").is_absolute());
        assert!(win(r"\\server\share").is_absolute());
        assert!(!win("C:proj").is_absolute(), "drive-relative, not absolute");
    }

    /// A bare UNC share is a root with nothing after it, unlike a bare drive.
    /// `\\srv\share` names the share's top directory; `C:` names whatever the
    /// current directory on that drive happens to be, so it stays relative.
    #[test]
    fn a_bare_unc_share_is_its_own_root() {
        assert!(win(r"\\srv\share").is_absolute());
        assert_eq!(win(r"\\srv\share").parent(), None);
        assert_eq!(win(r"\\srv\share").file_name(), None);
        assert!(!win("C:").is_absolute());
        // And a path under it relates to it, trailing separator or not.
        assert_eq!(
            win(r"\\srv\share\a\b").rel_to(h(r"\\srv\share")).as_deref(),
            Some("a/b")
        );
        assert_eq!(
            win(r"\\srv\share\a\b")
                .rel_to(h(r"\\srv\share\"))
                .as_deref(),
            Some("a/b")
        );
    }

    /// A Windows path rooted at `\` with no drive is read correctly — its
    /// components split, its parent climbs — but it is **not** fully
    /// qualified: which drive it is on depends on the current directory, so
    /// `is_absolute` reports false, exactly as `std` does.
    ///
    /// The distinction matters because every caller uses `is_absolute` to
    /// decide whether to join a path onto a base. Reporting `\windows\x` as
    /// absolute (because it has a root) also reported a plugin's
    /// `/src/main.rs` as absolute under Windows rules, and those stopped
    /// being joined onto the workspace root.
    #[test]
    fn a_driveless_rooted_windows_path_is_read_but_is_not_fully_qualified() {
        assert!(!win(r"\windows\system32").is_absolute());
        assert!(!win("/src/main.rs").is_absolute());
        assert_eq!(
            win(r"\windows\system32").file_name(),
            Some(OsStr::new("system32"))
        );
        assert_eq!(win(r"\windows\system32").parent(), Some(h(r"\windows")));
    }

    // ── parent / file_name edges ─────────────────────────────────────

    #[test]
    fn the_parent_of_a_root_is_nothing() {
        assert_eq!(posix("/").parent(), None);
        assert_eq!(posix("/").file_name(), None);
        assert_eq!(win(r"C:\").parent(), None);
    }

    #[test]
    fn the_parent_of_a_top_level_entry_is_the_root_itself() {
        assert_eq!(posix("/etc").parent(), Some(h("/")));
        assert_eq!(win(r"C:\w").parent(), Some(h(r"C:\")));
    }

    #[test]
    fn a_trailing_separator_is_not_a_component() {
        assert_eq!(posix("/srv/proj/").file_name(), Some(OsStr::new("proj")));
        assert_eq!(posix("/srv/proj/").parent(), Some(h("/srv")));
    }

    #[test]
    fn a_relative_path_runs_out_of_parents() {
        assert_eq!(posix("a/b").parent(), Some(h("a")));
        assert_eq!(posix("a").parent(), None);
    }

    // ── rel_to ───────────────────────────────────────────────────────

    #[test]
    fn a_path_outside_the_root_has_no_relative_form() {
        assert_eq!(posix("/other/x").rel_to(h("/srv/proj")), None);
        assert_eq!(posix("/srv").rel_to(h("/srv/proj")), None);
    }

    #[test]
    fn the_root_itself_is_the_empty_relative_path() {
        assert_eq!(
            posix("/srv/proj").rel_to(h("/srv/proj")).as_deref(),
            Some("")
        );
    }

    #[test]
    fn a_rooted_path_is_not_under_a_relative_one() {
        assert_eq!(posix("/srv/proj").rel_to(h("srv")), None);
        assert_eq!(posix("srv/proj").rel_to(h("/srv")), None);
    }

    #[test]
    fn a_sibling_whose_name_merely_starts_the_same_is_not_under_the_root() {
        // A plain `starts_with` on the string would say `/srv/project` is under
        // `/srv/proj`. Components are what decide.
        assert_eq!(posix("/srv/project/x").rel_to(h("/srv/proj")), None);
    }

    // ── normalize ────────────────────────────────────────────────────

    #[test]
    fn normalizing_resolves_dot_segments_without_re_spelling_separators() {
        // The old `normalize_path` returned `\home\user\baz` here on Windows.
        assert_eq!(
            posix("/home/./user/bar/../baz").normalize().as_host_path(),
            h("/home/user/baz")
        );
    }

    #[test]
    fn normalizing_a_windows_path_keeps_backslashes() {
        assert_eq!(
            win(r"C:\a\.\b\..\c").normalize().as_host_path(),
            h(r"C:\a\c")
        );
    }

    #[test]
    fn normalizing_cannot_climb_out_of_a_root() {
        assert_eq!(posix("/../../x").normalize().as_host_path(), h("/x"));
    }

    #[test]
    fn normalizing_keeps_a_relative_paths_leading_parent_segments() {
        assert_eq!(posix("../../x").normalize().as_host_path(), h("../../x"));
        assert_eq!(posix("a/../../x").normalize().as_host_path(), h("../x"));
    }

    #[test]
    fn normalizing_away_everything_leaves_the_current_directory() {
        assert_eq!(posix("a/..").normalize().as_host_path(), h("."));
    }

    #[test]
    fn normalizing_does_not_treat_a_posix_backslash_as_a_segment() {
        assert_eq!(
            posix(r"/srv/..\x").normalize().as_host_path(),
            h(r"/srv/..\x"),
            r"`..\x` is a file name on a POSIX host, not a parent segment"
        );
    }

    /// The same relative path under either set of rules, and no `#[cfg]` to
    /// pick one: which host it belongs to is an argument now, not a property
    /// of the machine running the test.
    #[test]
    fn normalizing_a_relative_path_keeps_its_own_separators() {
        assert_eq!(posix("../../x").normalize().as_host_path(), h("../../x"));
        assert_eq!(posix("a/../b").normalize().as_host_path(), h("b"));
        assert_eq!(
            posix(r"a\..\b").normalize().as_host_path(),
            h(r"a\..\b"),
            r"on a POSIX host `a\..\b` is one file name, with nothing to resolve"
        );
        assert_eq!(
            win(r"a\..\b").normalize().as_host_path(),
            h("b"),
            "on a Windows host the same bytes are three components"
        );
    }

    /// A verbatim or UNC prefix as Windows spells it, and as a plugin's
    /// `pathJoin` does: `editor.pathJoin` emits forward slashes, and
    /// `//?/C:/…` is the same path as `\\?\C:\…`.
    #[test]
    fn a_verbatim_prefix_is_read_with_either_separator() {
        assert_eq!(
            win(r"\\?\C:\a\..\b").normalize().as_host_path(),
            h(r"\\?\C:\b")
        );
        assert_eq!(
            win("//?/C:/a/../b").normalize().as_host_path(),
            h("//?/C:/b")
        );
        assert!(win("//?/C:/a").is_absolute());
        assert_eq!(
            win(r"\\server\share\a\b")
                .rel_to(h(r"\\server\share"))
                .as_deref(),
            Some("a/b")
        );
    }

    // ── the verbatim prefix `canonicalize` adds ─────────────────────

    #[test]
    fn a_verbatim_prefix_comes_off_so_two_spellings_can_be_compared() {
        assert_eq!(
            win(r"\\?\C:\proj\a").strip_verbatim_prefix().as_host_path(),
            h(r"C:\proj\a")
        );
        // As a plugin's `pathJoin` spells it.
        assert_eq!(
            win("//?/C:/proj/a").strip_verbatim_prefix().as_host_path(),
            h("C:/proj/a")
        );
        // A UNC share's verbatim form spells the leading `\\` as `UNC\`.
        assert_eq!(
            win(r"\\?\UNC\srv\share\a")
                .strip_verbatim_prefix()
                .as_host_path(),
            h(r"\\srv\share\a")
        );
    }

    #[test]
    fn a_path_without_a_verbatim_prefix_is_left_alone() {
        for p in [
            r"C:\proj",
            "C:/proj",
            r"\\srv\share",
            r"\windows",
            "proj\\a",
        ] {
            assert_eq!(
                win(p).strip_verbatim_prefix().as_host_path(),
                h(p),
                "{p:?} has no verbatim prefix to take off"
            );
        }
    }

    /// Under POSIX rules there is no such prefix: `\\?\x` is a file name
    /// beginning with backslashes, and shortening it would name another file.
    #[test]
    fn a_posix_path_is_never_treated_as_verbatim() {
        assert_eq!(
            posix(r"\\?\C:\proj").strip_verbatim_prefix().as_host_path(),
            h(r"\\?\C:\proj")
        );
        assert_eq!(
            posix("//?/C:/proj").strip_verbatim_prefix().as_host_path(),
            h("//?/C:/proj")
        );
    }

    // ── components ───────────────────────────────────────────────────

    #[test]
    fn components_drop_the_root_and_the_separators() {
        let got: Vec<_> = posix("/srv//proj/a")
            .components()
            .map(|c| c.to_str().unwrap().to_string())
            .collect();
        assert_eq!(got, vec!["srv", "proj", "a"]);
    }

    // ── the wire form ────────────────────────────────────────────────

    #[test]
    fn the_wire_string_is_the_hosts_own_spelling() {
        assert_eq!(
            posix("/home/user/proj")
                .join_rel("a/b.txt")
                .to_wire_string(),
            "/home/user/proj/a/b.txt"
        );
    }

    // ── set_file_name, used for temp files beside a target ───────────

    #[test]
    fn replacing_a_file_name_keeps_the_directory_and_its_separator() {
        let mut p = h("/home/user/notes.txt").to_host_path_buf();
        p.set_file_name_in(PathStyle::Posix, OsStr::new(".notes.txt.tmp"));
        assert_eq!(p.as_host_path(), h("/home/user/.notes.txt.tmp"));
    }

    // ── A name that is not UTF-8 is still a name ────────────────────

    /// Dropping it would hide the file from a walk — and for a directory,
    /// everything under it, since the walk skips the whole subtree.
    #[test]
    // `OsStrExt::from_bytes` is unix-only, and a lone 0xFF byte is not
    // valid WTF-8, so there is no Windows spelling of this case: a path
    // that is not UTF-8 is a unix phenomenon. The Windows analogue would
    // need an unpaired surrogate via `OsStringExt::from_wide`.
    #[cfg(unix)]
    fn a_name_that_is_not_utf8_is_related_lossily_not_refused() {
        use std::os::unix::ffi::OsStrExt;
        let root = HostPath::from_os_str(OsStr::from_bytes(b"/srv/proj"));
        let path = HostPath::from_os_str(OsStr::from_bytes(b"/srv/proj/b\xFFad.rs"));
        let path = path.under(PathStyle::Posix);
        assert!(path.starts_with(root), "it is plainly under the root");
        let rel = path
            .rel_to(root)
            .expect("a relative path, lossy if need be");
        assert!(rel.ends_with("ad.rs"), "got {rel:?}");
    }

    #[test]
    // As above: unix-only, because a lone 0xFF byte has no Windows spelling.
    #[cfg(unix)]
    fn a_directory_whose_name_is_not_utf8_still_relates_to_its_root() {
        use std::os::unix::ffi::OsStrExt;
        let root = HostPath::from_os_str(OsStr::from_bytes(b"/srv"));
        let dir = HostPath::from_os_str(OsStr::from_bytes(b"/srv/d\xFFir")).under(PathStyle::Posix);
        assert!(dir.starts_with(root));
        assert!(dir.rel_to(root).is_some());
    }

    // ── Windows roots compare as Windows compares them ──────────────

    #[test]
    fn a_drive_letters_case_does_not_decide_whether_a_path_is_under_a_root() {
        // std normalizes a drive's case and treats `/` and `\` alike, so
        // these are the same root and `strip_prefix` succeeds on both.
        assert_eq!(
            win(r"c:\proj\a").rel_to(h(r"C:\proj")).as_deref(),
            Some("a")
        );
        assert_eq!(win("C:/proj/a").rel_to(h(r"C:\proj")).as_deref(), Some("a"));
        assert!(win(r"C:\proj\a").starts_with(h("c:/proj")));
    }

    #[test]
    fn a_posix_root_is_compared_exactly() {
        assert!(
            !posix("/SRV/a").starts_with(h("/srv")),
            "POSIX is case-sensitive"
        );
    }

    // ── A bare drive prefix is relative to that drive ───────────────

    #[test]
    fn a_bare_drive_prefix_is_not_promoted_to_the_drive_root() {
        // `std::path`: `Path::new("C:").join("a")` is `C:a`, not `C:\a`.
        assert_eq!(win("C:").join_rel("a").as_host_path(), h("C:a"));
        assert_eq!(win("C:x").normalize().as_host_path(), h("C:x"));
        assert!(!win("C:x").is_absolute());
        assert!(win(r"C:\x").is_absolute());
    }

    // ── Degenerate roots terminate a climb ─────────────────────────

    #[test]
    fn a_root_however_spelled_has_no_parent() {
        for r in ["/", "//", "///"] {
            assert_eq!(posix(r).parent(), None, "parent of {r:?}");
            assert_eq!(posix(r).file_name(), None, "file_name of {r:?}");
        }
        assert_eq!(win("C:").parent(), None);
        assert_eq!(win(r"C:\").parent(), None);
    }

    // ── `.` and `..` are not names ──────────────────────────────────

    #[test]
    fn a_dot_segment_is_not_a_file_name() {
        assert_eq!(posix("/a/.").file_name(), Some(OsStr::new("a")));
        assert_eq!(posix("/a/..").file_name(), None);
        assert_eq!(posix("/a/..").file_stem(), None);
        assert_eq!(posix("/a/..").extension(), None);
    }

    #[test]
    fn an_interior_dot_segment_does_not_break_the_relative_path() {
        assert_eq!(
            posix("/srv/./proj/a").rel_to(h("/srv/proj")).as_deref(),
            Some("a")
        );
    }

    // ── A sibling temp name at the root stays rooted ────────────────

    #[test]
    fn replacing_the_file_name_of_a_path_at_the_root_keeps_the_root() {
        let mut p = h("/notes.txt").to_host_path_buf();
        p.set_file_name_in(PathStyle::Posix, OsStr::new(".notes.txt.tmp"));
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
