//! Lightweight file path ↔ `file://` URI conversion.
//!
//! Replaces the `url` crate's `Url::from_file_path` and `Url::to_file_path`
//! with a minimal implementation that produces RFC 3986–compliant URIs
//! (compatible with `lsp_types::Uri`).

use std::path::{Path, PathBuf};

/// Characters allowed unencoded in an RFC 3986 path segment (pchar minus `/`).
///
/// pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
fn is_path_char(b: u8) -> bool {
    matches!(b,
        b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' |
        b'-' | b'.' | b'_' | b'~' |                        // unreserved
        b'!' | b'$' | b'&' | b'\'' | b'(' | b')' |         // sub-delims
        b'*' | b'+' | b',' | b';' | b'=' |                  // sub-delims cont.
        b':' | b'@'                                          // pchar extras
    )
}

/// Convert a filesystem path to a `file://` URI string (RFC 3986).
///
/// Returns `None` for relative paths.
pub fn path_to_file_uri(path: &Path) -> Option<String> {
    if !path.is_absolute() {
        return None;
    }

    let mut uri = String::from("file://");

    #[cfg(windows)]
    {
        use std::path::Component;
        use std::path::Prefix;

        // Peek at the prefix to decide the URI structure.
        let prefix_kind = path.components().next().and_then(|c| match c {
            Component::Prefix(p) => Some(p.kind()),
            _ => None,
        });

        match prefix_kind {
            // C:\ or \\?\C:\ → file:///C:/...
            Some(Prefix::Disk(drive)) | Some(Prefix::VerbatimDisk(drive)) => {
                uri.push('/');
                uri.push(drive as char);
                uri.push(':');
            }
            // \\server\share or \\?\UNC\server\share → file://server/share/...
            Some(Prefix::UNC(server, share)) | Some(Prefix::VerbatimUNC(server, share)) => {
                let server = server.to_str()?;
                let share = share.to_str()?;
                uri.push_str(server);
                uri.push('/');
                percent_encode_segment(&mut uri, share);
            }
            // \\?\<something> (non-disk, non-UNC) or \\.\device — not representable
            Some(Prefix::Verbatim(_)) | Some(Prefix::DeviceNS(_)) | None => {
                return None;
            }
        }

        let mut first = true;
        for component in path.components() {
            match component {
                // Already handled above
                Component::Prefix(_) => {}
                Component::RootDir => {
                    if !uri.ends_with('/') {
                        uri.push('/');
                    }
                }
                Component::Normal(seg) => {
                    if !first && !uri.ends_with('/') {
                        uri.push('/');
                    }
                    let s = seg.to_str()?;
                    percent_encode_segment(&mut uri, s);
                }
                Component::CurDir => {
                    if !first && !uri.ends_with('/') {
                        uri.push('/');
                    }
                    uri.push('.');
                }
                Component::ParentDir => {
                    if !first && !uri.ends_with('/') {
                        uri.push('/');
                    }
                    uri.push_str("..");
                }
            }
            first = false;
        }
    }

    #[cfg(not(windows))]
    {
        use std::path::Component;
        for component in path.components() {
            match component {
                Component::RootDir => uri.push('/'),
                Component::Normal(seg) => {
                    let s = seg.to_str()?;
                    percent_encode_segment(&mut uri, s);
                    uri.push('/');
                }
                Component::CurDir => {
                    uri.push_str("./");
                }
                Component::ParentDir => {
                    uri.push_str("../");
                }
                _ => {}
            }
        }
        // Remove trailing slash (unless path is just "/")
        if uri.len() > "file:///".len() && uri.ends_with('/') {
            uri.pop();
        }
    }

    Some(uri)
}

/// Convert a `file://` URI string to a filesystem path.
///
/// Returns `None` if the URI is not a valid `file://` URI.
pub fn file_uri_to_path(uri: &str) -> Option<PathBuf> {
    // Must start with file:// (accept file:/// and file://localhost/)
    let path_str = if let Some(rest) = uri.strip_prefix("file:///") {
        rest
    } else if let Some(rest) = uri.strip_prefix("file://localhost/") {
        rest
    } else {
        return None;
    };

    let decoded = percent_decode(path_str);

    #[cfg(windows)]
    {
        // On Windows, require a drive letter (e.g. "C:/..."). URIs without one
        // (like file:///home/user/...) are not valid Windows file paths.
        let bytes = decoded.as_bytes();
        if bytes.len() < 2 || !bytes[0].is_ascii_alphabetic() || bytes[1] != b':' {
            return None;
        }
        // Normalize forward slashes to backslashes for canonical Windows paths.
        Some(PathBuf::from(decoded.replace('/', "\\")))
    }

    #[cfg(not(windows))]
    {
        // On Unix, prepend the leading /
        Some(PathBuf::from(format!("/{decoded}")))
    }
}

/// Convert a file path to an `lsp_types::Uri`.
pub fn path_to_lsp_uri(path: &Path) -> Option<lsp_types::Uri> {
    let uri_string = path_to_file_uri(path)?;
    uri_string.parse::<lsp_types::Uri>().ok()
}

/// Convert an `lsp_types::Uri` to a file path.
pub fn lsp_uri_to_path(uri: &lsp_types::Uri) -> Option<PathBuf> {
    file_uri_to_path(uri.as_str())
}

fn percent_encode_segment(out: &mut String, segment: &str) {
    for &b in segment.as_bytes() {
        if is_path_char(b) {
            out.push(b as char);
        } else {
            write_percent_encoded(out, b);
        }
    }
}

fn write_percent_encoded(out: &mut String, byte: u8) {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";
    out.push('%');
    out.push(HEX[(byte >> 4) as usize] as char);
    out.push(HEX[(byte & 0xF) as usize] as char);
}

fn percent_decode(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let (Some(hi), Some(lo)) = (from_hex(bytes[i + 1]), from_hex(bytes[i + 2])) {
                out.push((hi << 4) | lo);
                i += 3;
                continue;
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8(out).unwrap_or_else(|e| String::from_utf8_lossy(e.as_bytes()).into_owned())
}

fn from_hex(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'A'..=b'F' => Some(b - b'A' + 10),
        b'a'..=b'f' => Some(b - b'a' + 10),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn abs_path(suffix: &str) -> PathBuf {
        std::env::temp_dir().join(suffix)
    }

    // ── Basic functionality ──────────────────────────────────────

    #[test]
    fn roundtrip_simple() {
        let path = abs_path("project/main.go");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(uri.starts_with("file:///"));
        assert!(uri.ends_with("project/main.go"));
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn root_path() {
        // On Windows, "/" is not an absolute path (no drive letter), so both
        // path_to_file_uri and file_uri_to_path return None — matching url::Url behavior.
        #[cfg(not(windows))]
        {
            let uri = path_to_file_uri(Path::new("/")).unwrap();
            assert_eq!(uri, "file:///");
            assert_eq!(file_uri_to_path(&uri).unwrap(), PathBuf::from("/"));
        }
        #[cfg(windows)]
        {
            assert!(path_to_file_uri(Path::new("/")).is_none());
            assert!(file_uri_to_path("file:///").is_none());
        }
    }

    #[test]
    fn relative_path_returns_none() {
        assert!(path_to_file_uri(Path::new("main.go")).is_none());
        assert!(path_to_file_uri(Path::new("../foo")).is_none());
        assert!(path_to_file_uri(Path::new("")).is_none());
    }

    #[test]
    fn deeply_nested_path() {
        let path = abs_path("a/b/c/d/e/f/g/h/file.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn single_file_at_root() {
        #[cfg(not(windows))]
        {
            let path = PathBuf::from("/file.txt");
            let uri = path_to_file_uri(&path).unwrap();
            assert_eq!(uri, "file:///file.txt");
            assert_eq!(file_uri_to_path(&uri).unwrap(), path);
        }
        #[cfg(windows)]
        {
            // "/file.txt" has no drive letter, not absolute on Windows
            assert!(path_to_file_uri(Path::new("/file.txt")).is_none());
        }
    }

    // ── Percent-encoding edge cases ─────────────────────────────

    #[test]
    fn spaces() {
        let path = abs_path("My Projects/src/main.go");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(uri.contains("My%20Projects"));
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn brackets() {
        let path = abs_path("MY_PROJECTS [temp]/gogame/main.go");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(
            uri.contains("%5Btemp%5D"),
            "Brackets should be encoded: {uri}"
        );
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn rfc3986_special_chars() {
        let path = abs_path("a[b]c{d}e^g`h/file.rs");
        let uri = path_to_file_uri(&path).unwrap();
        for ch in ['[', ']', '{', '}', '^', '`'] {
            assert!(!uri.contains(ch), "{ch} should be encoded in {uri}");
        }
    }

    #[test]
    fn hash_in_path() {
        let path = abs_path("C# Projects/main.cs");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(
            uri.contains("C%23%20Projects"),
            "# and space should be encoded: {uri}"
        );
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn percent_in_path() {
        let path = abs_path("100%done/file.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(uri.contains("100%25done"), "% should be encoded: {uri}");
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn question_mark_in_path() {
        let path = abs_path("what?/file.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(!uri.contains('?'), "? should be encoded: {uri}");
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn ampersand_and_equals_preserved() {
        // & and = are sub-delimiters, allowed unencoded in RFC 3986 paths
        let path = abs_path("a&b=c/file.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(
            uri.contains("a&b=c"),
            "sub-delimiters should be preserved: {uri}"
        );
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn tilde_preserved() {
        let path = abs_path("~user/file.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(uri.contains("~user"), "tilde is unreserved: {uri}");
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn all_ascii_control_chars_encoded() {
        // Tab and other control chars must be percent-encoded
        let path = abs_path("has\ttab/file.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert!(uri.contains("%09"), "tab should be encoded: {uri}");
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    // ── Unicode ─────────────────────────────────────────────────

    #[test]
    fn unicode_cjk() {
        let path = abs_path("项目/源码/主程序.rs");
        let uri = path_to_file_uri(&path).unwrap();
        // CJK characters are multi-byte, each byte gets %XX encoded
        assert!(!uri.contains('项'), "CJK should be percent-encoded");
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn unicode_emoji() {
        let path = abs_path("my-project-🚀/src/main.rs");
        let uri = path_to_file_uri(&path).unwrap();
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn unicode_accented_latin() {
        let path = abs_path("café/résumé.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn unicode_arabic() {
        let path = abs_path("مشروع/ملف.rs");
        let uri = path_to_file_uri(&path).unwrap();
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    #[test]
    fn unicode_mixed_scripts() {
        let path = abs_path("日本語/한국어/Ελληνικά/file.txt");
        let uri = path_to_file_uri(&path).unwrap();
        assert_eq!(file_uri_to_path(&uri).unwrap(), path);
    }

    // ── lsp_types::Uri compatibility ────────────────────────────

    #[test]
    fn lsp_uri_roundtrip_special_chars() {
        let path = abs_path("a[b]c{d}e^g`h/file.rs");
        let uri = path_to_lsp_uri(&path).expect("should produce valid lsp_types::Uri");
        let back = lsp_uri_to_path(&uri).unwrap();
        assert_eq!(back, path);
    }

    #[test]
    fn lsp_uri_roundtrip_unicode() {
        let path = abs_path("プロジェクト/ソース.rs");
        let uri = path_to_lsp_uri(&path).expect("should produce valid lsp_types::Uri");
        let back = lsp_uri_to_path(&uri).unwrap();
        assert_eq!(back, path);
    }

    #[test]
    fn lsp_uri_roundtrip_spaces_and_hash() {
        let path = abs_path("My C# Project/src/main.cs");
        let uri = path_to_lsp_uri(&path).expect("should produce valid lsp_types::Uri");
        let back = lsp_uri_to_path(&uri).unwrap();
        assert_eq!(back, path);
    }

    // ── file_uri_to_path edge cases ─────────────────────────────

    #[test]
    fn rejects_non_file_scheme() {
        assert!(file_uri_to_path("http://example.com/foo").is_none());
        assert!(file_uri_to_path("https://example.com/foo").is_none());
        assert!(file_uri_to_path("ftp://example.com/foo").is_none());
    }

    #[test]
    fn rejects_malformed_uri() {
        assert!(file_uri_to_path("file:/missing-slash").is_none());
        assert!(file_uri_to_path("not-a-uri").is_none());
        assert!(file_uri_to_path("").is_none());
    }

    #[test]
    fn accepts_localhost() {
        // On Windows, URIs without drive letters are rejected (matches url::Url behavior)
        #[cfg(not(windows))]
        {
            let path = file_uri_to_path("file://localhost/home/user/file.txt").unwrap();
            assert_eq!(path, PathBuf::from("/home/user/file.txt"));
        }
        #[cfg(windows)]
        assert!(file_uri_to_path("file://localhost/home/user/file.txt").is_none());
    }

    #[test]
    fn decodes_mixed_case_percent() {
        // %2f is lowercase hex for '/', but it appears within a segment (not a separator)
        #[cfg(not(windows))]
        {
            let path = file_uri_to_path("file:///home/user/my%20file%2Ftxt").unwrap();
            assert_eq!(path, PathBuf::from("/home/user/my file/txt"));
        }
        #[cfg(windows)]
        assert!(file_uri_to_path("file:///home/user/my%20file%2Ftxt").is_none());
    }

    #[test]
    fn incomplete_percent_sequence_preserved() {
        // Malformed %X or trailing % should be passed through.
        // The trailing forms (`%` at len-1 and `%X` at len-2) exercise the
        // `i + 2 < bytes.len()` guard in percent_decode — without it, the
        // function would read past the end of the input and panic.
        // percent_decode runs on all platforms, so these cases are effective
        // regardless of target.
        #[cfg(not(windows))]
        {
            let path = file_uri_to_path("file:///home/100%/file.txt").unwrap();
            assert_eq!(path, PathBuf::from("/home/100%/file.txt"));

            // `%` is the final byte: no trailing nibbles at all.
            let path = file_uri_to_path("file:///a%").unwrap();
            assert_eq!(path, PathBuf::from("/a%"));

            // `%X` is the final two bytes: exactly one trailing nibble, which
            // is the off-by-one that the `i + 2 < len` check guards against.
            let path = file_uri_to_path("file:///a%X").unwrap();
            assert_eq!(path, PathBuf::from("/a%X"));

            // `%XY` where Y is a non-hex char: both trailing bytes present but
            // not a valid escape, so the `%` is preserved literally.
            let path = file_uri_to_path("file:///a%XY").unwrap();
            assert_eq!(path, PathBuf::from("/a%XY"));
        }
        #[cfg(windows)]
        {
            // On Windows the drive-letter check rejects these URIs, but
            // percent_decode still runs first — so the boundary cases above
            // (`a%` and especially `a%X`) are what exercise its guard here.
            assert!(file_uri_to_path("file:///home/100%/file.txt").is_none());
            assert!(file_uri_to_path("file:///a%").is_none());
            assert!(file_uri_to_path("file:///a%X").is_none());
        }
    }

    // ── Windows-style URI decoding (testable on all platforms) ─

    #[test]
    fn decode_windows_drive_letter_uri() {
        let path = file_uri_to_path("file:///C:/Users/admin/project/main.rs").unwrap();
        #[cfg(not(windows))]
        assert_eq!(path, PathBuf::from("/C:/Users/admin/project/main.rs"));
        #[cfg(windows)]
        assert_eq!(path, PathBuf::from(r"C:\Users\admin\project\main.rs"));
    }

    #[test]
    fn decode_windows_encoded_colon() {
        // Some tools encode the colon in drive letters
        let path = file_uri_to_path("file:///C%3A/Users/admin/file.cs").unwrap();
        #[cfg(not(windows))]
        assert_eq!(path, PathBuf::from("/C:/Users/admin/file.cs"));
        #[cfg(windows)]
        assert_eq!(path, PathBuf::from(r"C:\Users\admin\file.cs"));
    }

    #[test]
    fn decode_windows_spaces_in_path() {
        let path = file_uri_to_path("file:///C:/Program%20Files/My%20App/run.exe").unwrap();
        #[cfg(not(windows))]
        assert_eq!(path, PathBuf::from("/C:/Program Files/My App/run.exe"));
        #[cfg(windows)]
        assert_eq!(path, PathBuf::from(r"C:\Program Files\My App\run.exe"));
    }

    #[test]
    fn decode_windows_unicode_in_path() {
        let path = file_uri_to_path(
            "file:///C:/%E3%83%97%E3%83%AD%E3%82%B8%E3%82%A7%E3%82%AF%E3%83%88/%E3%82%BD%E3%83%BC%E3%82%B9.rs"
        ).unwrap();
        #[cfg(not(windows))]
        assert_eq!(path, PathBuf::from("/C:/プロジェクト/ソース.rs"));
        #[cfg(windows)]
        assert_eq!(path, PathBuf::from("C:\\プロジェクト\\ソース.rs"));
    }

    #[test]
    fn decode_windows_brackets_and_special() {
        let path = file_uri_to_path("file:///D:/MY_PROJECTS%20%5Btemp%5D/src/main.go").unwrap();
        #[cfg(not(windows))]
        assert_eq!(path, PathBuf::from("/D:/MY_PROJECTS [temp]/src/main.go"));
        #[cfg(windows)]
        assert_eq!(path, PathBuf::from(r"D:\MY_PROJECTS [temp]\src\main.go"));
    }

    #[test]
    fn decode_windows_localhost_variant() {
        let path = file_uri_to_path("file://localhost/C:/Users/test/file.txt").unwrap();
        #[cfg(not(windows))]
        assert_eq!(path, PathBuf::from("/C:/Users/test/file.txt"));
        #[cfg(windows)]
        assert_eq!(path, PathBuf::from(r"C:\Users\test\file.txt"));
    }

    #[test]
    fn decode_windows_deep_path() {
        let path = file_uri_to_path(
            "file:///C:/Users/admin/Documents/Visual%20Studio%202022/Projects/MyApp/src/lib.rs",
        )
        .unwrap();
        #[cfg(not(windows))]
        assert_eq!(
            path,
            PathBuf::from("/C:/Users/admin/Documents/Visual Studio 2022/Projects/MyApp/src/lib.rs")
        );
        #[cfg(windows)]
        assert_eq!(
            path,
            PathBuf::from(r"C:\Users\admin\Documents\Visual Studio 2022\Projects\MyApp\src\lib.rs")
        );
    }

    #[test]
    fn dotdot_preserved_in_uri() {
        // `..` should be preserved in the URI, matching the `url` crate.
        #[cfg(not(windows))]
        {
            let uri = path_to_file_uri(Path::new("/tmp/../file.txt")).unwrap();
            assert_eq!(uri, "file:///tmp/../file.txt");
            let back = file_uri_to_path(&uri).unwrap();
            assert_eq!(back, PathBuf::from("/tmp/../file.txt"));
        }
        #[cfg(windows)]
        {
            let uri = path_to_file_uri(Path::new(r"C:\tmp\..\file.txt")).unwrap();
            assert_eq!(uri, "file:///C:/tmp/../file.txt");
            let back = file_uri_to_path(&uri).unwrap();
            assert_eq!(back, PathBuf::from(r"C:\tmp\..\file.txt"));
        }
    }

    // ── Windows verbatim (\\?\) path handling ─────────────────

    #[cfg(windows)]
    #[test]
    fn verbatim_disk_path_produces_valid_uri() {
        // fs::canonicalize() on Windows returns \\?\C:\... paths.
        // These must produce the same URI as plain C:\... paths.
        let verbatim = PathBuf::from(r"\\?\C:\Users\vboxuser\fresh\index.html");
        let plain = PathBuf::from(r"C:\Users\vboxuser\fresh\index.html");

        let verbatim_uri = path_to_file_uri(&verbatim).expect("verbatim path should produce URI");
        let plain_uri = path_to_file_uri(&plain).expect("plain path should produce URI");

        assert_eq!(
            verbatim_uri, plain_uri,
            "verbatim and plain paths must produce identical URIs"
        );
        assert_eq!(verbatim_uri, "file:///C:/Users/vboxuser/fresh/index.html");
    }

    #[cfg(windows)]
    #[test]
    fn verbatim_disk_path_lsp_uri_roundtrip() {
        let verbatim = PathBuf::from(r"\\?\C:\Users\vboxuser\fresh\CHANGELOG.md");
        let uri = path_to_lsp_uri(&verbatim).expect("verbatim path should produce valid lsp URI");

        // The URI should parse successfully (the old bug produced unparseable URIs)
        assert_eq!(uri.as_str(), "file:///C:/Users/vboxuser/fresh/CHANGELOG.md");

        // Round-trip back to a path (will be the non-verbatim form)
        let back = lsp_uri_to_path(&uri).unwrap();
        assert_eq!(back, PathBuf::from(r"C:\Users\vboxuser\fresh\CHANGELOG.md"));
    }

    #[cfg(windows)]
    #[test]
    fn verbatim_disk_path_with_special_chars() {
        let verbatim = PathBuf::from(r"\\?\D:\My Projects [temp]\src\main.go");
        let uri = path_to_file_uri(&verbatim).expect("verbatim path should produce URI");

        assert!(
            uri.starts_with("file:///D:/"),
            "URI should start with file:///D:/, got: {}",
            uri
        );
        assert!(
            !uri.contains(r"\\?\"),
            "URI must not contain verbatim prefix, got: {}",
            uri
        );
        assert!(
            uri.contains("%5Btemp%5D"),
            "brackets should be percent-encoded: {}",
            uri
        );
    }

    // ── Windows prefix edge cases (drive letter casing, UNC, device) ──

    #[cfg(windows)]
    #[test]
    fn verbatim_disk_lowercase_drive_uppercased() {
        // Rust's VerbatimDisk normalizes the drive byte to uppercase,
        // so \\?\c: becomes VerbatimDisk(67) = 'C'. Verify we preserve this.
        let path = PathBuf::from(r"\\?\c:\Users\file.txt");
        let uri = path_to_file_uri(&path).expect("should produce URI");
        assert_eq!(
            uri, "file:///C:/Users/file.txt",
            "lowercase drive letter should be uppercased in URI"
        );
    }

    #[cfg(windows)]
    #[test]
    fn unc_path_produces_file_uri_with_server_authority() {
        // \\server\share\file.txt → file://server/share/file.txt
        // The server name becomes the URI authority (host), matching the url crate.
        let path = PathBuf::from(r"\\server\share\dir\file.txt");
        let uri = path_to_file_uri(&path).expect("UNC path should produce URI");
        assert_eq!(uri, "file://server/share/dir/file.txt");
        assert!(
            !uri.contains('\\'),
            "URI must not contain backslashes: {}",
            uri
        );
    }

    #[cfg(windows)]
    #[test]
    fn unc_path_lsp_uri_roundtrip() {
        let path = PathBuf::from(r"\\server\share\project\src\main.rs");
        let uri = path_to_lsp_uri(&path).expect("UNC path should produce valid lsp URI");
        assert_eq!(uri.as_str(), "file://server/share/project/src/main.rs");
    }

    #[cfg(windows)]
    #[test]
    fn verbatim_unc_path_produces_file_uri_with_server_authority() {
        // \\?\UNC\server\share\file.txt → file://server/share/file.txt
        // VerbatimUNC should be normalized to the same URI as plain UNC.
        let path = PathBuf::from(r"\\?\UNC\server\share\dir\file.txt");
        let uri = path_to_file_uri(&path).expect("VerbatimUNC path should produce URI");
        assert_eq!(uri, "file://server/share/dir/file.txt");
    }

    #[cfg(windows)]
    #[test]
    fn verbatim_unc_matches_plain_unc() {
        let unc = PathBuf::from(r"\\server\share\file.txt");
        let verbatim_unc = PathBuf::from(r"\\?\UNC\server\share\file.txt");
        let unc_uri = path_to_file_uri(&unc).expect("UNC should produce URI");
        let verbatim_uri = path_to_file_uri(&verbatim_unc).expect("VerbatimUNC should produce URI");
        assert_eq!(
            unc_uri, verbatim_uri,
            "UNC and VerbatimUNC must produce identical URIs"
        );
    }

    #[cfg(windows)]
    #[test]
    fn verbatim_non_disk_returns_none() {
        // \\?\BootPartition\... is a Verbatim prefix that is not a disk or UNC.
        // The url crate rejects these; we should too.
        let path = PathBuf::from(r"\\?\BootPartition\file.txt");
        assert!(
            path_to_file_uri(&path).is_none(),
            "Verbatim (non-disk, non-UNC) paths cannot be represented as file URIs"
        );
    }

    #[cfg(windows)]
    #[test]
    fn device_namespace_returns_none() {
        // \\.\COM1, \\.\PhysicalDrive0 etc. are device paths, not file paths.
        // The url crate rejects these; we should too.
        let path = PathBuf::from(r"\\.\COM1");
        assert!(
            path_to_file_uri(&path).is_none(),
            "DeviceNS paths cannot be represented as file URIs"
        );
        let path2 = PathBuf::from(r"\\.\PhysicalDrive0");
        assert!(
            path_to_file_uri(&path2).is_none(),
            "DeviceNS paths cannot be represented as file URIs"
        );
    }

    // ── Property tests ──────────────────────────────────────────

    mod prop {
        use super::*;
        use proptest::prelude::*;

        /// Strategy for valid path component characters (no NUL, no `/`).
        fn path_component() -> impl Strategy<Value = String> {
            proptest::string::string_regex("[a-zA-Z0-9 _.~!@#$%^&()\\[\\]{}`+=,-]{1,30}").unwrap()
        }

        /// Strategy for Unicode path components.
        fn unicode_component() -> impl Strategy<Value = String> {
            "[^\x00/]{1,20}"
        }

        proptest! {
            #[test]
            fn roundtrip_ascii(
                comp1 in path_component(),
                comp2 in path_component(),
            ) {
                let path = PathBuf::from(format!("/tmp/{comp1}/{comp2}/file.txt"));
                if let Some(uri) = path_to_file_uri(&path) {
                    let back = file_uri_to_path(&uri).unwrap();
                    prop_assert_eq!(back, path, "roundtrip failed");
                }
            }

            #[test]
            fn roundtrip_unicode(
                comp in unicode_component(),
            ) {
                let path = PathBuf::from(format!("/tmp/{comp}/file.txt"));
                if let Some(uri) = path_to_file_uri(&path) {
                    let back = file_uri_to_path(&uri).unwrap();
                    // `..` and `.` are preserved in the URI, matching the `url` crate.
                    // Path::components().collect() also preserves them.
                    let normalised: PathBuf = path.components().collect();
                    prop_assert_eq!(back, normalised, "roundtrip failed");
                }
            }

            #[test]
            fn uri_is_always_ascii(
                comp in unicode_component(),
            ) {
                let path = PathBuf::from(format!("/tmp/{comp}/file.txt"));
                if let Some(uri) = path_to_file_uri(&path) {
                    prop_assert!(
                        uri.is_ascii(),
                        "URI should be pure ASCII after encoding: {uri}"
                    );
                }
            }

            #[test]
            fn uri_is_valid_lsp_uri(
                comp in unicode_component(),
            ) {
                let path = PathBuf::from(format!("/tmp/{comp}/file.txt"));
                if let Some(uri_str) = path_to_file_uri(&path) {
                    let parsed = uri_str.parse::<lsp_types::Uri>();
                    prop_assert!(
                        parsed.is_ok(),
                        "Should be valid lsp_types::Uri: {uri_str} (err: {:?})",
                        parsed.err()
                    );
                }
            }

            #[test]
            fn lsp_uri_roundtrip(
                comp in unicode_component(),
            ) {
                let path = PathBuf::from(format!("/tmp/{comp}/file.txt"));
                if let Some(uri) = path_to_lsp_uri(&path) {
                    let back = lsp_uri_to_path(&uri).unwrap();
                    let normalised: PathBuf = path.components().collect();
                    prop_assert_eq!(back, normalised);
                }
            }

            #[test]
            fn relative_paths_always_none(s in "([a-z./]{0,20})") {
                let path = Path::new(&s);
                if !path.is_absolute() {
                    prop_assert!(path_to_file_uri(path).is_none());
                }
            }
        }
    }
}
