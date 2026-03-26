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

    // Resolve `.` and `..` without touching the filesystem (unlike
    // std::fs::canonicalize which requires the path to exist).
    let path = normalize_path(path);

    let mut uri = String::from("file://");

    #[cfg(windows)]
    {
        // file:///C:/path/to/file
        uri.push('/');
        let mut first = true;
        for component in path.components() {
            use std::path::Component;
            match component {
                Component::Prefix(prefix) => {
                    // Drive letter: C: → C:
                    let s = prefix.as_os_str().to_str()?;
                    uri.push_str(s);
                }
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
                _ => {}
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

/// Normalize a path by resolving `.` and `..` components purely
/// lexically (without touching the filesystem).
fn normalize_path(path: &Path) -> PathBuf {
    use std::path::Component;
    let mut parts: Vec<&std::ffi::OsStr> = Vec::new();
    for component in path.components() {
        match component {
            Component::ParentDir => {
                parts.pop();
            }
            Component::Normal(seg) => {
                parts.push(seg);
            }
            Component::RootDir | Component::Prefix(_) => {}
            Component::CurDir => {}
        }
    }
    // Rebuild as absolute path
    let mut result = PathBuf::from(
        path.components()
            .take_while(|c| matches!(c, Component::Prefix(_) | Component::RootDir))
            .collect::<PathBuf>(),
    );
    for part in parts {
        result.push(part);
    }
    result
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
        // Malformed %X or trailing % should be passed through
        #[cfg(not(windows))]
        {
            let path = file_uri_to_path("file:///home/100%/file.txt").unwrap();
            assert_eq!(path, PathBuf::from("/home/100%/file.txt"));
        }
        #[cfg(windows)]
        assert!(file_uri_to_path("file:///home/100%/file.txt").is_none());
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
                    // path_to_file_uri resolves `.` and `..` lexically.
                    let normalised = normalize_path(&path);
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
                    // path_to_lsp_uri resolves `.` and `..` lexically.
                    let normalised = normalize_path(&path);
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
