//! Web-UI skins served from a folder on disk — the skin author's dev loop.
//!
//! A web skin is a folder: `skin.css` (with a `==FreshSkin==` header), an
//! optional `furniture.html`, and any images or fonts the CSS references by
//! relative `url(…)` (docs/features/web-ui-skins.md). Folders under the skins
//! directory — `$FRESH_WEB_SKINS` if set, else `<config dir>/web-skins/` — are
//! offered by the page's skin picker alongside the built-in ones:
//!
//! - `GET /skins/index.json` → `[{"id": "<folder>", "stamp": "<hex>",
//!   "theme"?: "theme.json"}, …]`, one
//!   entry per folder holding a `skin.css`. `stamp` changes whenever any file
//!   of the folder does (name, size, mtime), so the page polls it and reloads
//!   a skin the author just saved.
//! - `GET /skins/<folder>/<file…>` → that file, read-only.
//!
//! Only plain names are served: every path segment is `[A-Za-z0-9._-]`, never
//! `.`/`..`, so a request cannot leave the skins directory.
//!
//! A skin may also carry an editor theme (`theme.json` in the ordinary Fresh
//! theme format, or the name of an installed theme). The page asks for it to
//! be applied with a `skinTheme` message, handled by [`apply_skin_theme`]: a
//! bundled theme is validated by the theme loader, saved as
//! `<themes dir>/web-skins/<skin id>.json` (so the TUI's theme picker lists it
//! too) and applied exactly as choosing it in the editor would.

use std::fs;
use std::path::{Path, PathBuf};

/// How deep under a skin folder files are served / stamped.
const MAX_DEPTH: usize = 4;
/// Largest file served (a skin's images and fonts are small).
const MAX_FILE: u64 = 16 * 1024 * 1024;
/// Files looked at per folder when stamping.
const MAX_STAMP_FILES: usize = 2000;

/// The skins directory: `$FRESH_WEB_SKINS`, else `<config_dir>/web-skins`.
pub(super) fn skins_dir(config_dir: &Path) -> PathBuf {
    match std::env::var_os("FRESH_WEB_SKINS") {
        Some(d) if !d.is_empty() => PathBuf::from(d),
        _ => config_dir.join("web-skins"),
    }
}

/// A skin folder name: lower-case letters, digits and `-` (the skin's id).
fn is_skin_id(s: &str) -> bool {
    !s.is_empty()
        && s.len() <= 48
        && s.bytes()
            .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'-')
        && !s.starts_with('-')
        && !s.ends_with('-')
}

fn is_plain_segment(s: &str) -> bool {
    !s.is_empty()
        && s != "."
        && s != ".."
        && s.len() <= 128
        && s.bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'.' | b'_' | b'-'))
}

/// `GET /skins/index.json`: the served skin folders and their change stamps.
pub(super) fn index_json(dir: &Path) -> String {
    let mut out = Vec::new();
    if let Ok(entries) = fs::read_dir(dir) {
        let mut names: Vec<String> = entries
            .filter_map(|e| e.ok())
            .filter(|e| e.path().join("skin.css").is_file())
            .filter_map(|e| e.file_name().to_str().map(str::to_string))
            .filter(|n| is_skin_id(n))
            .collect();
        names.sort();
        for name in names {
            let stamp = folder_stamp(&dir.join(&name));
            let mut entry = serde_json::json!({ "id": name, "stamp": format!("{stamp:016x}") });
            // The conventional editor theme, so the page asks for it only when present.
            if dir.join(&name).join("theme.json").is_file() {
                entry["theme"] = "theme.json".into();
            }
            out.push(entry);
        }
    }
    serde_json::Value::Array(out).to_string()
}

/// A hash over (relative path, size, mtime) of every file in a skin folder.
fn folder_stamp(root: &Path) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut files = Vec::new();
    collect_files(root, root, 0, &mut files);
    files.sort();
    let mut h = std::collections::hash_map::DefaultHasher::new();
    for (rel, len, mtime) in files {
        rel.hash(&mut h);
        len.hash(&mut h);
        mtime.hash(&mut h);
    }
    h.finish()
}

fn collect_files(root: &Path, dir: &Path, depth: usize, out: &mut Vec<(String, u64, u128)>) {
    if depth > MAX_DEPTH || out.len() >= MAX_STAMP_FILES {
        return;
    }
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for e in entries.filter_map(|e| e.ok()) {
        let path = e.path();
        let Ok(meta) = fs::metadata(&path) else {
            continue;
        };
        if meta.is_dir() {
            collect_files(root, &path, depth + 1, out);
        } else if out.len() < MAX_STAMP_FILES {
            let rel = path
                .strip_prefix(root)
                .map(|p| p.to_string_lossy().into_owned())
                .unwrap_or_default();
            let mtime = meta
                .modified()
                .ok()
                .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                .map_or(0, |d| d.as_nanos());
            out.push((rel, meta.len(), mtime));
        }
    }
}

/// `GET /skins/<rest>` (`rest` without the prefix or a query string): the
/// file's content type and bytes, or `None` for a path that is not a plain
/// file of a skin folder.
pub(super) fn read_file(dir: &Path, rest: &str) -> Option<(&'static str, Vec<u8>)> {
    let segs: Vec<&str> = rest.split('/').collect();
    if segs.len() < 2 || segs.len() > MAX_DEPTH + 2 {
        return None;
    }
    if !is_skin_id(segs[0]) || !segs.iter().all(|s| is_plain_segment(s)) {
        return None;
    }
    let mut path = dir.to_path_buf();
    for s in &segs {
        path.push(s);
    }
    let meta = fs::metadata(&path).ok()?;
    if !meta.is_file() || meta.len() > MAX_FILE {
        return None;
    }
    let bytes = fs::read(&path).ok()?;
    Some((content_type(segs[segs.len() - 1]), bytes))
}

fn content_type(name: &str) -> &'static str {
    let ext = name.rsplit('.').next().unwrap_or("").to_ascii_lowercase();
    match ext.as_str() {
        "css" => "text/css; charset=utf-8",
        "html" | "htm" => "text/html; charset=utf-8",
        "json" => "application/json",
        "svg" => "image/svg+xml",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "gif" => "image/gif",
        "webp" => "image/webp",
        "avif" => "image/avif",
        "ico" => "image/x-icon",
        "woff2" => "font/woff2",
        "woff" => "font/woff",
        "ttf" => "font/ttf",
        "otf" => "font/otf",
        "txt" | "md" => "text/plain; charset=utf-8",
        _ => "application/octet-stream",
    }
}

/// The largest bundled theme accepted (a full theme file is a few KB).
const MAX_THEME: usize = 512 * 1024;

/// `skinTheme` message: `{"skin": "<id>", "theme": {…}}` (a bundled theme) or
/// `{"name": "<theme key or name>"}` (an installed one). Applies it through
/// the editor's own theme switch, which also persists it to `config.json` —
/// the editor theme is shared, saved state, unlike the skin.
pub(super) fn apply_skin_theme(editor: &mut crate::app::Editor, v: &serde_json::Value) {
    if let Some(name) = v.get("name").and_then(|n| n.as_str()) {
        editor.apply_theme(name);
        return;
    }
    let (Some(id), Some(theme)) = (v.get("skin").and_then(|s| s.as_str()), v.get("theme")) else {
        return;
    };
    if !is_skin_id(id) {
        return;
    }
    let text = match serde_json::to_string_pretty(theme) {
        Ok(t) if t.len() <= MAX_THEME => t,
        _ => return,
    };
    if let Err(e) = crate::view::theme::Theme::from_json(&text) {
        editor.set_status_message(format!("Skin \"{id}\": its editor theme is invalid: {e}"));
        return;
    }
    let path = theme_path(&editor.dir_context().themes_dir(), id);
    if fs::read_to_string(&path).ok().as_deref() != Some(text.as_str()) {
        let written = path
            .parent()
            .map_or(Ok(()), fs::create_dir_all)
            .and_then(|()| fs::write(&path, &text));
        if let Err(e) = written {
            editor.set_status_message(format!(
                "Skin \"{id}\": could not save its editor theme: {e}"
            ));
            return;
        }
        editor.reload_themes();
    }
    editor.apply_theme(&format!("file://{}", path.display()));
}

/// Where a skin's bundled editor theme is saved.
fn theme_path(themes_dir: &Path, id: &str) -> PathBuf {
    themes_dir.join("web-skins").join(format!("{id}.json"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serves_only_plain_paths_inside_a_skin_folder() {
        let tmp = tempfile::tempdir().unwrap();
        let skin = tmp.path().join("neon");
        fs::create_dir_all(skin.join("img")).unwrap();
        fs::write(skin.join("skin.css"), "/* css */").unwrap();
        fs::write(skin.join("img/bg.png"), [1u8, 2, 3]).unwrap();
        fs::write(tmp.path().join("secret.txt"), "no").unwrap();

        let (ct, body) = read_file(tmp.path(), "neon/skin.css").unwrap();
        assert_eq!(ct, "text/css; charset=utf-8");
        assert_eq!(body, b"/* css */");
        assert_eq!(
            read_file(tmp.path(), "neon/img/bg.png").unwrap().0,
            "image/png"
        );
        for bad in [
            "secret.txt",
            "neon/../secret.txt",
            "../secret.txt",
            "neon/./skin.css",
            "neon//skin.css",
            "Neon/skin.css",
            "neon/img",
            "neon/missing.css",
            "neon/sk%2e.css",
        ] {
            assert!(
                read_file(tmp.path(), bad).is_none(),
                "{bad} must not be served"
            );
        }
    }

    #[test]
    fn index_lists_skin_folders_and_restamps_on_change() {
        let tmp = tempfile::tempdir().unwrap();
        fs::create_dir_all(tmp.path().join("neon")).unwrap();
        fs::create_dir_all(tmp.path().join("not-a-skin")).unwrap();
        fs::create_dir_all(tmp.path().join("Bad_Name")).unwrap();
        fs::write(tmp.path().join("Bad_Name/skin.css"), "").unwrap();
        fs::write(tmp.path().join("neon/skin.css"), "a").unwrap();
        let v: serde_json::Value = serde_json::from_str(&index_json(tmp.path())).unwrap();
        let list = v.as_array().unwrap();
        assert_eq!(list.len(), 1);
        assert_eq!(list[0]["id"], "neon");
        let before = list[0]["stamp"].clone();
        fs::write(tmp.path().join("neon/skin.css"), "ab").unwrap();
        let v: serde_json::Value = serde_json::from_str(&index_json(tmp.path())).unwrap();
        assert_ne!(v[0]["stamp"], before, "a changed file changes the stamp");
        assert_eq!(index_json(&tmp.path().join("absent")), "[]");
    }
}
