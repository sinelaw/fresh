//! Skins: how the client looks, as folders of plain web files.
//!
//! A skin is a folder with a `skin.css` (whose header comment names it and
//! sets its frame, density, furniture and editor theme), and optionally a
//! `furniture.html` of decorative markup, a `theme.json` editor theme, and
//! the images and fonts its CSS uses. Nothing in a skin runs: the client
//! links its stylesheet and rebuilds its furniture from inert elements.
//!
//! Skins come from two places. The built-in ones are part of the client's
//! own files (`web/skins/<NN>-<id>/`, embedded at build time; the number
//! orders the list). A user's own are folders in `~/.config/fresh/web-skins/`
//! on the workspace's host, read through the authority's filesystem like
//! every other file the server reads. A user skin cannot take a built-in
//! skin's id.
//!
//! The client lists skins with the `skin.list` operation and loads a skin's
//! files from `GET /skins/<id>/<path>` (a built-in one) or
//! `GET /workspaces/<ws>/skins/<id>/<path>` (one of the workspace's host),
//! so a stylesheet's relative `url()`s resolve inside its own folder. A
//! client viewing no workspace has the built-in skins only. A skin's bundled editor theme is a theme
//! like any other: `skin/<id>`, resolved by the editor's theme loader.

use crate::error::{err, Code};
use crate::workspace::{host, Workspace};
use anyhow::Result;
use fresh_editor_core::theme::{Theme, ThemeFile};
use serde_json::{json, Map, Value};
use std::path::PathBuf;

/// The most a skin file may be: a stylesheet with an inlined font is a few
/// hundred kilobytes; a wallpaper a few megabytes.
const MAX_FILE: u64 = 8 << 20;

/// The theme keys of the themes skins bring: `skin/<id>`.
pub const THEME_PREFIX: &str = "skin/";

/// Where a skin's files are.
enum Folder {
    /// Under the client's own files: `skins/<folder>/`.
    Builtin(String),
    /// A folder on the workspace's host.
    User(PathBuf),
}

struct Skin {
    id: String,
    folder: Folder,
    header: Map<String, Value>,
}

/// The `==FreshSkin==` header of a stylesheet: `@key value` lines.
fn header(css: &str) -> Option<Map<String, Value>> {
    let start = css.find("==FreshSkin==")? + "==FreshSkin==".len();
    let end = start + css[start..].find("==/FreshSkin==")?;
    let mut out = Map::new();
    for line in css[start..end].lines() {
        let Some(rest) = line.trim().strip_prefix('@') else {
            continue;
        };
        let (k, v) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
        out.insert(k.to_string(), Value::String(v.trim().to_string()));
    }
    Some(out)
}

/// A skin id: `a-z`, `0-9` and `-`, 1 to 48 characters.
fn valid_id(id: &str) -> bool {
    !id.is_empty()
        && id.len() <= 48
        && id
            .bytes()
            .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'-')
}

/// A plain relative path inside a skin folder ("furniture.html", "img/a.png").
fn plain_path(p: &str) -> bool {
    !p.is_empty()
        && !p.starts_with('/')
        && !p.contains('\\')
        && p.split('/').all(|s| !s.is_empty() && s != "." && s != "..")
}

/// A folder's name without its ordering number ("40-winamp" → "winamp").
fn folder_id(name: &str) -> String {
    match name.split_once('-') {
        Some((n, rest)) if !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()) => rest.into(),
        _ => name.into(),
    }
}

/// `~/.config/fresh/web-skins` on the workspace's host; none without a
/// workspace.
fn user_dir(ws: Option<&Workspace>) -> Option<PathBuf> {
    let home = ws?.fs().home_dir().ok()?;
    Some(PathBuf::from(home).join(".config/fresh/web-skins"))
}

fn read_user(ws: Option<&Workspace>, p: &std::path::Path) -> Option<Vec<u8>> {
    let fs = ws?.fs();
    let hp = host(p);
    if fs.metadata(hp.as_host_path()).ok()?.size > MAX_FILE {
        return None;
    }
    fs.read_file(hp.as_host_path()).ok()
}

fn skin(id: String, folder: Folder, css: &[u8]) -> Option<Skin> {
    let header = header(&String::from_utf8_lossy(css))?;
    let id = match header.get("id").and_then(Value::as_str) {
        Some(h) if !h.is_empty() => h.to_string(),
        _ => id,
    };
    valid_id(&id).then_some(Skin { id, folder, header })
}

/// Every skin: the built-in ones in their folders' order, then the user's.
fn all(ws: Option<&Workspace>) -> Vec<Skin> {
    let mut out: Vec<Skin> = Vec::new();
    for name in crate::asset_children("skins") {
        if let Some(css) = crate::asset(&format!("skins/{name}/skin.css")) {
            out.extend(skin(folder_id(&name), Folder::Builtin(name.clone()), &css));
        }
    }
    let builtin = out.len();
    if let (Some(dir), Some(w)) = (user_dir(ws), ws) {
        let mut entries = w.list_dir(&dir).unwrap_or_default();
        entries.retain(|e| e["dir"].as_bool() == Some(true));
        for e in entries {
            let (Some(name), Some(path)) = (e["name"].as_str(), e["path"].as_str()) else {
                continue;
            };
            let path = PathBuf::from(path);
            let Some(css) = read_user(ws, &path.join("skin.css")) else {
                continue;
            };
            if let Some(s) = skin(folder_id(name), Folder::User(path), &css) {
                if !out.iter().any(|o| o.id == s.id) {
                    out.push(s);
                }
            }
        }
    }
    // Built-in first, by folder; the user's by id.
    out[builtin..].sort_by(|a, b| a.id.cmp(&b.id));
    out
}

impl Skin {
    fn get(&self, k: &str) -> &str {
        self.header.get(k).and_then(Value::as_str).unwrap_or("")
    }

    fn file(&self, ws: Option<&Workspace>, path: &str) -> Option<Vec<u8>> {
        if !plain_path(path) {
            return None;
        }
        match &self.folder {
            Folder::Builtin(name) => crate::asset(&format!("skins/{name}/{path}")),
            Folder::User(dir) => read_user(ws, &dir.join(path)),
        }
    }

    /// The skin's bundled theme file: `@editor-theme` naming a `.json` file,
    /// or a `theme.json` beside the stylesheet.
    fn theme_file(&self, ws: Option<&Workspace>) -> Option<String> {
        let named = self.get("editor-theme");
        if named.ends_with(".json") {
            return plain_path(named).then(|| named.to_string());
        }
        (named.is_empty() && self.file(ws, "theme.json").is_some()).then(|| "theme.json".into())
    }

    /// The editor theme that goes with the skin, as a theme key: its own
    /// (`skin/<id>`), or an installed theme's (`builtin://nord` → `nord`).
    fn theme_key(&self, ws: Option<&Workspace>) -> Option<String> {
        if self.theme_file(ws).is_some() {
            return Some(format!("{THEME_PREFIX}{}", self.id));
        }
        let named = self.get("editor-theme");
        (!named.is_empty() && !named.ends_with(".json")).then(|| {
            named
                .strip_prefix("builtin://")
                .unwrap_or(named)
                .to_string()
        })
    }

    fn describe(&self, ws: Option<&Workspace>) -> Value {
        let scale = self
            .get("scale")
            .parse::<f64>()
            .ok()
            .filter(|s| (0.5..=1.5).contains(s));
        let furniture = Some(self.get("furniture")).filter(|f| plain_path(f));
        json!({
            "id": self.id,
            "name": Some(self.get("name")).filter(|n| !n.is_empty()).unwrap_or(&self.id),
            "description": self.get("description"),
            "author": self.get("author"),
            "extends": Some(self.get("extends")).filter(|e| !e.is_empty()),
            "frame": if self.get("frame") == "bezel" { "bezel" } else { "full" },
            "scale": scale,
            "furniture": furniture,
            "theme": self.theme_key(ws),
            "keys": if self.get("keys") == "symbols" { "symbols" } else { "words" },
            "source": match self.folder { Folder::Builtin(_) => "builtin", Folder::User(_) => "user" },
        })
    }
}

/// `skin.list`: every skin's header, resolved.
pub fn list(ws: Option<&Workspace>) -> Value {
    let skins: Vec<Value> = all(ws).iter().map(|s| s.describe(ws)).collect();
    json!({ "skins": skins })
}

/// A file of a skin, for `GET /skins/<id>/<path>` (no workspace: a
/// built-in skin's) and `GET /workspaces/<ws>/skins/<id>/<path>`.
pub fn file(ws: Option<&Workspace>, id: &str, path: &str) -> Option<Vec<u8>> {
    all(ws).into_iter().find(|s| s.id == id)?.file(ws, path)
}

/// The themes the skins bring, as `(key, name)`: the editor's theme list
/// shows them beside its own.
pub fn themes(ws: Option<&Workspace>) -> Vec<(String, String)> {
    all(ws)
        .iter()
        .filter(|s| s.theme_file(ws).is_some())
        .map(|s| {
            let name = Some(s.get("name"))
                .filter(|n| !n.is_empty())
                .unwrap_or(&s.id);
            (format!("{THEME_PREFIX}{}", s.id), name.to_string())
        })
        .collect()
}

/// A skin's theme (`skin/<id>`), resolved by the editor's theme loader (so
/// what it `extends` arrives filled in), in the theme file format.
pub fn theme(ws: Option<&Workspace>, key: &str) -> Result<Value> {
    let missing = || err(Code::NotFound, format!("no theme {key}"));
    let id = key.strip_prefix(THEME_PREFIX).ok_or_else(missing)?;
    let s = all(ws)
        .into_iter()
        .find(|s| s.id == id)
        .ok_or_else(missing)?;
    let f = s.theme_file(ws).ok_or_else(missing)?;
    let text = s.file(ws, &f).ok_or_else(missing)?;
    let theme = Theme::from_json(&String::from_utf8_lossy(&text))
        .map_err(|e| err(Code::Failed, format!("skin {id}: {e}")))?;
    Ok(serde_json::to_value(ThemeFile::from(theme))?)
}

/// `host.stats`: the workspace host's load, for the live readouts a skin's
/// furniture shows. Read from the host's `/proc` through the authority's
/// filesystem, so it describes the machine the workspace is on (over SSH,
/// the remote one); a host without `/proc` reports nothing. CPU use is the
/// busy share since the previous call.
pub fn host_stats(ws: &Workspace) -> Value {
    use std::sync::Mutex;
    static LAST_CPU: Mutex<Option<(u64, u64)>> = Mutex::new(None);
    let fs = ws.fs();
    let read = |p: &str| {
        fs.read_file(host(std::path::Path::new(p)).as_host_path())
            .ok()
            .map(|b| String::from_utf8_lossy(&b).into_owned())
    };
    let mut out = Map::new();
    if let Some(s) = read("/proc/loadavg") {
        let l: Vec<&str> = s.split_whitespace().take(3).collect();
        out.insert("load".into(), json!(l.join(" ")));
    }
    if let Some(s) = read("/proc/uptime") {
        if let Some(secs) = s
            .split_whitespace()
            .next()
            .and_then(|x| x.parse::<f64>().ok())
        {
            let (d, h, m) = (
                (secs / 86400.0) as u64,
                (secs / 3600.0) as u64 % 24,
                (secs / 60.0) as u64 % 60,
            );
            let text = if d > 0 {
                format!("{d}d {h}h")
            } else {
                format!("{h}h {m}m")
            };
            out.insert("uptime".into(), json!(text));
        }
    }
    if let Some(s) = read("/proc/meminfo") {
        let kb = |k: &str| {
            s.lines()
                .find(|l| l.starts_with(k))
                .and_then(|l| l.split_whitespace().nth(1)?.parse::<u64>().ok())
        };
        if let (Some(total), Some(avail)) = (kb("MemTotal:"), kb("MemAvailable:")) {
            let used = total.saturating_sub(avail);
            out.insert("mem".into(), json!(used * 100 / total.max(1)));
            let g = |k: u64| k as f64 / 1048576.0;
            out.insert(
                "memText".into(),
                json!(format!("{:.1} / {:.1}G", g(used), g(total))),
            );
        }
    }
    if let Some(s) = read("/proc/stat") {
        let f: Vec<u64> = s
            .lines()
            .next()
            .unwrap_or("")
            .split_whitespace()
            .skip(1)
            .filter_map(|x| x.parse().ok())
            .collect();
        if f.len() >= 4 {
            let total: u64 = f.iter().sum();
            let idle = f[3] + f.get(4).copied().unwrap_or(0);
            let mut last = LAST_CPU.lock().unwrap();
            if let Some((t0, i0)) = *last {
                let (dt, di) = (total.saturating_sub(t0), idle.saturating_sub(i0));
                if let Some(cpu) = ((dt - di.min(dt)) * 100).checked_div(dt) {
                    out.insert("cpu".into(), json!(cpu));
                }
            }
            *last = Some((total, idle));
        }
    }
    Value::Object(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reads_a_header() {
        let h = header("/* ==FreshSkin==\n@id  bridge\n@name   Bridge Console\n@frame bezel\n==/FreshSkin== */\nbody{}").unwrap();
        assert_eq!(h["id"], "bridge");
        assert_eq!(h["name"], "Bridge Console");
        assert_eq!(h["frame"], "bezel");
        assert!(header("body {}").is_none());
    }

    #[test]
    fn paths_and_ids() {
        assert!(plain_path("img/a.png"));
        assert!(!plain_path("../x"));
        assert!(!plain_path("/etc/passwd"));
        assert!(!plain_path("a//b"));
        assert_eq!(folder_id("40-winamp"), "winamp");
        assert_eq!(folder_id("my-skin"), "my-skin");
        assert!(valid_id("neon-2"));
        assert!(!valid_id("Neon"));
    }

    #[test]
    fn builtin_skins_parse_and_their_themes_resolve() {
        let ws = Workspace {
            root: std::env::temp_dir(),
            authority: Arc::new(crate::authority::Local::new()),
        };
        let l = list(Some(&ws));
        let ids: Vec<&str> = l["skins"]
            .as_array()
            .unwrap()
            .iter()
            .map(|s| s["id"].as_str().unwrap())
            .collect();
        for id in ["default", "compact", "winamp", "bridge"] {
            assert!(ids.contains(&id), "{id} in {ids:?}");
        }
        let t = theme(Some(&ws), "skin/bridge").unwrap();
        assert!(
            t["editor"]["bg"].is_array() || t["editor"]["bg"].is_string(),
            "{t}"
        );
        assert!(file(Some(&ws), "bridge", "../index.html").is_none());
        assert!(file(None, "bridge", "skin.css").is_some(), "built in");
    }
    use std::sync::Arc;
}
