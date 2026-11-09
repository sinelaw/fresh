//! Changelog extraction for the welcome screen's "What's new" list.
//!
//! The extraction is shared with `build.rs`, which includes this file
//! by path and runs [`latest_release`] over `CHANGELOG.md` at build time; the
//! result is compiled in as `$OUT_DIR/release_notes.rs`. Keeping the parser
//! here, rather than in the build script, is what lets it carry unit tests.
//!
//! Nothing here may depend on anything outside `std`: the build script
//! compiles this file on its own.

/// One release's notes, reduced to a plain list of short bullets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Release {
    /// The heading's version, e.g. `0.5.8`.
    pub version: String,
    /// One short line per change, features first, in changelog order.
    pub items: Vec<String>,
}

/// Sections of a release that are not news to the person using the editor.
const SKIPPED_SECTIONS: &[&str] = &["internals", "internal", "chores", "chore"];

/// A bold lead followed by a separate detail is used on its own when it has
/// at least this many words; shorter leads are labels (`**Markdown** - Tab
/// honours …`) and the rest of the bullet is what says what changed.
const MIN_LEAD_WORDS: usize = 3;

/// The first `## x.y.z` section of a changelog, reduced to short bullets.
///
/// `None` when there is no such section or it has no bullets — the caller
/// then shows no "What's new" at all, which is the right thing for a build
/// from a source tree without the changelog (or with a mangled one).
pub fn latest_release(changelog: &str) -> Option<Release> {
    let mut version: Option<String> = None;
    let mut items = Vec::new();
    let mut skipping = false;
    // A bullet may continue on following indented lines.
    let mut current: Option<String> = None;

    let flush = |current: &mut Option<String>, items: &mut Vec<String>| {
        if let Some(raw) = current.take() {
            let short = shorten(&raw);
            if !short.is_empty() {
                items.push(short);
            }
        }
    };

    for line in changelog.lines() {
        let trimmed = line.trim_end();
        if let Some(rest) = trimmed.strip_prefix("## ") {
            if version.is_some() {
                break; // The next release: done.
            }
            let v = rest.trim().trim_start_matches('v');
            if looks_like_version(v) {
                version = Some(v.to_string());
            }
            continue;
        }
        if version.is_none() {
            continue;
        }
        if let Some(title) = trimmed.strip_prefix("### ") {
            flush(&mut current, &mut items);
            let t = title.trim().to_ascii_lowercase();
            skipping = SKIPPED_SECTIONS.contains(&t.as_str());
            continue;
        }
        if skipping {
            continue;
        }
        let bullet = trimmed
            .strip_prefix("* ")
            .or_else(|| trimmed.strip_prefix("- "));
        if let Some(text) = bullet {
            flush(&mut current, &mut items);
            current = Some(text.to_string());
        } else if trimmed.is_empty() || !line.starts_with(' ') {
            // A blank line, a quote or a paragraph ends the bullet.
            flush(&mut current, &mut items);
        } else if let Some(c) = current.as_mut() {
            // An indented continuation of the bullet above.
            c.push(' ');
            c.push_str(trimmed.trim());
        }
    }
    flush(&mut current, &mut items);

    let version = version?;
    if items.is_empty() {
        return None;
    }
    Some(Release { version, items })
}

/// `0.5.8`, `1.0.0-rc1`: digits and dots first. Rejects `Unreleased`,
/// `Release Notes` and the like.
fn looks_like_version(s: &str) -> bool {
    let core = s.split(['-', ' ', '+']).next().unwrap_or("");
    !core.is_empty() && core.contains('.') && core.chars().all(|c| c.is_ascii_digit() || c == '.')
}

/// One changelog bullet as one short plain line; markdown and the
/// `(#123, reported by @x)` credits stripped.
///
/// A bullet is `**lead** rest`. The lead is the headline, so it is kept, and
/// the rest is kept only as far as it needs to be:
/// - rest continues the sentence (`**C# typing lag** from slow highlighting
///   … is gone`): the lead alone would be a fragment, so keep the clause, up
///   to a `;` that starts a second one;
/// - rest is a separate detail (` - …`, `; …`): drop it, unless the lead is
///   too short to say anything (`**Markdown** - Tab honours …`).
fn shorten(raw: &str) -> String {
    let raw = raw.trim();
    let split = raw
        .strip_prefix("**")
        .and_then(|r| r.find("**").map(|end| (&r[..end], &r[end + 2..])));
    let text = match split {
        Some((lead, rest)) => {
            let rest = rest.trim_start();
            let continues = rest
                .chars()
                .next()
                .is_some_and(|c| c.is_lowercase() || c.is_ascii_digit() || c == '`');
            if continues {
                let clause = rest.split(';').next().unwrap_or("");
                format!("{lead} {clause}")
            } else if lead.split_whitespace().count() >= MIN_LEAD_WORDS {
                lead.to_string()
            } else {
                raw.to_string()
            }
        }
        None => raw.to_string(),
    };
    tidy(&strip_credits(&text))
}

/// Remove parentheticals that hold issue numbers or credits.
fn strip_credits(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut rest = s;
    while let Some(open) = rest.find('(') {
        let Some(len) = rest[open..].find(')') else {
            break;
        };
        let inner = &rest[open + 1..open + len];
        let is_credit = inner.contains('#')
            || inner.contains(" by @")
            || inner.starts_with("by @")
            || inner.contains("reported by")
            || inner.contains("requested by");
        out.push_str(&rest[..open]);
        if !is_credit {
            out.push_str(&rest[open..=open + len]);
        }
        rest = &rest[open + len + 1..];
    }
    out.push_str(rest);
    out
}

/// Drop markdown emphasis and code ticks, unwrap `[text](url)` links, turn a
/// spaced hyphen into an em dash, and collapse whitespace.
fn tidy(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut rest = s;
    while let Some(open) = rest.find('[') {
        out.push_str(&rest[..open]);
        let after = &rest[open + 1..];
        match (after.find("]("), after.find(')')) {
            (Some(mid), Some(close)) if mid < close => {
                out.push_str(&after[..mid]);
                rest = &after[close + 1..];
            }
            _ => {
                out.push('[');
                rest = after;
            }
        }
    }
    out.push_str(rest);
    let out = out.replace("**", "").replace('`', "").replace(" - ", " — ");
    let words: Vec<&str> = out.split_whitespace().collect();
    words
        .join(" ")
        .trim_end_matches([';', ',', ':'])
        .replace(" ;", ";")
        .replace(" ,", ",")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = "# Release Notes

## 0.5.8

For live updates on Fresh, [follow me on X](https://x.com/TheNoamLewis).

> Most config options below can be changed in the **Settings UI**.

### Features

* **Auto-hide the tab bar** - **Auto Hide Tab Bar** (Settings > Editor > Display) hides it (#3696, requested by @garfunkel)
* **Option+Backspace / Option+Delete delete words on macOS** (#3643, requested by @gaojunran)
* **C# typing lag** from slow highlighting of interpolated strings is gone (#3667, reported by @x); also faster

### Bug Fixes

* **Markdown** - Tab honours the configured tab size (#3713, reported by @hbarel)
* **Python `match`/`case` and more C++ keywords are highlighted**; `.c` files use the C grammar (#3509)
* A plain bullet without a bold lead,
  continued on the next line (#1)

### Internals

* Reduced flaky tests.

## 0.5.7

### Features

* **Older release** - must not appear
";

    #[test]
    fn takes_the_first_release_only_and_shortens_its_bullets() {
        let r = latest_release(SAMPLE).expect("a release");
        assert_eq!(r.version, "0.5.8");
        assert_eq!(
            r.items,
            vec![
                "Auto-hide the tab bar",
                "Option+Backspace / Option+Delete delete words on macOS",
                "C# typing lag from slow highlighting of interpolated strings is gone",
                "Markdown — Tab honours the configured tab size",
                "Python match/case and more C++ keywords are highlighted",
                "A plain bullet without a bold lead, continued on the next line",
            ]
        );
    }

    #[test]
    fn a_missing_or_empty_changelog_has_no_release() {
        assert_eq!(latest_release(""), None);
        assert_eq!(latest_release("# Release Notes\n\nNothing yet.\n"), None);
    }

    #[test]
    fn odd_changelogs_degrade_to_nothing_rather_than_garbage() {
        // A symlink checked out as a plain file (git on Windows without
        // symlink support) holds just the link target.
        assert_eq!(latest_release("../../CHANGELOG.md"), None);
        // A heading that is not a version.
        assert_eq!(
            latest_release("## Unreleased\n\n* **Thing** happens\n"),
            None
        );
        // A version with no bullets.
        assert_eq!(latest_release("## 1.2.3\n\n### Features\n\n"), None);
        // Only internal changes.
        assert_eq!(
            latest_release("## 1.2.3\n\n### Internals\n\n* Refactor\n"),
            None
        );
        // An unclosed bold lead keeps the bullet rather than panicking.
        let r = latest_release("## v1.2.3\n\n* **Unclosed lead and more\n").unwrap();
        assert_eq!(r.version, "1.2.3");
        assert_eq!(r.items, vec!["Unclosed lead and more"]);
    }

    #[test]
    fn the_real_changelog_parses() {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../CHANGELOG.md");
        let Ok(text) = std::fs::read_to_string(path) else {
            return; // Packaged builds may not carry it.
        };
        let r = latest_release(&text).expect("CHANGELOG.md has a latest release");
        assert!(!r.items.is_empty());
        for item in &r.items {
            assert!(!item.contains("**"), "markdown left in {item:?}");
            assert!(!item.contains("(#"), "credit left in {item:?}");
        }
    }
}
