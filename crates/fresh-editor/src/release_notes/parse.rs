//! Changelog extraction for the welcome screen's "What's new".
//!
//! The extraction is shared with `build.rs`, which includes this file
//! by path and runs [`latest_release`] over `CHANGELOG.md` at build time; the
//! result is compiled in as `$OUT_DIR/release_notes.rs`. Keeping the parser
//! here, rather than in the build script, is what lets it carry unit tests.
//!
//! Nothing here may depend on anything outside `std`: the build script
//! compiles this file on its own.

/// One release's notes, as written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Release {
    /// The heading's version, e.g. `0.5.8`.
    pub version: String,
    /// Everything between the release's `## x.y.z` heading and the next
    /// release's, unchanged: the same markdown the release notes are made
    /// of, without the blank lines at either end.
    pub markdown: String,
}

/// The first `## x.y.z` section of a changelog, as written.
///
/// `None` when there is no such section or it is empty — the caller then
/// shows no "What's new" at all, which is the right thing for a build from a
/// source tree without the changelog (or with a mangled one).
pub fn latest_release(changelog: &str) -> Option<Release> {
    let mut version: Option<String> = None;
    let mut body: Vec<&str> = Vec::new();
    for line in changelog.lines() {
        if let Some(rest) = line.trim_end().strip_prefix("## ") {
            if version.is_some() {
                break; // The next release: done.
            }
            let v = rest.trim().trim_start_matches('v');
            if looks_like_version(v) {
                version = Some(v.to_string());
            }
            continue;
        }
        if version.is_some() {
            body.push(line.trim_end());
        }
    }
    let version = version?;
    let first = body.iter().position(|l| !l.is_empty())?;
    let last = body.iter().rposition(|l| !l.is_empty())?;
    Some(Release {
        version,
        markdown: body[first..=last].join("\n"),
    })
}

/// `0.5.8`, `1.0.0-rc1`: digits and dots first. Rejects `Unreleased`,
/// `Release Notes` and the like.
fn looks_like_version(s: &str) -> bool {
    let core = s.split(['-', ' ', '+']).next().unwrap_or("");
    !core.is_empty() && core.contains('.') && core.chars().all(|c| c.is_ascii_digit() || c == '.')
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = "# Release Notes

## 0.5.8

For live updates on Fresh, [follow me on X](https://x.com/TheNoamLewis).

### Features

* **Auto-hide the tab bar** - **Auto Hide Tab Bar** (Settings > Editor > Display) hides it (#3696, requested by @garfunkel)

### Bug Fixes

* **Markdown** - Tab honours the configured tab size (#3713, reported by @hbarel)
* A plain bullet without a bold lead,
  continued on the next line (#1)

## 0.5.7

### Features

* **Older release** - must not appear
";

    /// The page shows the release notes the way they are written: the
    /// headings, the bold leads, the credits — nothing shortened or dropped.
    #[test]
    fn takes_the_first_release_as_written() {
        let r = latest_release(SAMPLE).expect("a release");
        assert_eq!(r.version, "0.5.8");
        assert_eq!(
            r.markdown,
            "For live updates on Fresh, [follow me on X](https://x.com/TheNoamLewis).

### Features

* **Auto-hide the tab bar** - **Auto Hide Tab Bar** (Settings > Editor > Display) hides it (#3696, requested by @garfunkel)

### Bug Fixes

* **Markdown** - Tab honours the configured tab size (#3713, reported by @hbarel)
* A plain bullet without a bold lead,
  continued on the next line (#1)"
        );
    }

    #[test]
    fn the_last_release_runs_to_the_end_of_the_file() {
        let r = latest_release("## v1.2.3\n\n* **Only change**\n\n").unwrap();
        assert_eq!(r.version, "1.2.3");
        assert_eq!(r.markdown, "* **Only change**");
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
        // A version with nothing under it.
        assert_eq!(latest_release("## 1.2.3\n\n\n## 1.2.2\n\n* Old\n"), None);
    }

    #[test]
    fn the_real_changelog_parses() {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../CHANGELOG.md");
        let Ok(text) = std::fs::read_to_string(path) else {
            return; // Packaged builds may not carry it.
        };
        let r = latest_release(&text).expect("CHANGELOG.md has a latest release");
        assert!(
            r.markdown.contains("\n* "),
            "no bullets in {:?}",
            r.markdown
        );
        assert!(!r.markdown.contains("\n## "), "ran into the next release");
    }
}
