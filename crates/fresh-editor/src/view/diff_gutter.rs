//! Line numbers for a unified-diff stream, derived from its `@@` headers.
//!
//! A review buffer is git's own diff text with the plugin's chrome rows
//! spliced in at header positions. Git does not number its rows; the
//! numbers follow from each hunk header's `-old,+new` starts and from
//! which side each row belongs to. Working that out at paint time would
//! mean re-walking a hunk for every visible row, so it is done once, in a
//! single pass when the content is set, into one pair per logical line.
//! The buffer is replaced wholesale rather than edited, so that pass is
//! exact for as long as the content lives.

/// Per-line `(old, new)` numbers, `0` where a side has none.
#[derive(Debug, Clone, Default)]
pub struct DiffGutter {
    rows: Vec<(u32, u32)>,
    digits: usize,
}

impl DiffGutter {
    /// Number every line of `text`.
    ///
    /// Inside a hunk a row is classified by its first byte alone, as the
    /// diff grammar does: a removed line that happens to begin `--` is
    /// code, not a file header. Anything else inside a hunk — a chrome row
    /// the plugin spliced in — is numbered blank and advances neither side.
    pub fn build(text: &str) -> Self {
        let mut rows = Vec::with_capacity(text.len() / 32);
        let mut in_hunk = false;
        let (mut old, mut new) = (0u32, 0u32);
        let mut max = 0u32;
        for line in text.split('\n') {
            if let Some(rest) = line.strip_prefix("@@") {
                if let Some((o, n)) = parse_hunk_starts(rest) {
                    old = o;
                    new = n;
                    in_hunk = true;
                }
                rows.push((0, 0));
                continue;
            }
            if line.starts_with("diff --git") {
                in_hunk = false;
                rows.push((0, 0));
                continue;
            }
            if !in_hunk {
                rows.push((0, 0));
                continue;
            }
            match line.as_bytes().first() {
                Some(b' ') => {
                    rows.push((old, new));
                    max = max.max(old).max(new);
                    old += 1;
                    new += 1;
                }
                Some(b'-') => {
                    rows.push((old, 0));
                    max = max.max(old);
                    old += 1;
                }
                Some(b'+') => {
                    rows.push((0, new));
                    max = max.max(new);
                    new += 1;
                }
                // `\ No newline at end of file`, or a chrome row.
                _ => rows.push((0, 0)),
            }
        }
        let digits = digit_count(max);
        Self { rows, digits }
    }

    /// Numbers for 0-based logical `line`; `None` per side where blank.
    pub fn numbers(&self, line: usize) -> (Option<u32>, Option<u32>) {
        match self.rows.get(line) {
            Some(&(o, n)) => (nonzero(o), nonzero(n)),
            None => (None, None),
        }
    }

    /// Width one number column needs.
    pub fn digits(&self) -> usize {
        self.digits
    }

    /// Width of the whole gutter: two columns and the space between.
    pub fn width(&self) -> usize {
        self.digits * 2 + 1
    }

    /// Render `line`'s pair right-aligned into the gutter's width.
    pub fn render(&self, line: usize) -> String {
        let (o, n) = self.numbers(line);
        let d = self.digits;
        let cell = |v: Option<u32>| v.map(|v| v.to_string()).unwrap_or_default();
        format!("{:>d$} {:>d$}", cell(o), cell(n))
    }
}

fn nonzero(v: u32) -> Option<u32> {
    (v != 0).then_some(v)
}

fn digit_count(v: u32) -> usize {
    v.checked_ilog10().map_or(1, |l| l as usize + 1)
}

/// `-a[,b] +c[,d] @@ …` after the leading `@@` → `(a, c)`.
fn parse_hunk_starts(rest: &str) -> Option<(u32, u32)> {
    let mut it = rest.split_whitespace();
    let old = it.next()?.strip_prefix('-')?;
    let new = it.next()?.strip_prefix('+')?;
    let start = |s: &str| s.split(',').next()?.parse::<u32>().ok();
    Some((start(old)?, start(new)?))
}

#[cfg(test)]
mod tests {
    use super::*;

    const STREAM: &str = "\
▾ UNSTAGED  (2)
▾ src/a.rs   +1 / -1
diff --git a/src/a.rs b/src/a.rs
index 1111111..2222222 100644
--- a/src/a.rs
+++ b/src/a.rs
@@ -10,3 +12,4 @@ fn ctx()
 fn keep() {}
-let x = 1;
+let x = 2;
+let y = 3;
\\ No newline at end of file
▾ src/b.py   +1 / -0
diff --git a/src/b.py b/src/b.py
index 3333333..4444444 100644
--- a/src/b.py
+++ b/src/b.py
@@ -1,2 +1,3 @@
 def f():
+    return 1
";

    fn nums(g: &DiffGutter, line: usize) -> (Option<u32>, Option<u32>) {
        g.numbers(line)
    }

    /// Every row's pair follows from its hunk header and its side.
    #[test]
    fn numbers_follow_the_hunk_header_and_the_side() {
        let g = DiffGutter::build(STREAM);
        // chrome + git metadata: blank
        for line in 0..=5 {
            assert_eq!(nums(&g, line), (None, None), "line {line}");
        }
        assert_eq!(nums(&g, 6), (None, None), "the @@ row itself is blank");
        assert_eq!(nums(&g, 7), (Some(10), Some(12)), "context: both");
        assert_eq!(nums(&g, 8), (Some(11), None), "removed: old only");
        assert_eq!(nums(&g, 9), (None, Some(13)), "added: new only");
        assert_eq!(nums(&g, 10), (None, Some(14)), "second add");
        assert_eq!(nums(&g, 11), (None, None), "no-newline marker");
        assert_eq!(nums(&g, 12), (None, None), "chrome inside region");
        assert_eq!(nums(&g, 18), (Some(1), Some(1)), "second file restarts");
        assert_eq!(nums(&g, 19), (None, Some(2)));
    }

    /// A chrome row inside a hunk must not consume a line number, or
    /// every row after it would be off by one.
    #[test]
    fn a_chrome_row_inside_a_hunk_does_not_advance() {
        let text = "@@ -1,2 +1,2 @@\n line\n▾ chrome\n line\n";
        let g = DiffGutter::build(text);
        assert_eq!(nums(&g, 1), (Some(1), Some(1)));
        assert_eq!(nums(&g, 2), (None, None));
        assert_eq!(nums(&g, 3), (Some(2), Some(2)));
    }

    /// Inside a hunk `---`/`+++` are rows, not headers.
    #[test]
    fn removed_and_added_rows_that_look_like_headers_still_count() {
        let text = "@@ -1,1 +1,1 @@\n--- x\n+++ y\n";
        let g = DiffGutter::build(text);
        assert_eq!(nums(&g, 1), (Some(1), None));
        assert_eq!(nums(&g, 2), (None, Some(1)));
    }

    /// Width tracks the widest number, so a 12,000-line file gets a
    /// five-digit column and a short one does not pay for it.
    #[test]
    fn width_tracks_the_widest_number() {
        let short = DiffGutter::build("@@ -1,1 +1,1 @@\n x\n");
        assert_eq!(short.digits(), 1);
        assert_eq!(short.width(), 3);
        let deep = DiffGutter::build("@@ -12000,1 +12000,1 @@\n x\n");
        assert_eq!(deep.digits(), 5);
        assert_eq!(deep.render(1), "12000 12000");
        assert_eq!(deep.render(0), "           ");
    }

    /// A hunk header with a bare count (`+1 @@`, no `,n`) still parses.
    #[test]
    fn bare_counts_parse() {
        assert_eq!(parse_hunk_starts(" -7 +9 @@"), Some((7, 9)));
        assert_eq!(parse_hunk_starts(" -7,2 +9,3 @@"), Some((7, 9)));
        assert_eq!(parse_hunk_starts(" garbage"), None);
    }
}
