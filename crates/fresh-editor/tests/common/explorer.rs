//! Rendered-output readers for File Explorer tests.
//!
//! CONTRIBUTING Testing 2 keeps e2e tests off model/view accessors, so
//! explorer tests identify entries by the text on screen. Pulling a name out
//! of a row is fiddly enough — tree connectors, the selection marker, status
//! glyphs — that every test doing it should do it the same way.

/// The first non-whitespace token at/after `prefix` on the line, e.g. for
/// prefix "file_" on "│▌   file_11.txt  ●" returns "file_11.txt".
pub fn token_after(line: &str, prefix: &str) -> Option<String> {
    let idx = line.find(prefix)?;
    let tok: String = line[idx..]
        .chars()
        .take_while(|c| !c.is_whitespace())
        .collect();
    Some(tok)
}

/// The `prefix`-token on the first screen line that also contains `marker`.
pub fn token_on_line_with(screen: &str, marker: &str, prefix: &str) -> Option<String> {
    screen
        .lines()
        .find(|l| l.contains(marker))
        .and_then(|l| token_after(l, prefix))
}

/// The `prefix`-token on the first *file-explorer body* row (those start with
/// the explorer's left border `│`), so we read the top tree entry rather than
/// a tab title or status-bar mention of the same name.
pub fn first_explorer_token(screen: &str, prefix: &str) -> Option<String> {
    screen
        .lines()
        .filter(|l| l.starts_with('│'))
        .find_map(|l| token_after(l, prefix))
}
