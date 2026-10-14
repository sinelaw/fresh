//! The preview a drag-and-drop move plays before it happens.
//!
//! While a row is dragged over another directory (the file explorer) or
//! folder (a plugin tree, such as the orchestrator dock), the tree shows the
//! move a drop there would make: an empty row opens where the entry would
//! land, then its name is *typed* into that row a character at a time, in the
//! diff colour for an addition, while the same characters ghost out of the
//! original row in the diff colour for a removal. Moving on to another target
//! drops the preview and starts it over there. Once dropped, the move goes on
//! when the name is across — to the explorer's confirmation, or to the
//! plugin's filing.
//!
//! Everything here is the part the two surfaces share and nothing that
//! belongs to either: the clock ([`MovePreview`]), where the empty rows go in
//! a flat list of rows ([`Splice`]), and how a name splits into its typed and
//! untyped halves ([`typed_split`]). Each surface draws those halves in its
//! own row shape, with the theme keys below.

use std::time::{Duration, Instant};

/// The ground of a character typed into the new row: the diff addition.
pub const ADDED_BG: &str = "editor.diff_add_bg";
/// The ground of the character typed last — the "caret" of the typing.
pub const ADDED_CARET_BG: &str = "editor.diff_add_highlight_bg";
/// The ground of a character that has left the original row: the diff
/// removal.
pub const GHOST_BG: &str = "editor.diff_remove_bg";

/// How long the empty row stands open before the first character arrives,
/// so the opening reads as its own step.
const OPEN: Duration = Duration::from_millis(150);
/// How long the typing takes, whatever the name's length: a long name types
/// faster, a short one slower, and every preview reads in the same time. The
/// count is read off the clock each frame rather than stepped per frame, so
/// a frame that comes late skips characters instead of slowing the typing.
const TYPING: Duration = Duration::from_millis(500);
/// How long the finished picture holds before the move goes on.
const HOLD: Duration = Duration::from_millis(200);

/// The clock of one move's preview: how many characters have been typed by a
/// given instant, and when it is over.
///
/// Every name of a multi-entry move types at once, so one count serves them
/// all; a shorter name is simply finished sooner ([`typed_split`] clamps).
#[derive(Debug, Clone)]
pub struct MovePreview {
    started: Instant,
    /// The longest name's length in characters — the typing's length.
    chars: usize,
}

impl MovePreview {
    pub fn new<'a>(names: impl IntoIterator<Item = &'a str>, now: Instant) -> Self {
        use unicode_segmentation::UnicodeSegmentation;
        let chars = names
            .into_iter()
            .map(|n| n.graphemes(true).count())
            .max()
            .unwrap_or(0);
        Self {
            started: now,
            chars,
        }
    }

    /// When character `k` (1-based) arrives, after the row has opened: the
    /// first at once, the rest evenly through [`TYPING`], so a one-character
    /// name does not sit in an empty row for the whole of it.
    fn char_at(&self, k: usize) -> Duration {
        let n = self.chars.max(1) as u128;
        // Rounded up, so `typed` at this instant already counts it.
        Duration::from_nanos((TYPING.as_nanos() * (k as u128 - 1)).div_ceil(n) as u64)
    }

    /// How many characters have been typed at `now`: none while the row is
    /// opening, then the first, then the rest evenly through [`TYPING`].
    pub fn typed(&self, now: Instant) -> usize {
        let Some(t) = now
            .saturating_duration_since(self.started)
            .checked_sub(OPEN)
        else {
            return 0;
        };
        let n = self.chars as u128;
        ((t.as_nanos() * n / TYPING.as_nanos()) as usize + 1).min(self.chars)
    }

    /// The preview has played out: the name is across and the hold is over.
    pub fn finished(&self, now: Instant) -> bool {
        now.saturating_duration_since(self.started) >= OPEN + TYPING + HOLD
    }

    /// When the picture next changes — the next character, or the end —
    /// for the loop to wake at. `None` once it is finished.
    pub fn next_deadline(&self, now: Instant) -> Option<Instant> {
        if self.finished(now) {
            return None;
        }
        let end = OPEN + TYPING + HOLD;
        let typed = self.typed(now);
        let next = match now.saturating_duration_since(self.started) < OPEN {
            true => OPEN,
            false if typed < self.chars => OPEN + self.char_at(typed + 1),
            false => end,
        };
        Some(self.started + next.min(end))
    }
}

/// `name` split after its first `typed` characters (clamped to its length):
/// what has been typed, and what has not. A character is a grapheme — what
/// the reader sees as one — so an accent or an emoji sequence is typed whole.
pub fn typed_split(name: &str, typed: usize) -> (&str, &str) {
    use unicode_segmentation::UnicodeSegmentation;
    let at = name
        .grapheme_indices(true)
        .nth(typed)
        .map(|(i, _)| i)
        .unwrap_or(name.len());
    name.split_at(at)
}

/// The typed half of `name` for the new row, as the runs it is drawn in: the
/// characters before the last on [`ADDED_BG`], the last on
/// [`ADDED_CARET_BG`]. Empty while nothing has been typed.
pub fn typed_runs(name: &str, typed: usize) -> Vec<(&str, &'static str)> {
    use unicode_segmentation::UnicodeSegmentation;
    let (done, _) = typed_split(name, typed);
    let Some((last_at, _)) = done.grapheme_indices(true).last() else {
        return Vec::new();
    };
    // The caret stays only while characters are still arriving: a finished
    // name is one colour.
    let still_typing = done.len() < name.len();
    match still_typing {
        true => {
            let (head, last) = done.split_at(last_at);
            [(head, ADDED_BG), (last, ADDED_CARET_BG)]
                .into_iter()
                .filter(|(t, _)| !t.is_empty())
                .collect()
        }
        false => vec![(done, ADDED_BG)],
    }
}

/// Where a preview's empty rows go in a flat list of rows, and how an index
/// into the list with them in it maps back to the list without them.
///
/// `at[k]` is the row the `k`th empty row goes before — the list's length to
/// go last — in ascending order. Previews insert a handful of rows into lists
/// of any length, so nothing here is proportional to the list.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Splice {
    at: Vec<usize>,
}

/// What a row of the spliced list is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Spot {
    /// The original list's row `i`.
    Row(usize),
    /// The `k`th empty row.
    Inserted(usize),
}

impl Splice {
    /// Empty rows before each of `at` (sorted here; equal positions keep
    /// their order, so the `k`s follow the caller's).
    pub fn new(mut at: Vec<usize>) -> Self {
        at.sort();
        Self { at }
    }

    /// How many rows there are with the empty ones in.
    pub fn len(&self, rows: usize) -> usize {
        rows + self.at.len()
    }

    /// What spliced row `j` is.
    pub fn spot(&self, j: usize) -> Spot {
        let mut before = 0;
        for (k, &at) in self.at.iter().enumerate() {
            let pos = at + k;
            if pos == j {
                return Spot::Inserted(k);
            }
            if pos < j {
                before += 1;
            }
        }
        Spot::Row(j - before)
    }

    /// Where the original row `i` is in the spliced list.
    pub fn index_of(&self, i: usize) -> usize {
        i + self.at.iter().filter(|&&at| at <= i).count()
    }

    /// Where the `k`th empty row is in the spliced list.
    pub fn inserted_index(&self, k: usize) -> usize {
        self.at[k] + k
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typing_opens_first_then_spreads_the_name_over_the_typing_time() {
        let t0 = Instant::now();
        let p = MovePreview::new(["abcde"], t0);
        let ms = Duration::from_millis;
        assert_eq!(p.typed(t0), 0);
        assert_eq!(p.typed(t0 + OPEN - ms(1)), 0);
        assert_eq!(p.typed(t0 + OPEN), 1, "the first arrives as the row opens");
        assert_eq!(p.typed(t0 + OPEN + ms(99)), 1);
        assert_eq!(p.typed(t0 + OPEN + ms(100)), 2);
        assert_eq!(p.typed(t0 + OPEN + ms(250)), 3);
        assert_eq!(p.typed(t0 + OPEN + ms(400)), 5);
        assert!(!p.finished(t0 + OPEN + TYPING));
        assert!(p.finished(t0 + OPEN + TYPING + HOLD));
    }

    #[test]
    fn every_name_takes_the_same_time_whatever_its_length() {
        let t0 = Instant::now();
        for len in [1, 9, 200] {
            let name = "x".repeat(len);
            let p = MovePreview::new([name.as_str()], t0);
            assert_eq!(p.typed(t0 + OPEN), 1);
            assert_eq!(p.typed(t0 + OPEN + TYPING), len);
            assert!(!p.finished(t0 + OPEN + TYPING));
            assert!(p.finished(t0 + OPEN + TYPING + HOLD));
        }
    }

    #[test]
    fn a_late_frame_skips_ahead_rather_than_slowing_down() {
        let t0 = Instant::now();
        let p = MovePreview::new(["abcde"], t0);
        // No frame since the row opened: it shows what the clock says.
        assert_eq!(p.typed(t0 + OPEN + Duration::from_millis(300)), 4);
    }

    #[test]
    fn deadlines_walk_every_change_and_stop_at_the_end() {
        let t0 = Instant::now();
        let p = MovePreview::new(["ab"], t0);
        let half = TYPING / 2;
        assert_eq!(p.next_deadline(t0), Some(t0 + OPEN));
        assert_eq!(p.next_deadline(t0 + OPEN), Some(t0 + OPEN + half));
        assert_eq!(
            p.next_deadline(t0 + OPEN + half),
            Some(t0 + OPEN + TYPING + HOLD)
        );
        assert_eq!(p.next_deadline(t0 + OPEN + TYPING + HOLD), None);
        // Every deadline is an instant the count has changed by.
        let mut at = t0;
        for k in 1..=2 {
            at = p.next_deadline(at).unwrap();
            assert_eq!(p.typed(at), k);
        }
    }

    #[test]
    fn a_character_is_what_the_reader_sees_as_one() {
        // "e" + combining acute, then "x": two characters, three chars.
        let name = "e\u{301}x";
        assert_eq!(typed_split(name, 1), ("e\u{301}", "x"));
        assert_eq!(typed_runs(name, 1), vec![("e\u{301}", ADDED_CARET_BG)]);
        let t0 = Instant::now();
        assert!(MovePreview::new([name], t0).typed(t0 + OPEN + TYPING) == 2);
    }

    #[test]
    fn split_and_runs_count_characters_not_bytes() {
        assert_eq!(typed_split("héllo", 2), ("hé", "llo"));
        assert_eq!(typed_split("ab", 9), ("ab", ""));
        assert!(typed_runs("héllo", 0).is_empty());
        assert_eq!(
            typed_runs("héllo", 2),
            vec![("h", ADDED_BG), ("é", ADDED_CARET_BG)]
        );
        assert_eq!(typed_runs("héllo", 1), vec![("h", ADDED_CARET_BG)]);
        assert_eq!(typed_runs("héllo", 5), vec![("héllo", ADDED_BG)]);
    }

    #[test]
    fn splice_maps_both_ways() {
        // Rows a b c d; empty rows before c and at the end.
        let s = Splice::new(vec![4, 2]);
        assert_eq!(s.len(4), 6);
        let spots: Vec<Spot> = (0..6).map(|j| s.spot(j)).collect();
        assert_eq!(
            spots,
            vec![
                Spot::Row(0),
                Spot::Row(1),
                Spot::Inserted(0),
                Spot::Row(2),
                Spot::Row(3),
                Spot::Inserted(1),
            ]
        );
        assert_eq!(
            (0..4).map(|i| s.index_of(i)).collect::<Vec<_>>(),
            vec![0, 1, 3, 4]
        );
        assert_eq!(s.inserted_index(0), 2);
        assert_eq!(s.inserted_index(1), 5);
    }

    #[test]
    fn two_empty_rows_in_one_place_stay_in_order() {
        let s = Splice::new(vec![1, 1]);
        assert_eq!(s.spot(1), Spot::Inserted(0));
        assert_eq!(s.spot(2), Spot::Inserted(1));
        assert_eq!(s.spot(3), Spot::Row(1));
        assert_eq!(s.index_of(1), 3);
    }
}
