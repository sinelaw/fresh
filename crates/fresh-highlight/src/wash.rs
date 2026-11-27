//! Rows a diff category washes, filled to their end.

use std::ops::Range;

use crate::Category;

/// A sink for one row's spans that fills the gaps of a washed row with its
/// wash.
///
/// The diff categories ([`Category::bg_extends_to_line_end`]) are a
/// whole-row background, not styling of the scoped text. A diff grammar
/// mostly scopes a `+`/`-` row as one span, but not every washed row: a
/// hunk header scopes `@@ -1,2 +3,4 @@` as `meta.diff.range` and leaves the
/// section name `git diff` appends after it unscoped, and the source tokens
/// of a diff row an embedded parser colours leave gaps between them. So the
/// rule is per row, not per scope: when the span covering the row's start
/// is a wash, every part of the row no span claims gets that wash. A span
/// that does claim a part keeps its own categories. Rows whose first span
/// is no wash pass through untouched.
pub struct RowWash<F: FnMut(Range<usize>, Category, Option<Category>)> {
    row: Range<usize>,
    /// The row's wash, once the span covering its start decided it.
    wash: Option<Category>,
    /// Whether the span covering the row's start has been seen.
    decided: bool,
    /// End of the furthest span emitted.
    covered: usize,
    sink: F,
}

impl<F: FnMut(Range<usize>, Category, Option<Category>)> RowWash<F> {
    /// A row spanning `row`, its spans passed on to `sink` as (range,
    /// category, background category).
    pub fn new(row: Range<usize>, sink: F) -> Self {
        RowWash {
            covered: row.start,
            row,
            wash: None,
            decided: false,
            sink,
        }
    }

    /// The next span of the row, after those emitted before it.
    pub fn emit(&mut self, range: Range<usize>, category: Category, background: Option<Category>) {
        // Decided by the span that covers the row's start, so an empty span
        // there decides nothing.
        if !self.decided && range.end > self.row.start {
            self.decided = true;
            if range.start <= self.row.start && category.bg_extends_to_line_end() {
                self.wash = Some(category);
            }
        }
        if let Some(wash) = self.wash {
            if range.start > self.covered {
                (self.sink)(self.covered..range.start, wash, None);
            }
        }
        self.covered = self.covered.max(range.end);
        (self.sink)(range, category, background);
    }

    /// Fill the row's unclaimed end, once its spans have all been emitted.
    pub fn finish(mut self) {
        if let Some(wash) = self.wash {
            if self.covered < self.row.end {
                (self.sink)(self.covered..self.row.end, wash, None);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn washed(
        row: Range<usize>,
        spans: &[(Range<usize>, Category)],
    ) -> Vec<(Range<usize>, Category)> {
        let mut out = Vec::new();
        let mut wash = RowWash::new(row, |range, category, _| out.push((range, category)));
        for (range, category) in spans {
            wash.emit(range.clone(), *category, None);
        }
        wash.finish();
        out
    }

    #[test]
    fn a_washed_row_has_its_gaps_and_end_filled() {
        use Category::{Changed, Keyword};
        assert_eq!(
            washed(0..20, &[(0..5, Changed), (8..10, Keyword)]),
            [
                (0..5, Changed),
                (5..8, Changed),
                (8..10, Keyword),
                (10..20, Changed)
            ]
        );
    }

    #[test]
    fn a_row_not_starting_in_a_wash_is_untouched() {
        use Category::{Inserted, Keyword};
        let spans = [(0..2, Keyword), (4..6, Inserted)];
        assert_eq!(washed(0..10, &spans), spans);
    }
}
