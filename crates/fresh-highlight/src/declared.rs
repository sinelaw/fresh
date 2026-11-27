//! Declared regions: the code rows of a document no grammar can parse — a
//! buffer a plugin composed, such as a diff stream or a log — as the plugin
//! declares them (`setSyntaxRegions` in the plugin API, `TsSyntaxRegion`),
//! rather than as a host grammar finds them ([`crate::embedding`]).
//!
//! A document opened in declared-regions mode is plain text of its own. A
//! region is a byte range of whole rows in a language; a row whose first
//! byte lies in a region is handed, its first `prefix` bytes skipped, to the
//! parser of each stream the region names, and coloured by the first. A row
//! outside every region keeps the styling it has and advances no parser.
//! Regions that share a stream continue one parse across the rows between
//! them, so the two sides of a diff hunk interleave and each is still read
//! as the contiguous text it is. The parsers of the [`MAX_STREAMS`] most
//! recently fed streams are kept; a stream fed again after that many others
//! starts afresh. Regions move with edits as span markers whose ends both
//! have right gravity do: text inserted at a region's start goes before it,
//! text inserted at its end goes into it.
//!
//! As natively, regions are kept sorted and disjoint: one that overlaps the
//! region before it (in byte order) is dropped, as is an empty one, and one
//! in a language no syntax claims leaves its rows as they are. Each
//! region's fate is reported ([`RegionStatus`]) for the caller to log.

use std::ops::Range;

/// The stream a region that names none feeds.
pub const DEFAULT_STREAM: u32 = 0;

/// Parsers a declared-regions document carries between rows. A diff panel
/// interleaves the two sides of one hunk and never needs more; anything
/// older is dropped rather than carried in every checkpoint.
pub const MAX_STREAMS: usize = 4;

/// A run of rows a caller declares as code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredRegion {
    /// Byte offset of the first row's first byte.
    pub start: usize,
    /// Byte offset one past the last row's newline.
    pub end: usize,
    /// What the rows are written in: a language token (`py`, `rust`) or a
    /// path (`src/main.rs`, `Makefile`), resolved by
    /// [`crate::embedding::find_embedded_syntax`].
    pub language: String,
    /// Bytes at the start of every row that are not code.
    pub prefix: usize,
    /// The parsers the rows feed, in order; the first colours them. Empty
    /// means [`DEFAULT_STREAM`].
    pub streams: Vec<u32>,
}

/// What became of a declared region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionStatus {
    /// Its rows are parsed in its language.
    Kept,
    /// Kept, but no syntax claims its language: its rows keep the styling
    /// they have and advance no parser.
    UnknownLanguage,
    /// Dropped: it overlaps the region before it in byte order.
    Overlaps,
    /// Dropped: it ends at or before its start.
    Empty,
}

/// A kept region, its language resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Region {
    /// The live bounds: edits move them.
    pub range: Range<usize>,
    /// Index of its syntax in the syntax set; `None` when none claims its
    /// language.
    pub syntax: Option<usize>,
    pub prefix: usize,
    /// Never empty.
    pub streams: Vec<u32>,
}

/// The regions to keep of `declared`, sorted and disjoint, their languages
/// resolved by `language`, and each declared region's status, in the order
/// declared.
pub(crate) fn resolve(
    declared: &[DeclaredRegion],
    mut language: impl FnMut(&str) -> Option<usize>,
) -> (Vec<Region>, Vec<RegionStatus>) {
    let mut order: Vec<usize> = (0..declared.len()).collect();
    order.sort_by_key(|&i| declared[i].start);
    let mut statuses = vec![RegionStatus::Kept; declared.len()];
    let mut regions: Vec<Region> = Vec::new();
    for i in order {
        let region = &declared[i];
        let previous_end = regions.last().map_or(0, |r| r.range.end);
        statuses[i] = if region.end <= region.start {
            RegionStatus::Empty
        } else if region.start < previous_end {
            RegionStatus::Overlaps
        } else {
            let syntax = language(&region.language);
            regions.push(Region {
                range: region.start..region.end,
                syntax,
                prefix: region.prefix,
                streams: if region.streams.is_empty() {
                    vec![DEFAULT_STREAM]
                } else {
                    region.streams.clone()
                },
            });
            match syntax {
                Some(_) => RegionStatus::Kept,
                None => RegionStatus::UnknownLanguage,
            }
        };
    }
    (regions, statuses)
}

/// The region the row starting at byte `row_start` belongs to: the one its
/// first byte lies in.
pub(crate) fn region_at(regions: &[Region], row_start: usize) -> Option<&Region> {
    // Regions are sorted and disjoint, and edits keep them so: their ends
    // ascend.
    let at = regions.partition_point(|r| r.range.end <= row_start);
    regions.get(at).filter(|r| r.range.start <= row_start)
}

/// Move `regions` through an edit replacing `deleted` bytes at `start` with
/// `inserted` bytes. A bound inside the deleted text moves to its start; a
/// bound at the insertion point moves past the inserted text.
pub(crate) fn shift(regions: &mut [Region], start: usize, deleted: usize, inserted: usize) {
    let moved = |bound: usize| {
        let bound = if bound >= start + deleted {
            bound - deleted
        } else {
            bound.min(start)
        };
        if bound >= start {
            bound + inserted
        } else {
            bound
        }
    };
    for region in regions {
        region.range = moved(region.range.start)..moved(region.range.end);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn declared(start: usize, end: usize, language: &str) -> DeclaredRegion {
        DeclaredRegion {
            start,
            end,
            language: language.to_string(),
            prefix: 0,
            streams: Vec::new(),
        }
    }

    #[test]
    fn overlapping_and_empty_regions_are_dropped_and_reported() {
        let (regions, statuses) = resolve(
            &[
                declared(10, 20, "py"),
                declared(0, 10, "py"),
                declared(15, 30, "py"),
                declared(30, 30, "py"),
                declared(30, 40, "nothing"),
            ],
            |language| (language == "py").then_some(7),
        );
        assert_eq!(
            statuses,
            [
                RegionStatus::Kept,
                RegionStatus::Kept,
                RegionStatus::Overlaps,
                RegionStatus::Empty,
                RegionStatus::UnknownLanguage,
            ]
        );
        let ranges: Vec<Range<usize>> = regions.iter().map(|r| r.range.clone()).collect();
        assert_eq!(ranges, [0..10, 10..20, 30..40]);
        assert_eq!(regions[0].streams, [DEFAULT_STREAM]);
        assert_eq!(regions[2].syntax, None);
    }

    #[test]
    fn a_row_belongs_to_the_region_its_first_byte_lies_in() {
        let (regions, _) = resolve(&[declared(5, 10, "x"), declared(10, 12, "x")], |_| None);
        let at = |row| region_at(&regions, row).map(|r| r.range.clone());
        assert_eq!(at(4), None);
        assert_eq!(at(5), Some(5..10));
        assert_eq!(at(9), Some(5..10));
        assert_eq!(at(10), Some(10..12));
        assert_eq!(at(12), None);
    }

    #[test]
    fn regions_follow_edits_with_right_gravity() {
        let (mut regions, _) = resolve(&[declared(5, 10, "x")], |_| None);
        // Inserted at the start: before the region.
        shift(&mut regions, 5, 0, 3);
        assert_eq!(regions[0].range, 8..13);
        // Inserted at the end: into it.
        shift(&mut regions, 13, 0, 2);
        assert_eq!(regions[0].range, 8..15);
        // A deletion over the start pulls it to the deletion's start.
        shift(&mut regions, 6, 4, 0);
        assert_eq!(regions[0].range, 6..11);
        // Replacing all of it leaves it empty, past the inserted text.
        shift(&mut regions, 6, 5, 1);
        assert_eq!(regions[0].range, 7..7);
    }
}
