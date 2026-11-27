//! Grapheme clusters and their widths, measured as the terminal editor
//! measures a cell (`fresh_core::display_width::str_width` of a
//! cluster, its `unicode-segmentation` extended clusters): what the web
//! client wraps, draws and moves the caret by.
//!
//! A cluster's width is its columns: 0 (a lone combining mark, a zero
//! width space), 1 or 2. A control character is a cluster of its own; a
//! tab's width depends on the column it starts at, so the caller gives it.

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

/// One cluster: its length in UTF-8 bytes and in UTF-16 code units, and
/// its width in columns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cluster {
    pub bytes: usize,
    pub units: usize,
    pub width: usize,
}

/// The clusters of `text`, in order. The last one may continue past the
/// text's end; a caller reading a line in slices starts the next slice at
/// its start.
pub fn clusters(text: &str) -> Vec<Cluster> {
    text.graphemes(true)
        .map(|g| Cluster {
            bytes: g.len(),
            units: g.chars().map(char::len_utf16).sum(),
            width: g.width(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn widths(text: &str) -> Vec<(usize, usize)> {
        clusters(text).iter().map(|c| (c.bytes, c.width)).collect()
    }

    #[test]
    fn ascii_is_one_cluster_a_character() {
        assert_eq!(widths("ab c"), vec![(1, 1), (1, 1), (1, 1), (1, 1)]);
    }

    #[test]
    fn a_combining_mark_joins_its_base() {
        // e + COMBINING ACUTE ACCENT: one cluster, one column.
        assert_eq!(widths("e\u{301}x"), vec![(3, 1), (1, 1)]);
    }

    #[test]
    fn wide_characters_are_two_columns() {
        assert_eq!(widths("你好"), vec![(3, 2), (3, 2)]);
        assert_eq!(widths("한"), vec![(3, 2)]);
    }

    #[test]
    fn hangul_jamo_join_into_one_syllable() {
        // L + V + T jamo: one cluster, as wide as the syllable.
        assert_eq!(widths("\u{1100}\u{1161}\u{11A8}"), vec![(9, 2)]);
    }

    #[test]
    fn emoji_sequences_are_one_cluster() {
        let family = "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}";
        assert_eq!(widths(family), vec![(family.len(), 2)]);
        let flags = "\u{1F1EB}\u{1F1F7}\u{1F1FA}\u{1F1F8}";
        assert_eq!(widths(flags), vec![(8, 2), (8, 2)]);
    }

    #[test]
    fn utf16_units_count_surrogate_pairs() {
        let c = clusters("\u{1F600}é");
        assert_eq!(c[0].units, 2);
        assert_eq!(c[1].units, 1);
    }

    #[test]
    fn control_characters_stand_alone() {
        // The client gives a tab its width by column, and draws no cell for
        // the carriage return of a CRLF line ending.
        let c = clusters("a\tb\re\u{301}");
        assert_eq!(
            c.iter().map(|c| c.bytes).collect::<Vec<_>>(),
            vec![1, 1, 1, 1, 3]
        );
    }

    proptest! {
        /// The clusters cover the text exactly, each one on a character
        /// boundary, and their widths sum to the text's width.
        #[test]
        fn clusters_cover_the_text(text in "\\PC{0,40}") {
            let c = clusters(&text);
            prop_assert_eq!(c.iter().map(|c| c.bytes).sum::<usize>(), text.len());
            prop_assert_eq!(c.iter().map(|c| c.units).sum::<usize>(), text.encode_utf16().count());
            let mut at = 0;
            for x in &c {
                prop_assert!(text.is_char_boundary(at));
                prop_assert_eq!(x.width, text[at..at + x.bytes].width());
                at += x.bytes;
            }
        }

        /// Read from a cluster boundary, a slice's clusters before its last
        /// are the whole text's: what lets the client read a line in slices.
        #[test]
        fn a_slice_agrees_but_for_its_last(text in "[a\u{301}\u{200D}\u{1F468}\u{1F1EB}\u{1100}\u{1161} \t]{0,30}", cut in 0usize..40) {
            let whole = clusters(&text);
            let mut cut = cut.min(text.len());
            while !text.is_char_boundary(cut) { cut -= 1; }
            let part = clusters(&text[..cut]);
            let keep = part.len().saturating_sub(1);
            prop_assert_eq!(&part[..keep], &whole[..keep]);
        }
    }
}
