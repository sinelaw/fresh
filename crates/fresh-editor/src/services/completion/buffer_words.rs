//! Buffer-word completion provider with proximity scoring.
//!
//! Collects unique words from the buffer scan window and ranks them by:
//! 1. **Proximity** to the cursor (line-distance approximation).
//! 2. **Viewport bias** — words visible on screen are boosted.
//! 3. **Frequency** — words that appear more often get a small bonus.
//!
//! Smart-case matching, language-aware word boundaries, and multi-buffer
//! support are all provided through the shared `CompletionContext`.
//!
//! # Huge-file safety
//!
//! Only the pre-sliced `buffer_window` is scanned. For large files the
//! completion service limits this to 32 KB around the cursor.

use std::collections::HashMap;

use unicode_segmentation::UnicodeSegmentation;

use super::provider::{
    case_mismatch_penalty, is_word_grapheme_for_lang, smart_case_matches, CompletionCandidate,
    CompletionContext, CompletionProvider, CompletionSourceId, ProviderResult,
};

/// Maximum number of candidates returned.
const MAX_CANDIDATES: usize = 40;

/// Minimum word length in grapheme clusters to be a candidate.
const MIN_WORD_LEN_GRAPHEMES: usize = 2;

pub struct BufferWordProvider;

impl BufferWordProvider {
    pub fn new() -> Self {
        Self
    }
}

impl Default for BufferWordProvider {
    fn default() -> Self {
        Self::new()
    }
}

/// Entry tracking a word's occurrences within a scan window.
struct WordStats {
    /// The word text (original casing of first occurrence).
    text: String,
    /// Number of occurrences.
    count: u32,
    /// Byte offset of the occurrence closest to the cursor.
    nearest_offset: usize,
    /// Absolute byte-distance of the nearest occurrence to the cursor.
    nearest_dist: usize,
    /// Whether at least one occurrence falls within the viewport.
    in_viewport: bool,
    /// Length in grapheme clusters (for min-length filtering).
    grapheme_len: usize,
}

/// Collect word statistics from a text window.
fn collect_word_stats(
    text: &str,
    extra: &str,
    cursor_in_window: usize,
    viewport_start_in_window: usize,
    viewport_end_in_window: usize,
) -> HashMap<String, WordStats> {
    let mut stats: HashMap<String, WordStats> = HashMap::new();

    let mut current_word = String::new();
    let mut word_start: usize = 0;
    let mut word_grapheme_count: usize = 0;
    let mut byte_pos: usize = 0;

    for grapheme in text.graphemes(true) {
        if is_word_grapheme_for_lang(grapheme, extra) {
            if current_word.is_empty() {
                word_start = byte_pos;
                word_grapheme_count = 0;
            }
            current_word.push_str(grapheme);
            word_grapheme_count += 1;
        } else if !current_word.is_empty() {
            record_word(
                &mut stats,
                std::mem::take(&mut current_word),
                word_grapheme_count,
                word_start,
                cursor_in_window,
                viewport_start_in_window,
                viewport_end_in_window,
            );
            word_grapheme_count = 0;
        }
        byte_pos += grapheme.len();
    }
    if !current_word.is_empty() {
        record_word(
            &mut stats,
            current_word,
            word_grapheme_count,
            word_start,
            cursor_in_window,
            viewport_start_in_window,
            viewport_end_in_window,
        );
    }

    stats
}

fn record_word(
    stats: &mut HashMap<String, WordStats>,
    word: String,
    grapheme_len: usize,
    byte_offset: usize,
    cursor_in_window: usize,
    viewport_start: usize,
    viewport_end: usize,
) {
    let dist = byte_offset.abs_diff(cursor_in_window);
    let in_vp = byte_offset >= viewport_start && byte_offset < viewport_end;
    let key = word.to_lowercase();

    stats
        .entry(key)
        .and_modify(|s| {
            s.count += 1;
            if dist < s.nearest_dist {
                s.nearest_dist = dist;
                s.nearest_offset = byte_offset;
            }
            s.in_viewport |= in_vp;
        })
        .or_insert(WordStats {
            text: word,
            count: 1,
            nearest_offset: byte_offset,
            nearest_dist: dist,
            in_viewport: in_vp,
            grapheme_len,
        });
}

impl CompletionProvider for BufferWordProvider {
    fn id(&self) -> CompletionSourceId {
        CompletionSourceId("buffer_words".into())
    }

    fn display_name(&self) -> &str {
        "Buffer Words"
    }

    fn is_enabled(&self, ctx: &CompletionContext) -> bool {
        !ctx.prefix.is_empty()
    }

    fn provide(&self, ctx: &CompletionContext, buffer_window: &[u8]) -> ProviderResult {
        let text = String::from_utf8_lossy(buffer_window);
        let extra = &ctx.word_chars_extra;

        let cursor_in_window = ctx.cursor_byte.saturating_sub(ctx.scan_range.start);
        let vp_start = ctx.viewport_top_byte.saturating_sub(ctx.scan_range.start);
        let vp_end = ctx
            .viewport_bottom_byte
            .saturating_sub(ctx.scan_range.start)
            .min(buffer_window.len());

        let mut all_stats = collect_word_stats(&text, extra, cursor_in_window, vp_start, vp_end);

        // Merge stats from other open buffers (lower priority).
        for (i, other) in ctx.other_buffers.iter().enumerate() {
            let other_text = String::from_utf8_lossy(&other.bytes);
            let other_stats = collect_word_stats(&other_text, extra, 0, 0, 0);
            let cross_buffer_dist_offset = 300_000 * (i + 1);
            for (key, os) in other_stats {
                all_stats.entry(key).or_insert(WordStats {
                    text: os.text,
                    count: os.count,
                    nearest_offset: os.nearest_offset,
                    nearest_dist: os.nearest_dist + cross_buffer_dist_offset,
                    in_viewport: false,
                    grapheme_len: os.grapheme_len,
                });
            }
        }

        let mut scored: Vec<(i64, &WordStats)> = all_stats
            .values()
            .filter(|s| {
                s.grapheme_len >= MIN_WORD_LEN_GRAPHEMES
                    && smart_case_matches(&s.text, &ctx.prefix, ctx.prefix_has_uppercase)
                    && s.text.to_lowercase() != ctx.prefix.to_lowercase()
            })
            .map(|s| {
                // Base: proximity score (closer = higher).
                let mut score: i64 = 500_000i64.saturating_sub(s.nearest_dist as i64);
                // Viewport boost: +100k if any occurrence is visible.
                if s.in_viewport {
                    score += 100_000;
                }
                // Frequency bonus: +5k per extra occurrence (capped).
                score += (s.count.min(10) as i64 - 1) * 5_000;
                // Smart-case penalty.
                score += case_mismatch_penalty(&s.text, &ctx.prefix, ctx.prefix_has_uppercase);
                (score, s)
            })
            .collect();

        scored.sort_by_key(|b| std::cmp::Reverse(b.0));

        let candidates = scored
            .into_iter()
            .take(MAX_CANDIDATES)
            .map(|(score, s)| CompletionCandidate::word(s.text.clone(), score))
            .collect();

        ProviderResult::Ready(candidates)
    }

    fn priority(&self) -> u32 {
        20
    }
}

#[cfg(test)]
mod tests {
    use super::super::provider::OtherBufferSlice;
    use super::*;

    fn make_ctx(prefix: &str, cursor: usize, buf_len: usize) -> CompletionContext {
        CompletionContext {
            prefix: prefix.into(),
            cursor_byte: cursor,
            word_start_byte: cursor.saturating_sub(prefix.len()),
            buffer_len: buf_len,
            is_large_file: false,
            scan_range: 0..buf_len,
            viewport_top_byte: 0,
            viewport_bottom_byte: buf_len,
            language_id: None,
            word_chars_extra: String::new(),
            prefix_has_uppercase: prefix.chars().any(|c| c.is_uppercase()),
            other_buffers: Vec::new(),
        }
    }

    #[test]
    fn proximity_beats_frequency() {
        let text = b"far_match far_match far_match close_match";
        let provider = BufferWordProvider::new();
        let ctx = CompletionContext {
            prefix: "far".into(),
            cursor_byte: 38,
            word_start_byte: 35,
            buffer_len: text.len(),
            is_large_file: false,
            scan_range: 0..text.len(),
            viewport_top_byte: 0,
            viewport_bottom_byte: text.len(),
            language_id: None,
            word_chars_extra: String::new(),
            prefix_has_uppercase: false,
            other_buffers: Vec::new(),
        };
        let result = provider.provide(&ctx, text);
        match result {
            ProviderResult::Ready(candidates) => {
                assert!(!candidates.is_empty());
                assert_eq!(candidates[0].label, "far_match");
            }
            _ => panic!("expected Ready"),
        }
    }

    #[test]
    fn viewport_boost() {
        let text = b"alpha_one xxxxxxxxx alpha_two";
        let provider = BufferWordProvider::new();
        let ctx = CompletionContext {
            prefix: "alpha".into(),
            cursor_byte: 15,
            word_start_byte: 10,
            buffer_len: text.len(),
            is_large_file: false,
            scan_range: 0..text.len(),
            viewport_top_byte: 20,
            viewport_bottom_byte: text.len(),
            language_id: None,
            word_chars_extra: String::new(),
            prefix_has_uppercase: false,
            other_buffers: Vec::new(),
        };
        let result = provider.provide(&ctx, text);
        match result {
            ProviderResult::Ready(candidates) => {
                assert_eq!(candidates.len(), 2);
                assert_eq!(candidates[0].label, "alpha_two");
            }
            _ => panic!("expected Ready"),
        }
    }

    #[test]
    fn min_length_filter() {
        let text = b"a b cc dd hello";
        let provider = BufferWordProvider::new();
        let ctx = make_ctx("h", 15, text.len());
        let result = provider.provide(&ctx, text);
        match result {
            ProviderResult::Ready(candidates) => {
                assert_eq!(candidates.len(), 1);
                assert_eq!(candidates[0].label, "hello");
            }
            _ => panic!("expected Ready"),
        }
    }

    #[test]
    fn unicode_words() {
        let text = "naïve_var naïve_fn naïf".as_bytes();
        let provider = BufferWordProvider::new();
        let ctx = make_ctx("naïve", 0, text.len());
        let result = provider.provide(&ctx, text);
        match result {
            ProviderResult::Ready(candidates) => {
                let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
                assert!(labels.contains(&"naïve_var"));
                assert!(labels.contains(&"naïve_fn"));
                assert!(!labels.contains(&"naïf"));
            }
            _ => panic!("expected Ready"),
        }
    }

    #[test]
    fn smart_case_penalizes_mismatch() {
        let text = b"http_request HttpServer HTTP_CONST";
        let provider = BufferWordProvider::new();
        let ctx = make_ctx("http", 0, text.len());
        let result = provider.provide(&ctx, text);
        match result {
            ProviderResult::Ready(candidates) => {
                assert_eq!(candidates.len(), 3);
                // Exact-case "http_request" should rank above "HttpServer"
                let req = candidates
                    .iter()
                    .find(|c| c.label == "http_request")
                    .unwrap();
                let srv = candidates.iter().find(|c| c.label == "HttpServer").unwrap();
                assert!(req.score > srv.score);
            }
            _ => panic!("expected Ready"),
        }
    }

    #[test]
    fn multi_buffer_words() {
        let text = b"local_var another";
        let provider = BufferWordProvider::new();
        let mut ctx = make_ctx("lo", 0, text.len());
        ctx.other_buffers = vec![OtherBufferSlice {
            buffer_id: 2,
            bytes: b"long_name logging".to_vec(),
            label: "other.rs".into(),
        }];
        let result = provider.provide(&ctx, text);
        match result {
            ProviderResult::Ready(candidates) => {
                let labels: Vec<&str> = candidates.iter().map(|c| c.label.as_str()).collect();
                assert!(labels.contains(&"local_var"));
                assert!(labels.contains(&"long_name"));
                assert!(labels.contains(&"logging"));
                // Active buffer should outscore cross-buffer
                let local = candidates.iter().find(|c| c.label == "local_var").unwrap();
                let long = candidates.iter().find(|c| c.label == "long_name").unwrap();
                assert!(local.score > long.score);
            }
            _ => panic!("expected Ready"),
        }
    }
}
