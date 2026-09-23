//! **The pop-up list both floating lists are made of** — a `Dropdown`'s
//! option list and a `Text` field's suggestion list.
//!
//! The two were separate implementations of the same thing: each had its own
//! highlight arithmetic (one wrapped, one clamped), its own window follow (one
//! followed both ways at render time, the other forward at render time and
//! backward only on a key), and its own scrollbar. This is the one copy of
//! each rule, as pure functions of `(length, highlight, window)`:
//!
//! - [`Nav`] / [`nav_of`] / [`step`]: which keys move the highlight, and where
//!   they move it — ↑/↓ one row, PgUp/PgDn a window, Home/End the ends,
//!   clamped rather than wrapped (a list you are reading should not jump from
//!   its last row to its first under your finger).
//! - [`window`]: the first row shown, moved just far enough that the
//!   highlight is inside the window and clamped so the window never runs past
//!   the end. It is computed **every layout** from the stored offset, so the
//!   highlight is in view whatever moved it — a key, a spec that shrank the
//!   list, a wheel that has since been overtaken by a key.
//! - [`scrollbar`]: which of the window's rows the thumb covers.
//! - [`jump_to_prefix`]: type-to-jump, as every native list does it.

use crossterm::event::{KeyCode, KeyModifiers};

/// A highlight move a pop-up list answers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Nav {
    Up,
    Down,
    PageUp,
    PageDown,
    Home,
    End,
}

/// The move `key` means to an open list, if it means one. Unmodified keys
/// only: a chord belongs to whatever is around the list.
pub fn nav_of(key: crate::keys::Key) -> Option<Nav> {
    if key.mods() != KeyModifiers::NONE {
        return None;
    }
    Some(match key.code() {
        KeyCode::Up => Nav::Up,
        KeyCode::Down => Nav::Down,
        KeyCode::PageUp => Nav::PageUp,
        KeyCode::PageDown => Nav::PageDown,
        KeyCode::Home => Nav::Home,
        KeyCode::End => Nav::End,
        _ => return None,
    })
}

/// Where `nav` takes the highlight in a list of `len` rows shown `page` rows
/// at a time. Clamped at both ends; an empty list stays at 0.
pub fn step(len: usize, highlight: usize, nav: Nav, page: usize) -> usize {
    if len == 0 {
        return 0;
    }
    let last = len - 1;
    let cur = highlight.min(last);
    // One row of overlap on a page, like every other list here, so the eye
    // keeps an anchor across the jump.
    let page = page.saturating_sub(1).max(1);
    match nav {
        Nav::Up => cur.saturating_sub(1),
        Nav::Down => (cur + 1).min(last),
        Nav::PageUp => cur.saturating_sub(page),
        Nav::PageDown => (cur + page).min(last),
        Nav::Home => 0,
        Nav::End => last,
    }
}

/// The window a list of `len` rows shows: at most `max_visible` rows, starting
/// at the stored `scroll` moved just enough to show `highlight` (when there is
/// one) and clamped so it never runs past the end. Returns `(scroll, visible)`.
pub fn window(
    len: usize,
    max_visible: usize,
    highlight: Option<usize>,
    scroll: usize,
) -> (usize, usize) {
    let visible = len.min(max_visible.max(1));
    let max_scroll = len.saturating_sub(visible);
    let mut scroll = scroll.min(max_scroll);
    if let Some(h) = highlight.filter(|_| visible > 0) {
        let h = h.min(len.saturating_sub(1));
        if h < scroll {
            scroll = h;
        } else if h >= scroll + visible {
            scroll = h + 1 - visible;
        }
    }
    (scroll.min(max_scroll), visible)
}

/// Which of the window's rows the scrollbar's thumb covers, or `None` when
/// the whole list fits and there is nothing to scroll.
pub fn scrollbar(len: usize, visible: usize, scroll: usize) -> Option<Vec<bool>> {
    if visible == 0 || len <= visible {
        return None;
    }
    let thumb = (visible * visible / len).max(1);
    let max_scroll = len - visible;
    let start = (scroll.min(max_scroll) * (visible - thumb) + max_scroll / 2) / max_scroll;
    Some(
        (0..visible)
            .map(|r| r >= start && r < start + thumb)
            .collect(),
    )
}

/// Type-to-jump: the first row after `from` (wrapping round) whose text starts
/// with `typed`, case-insensitively. Starting *after* the current row is what
/// lets a repeated letter walk through the rows that share it.
pub fn jump_to_prefix(rows: &[String], from: usize, typed: &str) -> Option<usize> {
    if rows.is_empty() || typed.is_empty() {
        return None;
    }
    let typed = typed.to_lowercase();
    let n = rows.len();
    (1..=n)
        .map(|i| (from + i) % n)
        .find(|&i| rows[i].trim_start().to_lowercase().starts_with(&typed))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_move_clamps_at_both_ends_and_pages_with_one_row_of_overlap() {
        assert_eq!(step(10, 0, Nav::Up, 5), 0);
        assert_eq!(step(10, 9, Nav::Down, 5), 9);
        assert_eq!(step(10, 2, Nav::Down, 5), 3);
        assert_eq!(step(10, 0, Nav::PageDown, 5), 4);
        assert_eq!(step(10, 8, Nav::PageDown, 5), 9);
        assert_eq!(step(10, 6, Nav::PageUp, 5), 2);
        assert_eq!(step(10, 4, Nav::Home, 5), 0);
        assert_eq!(step(10, 4, Nav::End, 5), 9);
        assert_eq!(step(0, 4, Nav::End, 5), 0);
    }

    #[test]
    fn the_window_keeps_the_highlight_in_view_from_either_side() {
        assert_eq!(window(20, 5, Some(0), 0), (0, 5));
        assert_eq!(window(20, 5, Some(7), 0), (3, 5));
        assert_eq!(window(20, 5, Some(2), 10), (2, 5));
        assert_eq!(window(20, 5, Some(19), 0), (15, 5));
        // No highlight: the stored offset stands, clamped.
        assert_eq!(window(20, 5, None, 30), (15, 5));
        // A list shorter than the window shows whole.
        assert_eq!(window(3, 5, Some(2), 4), (0, 3));
    }

    #[test]
    fn only_a_list_longer_than_its_window_has_a_scrollbar() {
        assert!(scrollbar(3, 3, 0).is_none());
        let bar = scrollbar(20, 5, 0).expect("a bar");
        assert!(bar[0]);
        let bar = scrollbar(20, 5, 15).expect("a bar");
        assert!(bar[4]);
    }

    #[test]
    fn typing_jumps_to_the_next_row_that_starts_with_it() {
        let rows: Vec<String> = ["alpha", "beta", "banana", "cherry"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        assert_eq!(jump_to_prefix(&rows, 0, "b"), Some(1));
        assert_eq!(jump_to_prefix(&rows, 1, "b"), Some(2));
        assert_eq!(jump_to_prefix(&rows, 2, "b"), Some(1));
        assert_eq!(jump_to_prefix(&rows, 0, "C"), Some(3));
        assert_eq!(jump_to_prefix(&rows, 0, "z"), None);
    }
}
