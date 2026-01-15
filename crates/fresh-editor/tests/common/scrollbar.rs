//! Scrollbar detection utilities for tests
//!
//! The scrollbar is rendered using background colors instead of box-drawing characters
//! to avoid glyph gaps in some terminal emulators. This module provides utilities
//! to detect scrollbar cells by their background color.

use ratatui::style::{Color, Style};

/// Check if a style indicates a scrollbar thumb cell.
///
/// The scrollbar thumb is rendered with a non-default background color.
/// In the default theme, this is Color::Gray.
pub fn is_scrollbar_thumb_style(style: Style) -> bool {
    matches!(
        style.bg,
        Some(Color::DarkGray)     // Default (non-themed) thumb
            | Some(Color::Gray)       // Default theme thumb
            | Some(Color::White)      // Hover thumb
            | Some(Color::Rgb(180, 180, 180))  // Light theme thumb
            | Some(Color::Rgb(140, 140, 140))  // Light theme hover
            | Some(Color::Rgb(170, 170, 170))  // Retro theme thumb
            | Some(Color::Rgb(255, 255, 255))  // Retro theme hover
            | Some(Color::Yellow)     // High contrast thumb
            | Some(Color::Cyan) // High contrast hover
    )
}

/// Check if a style indicates a scrollbar track cell.
///
/// The scrollbar track is rendered with a background color distinct from content.
/// In the default theme, this is Color::DarkGray.
pub fn is_scrollbar_track_style(style: Style) -> bool {
    matches!(
        style.bg,
        Some(Color::Black)        // Default (non-themed) track
            | Some(Color::DarkGray)   // Default theme track
            | Some(Color::Gray)       // Hover track in default theme
            | Some(Color::Rgb(220, 220, 220))  // Light theme track
            | Some(Color::Rgb(200, 200, 200))  // Light theme hover
            | Some(Color::Rgb(0, 0, 128))      // Retro theme track
            | Some(Color::White) // High contrast theme track
    )
}

/// Check if a style indicates any scrollbar cell (thumb or track).
///
/// Returns true if the cell has a background color typically used for scrollbars.
pub fn is_scrollbar_style(style: Style) -> bool {
    is_scrollbar_thumb_style(style) || is_scrollbar_track_style(style)
}

/// Check if a cell is a scrollbar cell based on having a non-reset background.
///
/// This is a more permissive check that considers any cell with a background color
/// in the scrollbar column as a scrollbar cell.
pub fn has_scrollbar_background(style: Style) -> bool {
    style.bg.is_some() && style.bg != Some(Color::Reset)
}
