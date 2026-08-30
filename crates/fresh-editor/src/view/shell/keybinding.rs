//! The keybinding editor's box.
//!
//! **The frame first, the interior after** — the same order the floating
//! plugin panel took (C.6). The editor is a table with its own scrollbar, its
//! own double-click semantics and ten rectangles its painter records for a
//! mouse handler to compare against; what moves here is the outermost of them,
//! which is the one both the painter and the handler used.
//!
//! `keybinding_modal_area` was four lines of arithmetic — ninety percent of
//! the area it was handed, capped at 120 columns, floored at 60 by 20, then
//! centred with `area.x`/`area.y` added back so it lands beside the dock
//! rather than under it. The floor and the cap are the *rule* and they stay;
//! the centring and the offsets are what a layer does, and naming the region
//! it may occupy is what "beside the dock" means.
//!
//! The cap has no property to be: `min_w` exists and `max_w` does not, so the
//! width is resolved from the extent the way §4.4 sanctions — a
//! `layout_reader`, which is content resolved from a *known* extent rather
//! than geometry recorded from a paint. `view::shell::calibration` does the
//! same thing for the same reason.

use fresh_ui::{layout_reader, row, Align, Anchor, LayoutInfo, Modality, Node, Place, Sizing};

use super::msg::UiMsg;

/// Never wider than this, however wide the area is.
pub const MAX_WIDTH: u16 = 120;
/// And never smaller than this, however small it is.
pub const MIN_WIDTH: u16 = 60;
pub const MIN_HEIGHT: u16 = 20;

pub fn key() -> fresh_ui::Key {
    fresh_ui::Key::Str("keybinding_modal".into())
}

/// The box's size in an area of `info`'s extent.
///
/// `keybinding_modal_area`'s own two lines, kept because they are the rule
/// rather than the placement: ninety percent, capped, floored, and never
/// wider than the area less the two columns it keeps clear.
pub fn fit(info: LayoutInfo) -> (u16, u16) {
    let (w, h) = (info.constraints.max_w, info.constraints.max_h);
    let width = ((w as f32 * 0.90) as u16)
        .min(MAX_WIDTH)
        .max(MIN_WIDTH)
        .min(w.saturating_sub(2));
    let height = ((h as f32 * 0.90) as u16)
        .max(MIN_HEIGHT)
        .min(h.saturating_sub(2));
    (width, height)
}

/// The editor's box as a layer: centred beside the dock, and exclusive.
///
/// It paints nothing — the interior still does, and a layer is in the overlay
/// band, so anything drawn here would land on top of the painter that owns the
/// surface. What it contributes is the rectangle, which the painter reads back
/// instead of computing, and the claim.
pub fn layer() -> Node<UiMsg> {
    fresh_ui::layer()
        .within(super::frame::chrome_key())
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .child(layout_reader(|info: LayoutInfo| {
            let (w, h) = fit(info);
            row().w(Sizing::Cells(w)).h(Sizing::Cells(h)).key(key())
        }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Size, Ui};

    fn laid_out(w: u16, h: u16, dock: Option<u16>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                keybinding: true,
                dock,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// **Ninety percent, capped, floored, centred** — the painter's own rule,
    /// arrived at by layout instead of by four lines of arithmetic.
    #[test]
    fn the_box_is_ninety_percent_capped_and_centred() {
        let ui = laid_out(200, 60, None);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert_eq!(r.w, MAX_WIDTH, "capped at 120 however wide the frame is");
        assert_eq!(r.h, 54, "ninety percent of sixty");
        assert_eq!(r.x, (200 - MAX_WIDTH as i32) / 2, "centred across");
    }

    /// A frame too small for the cap gets ninety percent of itself.
    #[test]
    fn a_narrow_frame_gets_ninety_percent_of_itself() {
        let ui = laid_out(100, 40, None);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert_eq!((r.w, r.h), (90, 36));
    }

    /// And one too small for the floor gets the floor, less the two columns
    /// the painter kept clear.
    #[test]
    fn a_tiny_frame_gets_the_floor_less_its_margin() {
        let ui = laid_out(50, 15, None);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert_eq!((r.w, r.h), (48, 13));
    }

    /// **Beside the dock, not under it.** The painter added `area.x` back by
    /// hand because it was handed the post-dock chrome area; naming the region
    /// the layer may occupy says the same thing where the placing happens.
    ///
    /// This is `modal_centres_within_offset_area_left_of_dock`, moved: the
    /// modal used to be placed relative to column 0 and bled left under the
    /// dock.
    #[test]
    fn the_box_centres_beside_the_dock() {
        let ui = laid_out(200, 60, Some(40));
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert!(r.x >= 40, "clear of a forty-column dock, at {}", r.x);
        assert_eq!(
            r.x,
            40 + (160 - MAX_WIDTH as i32) / 2,
            "centred in what is left"
        );
    }

    /// Nothing behind it is interactive: the modal's capture band is
    /// `Modality::Exclusive` here.
    #[test]
    fn nothing_behind_the_box_takes_a_press() {
        let mut ui = laid_out(200, 60, None);
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(2, 2),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(got.msgs.is_empty(), "{:?}", got.msgs);
    }
}
