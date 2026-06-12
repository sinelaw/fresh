//! Kitty terminal graphics protocol support for rendering inline images
//! inside the editor. The editor core knows nothing about *what* is being
//! rendered — plugins decide (via `placeImage` / `clearImages`) what any
//! file's content should look like; this module only owns the mechanics.
//!
//! # Integration model
//!
//! Images are displayed using kitty's *Unicode placeholder* mechanism
//! rather than absolute cursor-positioned placements. Core transmits the
//! image once (keyed by a 24-bit image id) and the buffer reserves rows of
//! *placeholder cells*: each cell is `U+10EEEE` followed by combining
//! row/column diacritics, and the cell's foreground color carries the image
//! id. Because the image follows its placeholder cells, it scrolls and
//! repaints naturally with the surrounding text — unlike a cursor-anchored
//! placement, which would have to be deleted and re-emitted every frame and
//! would fight the cell-diff renderer.
//!
//! The protocol bytes are queued here and flushed to stdout by the main
//! loop after each frame (see [`ImageManager::take_escape_sequences`]); the
//! placeholder cells themselves are produced by [`placeholder_row`] and
//! placed into the buffer as virtual lines.

use base64::Engine as _;
use std::collections::HashMap;
use std::path::PathBuf;

/// Unicode placeholder code point used by the kitty graphics protocol.
pub const IMAGE_PLACEHOLDER: char = '\u{10EEEE}';

/// Largest image id we hand out. Kept inside 24 bits so the id round-trips
/// losslessly through a cell's truecolor foreground (R<<16 | G<<8 | B).
pub const MAX_IMAGE_ID: u32 = 0x00FF_FFFF;

/// Combining diacritics that encode row/column numbers for Unicode
/// placeholders. Derived (per the kitty spec) from Unicode 6.0.0 combining
/// class 230 marks without decomposition mappings. Index `i` represents
/// row/column number `i`, so the table also bounds the maximum number of
/// rows/columns a single image placement may span (297).
static ROWCOLUMN_DIACRITICS: &[u32] = &[
    0x0305, 0x030D, 0x030E, 0x0310, 0x0312, 0x033D, 0x033E, 0x033F, 0x0346, 0x034A, 0x034B,
    0x034C, 0x0350, 0x0351, 0x0352, 0x0357, 0x035B, 0x0363, 0x0364, 0x0365, 0x0366, 0x0367,
    0x0368, 0x0369, 0x036A, 0x036B, 0x036C, 0x036D, 0x036E, 0x036F, 0x0483, 0x0484, 0x0485,
    0x0486, 0x0487, 0x0592, 0x0593, 0x0594, 0x0595, 0x0597, 0x0598, 0x0599, 0x059C, 0x059D,
    0x059E, 0x059F, 0x05A0, 0x05A1, 0x05A8, 0x05A9, 0x05AB, 0x05AC, 0x05AF, 0x05C4, 0x0610,
    0x0611, 0x0612, 0x0613, 0x0614, 0x0615, 0x0616, 0x0617, 0x0657, 0x0658, 0x0659, 0x065A,
    0x065B, 0x065D, 0x065E, 0x06D6, 0x06D7, 0x06D8, 0x06D9, 0x06DA, 0x06DB, 0x06DC, 0x06DF,
    0x06E0, 0x06E1, 0x06E2, 0x06E4, 0x06E7, 0x06E8, 0x06EB, 0x06EC, 0x0730, 0x0732, 0x0733,
    0x0735, 0x0736, 0x073A, 0x073D, 0x073F, 0x0740, 0x0741, 0x0743, 0x0745, 0x0747, 0x0749,
    0x074A, 0x07EB, 0x07EC, 0x07ED, 0x07EE, 0x07EF, 0x07F0, 0x07F1, 0x07F3, 0x0816, 0x0817,
    0x0818, 0x0819, 0x081B, 0x081C, 0x081D, 0x081E, 0x081F, 0x0820, 0x0821, 0x0822, 0x0823,
    0x0825, 0x0826, 0x0827, 0x0829, 0x082A, 0x082B, 0x082C, 0x082D, 0x0951, 0x0953, 0x0954,
    0x0F82, 0x0F83, 0x0F86, 0x0F87, 0x135D, 0x135E, 0x135F, 0x17DD, 0x193A, 0x1A17, 0x1A75,
    0x1A76, 0x1A77, 0x1A78, 0x1A79, 0x1A7A, 0x1A7B, 0x1A7C, 0x1B6B, 0x1B6D, 0x1B6E, 0x1B6F,
    0x1B70, 0x1B71, 0x1B72, 0x1B73, 0x1CD0, 0x1CD1, 0x1CD2, 0x1CDA, 0x1CDB, 0x1CE0, 0x1DC0,
    0x1DC1, 0x1DC3, 0x1DC4, 0x1DC5, 0x1DC6, 0x1DC7, 0x1DC8, 0x1DC9, 0x1DCB, 0x1DCC, 0x1DD1,
    0x1DD2, 0x1DD3, 0x1DD4, 0x1DD5, 0x1DD6, 0x1DD7, 0x1DD8, 0x1DD9, 0x1DDA, 0x1DDB, 0x1DDC,
    0x1DDD, 0x1DDE, 0x1DDF, 0x1DE0, 0x1DE1, 0x1DE2, 0x1DE3, 0x1DE4, 0x1DE5, 0x1DE6, 0x1DFE,
    0x20D0, 0x20D1, 0x20D4, 0x20D5, 0x20D6, 0x20D7, 0x20DB, 0x20DC, 0x20E1, 0x20E7, 0x20E9,
    0x20F0, 0x2CEF, 0x2CF0, 0x2CF1, 0x2DE0, 0x2DE1, 0x2DE2, 0x2DE3, 0x2DE4, 0x2DE5, 0x2DE6,
    0x2DE7, 0x2DE8, 0x2DE9, 0x2DEA, 0x2DEB, 0x2DEC, 0x2DED, 0x2DEE, 0x2DEF, 0x2DF0, 0x2DF1,
    0x2DF2, 0x2DF3, 0x2DF4, 0x2DF5, 0x2DF6, 0x2DF7, 0x2DF8, 0x2DF9, 0x2DFA, 0x2DFB, 0x2DFC,
    0x2DFD, 0x2DFE, 0x2DFF, 0xA66F, 0xA67C, 0xA67D, 0xA6F0, 0xA6F1, 0xA8E0, 0xA8E1, 0xA8E2,
    0xA8E3, 0xA8E4, 0xA8E5, 0xA8E6, 0xA8E7, 0xA8E8, 0xA8E9, 0xA8EA, 0xA8EB, 0xA8EC, 0xA8ED,
    0xA8EE, 0xA8EF, 0xA8F0, 0xA8F1, 0xAAB0, 0xAAB2, 0xAAB3, 0xAAB7, 0xAAB8, 0xAABE, 0xAABF,
    0xAAC1, 0xFE20, 0xFE21, 0xFE22, 0xFE23, 0xFE24, 0xFE25, 0xFE26, 0x10A0F, 0x10A38, 0x1D185,
    0x1D186, 0x1D187, 0x1D188, 0x1D189, 0x1D1AA, 0x1D1AB, 0x1D1AC, 0x1D1AD, 0x1D242, 0x1D243,
    0x1D244,
];

/// Maximum number of cells (rows or columns) one image placement can span.
pub fn max_placement_cells() -> usize {
    ROWCOLUMN_DIACRITICS.len()
}

/// The diacritic for a given row/column index, or `None` if out of range.
pub fn rowcolumn_diacritic(index: usize) -> Option<char> {
    ROWCOLUMN_DIACRITICS
        .get(index)
        .and_then(|&cp| char::from_u32(cp))
}

/// Foreground RGB that encodes a 24-bit image id for placeholder cells.
pub fn fg_for_id(id: u32) -> (u8, u8, u8) {
    (
        ((id >> 16) & 0xFF) as u8,
        ((id >> 8) & 0xFF) as u8,
        (id & 0xFF) as u8,
    )
}

/// Build one row of placeholder cells for image `id`, image-row `row`,
/// spanning `cols` columns. Each grapheme cell is
/// `U+10EEEE` + row-diacritic + column-diacritic; the caller must style the
/// whole line with `fg = fg_for_id(id)`. Returns `None` if `row`/`cols`
/// exceed [`max_placement_cells`].
pub fn placeholder_row(row: usize, cols: usize) -> Option<String> {
    let row_d = rowcolumn_diacritic(row)?;
    if cols == 0 || cols > max_placement_cells() {
        return None;
    }
    let mut s = String::with_capacity(cols * IMAGE_PLACEHOLDER.len_utf8() * 2);
    for col in 0..cols {
        let col_d = rowcolumn_diacritic(col)?;
        s.push(IMAGE_PLACEHOLDER);
        s.push(row_d);
        s.push(col_d);
    }
    Some(s)
}

/// Describes one row of a placed image: which image (`id`), which image-row
/// this buffer line represents (`image_row`), and how many columns wide the
/// placement is (`cols`). Carried on the reserved virtual line and consumed
/// by the render post-pass, which writes the placeholder graphemes directly
/// into the terminal cells.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ImageCellSpec {
    pub id: u32,
    pub image_row: u16,
    pub cols: u16,
}

impl ImageCellSpec {
    /// The truecolor foreground that encodes this image's id.
    pub fn fg(&self) -> (u8, u8, u8) {
        fg_for_id(self.id)
    }

    /// The placeholder grapheme for column `col` of this row:
    /// `U+10EEEE` + row diacritic + column diacritic. `None` if `col` or the
    /// row index is beyond the diacritic table.
    pub fn cell_symbol(&self, col: usize) -> Option<String> {
        let row_d = rowcolumn_diacritic(self.image_row as usize)?;
        let col_d = rowcolumn_diacritic(col)?;
        let mut s = String::with_capacity(IMAGE_PLACEHOLDER.len_utf8() + 6);
        s.push(IMAGE_PLACEHOLDER);
        s.push(row_d);
        s.push(col_d);
        Some(s)
    }
}

/// Terminal raster-graphics capability — shared with the plugin runtime via
/// `fresh-core` so plugins querying `editor.getGraphicsCapability()` always
/// agree with the editor's own gating.
pub use fresh_core::graphics::GraphicsCapability;

#[derive(Debug, Clone)]
struct Registered {
    key: String,
    namespace: String,
    cols: u16,
    rows: u16,
    path: PathBuf,
}

/// Owns image ids (deduplicated by content key) and the queue of
/// transmit/delete escape sequences flushed to the terminal after a frame.
pub struct ImageManager {
    capability: GraphicsCapability,
    next_id: u32,
    by_key: HashMap<String, u32>,
    by_namespace: HashMap<String, Vec<u32>>,
    images: HashMap<u32, Registered>,
    pending_transmit: Vec<u32>,
    pending_delete: Vec<u32>,
}

impl ImageManager {
    pub fn new(capability: GraphicsCapability) -> Self {
        ImageManager {
            capability,
            next_id: 1,
            by_key: HashMap::new(),
            by_namespace: HashMap::new(),
            images: HashMap::new(),
            pending_transmit: Vec::new(),
            pending_delete: Vec::new(),
        }
    }

    pub fn capability(&self) -> GraphicsCapability {
        self.capability
    }

    fn alloc_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id = if self.next_id >= MAX_IMAGE_ID {
            1
        } else {
            self.next_id + 1
        };
        id
    }

    /// Register (or look up) an image by content `key`. `cols`/`rows` are the
    /// placement size in cells. Returns the image id used to encode the
    /// placeholder foreground. Queues a transmit if newly registered or if
    /// the path/size changed; reuses the id and skips work otherwise.
    pub fn register(
        &mut self,
        key: &str,
        namespace: &str,
        path: PathBuf,
        cols: u16,
        rows: u16,
    ) -> u32 {
        if let Some(&id) = self.by_key.get(key) {
            let unchanged = self
                .images
                .get(&id)
                .map(|img| img.path == path && img.cols == cols && img.rows == rows)
                .unwrap_or(false);
            if unchanged {
                return id;
            }
            // Content changed under the same key: drop the old data and
            // re-transmit under the same id so existing placeholders update.
            self.pending_delete.push(id);
            self.images.insert(
                id,
                Registered {
                    key: key.to_string(),
                    namespace: namespace.to_string(),
                    cols,
                    rows,
                    path,
                },
            );
            self.pending_transmit.push(id);
            return id;
        }

        let id = self.alloc_id();
        self.by_key.insert(key.to_string(), id);
        self.by_namespace
            .entry(namespace.to_string())
            .or_default()
            .push(id);
        self.images.insert(
            id,
            Registered {
                key: key.to_string(),
                namespace: namespace.to_string(),
                cols,
                rows,
                path,
            },
        );
        self.pending_transmit.push(id);
        id
    }

    /// Forget an image (e.g. its placeholders were cleared) and queue a
    /// delete so the terminal frees the pixel data.
    pub fn forget(&mut self, key: &str) {
        if let Some(id) = self.by_key.remove(key) {
            if let Some(img) = self.images.remove(&id) {
                if let Some(ids) = self.by_namespace.get_mut(&img.namespace) {
                    ids.retain(|&i| i != id);
                }
            }
            self.pending_delete.push(id);
        }
    }

    /// Forget every image registered under `namespace` (used by
    /// `clearImages`) and queue deletes so the terminal frees the data.
    pub fn forget_namespace(&mut self, namespace: &str) {
        let Some(ids) = self.by_namespace.remove(namespace) else {
            return;
        };
        for id in ids {
            if let Some(img) = self.images.remove(&id) {
                self.by_key.remove(&img.key);
            }
            self.pending_delete.push(id);
        }
    }

    /// Forget every registered image (e.g. on shutdown / buffer teardown).
    pub fn forget_all(&mut self) {
        let ids: Vec<u32> = self.by_key.values().copied().collect();
        self.by_key.clear();
        self.by_namespace.clear();
        self.images.clear();
        self.pending_delete.extend(ids);
    }

    /// Drain queued transmit/delete operations into terminal escape bytes.
    /// Returns empty when graphics are unsupported (queues are still cleared
    /// so they don't accumulate).
    pub fn take_escape_sequences(&mut self) -> Vec<u8> {
        if self.capability == GraphicsCapability::None {
            self.pending_transmit.clear();
            self.pending_delete.clear();
            return Vec::new();
        }

        let mut out: Vec<u8> = Vec::new();

        for id in self.pending_delete.drain(..) {
            // a=d, d=I: delete the image *and* its placements, freeing data.
            out.extend_from_slice(format!("\x1b_Ga=d,d=I,i={id},q=2\x1b\\").as_bytes());
        }

        let transmits: Vec<u32> = self.pending_transmit.drain(..).collect();
        for id in transmits {
            if let Some(img) = self.images.get(&id) {
                // Transmit medium `t=f` (file path), format `f=100` (PNG);
                // `a=T` transmits and creates a placement; `U=1` marks it a
                // virtual placement for Unicode placeholders; `c`/`r` give the
                // placement size in cells; `q=2` suppresses acknowledgements.
                let path_b64 = base64::engine::general_purpose::STANDARD
                    .encode(img.path.to_string_lossy().as_bytes());
                out.extend_from_slice(
                    format!(
                        "\x1b_Ga=T,U=1,i={id},f=100,t=f,c={cols},r={rows},q=2;{path_b64}\x1b\\",
                        cols = img.cols,
                        rows = img.rows,
                    )
                    .as_bytes(),
                );
            }
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diacritic_table_has_297_entries() {
        assert_eq!(max_placement_cells(), 297);
        assert_eq!(rowcolumn_diacritic(0), char::from_u32(0x0305));
        assert_eq!(rowcolumn_diacritic(296), char::from_u32(0x1D244));
        assert_eq!(rowcolumn_diacritic(297), None);
    }

    #[test]
    fn fg_for_id_round_trips_24_bit() {
        let id = 0x12_34_56;
        let (r, g, b) = fg_for_id(id);
        assert_eq!((r, g, b), (0x12, 0x34, 0x56));
        assert_eq!(((r as u32) << 16) | ((g as u32) << 8) | b as u32, id);
    }

    #[test]
    fn placeholder_row_builds_one_grapheme_per_column() {
        let row = placeholder_row(0, 3).unwrap();
        // Each cell is placeholder + 2 combining diacritics = 3 scalars.
        let chars: Vec<char> = row.chars().collect();
        assert_eq!(chars.len(), 9);
        assert_eq!(chars[0], IMAGE_PLACEHOLDER);
        assert_eq!(chars[3], IMAGE_PLACEHOLDER);
        assert_eq!(chars[6], IMAGE_PLACEHOLDER);
        // First cell: row 0, col 0 diacritics.
        assert_eq!(chars[1], rowcolumn_diacritic(0).unwrap());
        assert_eq!(chars[2], rowcolumn_diacritic(0).unwrap());
        // Third cell: row 0, col 2 diacritics.
        assert_eq!(chars[7], rowcolumn_diacritic(0).unwrap());
        assert_eq!(chars[8], rowcolumn_diacritic(2).unwrap());
    }

    #[test]
    fn placeholder_row_rejects_out_of_range() {
        assert!(placeholder_row(0, 0).is_none());
        assert!(placeholder_row(0, max_placement_cells() + 1).is_none());
        assert!(placeholder_row(max_placement_cells(), 1).is_none());
    }

    #[test]
    fn transmit_and_delete_escape_sequences() {
        let mut mgr = ImageManager::new(GraphicsCapability::Kitty);
        let id = mgr.register("a", "ns", PathBuf::from("/tmp/x.png"), 10, 4);
        assert!(id >= 1 && id <= MAX_IMAGE_ID);

        let seq = String::from_utf8(mgr.take_escape_sequences()).unwrap();
        assert!(seq.contains(&format!("i={id}")));
        assert!(seq.contains("a=T"));
        assert!(seq.contains("U=1"));
        assert!(seq.contains("c=10,r=4"));
        assert!(seq.starts_with("\x1b_G"));
        assert!(seq.ends_with("\x1b\\"));

        // Re-registering identical content is a no-op (id reused, nothing queued).
        let id2 = mgr.register("a", "ns", PathBuf::from("/tmp/x.png"), 10, 4);
        assert_eq!(id, id2);
        assert!(mgr.take_escape_sequences().is_empty());

        // Forgetting queues a delete for that id.
        mgr.forget("a");
        let del = String::from_utf8(mgr.take_escape_sequences()).unwrap();
        assert!(del.contains(&format!("a=d,d=I,i={id}")));
    }

    #[test]
    fn forget_namespace_deletes_all_in_namespace() {
        let mut mgr = ImageManager::new(GraphicsCapability::Kitty);
        let a = mgr.register("a", "doc1", PathBuf::from("/tmp/a.png"), 4, 2);
        let b = mgr.register("b", "doc1", PathBuf::from("/tmp/b.png"), 4, 2);
        let c = mgr.register("c", "doc2", PathBuf::from("/tmp/c.png"), 4, 2);
        let _ = mgr.take_escape_sequences();

        mgr.forget_namespace("doc1");
        let del = String::from_utf8(mgr.take_escape_sequences()).unwrap();
        assert!(del.contains(&format!("i={a}")));
        assert!(del.contains(&format!("i={b}")));
        assert!(!del.contains(&format!("a=d,d=I,i={c}")));

        // doc2 image survives and is still deduped by key.
        let c2 = mgr.register("c", "doc2", PathBuf::from("/tmp/c.png"), 4, 2);
        assert_eq!(c, c2);
    }

    #[test]
    fn unsupported_capability_emits_nothing() {
        let mut mgr = ImageManager::new(GraphicsCapability::None);
        mgr.register("a", "ns", PathBuf::from("/tmp/x.png"), 10, 4);
        assert!(mgr.take_escape_sequences().is_empty());
    }
}
