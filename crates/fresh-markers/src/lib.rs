//! Positions in a text that follow its edits: an interval tree with lazy
//! deltas ([`marker_tree`]) and the gravity-aware marker list over it
//! ([`marker`]). Shared by the editor and the web client's WebAssembly module.

pub mod marker;
pub mod marker_tree;
