//! Core data model for documents
//!
//! This module contains pure data structures with minimal external dependencies.

pub mod buffer;
pub mod composite_buffer;
pub mod control_event;
pub mod cursor;
pub mod document_model;
pub mod edit;
pub mod encoding;
pub mod encoding_heuristics;
pub mod event;
pub mod filesystem;
pub mod line_diff;
pub mod marker;
pub mod marker_tree;
pub mod piece_tree;
pub mod piece_tree_diff;
