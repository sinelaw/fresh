//! What to send a language server for a buffer edit.
//!
//! Keeping a server's copy of a document in step with the buffer is a policy
//! decision with more cases than it first looks like, because the notification
//! can fail to send. This module is that decision, isolated from the plumbing
//! that carries it out, so every combination can be tested directly (#3038).

/// The notification to send one server for one buffer edit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ChangeToSend {
    /// The server's copy matches the buffer: forward the incremental edits.
    Incremental,
    /// The server's copy has diverged and the buffer's text is in hand:
    /// replace the whole document.
    FullText,
    /// Send nothing to this server right now.
    Skip,
}

/// How a server asked, in its `textDocumentSync` capability, to be told about
/// edits (#3436).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ServerSync {
    /// Range edits (`TextDocumentSyncKind.Incremental`). Also what a server
    /// that leaves the kind out gets: the spec's default there is `None`, but
    /// Fresh has always sent range edits to such servers, and silencing them
    /// would break every one that relies on it.
    #[default]
    Incremental,
    /// The whole document on every change (`TextDocumentSyncKind.Full`): the
    /// server reads a change's `text` as the entire new content, so a range
    /// edit leaves it holding only the typed fragment.
    Full,
    /// No `didChange` at all (`TextDocumentSyncKind.None`).
    None,
}

impl ServerSync {
    /// The kind a server declared, from its `textDocumentSync` capability:
    /// either a bare kind or an options object with a `change` field.
    pub fn from_capabilities(caps: &lsp_types::ServerCapabilities) -> Self {
        use lsp_types::TextDocumentSyncCapability;
        let kind = match &caps.text_document_sync {
            Some(TextDocumentSyncCapability::Kind(kind)) => Some(*kind),
            Some(TextDocumentSyncCapability::Options(opts)) => opts.change,
            None => None,
        };
        kind.map_or(Self::Incremental, Self::from_kind)
    }

    /// The kind a dynamic `textDocument/didChange` registration carries in its
    /// `syncKind` option. `None` when the options don't say.
    pub fn from_registration(register_options: Option<&serde_json::Value>) -> Option<Self> {
        let kind = register_options?.get("syncKind")?.as_i64()?;
        Some(match kind {
            0 => Self::None,
            1 => Self::Full,
            _ => Self::Incremental,
        })
    }

    fn from_kind(kind: lsp_types::TextDocumentSyncKind) -> Self {
        match kind {
            lsp_types::TextDocumentSyncKind::FULL => Self::Full,
            lsp_types::TextDocumentSyncKind::NONE => Self::None,
            _ => Self::Incremental,
        }
    }
}

/// Inputs to the decision, named so a call site reads as its own explanation.
#[derive(Debug, Clone, Copy)]
pub(crate) struct SyncState {
    /// What the server asked to be sent for an edit.
    pub server_sync: ServerSync,
    /// The server's copy of this document is known to have diverged from the
    /// buffer, because a notification for it could not be queued.
    pub desynced: bool,
    /// The server's command queue can accept a notification right now.
    pub has_capacity: bool,
    /// The buffer's full text is available. It is not, for a lazily unloaded
    /// buffer, and there is then nothing to rebuild the server's copy from.
    pub text_available: bool,
    /// This pass exists only to repair diverged servers, so a server that is
    /// in sync has nothing to receive.
    pub resync_only: bool,
}

/// Decide what a server should be sent.
///
/// The one rule that matters: **a diverged server is never sent an
/// incremental edit.** Ranges are meaningless against a document it does not
/// have, so anything but a whole-document replacement widens the divergence
/// rather than closing it. When the replacement cannot be sent — no capacity,
/// or no text to send — the answer is to send nothing and try again later,
/// never to fall back to the incremental edits.
///
/// Skipping rather than falling back also closes a race. Capacity is checked
/// before the send and can only grow in between (this editor is the only
/// sender), so an incremental edit issued because capacity looked unavailable
/// could still land, on exactly the server that must not receive one.
///
/// What an in-sync server is sent follows the kind it declared: range edits
/// for `Incremental`, the whole text for `Full` (or nothing, when the text is
/// not in hand — a range edit would be worse), and never anything for `None`
/// (#3436).
pub(crate) fn decide_change_to_send(state: SyncState) -> ChangeToSend {
    if state.server_sync == ServerSync::None {
        ChangeToSend::Skip
    } else if state.desynced {
        if state.has_capacity && state.text_available {
            ChangeToSend::FullText
        } else {
            ChangeToSend::Skip
        }
    } else if state.resync_only {
        ChangeToSend::Skip
    } else if state.server_sync == ServerSync::Full {
        if state.text_available {
            ChangeToSend::FullText
        } else {
            ChangeToSend::Skip
        }
    } else {
        ChangeToSend::Incremental
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn decide(
        desynced: bool,
        has_capacity: bool,
        text_available: bool,
        resync_only: bool,
    ) -> ChangeToSend {
        decide_change_to_send(SyncState {
            server_sync: ServerSync::Incremental,
            desynced,
            has_capacity,
            text_available,
            resync_only,
        })
    }

    /// Every input combination, enumerated. The decision has four booleans, so
    /// "all cases" is sixteen — small enough to state each one's reason.
    #[test]
    fn every_combination_is_accounted_for() {
        for &has_capacity in &[false, true] {
            for &text_available in &[false, true] {
                for &resync_only in &[false, true] {
                    // A diverged server is never sent an incremental edit,
                    // whatever else is true.
                    assert_ne!(
                        decide(true, has_capacity, text_available, resync_only),
                        ChangeToSend::Incremental,
                        "desynced with capacity={has_capacity} text={text_available} \
                         resync_only={resync_only} must not extend the broken edit stream"
                    );

                    // A server in sync is never sent a full-document
                    // replacement; that would be pointless traffic.
                    assert_ne!(
                        decide(false, has_capacity, text_available, resync_only),
                        ChangeToSend::FullText,
                        "in-sync server needs no resync"
                    );
                }
            }
        }
    }

    #[test]
    fn diverged_server_is_repaired_when_it_can_be() {
        assert_eq!(decide(true, true, true, false), ChangeToSend::FullText);
        assert_eq!(decide(true, true, true, true), ChangeToSend::FullText);
    }

    #[test]
    fn repair_waits_when_it_cannot_be_carried_out() {
        // No room in the queue.
        assert_eq!(decide(true, false, true, false), ChangeToSend::Skip);
        // Buffer text unavailable (lazily unloaded), so nothing to send.
        assert_eq!(decide(true, true, false, false), ChangeToSend::Skip);
        // Neither.
        assert_eq!(decide(true, false, false, true), ChangeToSend::Skip);
    }

    #[test]
    fn healthy_server_gets_the_edit_unless_this_is_a_repair_pass() {
        assert_eq!(decide(false, true, true, false), ChangeToSend::Incremental);
        assert_eq!(decide(false, true, true, true), ChangeToSend::Skip);
        // Capacity and text don't enter into it when nothing has diverged: the
        // send may still fail, and that failure is what marks it desynced.
        assert_eq!(
            decide(false, false, false, false),
            ChangeToSend::Incremental
        );
    }

    fn decide_for(
        server_sync: ServerSync,
        desynced: bool,
        text_available: bool,
        resync_only: bool,
    ) -> ChangeToSend {
        decide_change_to_send(SyncState {
            server_sync,
            desynced,
            has_capacity: true,
            text_available,
            resync_only,
        })
    }

    /// A server that declared Full sync reads a change's `text` as the whole
    /// document, so it must never get a range edit: it gets the full text,
    /// or nothing when the text is not available (#3436).
    #[test]
    fn full_sync_server_gets_the_whole_text_never_a_range_edit() {
        for &desynced in &[false, true] {
            for &text_available in &[false, true] {
                for &resync_only in &[false, true] {
                    assert_ne!(
                        decide_for(ServerSync::Full, desynced, text_available, resync_only),
                        ChangeToSend::Incremental
                    );
                }
            }
        }
        assert_eq!(
            decide_for(ServerSync::Full, false, true, false),
            ChangeToSend::FullText
        );
        assert_eq!(
            decide_for(ServerSync::Full, false, false, false),
            ChangeToSend::Skip
        );
        // A repair pass still leaves an in-sync Full server alone.
        assert_eq!(
            decide_for(ServerSync::Full, false, true, true),
            ChangeToSend::Skip
        );
    }

    /// A server that declared `TextDocumentSyncKind.None` is sent nothing.
    #[test]
    fn none_sync_server_is_sent_nothing() {
        for &desynced in &[false, true] {
            for &text_available in &[false, true] {
                for &resync_only in &[false, true] {
                    assert_eq!(
                        decide_for(ServerSync::None, desynced, text_available, resync_only),
                        ChangeToSend::Skip
                    );
                }
            }
        }
    }

    #[test]
    fn sync_kind_is_read_from_either_capability_form() {
        use lsp_types::{
            ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
            TextDocumentSyncOptions,
        };
        let with = |sync| ServerCapabilities {
            text_document_sync: sync,
            ..Default::default()
        };
        assert_eq!(
            ServerSync::from_capabilities(&with(Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::FULL
            )))),
            ServerSync::Full
        );
        assert_eq!(
            ServerSync::from_capabilities(&with(Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    change: Some(TextDocumentSyncKind::NONE),
                    ..Default::default()
                }
            )))),
            ServerSync::None
        );
        assert_eq!(
            ServerSync::from_capabilities(&with(Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL
            )))),
            ServerSync::Incremental
        );
        // Undeclared keeps the long-standing range edits.
        assert_eq!(
            ServerSync::from_capabilities(&with(None)),
            ServerSync::Incremental
        );
        assert_eq!(
            ServerSync::from_registration(Some(&serde_json::json!({ "syncKind": 1 }))),
            Some(ServerSync::Full)
        );
        assert_eq!(ServerSync::from_registration(None), None);
    }
}
