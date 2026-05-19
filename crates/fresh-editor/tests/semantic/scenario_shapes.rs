//! JSON round-trip gate for every scenario type.
//!
//! Each scenario type's data shape must round-trip through serde
//! without loss. This catches schema-breaking changes that would
//! invalidate the corpus on disk. Skeleton scenario types are
//! tested too — they don't run yet, but their JSON shape is the
//! contract external drivers (corpus generators, replay tooling)
//! bind to today.

use crate::common::scenario::buffer_scenario::{BufferScenario, CursorExpect};
use crate::common::scenario::context::{
    LspExchange, LspIncoming, LspScript, MockClock, MouseButton, MouseEvent, NamedBuffer,
    PromptKind, ThemeRef, VirtualFile, VirtualFs, WorkspaceContext,
};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::input_scenario::InputScenario;
use crate::common::scenario::layout_scenario::LayoutScenario;
use crate::common::scenario::lsp_scenario::LspScenario;
use crate::common::scenario::modal_scenario::ModalScenario;
use crate::common::scenario::observable::{
    ActivePathExpect, BufferPathsExpect, FsState, LspTraffic, ModalState, PopupSnapshot,
    RoundTripGrid, StyledFrame, WorkspaceExpect,
};
use crate::common::scenario::persistence_scenario::PersistenceScenario;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect};
use crate::common::scenario::style_scenario::{Inspect, StyleScenario};
use crate::common::scenario::temporal_scenario::TemporalScenario;
use crate::common::scenario::terminal_io_scenario::TerminalIoScenario;
use crate::common::scenario::trace_scenario::TraceScenario;
use crate::common::scenario::workspace_scenario::WorkspaceScenario;
use fresh::test_api::{Action, Caret};
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::path::PathBuf;
use std::time::Duration;

fn round_trip<T>(label: &str, value: T)
where
    T: Serialize + DeserializeOwned + std::fmt::Debug + PartialEq,
{
    let json = serde_json::to_string(&value).unwrap_or_else(|e| panic!("{label}: serialise: {e}"));
    let back: T = serde_json::from_str(&json)
        .unwrap_or_else(|e| panic!("{label}: deserialise: {e} from {json}"));
    assert_eq!(value, back, "{label}: round-trip mismatch");
}

fn round_trip_no_eq<T>(label: &str, value: T)
where
    T: Serialize + DeserializeOwned + std::fmt::Debug,
{
    let json = serde_json::to_string(&value).unwrap_or_else(|e| panic!("{label}: serialise: {e}"));
    let _: T = serde_json::from_str(&json)
        .unwrap_or_else(|e| panic!("{label}: deserialise: {e} from {json}"));
}

#[test]
fn json_roundtrip_buffer_scenario() {
    round_trip_no_eq(
        "BufferScenario",
        BufferScenario {
            description: "rt".into(),
            initial_text: "hi".into(),
            actions: vec![Action::ToUpperCase, Action::MoveDocumentEnd],
            expected_text: "HI".into(),
            expected_primary: CursorExpect::at(2),
            ..Default::default()
        },
    );
}

#[test]
fn json_roundtrip_layout_scenario() {
    round_trip_no_eq(
        "LayoutScenario",
        LayoutScenario {
            description: "rt".into(),
            initial_text: "x\ny".into(),
            width: 40,
            height: 12,
            actions: vec![Action::MoveDocumentEnd],
            expected_top_byte: Some(0),
            expected_snapshot: RenderSnapshotExpect {
                hardware_cursor: Some((2, 1)),
                ..Default::default()
            },
        },
    );
}

#[test]
fn json_roundtrip_trace_scenario() {
    round_trip_no_eq(
        "TraceScenario",
        TraceScenario {
            description: "rt".into(),
            initial_text: "a".into(),
            actions: vec![Action::InsertChar('b')],
            expected_text: "ba".into(),
            undo_count: 1,
        },
    );
}

#[test]
fn json_roundtrip_modal_scenario() {
    round_trip_no_eq(
        "ModalScenario",
        ModalScenario {
            description: "rt".into(),
            initial_text: String::new(),
            events: vec![
                InputEvent::OpenPrompt(PromptKind::CommandPalette),
                InputEvent::FilterPrompt("dup".into()),
                InputEvent::ConfirmPrompt,
            ],
            expected_modal: ModalState {
                top_popup: Some(PopupSnapshot {
                    kind: "list".into(),
                    title: Some("Commands".into()),
                    items: vec!["dup line".into()],
                    selected_index: Some(0),
                    query: Some("dup".into()),
                }),
                depth: 1,
                prompt: None,
            },
        },
    );
}

#[test]
fn json_roundtrip_workspace_scenario() {
    round_trip_no_eq(
        "WorkspaceScenario",
        WorkspaceScenario {
            description: "rt".into(),
            workspace: WorkspaceContext {
                initial_buffers: vec![
                    NamedBuffer {
                        filename: "a.rs".into(),
                        content: "fn a() {}".into(),
                    },
                    NamedBuffer {
                        filename: "b.rs".into(),
                        content: "fn b() {}".into(),
                    },
                ],
                initial_splits: None,
            },
            events: vec![],
            expected: WorkspaceExpect {
                buffer_count: 2,
                active_buffer_path: ActivePathExpect::EndsWith("a.rs".into()),
                buffer_paths: BufferPathsExpect::EndsWithInOrder(vec![
                    "a.rs".into(),
                    "b.rs".into(),
                ]),
            },
        },
    );
}

#[test]
fn json_roundtrip_persistence_scenario() {
    round_trip_no_eq(
        "PersistenceScenario",
        PersistenceScenario {
            description: "rt".into(),
            initial_fs: VirtualFs {
                files: [(
                    PathBuf::from("/tmp/x"),
                    VirtualFile {
                        content: "hello".into(),
                        mode: Some(0o644),
                        mtime_unix_secs: Some(1_700_000_000),
                    },
                )]
                .into_iter()
                .collect(),
            },
            initial_open: "/tmp/x".into(),
            events: vec![InputEvent::FsExternalEdit {
                path: PathBuf::from("/tmp/x"),
                content: "world".into(),
            }],
            expected_buffer: Default::default(),
            expected_fs: FsState {
                expected_files: [("/tmp/x".into(), "world".into())].into_iter().collect(),
            },
        },
    );
}

#[test]
fn json_roundtrip_lsp_scenario() {
    round_trip_no_eq(
        "LspScenario",
        LspScenario {
            description: "rt".into(),
            initial_text: "fn main() {}".into(),
            language: "rust".into(),
            script: LspScript {
                server: "rust-analyzer".into(),
                exchanges: vec![LspExchange {
                    expect_method: "textDocument/didOpen".into(),
                    expect_params: None,
                    server_reply: Some(LspIncoming {
                        method: "textDocument/publishDiagnostics".into(),
                        params: serde_json::json!({"diagnostics": []}),
                    }),
                }],
            },
            events: vec![],
            expected_buffer: Default::default(),
            expected_traffic: LspTraffic {
                client_methods: vec!["textDocument/didOpen".into()],
                server_notifications: vec!["textDocument/publishDiagnostics".into()],
            },
        },
    );
}

#[test]
fn json_roundtrip_input_scenario() {
    round_trip_no_eq(
        "InputScenario",
        InputScenario {
            description: "rt".into(),
            initial_text: "hi".into(),
            events: vec![InputEvent::Mouse(MouseEvent::Click {
                row: 0,
                col: 5,
                button: MouseButton::Left,
            })],
            expected: RenderSnapshotExpect::default(),
        },
    );
}

#[test]
fn json_roundtrip_temporal_scenario() {
    round_trip_no_eq(
        "TemporalScenario",
        TemporalScenario {
            description: "rt".into(),
            initial_text: String::new(),
            clock: Some(MockClock { epoch_ms: 0 }),
            events: vec![
                InputEvent::AdvanceClock(Duration::from_millis(50)),
                InputEvent::AdvanceClock(Duration::from_millis(150)),
            ],
            expected_frames: vec![RenderSnapshot::default(), RenderSnapshot::default()],
        },
    );
}

#[test]
fn json_roundtrip_style_scenario() {
    round_trip_no_eq(
        "StyleScenario",
        StyleScenario {
            description: "rt".into(),
            initial_text: "a".into(),
            theme: ThemeRef::HighContrast,
            events: vec![],
            inspect: Inspect::Cell { row: 0, col: 0 },
            expected: StyledFrame::default(),
        },
    );
}

#[test]
fn json_roundtrip_terminal_io_scenario() {
    round_trip_no_eq(
        "TerminalIoScenario",
        TerminalIoScenario {
            description: "rt".into(),
            initial_text: "x".into(),
            width: 80,
            height: 24,
            events: vec![],
            expected: RoundTripGrid::default(),
        },
    );
}

// PluginScenario / GuiScenario JSON-shape tests removed when
// those scenario types were dropped from the framework — see
// `docs/internal/e2e-test-migration-design.md` §12.

#[test]
fn caret_round_trips() {
    round_trip(
        "Caret(default)",
        Caret {
            position: 0,
            anchor: None,
        },
    );
    round_trip(
        "Caret(selection)",
        Caret {
            position: 5,
            anchor: Some(0),
        },
    );
}
