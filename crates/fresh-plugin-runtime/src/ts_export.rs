//! TypeScript type generation using ts-rs
//!
//! This module collects all API types with `#[derive(TS)]` and generates
//! TypeScript declarations that are combined with the proc macro output.
//! The generated TypeScript is validated and formatted using oxc.
//!
//! Types are automatically collected based on `JSEDITORAPI_REFERENCED_TYPES`
//! from the proc macro, so when you add a new type to method signatures,
//! it will automatically be included if it has `#[derive(TS)]`.

use oxc_allocator::Allocator;
use oxc_codegen::Codegen;
use oxc_parser::Parser;
use oxc_span::SourceType;
use ts_rs::{Config as TsConfig, TS};

use fresh_core::api::{
    ActionPopupAction, ActionPopupOptions, ActionSpec, AnimationRect, BackgroundProcessResult,
    BufferGroupResult, BufferInfo, BufferSavedDiff, CompositeHunk, CompositeLayoutConfig,
    CompositePaneStyle, CompositeSourceConfig, CreateCompositeBufferOptions, CreateTerminalOptions,
    CreateVirtualBufferInExistingSplitOptions, CreateVirtualBufferInSplitOptions,
    CreateVirtualBufferOptions, CursorInfo, DirEntry, FormatterPackConfig, GrammarInfoSnapshot,
    GrepMatch, JsDiagnostic, JsPosition, JsRange, JsTextPropertyEntry, KeyEventPayload,
    LanguagePackConfig, LayoutHints, LspServerPackConfig, OverlayColorSpec, OverlayOptions,
    PluginAnimationEdge, PluginAnimationKind, ProcessLimitsPackConfig, RemoteBackendInfo,
    ReplaceResult, ScreenSize, SearchTakeResult, SpawnResult, SplitSnapshot, TerminalResult,
    TextPropertiesAtCursor, TokenColor, TsHighlightSpan, ViewTokenStyle, ViewTokenWire,
    ViewTokenWireKind, ViewportInfo, VirtualBufferResult, WindowInfo,
};
use fresh_core::command::Suggestion;
use fresh_core::file_explorer::{
    FileExplorerDecoration, FileExplorerLeadingSlot, FileExplorerSlotEntry, FileExplorerTooltip,
    FileExplorerTrailingSlot,
};
use fresh_core::text_property::InlineOverlay;

/// Get the TypeScript declaration for a type by name
///
/// Returns None if the type is not known (not registered in this mapping).
/// Add new types here when they're added to api.rs with `#[derive(TS)]`.
fn get_type_decl(type_name: &str) -> Option<String> {
    let cfg = TsConfig::default();
    // Map TypeScript type names to their ts-rs declarations
    // The type name should match either the Rust struct name or the ts(rename = "...") value
    match type_name {
        // Animation types
        "AnimationRect" => Some(AnimationRect::decl(&cfg)),
        "PluginAnimationEdge" => Some(PluginAnimationEdge::decl(&cfg)),
        "PluginAnimationKind" => Some(PluginAnimationKind::decl(&cfg)),

        // Core types
        "BufferInfo" => Some(BufferInfo::decl(&cfg)),
        "WindowInfo" => Some(WindowInfo::decl(&cfg)),
        "RemoteBackendInfo" => Some(RemoteBackendInfo::decl(&cfg)),
        "CursorInfo" => Some(CursorInfo::decl(&cfg)),
        "ViewportInfo" => Some(ViewportInfo::decl(&cfg)),
        "ScreenSize" => Some(ScreenSize::decl(&cfg)),
        "KeyEventPayload" => Some(KeyEventPayload::decl(&cfg)),
        "SplitSnapshot" => Some(SplitSnapshot::decl(&cfg)),
        "ActionSpec" => Some(ActionSpec::decl(&cfg)),
        "BufferSavedDiff" => Some(BufferSavedDiff::decl(&cfg)),
        "LayoutHints" => Some(LayoutHints::decl(&cfg)),

        // Process types
        "SpawnResult" => Some(SpawnResult::decl(&cfg)),
        "BackgroundProcessResult" => Some(BackgroundProcessResult::decl(&cfg)),

        // Grep/Replace types
        "GrepMatch" => Some(GrepMatch::decl(&cfg)),
        "ReplaceResult" => Some(ReplaceResult::decl(&cfg)),
        "SearchTakeResult" => Some(SearchTakeResult::decl(&cfg)),
        // SearchHandle is the JS-side wrapper over a numeric handle id.
        // The Rust type can't be exported (non-serializable runtime state).
        "SearchHandle" => Some(
            "interface SearchHandle { searchId: number; take(): SearchTakeResult; cancel(): void; }".to_string(),
        ),

        // Terminal types
        "TerminalResult" => Some(TerminalResult::decl(&cfg)),
        "CreateTerminalOptions" => Some(CreateTerminalOptions::decl(&cfg)),
        "CreateWindowWithTerminalOptions" => {
            Some(fresh_core::api::CreateWindowWithTerminalOptions::decl(&cfg))
        }
        "SessionWithTerminalResult" => {
            Some(fresh_core::api::SessionWithTerminalResult::decl(&cfg))
        }

        // Composite buffer types (ts-rs renames these with Ts prefix)
        "TsCompositeLayoutConfig" | "CompositeLayoutConfig" => {
            Some(CompositeLayoutConfig::decl(&cfg))
        }
        "TsCompositeSourceConfig" | "CompositeSourceConfig" => {
            Some(CompositeSourceConfig::decl(&cfg))
        }
        "TsCompositePaneStyle" | "CompositePaneStyle" => Some(CompositePaneStyle::decl(&cfg)),
        "TsCompositeHunk" | "CompositeHunk" => Some(CompositeHunk::decl(&cfg)),
        "TsCreateCompositeBufferOptions" | "CreateCompositeBufferOptions" => {
            Some(CreateCompositeBufferOptions::decl(&cfg))
        }

        // View transform types
        "ViewTokenWireKind" => Some(ViewTokenWireKind::decl(&cfg)),
        "TokenColor" => Some(TokenColor::decl(&cfg)),
        "ViewTokenStyle" => Some(ViewTokenStyle::decl(&cfg)),
        "ViewTokenWire" => Some(ViewTokenWire::decl(&cfg)),

        // UI types (ts-rs renames these with Ts prefix)
        "TsActionPopupAction" | "ActionPopupAction" => Some(ActionPopupAction::decl(&cfg)),
        "ActionPopupOptions" => Some(ActionPopupOptions::decl(&cfg)),
        "TsLspMenuItem" | "LspMenuItem" => Some(fresh_core::api::LspMenuItem::decl(&cfg)),
        "TsHighlightSpan" => Some(TsHighlightSpan::decl(&cfg)),
        "FileExplorerDecoration" => Some(FileExplorerDecoration::decl(&cfg)),
        "FileExplorerSlotEntry" => Some(FileExplorerSlotEntry::decl(&cfg)),
        "FileExplorerLeadingSlot" => Some(FileExplorerLeadingSlot::decl(&cfg)),
        "FileExplorerTrailingSlot" => Some(FileExplorerTrailingSlot::decl(&cfg)),
        "FileExplorerTooltip" => Some(FileExplorerTooltip::decl(&cfg)),

        // Virtual buffer option types
        "TextPropertyEntry" | "JsTextPropertyEntry" => Some(JsTextPropertyEntry::decl(&cfg)),
        "CreateVirtualBufferOptions" => Some(CreateVirtualBufferOptions::decl(&cfg)),
        "CreateVirtualBufferInSplitOptions" => Some(CreateVirtualBufferInSplitOptions::decl(&cfg)),
        "CreateVirtualBufferInExistingSplitOptions" => {
            Some(CreateVirtualBufferInExistingSplitOptions::decl(&cfg))
        }

        // Return types
        "TextPropertiesAtCursor" => Some(TextPropertiesAtCursor::decl(&cfg)),
        "VirtualBufferResult" => Some(VirtualBufferResult::decl(&cfg)),
        "BufferGroupResult" => Some(BufferGroupResult::decl(&cfg)),

        // Prompt and directory types
        "PromptSuggestion" | "Suggestion" => Some(Suggestion::decl(&cfg)),
        "DirEntry" => Some(DirEntry::decl(&cfg)),

        // Diagnostic types
        "JsDiagnostic" => Some(JsDiagnostic::decl(&cfg)),
        "JsRange" => Some(JsRange::decl(&cfg)),
        "JsPosition" => Some(JsPosition::decl(&cfg)),

        // Grammar info types
        "GrammarInfoSnapshot" => Some(GrammarInfoSnapshot::decl(&cfg)),

        // Language pack types
        "LanguagePackConfig" => Some(LanguagePackConfig::decl(&cfg)),
        "LspServerPackConfig" => Some(LspServerPackConfig::decl(&cfg)),
        "ProcessLimitsPackConfig" => Some(ProcessLimitsPackConfig::decl(&cfg)),
        "FormatterPackConfig" => Some(FormatterPackConfig::decl(&cfg)),

        // Overlay/inline styling types
        "OverlayOptions" => Some(OverlayOptions::decl(&cfg)),
        "OverlayColorSpec" => Some(OverlayColorSpec::decl(&cfg)),
        "InlineOverlay" => Some(InlineOverlay::decl(&cfg)),
        "OffsetUnit" => Some(fresh_core::text_property::OffsetUnit::decl(&cfg)),
        "StyledSegment" => Some(fresh_core::text_property::StyledSegment::decl(&cfg)),
        "StyledText" => Some(fresh_core::api::StyledText::decl(&cfg)),

        // Widget library types — declarative plugin UI.
        // See docs/internal/plugin-widget-library-design.md.
        "WidgetSpec" => Some(fresh_core::api::WidgetSpec::decl(&cfg)),
        "HintEntry" => Some(fresh_core::api::HintEntry::decl(&cfg)),
        "ButtonKind" => Some(fresh_core::api::ButtonKind::decl(&cfg)),
        "WidgetAction" => Some(fresh_core::api::WidgetAction::decl(&cfg)),
        "WidgetMutation" => Some(fresh_core::api::WidgetMutation::decl(&cfg)),
        "TreeNode" => Some(fresh_core::api::TreeNode::decl(&cfg)),

        // Authority — payload schema for `editor.setAuthority(...)`.
        // Hand-written because the authoritative struct lives in
        // `fresh-editor` and this crate must not depend on it
        // (principle 3: core is opaque to backend kinds). Keep this in
        // sync with `crates/fresh-editor/src/services/authority/mod.rs`.
        "AuthorityPayload" => Some(AUTHORITY_PAYLOAD_DECL.to_string()),

        // Remote-agent attach spec for `editor.attachRemoteAgent(...)`.
        // Hand-written for the same reason as `AuthorityPayload`: the
        // authoritative `RemoteAgentSpec` struct lives in `fresh-editor`.
        // Keep in sync with
        // `crates/fresh-editor/src/services/authority/mod.rs`.
        "RemoteAgentSpec" => Some(REMOTE_AGENT_SPEC_DECL.to_string()),

        // Remote Indicator override — payload for
        // `editor.setRemoteIndicatorState(...)`. Same hand-written
        // rationale: the authoritative enum lives in
        // `fresh-editor::view::ui::status_bar::RemoteIndicatorOverride`
        // and this crate must not depend on it. Keep in sync.
        "RemoteIndicatorStatePayload" => Some(REMOTE_INDICATOR_STATE_DECL.to_string()),

        // Typed plugin paths. A bare `string` (an authority path on the active
        // window) stays valid everywhere for backward compatibility; these two
        // tag a path for a specific filesystem. Hand-written because the
        // authoritative `PluginPath` enum is decoded in the backend, not
        // ts-rs-derived. Keep in sync with `fresh_core::api::PluginPath` and
        // `editor.localPath` / `editor.windowPath`.
        "LocalPath" => Some(LOCAL_PATH_DECL.to_string()),
        "WindowPath" => Some(WINDOW_PATH_DECL.to_string()),
        "AuthorityPath" => Some(AUTHORITY_PATH_DECL.to_string()),

        _ => None,
    }
}

/// A path that always resolves on the local editor host. Built via
/// `editor.localPath(...)`.
const LOCAL_PATH_DECL: &str = r#"type LocalPath = { kind: "local"; value: string };"#;

/// A path that resolves on a specific window's authority filesystem. Built via
/// `editor.windowPath(windowId, ...)`.
const WINDOW_PATH_DECL: &str =
    r#"type WindowPath = { kind: "authority"; window: number; value: string };"#;

/// A path that resolves on the active window's authority filesystem, stated
/// explicitly. Built via `editor.authorityPath(...)`.
const AUTHORITY_PATH_DECL: &str = r#"type AuthorityPath = { kind: "authority"; value: string };"#;

/// Hand-written declaration for `AuthorityPayload` and its helpers.
/// See the doc comment on the match arm for why this isn't ts-rs.
///
/// Emitted as plain `type …` (not `export type …`) to match the rest of
/// the file — the generated d.ts lives in global scope and plugins
/// reference types by bare name without importing them.
const AUTHORITY_PAYLOAD_DECL: &str = r#"type AuthorityFilesystem = { kind: "local" };

type AuthoritySpawner =
  | { kind: "local" }
  | {
      kind: "docker-exec";
      container_id: string;
      user?: string | null;
      workspace?: string | null;
      env?: [string, string][];
    };

type AuthorityTerminalWrapper =
  | { kind: "host-shell" }
  | {
      kind: "explicit";
      command: string;
      args: string[];
      manages_cwd?: boolean;
    };

type AuthorityPayload = {
  filesystem: AuthorityFilesystem;
  spawner: AuthoritySpawner;
  terminal_wrapper: AuthorityTerminalWrapper;
  display_label?: string;
  /**
  * Optional host↔remote workspace path mapping. The dev-container
  * authority sets both roots (editor.getCwd() on host;
  * remoteWorkspaceFolder on container) so LSP URIs translate at the
  * host/container boundary. Local and SSH authorities omit it.
  */
  path_translation?: PathTranslationSpec;
};
type PathTranslationSpec = {
  host_root: string;
  remote_root: string;
};"#;

/// Hand-written declaration for `RemoteAgentSpec` (the
/// `editor.attachRemoteAgent(...)` payload), covering both transports
/// (`kubectl-exec` and `ssh`) and the window-mode fields. Keep in sync with
/// `crates/fresh-editor/src/services/authority/mod.rs`'s `RemoteAgentSpec` /
/// `RemoteTransportSpec` — this crate must not depend on `fresh-editor`, so the
/// shape is mirrored by hand.
const REMOTE_AGENT_SPEC_DECL: &str = r#"type RemoteAgentTransport = {
  kind: "kubectl-exec";
  /** kubeconfig context to select (`--context`); omit for the current one. */
  context?: string | null;
  namespace: string;
  pod: string;
  /** Target container in a multi-container pod (`-c`). */
  container?: string | null;
  /** Pod-side workspace root the terminal opens in. */
  workspace?: string | null;
} | {
  kind: "ssh";
  /** Login user. Optional — omit for `host` / `ssh://host`, letting ssh pick
  * the user from its own config or the current local user. */
  user?: string | null;
  host: string;
  port?: number | null;
  identity_file?: string | null;
  /** Remote directory to root the session at. */
  remote_path?: string | null;
  /** Extra `ssh` arguments (e.g. `-J jump`, `-o ProxyCommand=…`) applied to
  * every ssh invocation for this session. */
  extra_args?: string[];
};

type RemoteAgentSpec = {
  transport: RemoteAgentTransport;
  /**
  * Captured in-pod env (PATH/HOME/LANG/…) applied to LSP spawns and
  * binary-presence probes. Omit when no probe was run.
  */
  base_env?: [string, string][];
  /**
  * When true, attach as a NEW window (born-attached, coexisting with the
  * existing windows) instead of the default global restart that replaces the
  * whole editor's authority. The Orchestrator sets this so a cloud session is
  * a real session row beside local ones.
  */
  window?: boolean;
  /** Window label (window mode only). Omit to use the transport's display. */
  label?: string;
  /** Optional agent argv for the new window's seed terminal (window mode). */
  command?: string[];
};"#;

/// Hand-written declaration for `RemoteIndicatorStatePayload`. Keep in
/// sync with
/// `crates/fresh-editor/src/view/ui/status_bar.rs::RemoteIndicatorOverride`
/// (the struct this crate must not depend on).
const REMOTE_INDICATOR_STATE_DECL: &str = r#"type RemoteIndicatorStatePayload =
  | { kind: "local" }
  | { kind: "connecting"; label?: string | null }
  | { kind: "connected"; label?: string | null }
  | { kind: "failed_attach"; error?: string | null }
  | { kind: "disconnected"; label?: string | null };"#;

/// Types that are dependencies of other types and must always be included.
/// These are types referenced inside option structs or other complex types
/// that aren't directly in method signatures.
const DEPENDENCY_TYPES: &[&str] = &[
    "TextPropertyEntry",               // Used in CreateVirtualBuffer*Options.entries
    "TsCompositeLayoutConfig",         // Used in createCompositeBuffer opts
    "TsCompositeSourceConfig",         // Used in createCompositeBuffer opts.sources
    "TsCompositePaneStyle",            // Used in TsCompositeSourceConfig.style
    "TsCompositeHunk",                 // Used in createCompositeBuffer opts.hunks
    "TsCreateCompositeBufferOptions",  // Options for createCompositeBuffer
    "ViewportInfo",                    // Used by plugins for viewport queries
    "ScreenSize",                      // Used by editor.getScreenSize()
    "KeyEventPayload",                 // Used by editor.getNextKey()
    "SplitSnapshot",                   // Used by editor.listSplits()
    "LayoutHints",                     // Used by plugins for view transforms
    "ViewTokenWire",                   // Used by plugins for view transforms
    "ViewTokenWireKind",               // Used by ViewTokenWire
    "TokenColor",                      // Used by ViewTokenStyle fg/bg
    "ViewTokenStyle",                  // Used by ViewTokenWire
    "PromptSuggestion",                // Used by plugins for prompt suggestions
    "DirEntry",                        // Used by plugins for directory entries
    "BufferInfo",                      // Used by listBuffers, getBufferInfo
    "WindowInfo",                      // Used by listWindows
    "RemoteBackendInfo",               // Used by WindowInfo.remote
    "JsDiagnostic",                    // Used by getAllDiagnostics
    "JsRange",                         // Used by JsDiagnostic
    "JsPosition",                      // Used by JsRange
    "ActionSpec",                      // Used by executeActions
    "TsActionPopupAction",             // Used by ActionPopupOptions.actions
    "ActionPopupOptions",              // Used by showActionPopup
    "TsLspMenuItem",                   // Used by setLspMenuContributions
    "FileExplorerDecoration",          // Used by setFileExplorerDecorations
    "FileExplorerSlotEntry",           // Used by setFileExplorerSlots
    "FileExplorerLeadingSlot",         // Used by FileExplorerSlotEntry
    "FileExplorerTrailingSlot",        // Used by FileExplorerSlotEntry
    "FileExplorerTooltip",             // Used by FileExplorerTrailingSlot
    "FormatterPackConfig",             // Used by LanguagePackConfig.formatter
    "ProcessLimitsPackConfig",         // Used by LspServerPackConfig.process_limits
    "TerminalResult",                  // Used by createTerminal return type
    "CreateWindowWithTerminalOptions", // Used by createWindowWithTerminal opts
    "SessionWithTerminalResult",       // Used by createWindowWithTerminal return type
    "CreateTerminalOptions",           // Used by createTerminal opts parameter
    "CursorInfo",                      // Used by getPrimaryCursor, getAllCursors
    "OverlayOptions",                  // Used by TextPropertyEntry.style and InlineOverlay
    "OverlayColorSpec",                // Used by OverlayOptions.fg/bg
    "InlineOverlay",                   // Used by TextPropertyEntry.inlineOverlays
    "OffsetUnit",                      // Used by InlineOverlay.unit
    "StyledSegment",                   // Used by TextPropertyEntry.segments
    "GrammarInfoSnapshot",             // Used by listGrammars
    "AnimationRect",                   // Used by animateArea
    "PluginAnimationEdge",             // Used by PluginAnimationKind
    "PluginAnimationKind",             // Used by animateArea/animateVirtualBuffer
    // Widget library types (see docs/internal/plugin-widget-library-design.md)
    "HintEntry",      // Used by WidgetSpec::HintBar
    "ButtonKind",     // Used by WidgetSpec::Button.intent
    "TreeNode",       // Used by WidgetSpec::Tree.nodes
    "WidgetSpec",     // Used by mountWidgetPanel/updateWidgetPanel
    "WidgetAction",   // Used by widgetCommand
    "WidgetMutation", // Used by widgetMutate
    // Streaming-search pull handle (referenced via ts_raw on beginSearch)
    "SearchTakeResult",
    "SearchHandle",
    // Replace result (referenced via ts_raw on replaceInFile)
    "ReplaceResult",
];

/// Collect TypeScript type declarations based on referenced types from proc macro
///
/// Uses `JSEDITORAPI_REFERENCED_TYPES` to determine which types to include.
/// Also includes dependency types that are referenced by other types.
pub fn collect_ts_types() -> String {
    use crate::backend::quickjs_backend::JSEDITORAPI_REFERENCED_TYPES;

    let mut types = Vec::new();
    // Track by declaration content to prevent duplicates from aliases
    // (e.g., "CompositeHunk" and "TsCompositeHunk" both resolve to the same decl)
    let mut included_decls = std::collections::HashSet::new();

    // First, include dependency types (order matters - dependencies first)
    for type_name in DEPENDENCY_TYPES {
        if let Some(decl) = get_type_decl(type_name) {
            if included_decls.insert(decl.clone()) {
                types.push(decl);
            }
        }
    }

    // Collect types referenced by the API
    for type_name in JSEDITORAPI_REFERENCED_TYPES {
        if let Some(decl) = get_type_decl(type_name) {
            if included_decls.insert(decl.clone()) {
                types.push(decl);
            }
        } else {
            // Log warning for unknown types (these need to be added to get_type_decl)
            eprintln!(
                "Warning: Type '{}' is referenced in API but not registered in get_type_decl()",
                type_name
            );
        }
    }

    types.join("\n\n")
}

/// Validate TypeScript syntax using oxc parser
///
/// Returns Ok(()) if the syntax is valid, or an error with the parse errors.
pub fn validate_typescript(source: &str) -> Result<(), String> {
    let allocator = Allocator::default();
    let source_type = SourceType::d_ts();

    let parser_ret = Parser::new(&allocator, source, source_type).parse();

    if parser_ret.errors.is_empty() {
        Ok(())
    } else {
        let errors: Vec<String> = parser_ret
            .errors
            .iter()
            .map(|e: &oxc_diagnostics::OxcDiagnostic| e.to_string())
            .collect();
        Err(format!("TypeScript parse errors:\n{}", errors.join("\n")))
    }
}

/// Format TypeScript source code using oxc codegen
///
/// Parses the TypeScript and regenerates it with consistent formatting.
/// Returns the original source if parsing fails.
pub fn format_typescript(source: &str) -> String {
    let allocator = Allocator::default();
    let source_type = SourceType::d_ts();

    let parser_ret = Parser::new(&allocator, source, source_type).parse();

    if !parser_ret.errors.is_empty() {
        // Return original source if parsing fails
        return source.to_string();
    }

    // Generate formatted code from AST
    Codegen::new().build(&parser_ret.program).code
}

/// Generate and write the complete fresh.d.ts file
///
/// Combines ts-rs generated types with proc macro output,
/// validates the syntax, formats the output, and writes to disk.
pub fn write_fresh_dts() -> Result<(), String> {
    use crate::backend::quickjs_backend::{JSEDITORAPI_TS_EDITOR_API, JSEDITORAPI_TS_PREAMBLE};

    let ts_types = collect_ts_types();

    // After the macro-generated EditorAPI interface, merge in a
    // typed overload of `getPluginApi` that looks through the
    // `FreshPluginRegistry` interface (declared in the preamble,
    // augmented by each loaded plugin's `plugins.d.ts`). Declared
    // AFTER the base interface so TypeScript's overload resolution
    // prefers the typed form when the name is a known key; the
    // untyped `getPluginApi(name: string): unknown | null` from the
    // macro output is the fallback.
    let plugin_api_trailer = r#"

/**
 * Typed overload of `editor.getPluginApi`. When the caller passes a
 * key that some loaded plugin declared in `FreshPluginRegistry`, the
 * return type is narrowed to that plugin's API. Unknown names fall
 * through to the untyped `unknown | null` signature.
 */
interface EditorAPI {
  getPluginApi<K extends keyof FreshPluginRegistry>(name: K): FreshPluginRegistry[K] | null;
}

/**
 * Typed overload of `editor.defineConfigEnum`. The macro-generated
 * signature can't express `<E extends string>` propagating from the
 * `values` array into the return type, so it's declared here. Use
 * `as const` on the `values` array to get a literal-union return:
 *
 * ```ts
 * const mode = editor.defineConfigEnum("mode", {
 *   values: ["normal", "insert"] as const,
 *   default: "normal",
 * });
 * mode; // typed as "normal" | "insert"
 * ```
 *
 * Typed overload of `editor.getPluginConfig`. Plugins that declared
 * their fields via `defineConfigX` can pass the shape type explicitly:
 * `editor.getPluginConfig<{ autoEnable: boolean; ... }>()`. Without
 * the generic, falls back to `unknown`.
 */
interface EditorAPI {
  defineConfigEnum<E extends string>(
    name: string,
    options: { values: readonly E[]; default: NoInfer<E>; description?: string },
  ): E;
  getPluginConfig<T = unknown>(): T;
}

/**
 * Maps every hook event name to its payload type.
 *
 * Payloads match the flat JSON produced by `hook_args_to_json` on the Rust
 * side (`HookArgs` is `#[serde(untagged)]`, so each variant serializes as its
 * fields only). The TypeScript types here are derived directly from the Rust
 * field definitions and must be kept in sync with `fresh-core/src/hooks.rs`.
 *
 * `action` in `pre_command`/`post_command` is the serde JSON of the `Action`
 * enum: unit variants serialize as a plain string (e.g. `"MoveLeft"`),
 * tuple variants as a single-key object (e.g. `{"InsertChar": "a"}`).
 */
interface HookEventMap {
  // ── lifecycle ────────────────────────────────────────────────────────────
  editor_initialized: Record<string, never>;
  plugins_loaded: Record<string, never>;
  ready: Record<string, never>;
  focus_gained: Record<string, never>;
  authority_changed: { label: string };
  trust_changed: { level: "trusted" | "restricted" | "blocked" };

  // ── buffer lifecycle ─────────────────────────────────────────────────────
  buffer_activated: { buffer_id: number };
  buffer_deactivated: { buffer_id: number };
  buffer_closed: { buffer_id: number };

  // ── file I/O ─────────────────────────────────────────────────────────────
  before_file_open: { path: string };
  after_file_open: { path: string; buffer_id: number };
  before_file_save: { path: string; buffer_id: number };
  after_file_save: { path: string; buffer_id: number };
  /**
   * Fired by the file explorer after a paste/duplicate/etc. mutates
   * the filesystem without going through a buffer save. Plugins that
   * surface FS-derived state (git status badges, etc.) should
   * subscribe in addition to `after_file_save` to refresh on
   * explorer-driven changes too.
   */
  after_file_explorer_change: { path: string };

  // ── text edits ───────────────────────────────────────────────────────────
  before_insert: { buffer_id: number; position: number; text: string };
  after_insert: {
    buffer_id: number;
    position: number;
    text: string;
    affected_start: number;
    affected_end: number;
    start_line: number;
    end_line: number;
    lines_added: number;
  };
  before_delete: { buffer_id: number; start: number; end: number };
  after_delete: {
    buffer_id: number;
    start: number;
    end: number;
    deleted_text: string;
    affected_start: number;
    deleted_len: number;
    start_line: number;
    end_line: number;
    lines_removed: number;
  };

  // ── cursor & viewport ────────────────────────────────────────────────────
  cursor_moved: {
    buffer_id: number;
    cursor_id: number;
    old_position: number;
    new_position: number;
    line: number;
    text_properties: Record<string, unknown>[];
  };
  viewport_changed: {
    split_id: number;
    buffer_id: number;
    top_byte: number;
    top_line: number | null;
    width: number;
    height: number;
  };

  // ── rendering ────────────────────────────────────────────────────────────
  render_start: { buffer_id: number };
  render_line: {
    buffer_id: number;
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
  };
  lines_changed: {
    buffer_id: number;
    lines: { line_number: number; byte_start: number; byte_end: number; content: string }[];
    /** Buffer version these byte ranges were captured at. Pass back to
     * coordinate-mapping APIs to repair stale offsets from this batch. */
    epoch: number;
  };
  view_transform_request: {
    buffer_id: number;
    split_id: number;
    viewport_start: number;
    viewport_end: number;
    tokens: ViewTokenWire[];
    cursor_positions: number[];
  };

  // ── commands ─────────────────────────────────────────────────────────────
  pre_command: { action: string | Record<string, unknown> };
  post_command: { action: string | Record<string, unknown> };
  idle: { milliseconds: number };
  resize: { width: number; height: number };

  // ── prompts ──────────────────────────────────────────────────────────────
  prompt_changed: { prompt_type: string; input: string };
  prompt_confirmed: { prompt_type: string; input: string; selected_index: number | null };
  prompt_cancelled: { prompt_type: string; input: string };
  prompt_selection_changed: { prompt_type: string; selected_index: number };

  // ── mouse ────────────────────────────────────────────────────────────────
  mouse_click: MouseClickHookArgs;
  mouse_move: { column: number; row: number; content_x: number; content_y: number };
  mouse_scroll: { buffer_id: number; delta: number; col: number; row: number };

  // ── LSP ──────────────────────────────────────────────────────────────────
  diagnostics_updated: { uri: string; count: number };
  lsp_references: {
    symbol: string;
    locations: { file: string; line: number; column: number }[];
  };
  lsp_implementation: {
    symbol: string;
    locations: { file: string; line: number; column: number }[];
  };
  lsp_server_request: {
    language: string;
    method: string;
    server_command: string;
    params: string | null;
  };
  lsp_server_error: {
    language: string;
    server_command: string;
    error_type: string;
    message: string;
  };
  lsp_status_clicked: {
    language: string;
    has_error: boolean;
    missing_servers: string[];
    user_dismissed: boolean;
  };

  // ── UI events ────────────────────────────────────────────────────────────
  action_popup_result: { popup_id: string; action_id: string };
  /**
   * User clicked a plugin-registered status-bar token. Subscribers
   * filter by `plugin_name` + `token_name`. Use this to re-open a
   * deferred prompt or surface the relevant settings UI for whatever
   * the token represents (e.g. trust chip → trust-elevation popup).
   */
  status_bar_token_clicked: { plugin_name: string; token_name: string };
  process_output: { process_id: number; data: string };
  language_changed: { buffer_id: number; language: string };
  theme_inspect_key: { theme_name: string; key: string };
  keyboard_shortcuts: { bindings: { key: string; action: string }[] };

  // ── PTY terminals (see crates/fresh-core/src/hooks.rs) ───────────────────
  // `window_id` is the editor window owning the terminal (== session id),
  // so a plugin can attribute output to a session: output from ANY terminal
  // in the window counts, and it fires on every PTY read (in-place redraws
  // and carriage-return progress bars register, not just newlines).
  terminal_output: { terminal_id: number; window_id: number; last_line: string };
  terminal_exit: { terminal_id: number; window_id: number; exit_code: number | null };

  // ── filesystem watching (watchPath plugin API) ────────────────────────────
  path_changed: {
    handle: number;
    path: string;
    /** "modify" | "create" | "delete" | "rename" | "other" */
    kind: string;
  };

  // ── editor sessions (Orchestrator; see orchestrator-sessions-design.md) ────────
  window_created: { id: number; label: string; root: string };
  window_closed: { id: number };
  active_window_changed: { previous_id: number | null; active_id: number };

  // ── widget runtime ───────────────────────────────────────────────────────
  /**
   * A widget mounted via `editor.mountWidgetPanel` emitted a
   * semantic event. Fired when the host's hit-test routes a mouse
   * click to a `Toggle` / `Button` widget node within a mounted
   * widget panel. See `docs/internal/plugin-widget-library-design.md`.
   *
   * Panel ids are plugin-local: the host keys panels by
   * (plugin, id) and delivers each event only to the plugin that
   * owns the panel, so ids never need to be globally unique.
   * Routing is by `panel_id` (matches the id the plugin allocated
   * at mount time) plus `widget_key` (the stable `key` set on the
   * widget spec node, or empty when the spec did not assign one).
   *
   * `event_type` and `payload` shapes:
   *   * Toggle: `event_type = "toggle"`, `payload = { checked: <new> }`.
   *   * Button: `event_type = "activate"`, `payload = {}`.
   */
  widget_event: {
    panel_id: number;
    widget_key: string;
    event_type: string;
    payload: Record<string, unknown>;
  };
}

/**
 * Typed overloads of `editor.on` / `editor.off`.
 *
 * When the event name is a key of `HookEventMap` the handler receives a
 * fully-typed payload — TypeScript will flag misspelled field accesses at
 * compile time. Unknown event names fall through to the untyped base
 * signatures in the EditorAPI interface.
 *
 * Both function-value and handler-name forms are supported:
 *
 * ```ts
 * editor.on("buffer_activated", (args) => { /* args.buffer_id is number *\/ });
 * editor.on("buffer_activated", "myHandler");   // registerHandler("myHandler", fn)
 * ```
 */
interface EditorAPI {
  on<K extends keyof HookEventMap>(
    eventName: K,
    handler: (args: HookEventMap[K]) => boolean | void | Promise<boolean | void>,
  ): void;
  on<K extends keyof HookEventMap>(eventName: K, handlerName: string): void;
  off<K extends keyof HookEventMap>(
    eventName: K,
    handler: (args: HookEventMap[K]) => boolean | void | Promise<boolean | void>,
  ): void;
  off<K extends keyof HookEventMap>(eventName: K, handlerName: string): void;
  /**
   * Create a buffer group: multiple panels appearing as one tab.
   * This is an async runtime binding (not a direct #[qjs] method).
   */
  createBufferGroup(
    name: string,
    mode: string,
    layout: unknown,
  ): Promise<BufferGroupResult>;
}
"#;

    let content = format!(
        "{}\n{}\n{}{}",
        JSEDITORAPI_TS_PREAMBLE, ts_types, JSEDITORAPI_TS_EDITOR_API, plugin_api_trailer
    );

    // Validate the generated TypeScript syntax
    validate_typescript(&content)?;

    // Format the TypeScript
    let formatted = format_typescript(&content);

    // Determine output path - write to fresh-editor/plugins/lib/fresh.d.ts
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let output_path = std::path::Path::new(&manifest_dir)
        .parent() // crates/
        .and_then(|p| p.parent()) // workspace root
        .map(|p| p.join("crates/fresh-editor/plugins/lib/fresh.d.ts"))
        .unwrap_or_else(|| std::path::PathBuf::from("plugins/lib/fresh.d.ts"));

    // Only write if content changed
    let should_write = match std::fs::read_to_string(&output_path) {
        Ok(existing) => existing != formatted,
        Err(_) => true,
    };

    if should_write {
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
        }
        std::fs::write(&output_path, &formatted).map_err(|e| e.to_string())?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Generate, validate, format, and write fresh.d.ts
    /// Run with: cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored --nocapture
    #[test]
    #[ignore]
    fn write_fresh_dts_file() {
        // write_fresh_dts validates syntax and formats before writing
        write_fresh_dts().expect("Failed to write fresh.d.ts");
        println!("Successfully generated, validated, and formatted fresh.d.ts");
    }

    /// Type check all plugins using TypeScript compiler
    /// Skips if tsc is not available in PATH
    /// Run with: cargo test -p fresh-plugin-runtime type_check_plugins -- --ignored --nocapture
    #[test]
    #[ignore]
    fn type_check_plugins() {
        // Check if tsc is available
        let tsc_check = std::process::Command::new("tsc").arg("--version").output();

        match tsc_check {
            Ok(output) if output.status.success() => {
                println!(
                    "Found tsc: {}",
                    String::from_utf8_lossy(&output.stdout).trim()
                );
            }
            _ => {
                println!("tsc not found in PATH, skipping type check test");
                return;
            }
        }

        // Find the check-types.sh script
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
        let script_path = std::path::Path::new(&manifest_dir)
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("crates/fresh-editor/plugins/check-types.sh"))
            .expect("Failed to find check-types.sh");

        println!("Running type check script: {}", script_path.display());

        // Run the check-types.sh script
        let output = std::process::Command::new("bash")
            .arg(&script_path)
            .output()
            .expect("Failed to run check-types.sh");

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        println!("stdout:\n{}", stdout);
        if !stderr.is_empty() {
            println!("stderr:\n{}", stderr);
        }

        // The script outputs "X file(s) had type errors" if there are errors
        if stdout.contains("had type errors") || !output.status.success() {
            panic!(
                "TypeScript type check failed. Run 'crates/fresh-editor/plugins/check-types.sh' to see details."
            );
        }

        println!("All plugins type check successfully!");
    }

    // ========================================================================
    // Type declaration tests
    // ========================================================================

    #[test]
    fn test_get_type_decl_returns_all_expected_types() {
        let expected_types = vec![
            "BufferInfo",
            "WindowInfo",
            "RemoteBackendInfo",
            "CursorInfo",
            "ViewportInfo",
            "ScreenSize",
            "KeyEventPayload",
            "SplitSnapshot",
            "ActionSpec",
            "BufferSavedDiff",
            "LayoutHints",
            "SpawnResult",
            "BackgroundProcessResult",
            "TerminalResult",
            "CreateTerminalOptions",
            "CreateWindowWithTerminalOptions",
            "SessionWithTerminalResult",
            "TsCompositeLayoutConfig",
            "TsCompositeSourceConfig",
            "TsCompositePaneStyle",
            "TsCompositeHunk",
            "TsCreateCompositeBufferOptions",
            "ViewTokenWireKind",
            "TokenColor",
            "ViewTokenStyle",
            "ViewTokenWire",
            "TsActionPopupAction",
            "ActionPopupOptions",
            "TsHighlightSpan",
            "FileExplorerDecoration",
            "TextPropertyEntry",
            "CreateVirtualBufferOptions",
            "CreateVirtualBufferInSplitOptions",
            "CreateVirtualBufferInExistingSplitOptions",
            "TextPropertiesAtCursor",
            "VirtualBufferResult",
            "PromptSuggestion",
            "DirEntry",
            "JsDiagnostic",
            "JsRange",
            "JsPosition",
            "LanguagePackConfig",
            "LspServerPackConfig",
            "ProcessLimitsPackConfig",
            "FormatterPackConfig",
        ];

        for type_name in &expected_types {
            assert!(
                get_type_decl(type_name).is_some(),
                "get_type_decl should return a declaration for '{}'",
                type_name
            );
        }
    }

    #[test]
    fn test_get_type_decl_aliases_resolve_same() {
        // Rust name aliases should produce the same declaration as ts-rs name
        let alias_pairs = vec![
            ("CompositeHunk", "TsCompositeHunk"),
            ("CompositeLayoutConfig", "TsCompositeLayoutConfig"),
            ("CompositeSourceConfig", "TsCompositeSourceConfig"),
            ("CompositePaneStyle", "TsCompositePaneStyle"),
            (
                "CreateCompositeBufferOptions",
                "TsCreateCompositeBufferOptions",
            ),
            ("ActionPopupAction", "TsActionPopupAction"),
            ("Suggestion", "PromptSuggestion"),
            ("JsTextPropertyEntry", "TextPropertyEntry"),
        ];

        for (rust_name, ts_name) in &alias_pairs {
            let rust_decl = get_type_decl(rust_name);
            let ts_decl = get_type_decl(ts_name);
            assert!(
                rust_decl.is_some(),
                "get_type_decl should handle Rust name '{}'",
                rust_name
            );
            assert_eq!(
                rust_decl, ts_decl,
                "Alias '{}' and '{}' should produce identical declarations",
                rust_name, ts_name
            );
        }
    }

    #[test]
    fn test_terminal_types_exist() {
        let terminal_result = get_type_decl("TerminalResult");
        assert!(
            terminal_result.is_some(),
            "TerminalResult should be defined"
        );
        let decl = terminal_result.unwrap();
        assert!(
            decl.contains("bufferId"),
            "TerminalResult should have bufferId field"
        );
        assert!(
            decl.contains("terminalId"),
            "TerminalResult should have terminalId field"
        );
        assert!(
            decl.contains("splitId"),
            "TerminalResult should have splitId field"
        );

        let terminal_opts = get_type_decl("CreateTerminalOptions");
        assert!(
            terminal_opts.is_some(),
            "CreateTerminalOptions should be defined"
        );
    }

    #[test]
    fn test_cursor_info_type_exists() {
        let cursor_info = get_type_decl("CursorInfo");
        assert!(cursor_info.is_some(), "CursorInfo should be defined");
        let decl = cursor_info.unwrap();
        assert!(
            decl.contains("position"),
            "CursorInfo should have position field"
        );
        assert!(
            decl.contains("selection"),
            "CursorInfo should have selection field"
        );
    }

    #[test]
    fn test_collect_ts_types_no_duplicates() {
        let output = collect_ts_types();
        let lines: Vec<&str> = output.lines().collect();

        // Check for duplicate type/interface declarations
        let mut declarations = std::collections::HashSet::new();
        for line in &lines {
            let trimmed = line.trim();
            // Match type declarations: "type Foo = {" or "type Foo ="
            if trimmed.starts_with("type ") && trimmed.contains('=') {
                let name = trimmed
                    .strip_prefix("type ")
                    .unwrap()
                    .split(|c: char| c == '=' || c.is_whitespace())
                    .next()
                    .unwrap();
                assert!(
                    declarations.insert(name.to_string()),
                    "Duplicate type declaration found: '{}'",
                    name
                );
            }
        }
    }

    #[test]
    fn test_collect_ts_types_includes_dependency_types() {
        let output = collect_ts_types();
        let required_types = [
            "TextPropertyEntry",
            "TsCompositeLayoutConfig",
            "TsCompositeSourceConfig",
            "TsCompositePaneStyle",
            "TsCompositeHunk",
            "TsCreateCompositeBufferOptions",
            "PromptSuggestion",
            "BufferInfo",
            "CursorInfo",
            "TerminalResult",
            "CreateTerminalOptions",
        ];

        for type_name in &required_types {
            assert!(
                output.contains(type_name),
                "collect_ts_types output should contain type '{}'",
                type_name
            );
        }
    }

    #[test]
    fn test_generated_dts_validates_as_typescript() {
        use crate::backend::quickjs_backend::{JSEDITORAPI_TS_EDITOR_API, JSEDITORAPI_TS_PREAMBLE};

        let ts_types = collect_ts_types();
        let content = format!(
            "{}\n{}\n{}",
            JSEDITORAPI_TS_PREAMBLE, ts_types, JSEDITORAPI_TS_EDITOR_API
        );

        validate_typescript(&content).expect("Generated TypeScript should be syntactically valid");
    }

    #[test]
    fn test_generated_dts_no_undefined_type_references() {
        use crate::backend::quickjs_backend::{JSEDITORAPI_TS_EDITOR_API, JSEDITORAPI_TS_PREAMBLE};

        let ts_types = collect_ts_types();
        let content = format!(
            "{}\n{}\n{}",
            JSEDITORAPI_TS_PREAMBLE, ts_types, JSEDITORAPI_TS_EDITOR_API
        );

        // Collect all defined type names
        let mut defined_types = std::collections::HashSet::new();
        // Built-in types
        for builtin in &[
            "number",
            "string",
            "boolean",
            "void",
            "unknown",
            "null",
            "undefined",
            "Record",
            "Array",
            "Promise",
            "ProcessHandle",
            "PromiseLike",
            "BufferId",
            "SplitId",
            "EditorAPI",
        ] {
            defined_types.insert(builtin.to_string());
        }

        // Extract defined types from declarations
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("type ") && trimmed.contains('=') {
                if let Some(name) = trimmed
                    .strip_prefix("type ")
                    .unwrap()
                    .split(|c: char| c == '=' || c.is_whitespace())
                    .next()
                {
                    defined_types.insert(name.to_string());
                }
            }
            if trimmed.starts_with("interface ") {
                if let Some(name) = trimmed
                    .strip_prefix("interface ")
                    .unwrap()
                    .split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                {
                    defined_types.insert(name.to_string());
                }
            }
        }

        // Extract capitalized identifiers from EditorAPI method signature lines only
        // (skip JSDoc comment lines which contain prose with capitalized words)
        let interface_section = JSEDITORAPI_TS_EDITOR_API;
        let mut undefined_refs = Vec::new();

        for line in interface_section.lines() {
            let trimmed = line.trim();

            // Skip JSDoc comments and blank lines
            if trimmed.starts_with('*')
                || trimmed.starts_with("/*")
                || trimmed.starts_with("//")
                || trimmed.is_empty()
                || trimmed == "{"
                || trimmed == "}"
            {
                continue;
            }

            // This should be a method signature line
            for word in trimmed.split(|c: char| !c.is_alphanumeric() && c != '_') {
                if word.is_empty() {
                    continue;
                }
                // Type references start with uppercase letter
                if word.chars().next().is_some_and(|c| c.is_uppercase())
                    && !defined_types.contains(word)
                {
                    undefined_refs.push(word.to_string());
                }
            }
        }

        // Remove duplicates for clearer error message
        undefined_refs.sort();
        undefined_refs.dedup();

        assert!(
            undefined_refs.is_empty(),
            "Found undefined type references in EditorAPI interface: {:?}",
            undefined_refs
        );
    }

    #[test]
    fn test_editor_api_cursor_methods_have_typed_returns() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // getPrimaryCursor should return CursorInfo | null, not unknown
        assert!(
            api.contains("getPrimaryCursor(): CursorInfo | null;"),
            "getPrimaryCursor should return CursorInfo | null, got: {}",
            api.lines()
                .find(|l| l.contains("getPrimaryCursor"))
                .unwrap_or("not found")
        );

        // getAllCursors should return CursorInfo[], not unknown
        assert!(
            api.contains("getAllCursors(): CursorInfo[];"),
            "getAllCursors should return CursorInfo[], got: {}",
            api.lines()
                .find(|l| l.contains("getAllCursors"))
                .unwrap_or("not found")
        );

        // getAllCursorPositions should return number[], not unknown
        assert!(
            api.contains("getAllCursorPositions(): number[];"),
            "getAllCursorPositions should return number[], got: {}",
            api.lines()
                .find(|l| l.contains("getAllCursorPositions"))
                .unwrap_or("not found")
        );
    }

    #[test]
    fn test_editor_api_terminal_methods_use_defined_types() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // createTerminal should use CreateTerminalOptions and TerminalResult
        assert!(
            api.contains("CreateTerminalOptions"),
            "createTerminal should reference CreateTerminalOptions"
        );
        assert!(
            api.contains("TerminalResult"),
            "createTerminal should reference TerminalResult"
        );
    }

    #[test]
    fn test_editor_api_composite_methods_use_ts_prefix_types() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // updateCompositeAlignment should use TsCompositeHunk (not CompositeHunk)
        assert!(
            api.contains("TsCompositeHunk[]"),
            "updateCompositeAlignment should use TsCompositeHunk[], not CompositeHunk[]"
        );

        // createCompositeBuffer should use TsCreateCompositeBufferOptions
        assert!(
            api.contains("TsCreateCompositeBufferOptions"),
            "createCompositeBuffer should use TsCreateCompositeBufferOptions"
        );
    }

    #[test]
    fn test_editor_api_prompt_suggestions_use_prompt_suggestion() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // setPromptSuggestions should use PromptSuggestion (not Suggestion)
        assert!(
            api.contains("PromptSuggestion[]"),
            "setPromptSuggestions should use PromptSuggestion[], not Suggestion[]"
        );
    }

    #[test]
    fn test_all_editor_api_methods_present() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // Comprehensive list of all expected methods
        let expected_methods = vec![
            "apiVersion",
            "getActiveBufferId",
            "getActiveSplitId",
            "listBuffers",
            "debug",
            "info",
            "warn",
            "error",
            "setStatus",
            "copyToClipboard",
            "setClipboard",
            "registerCommand",
            "unregisterCommand",
            "setContext",
            "executeAction",
            "cancelPrompt",
            "getCursorPosition",
            "getBufferPath",
            "getBufferLength",
            "isBufferModified",
            "saveBufferToPath",
            "getBufferInfo",
            "getPrimaryCursor",
            "getAllCursors",
            "getAllCursorPositions",
            "getViewport",
            "getScreenSize",
            "getCursorLine",
            "getLineStartPosition",
            "getLineEndPosition",
            "getBufferLineCount",
            "scrollToLineCenter",
            "findBufferByPath",
            "getBufferSavedDiff",
            "insertText",
            "deleteRange",
            "insertAtCursor",
            "openFile",
            "openFileInSplit",
            "showBuffer",
            "closeBuffer",
            "animateArea",
            "animateVirtualBuffer",
            "cancelAnimation",
            "on",
            "off",
            "getEnv",
            "getCwd",
            "pathJoin",
            "pathDirname",
            "pathBasename",
            "pathExtname",
            "pathIsAbsolute",
            "utf8ByteLength",
            "fileExists",
            "readFile",
            "writeFile",
            "readDir",
            "createDir",
            "removePath",
            "renamePath",
            "copyPath",
            "getTempDir",
            "getConfig",
            "getUserConfig",
            "getPluginConfig",
            "defineConfigBoolean",
            "defineConfigInteger",
            "defineConfigNumber",
            "defineConfigString",
            // `defineConfigEnum` is hand-written in the d.ts trailer
            // (generic <E> propagates `values` into the return), so it's
            // NOT in the macro-generated EditorAPI interface.
            "defineConfigStringArray",
            "reloadConfig",
            "reloadThemes",
            "reloadAndApplyTheme",
            "registerGrammar",
            "registerLanguageConfig",
            "registerLspServer",
            "reloadGrammars",
            "getConfigDir",
            "getDataDir",
            "getWorkingDataDir",
            "getTerminalDir",
            "getThemesDir",
            "applyTheme",
            "getThemeSchema",
            "getBuiltinThemes",
            "getAllThemes",
            "getThemeData",
            "saveThemeFile",
            "themeFileExists",
            "deleteTheme",
            "fileStat",
            "isProcessRunning",
            "killProcess",
            "pluginTranslate",
            "createCompositeBuffer",
            "updateCompositeAlignment",
            "closeCompositeBuffer",
            "flushLayout",
            "compositeNextHunk",
            "compositePrevHunk",
            "getHighlights",
            "addOverlay",
            "clearNamespace",
            "clearAllOverlays",
            "clearOverlaysInRange",
            "clearOverlaysInRangeForNamespace",
            "removeOverlay",
            "addConceal",
            "clearConcealNamespace",
            "clearConcealsInRange",
            "charWidth",
            "stringWidth",
            "clearConcealsInRangeForNamespace",
            "addSoftBreak",
            "clearSoftBreakNamespace",
            "clearSoftBreaksInRange",
            "submitViewTransform",
            "clearViewTransform",
            "setLayoutHints",
            "setFileExplorerDecorations",
            "clearFileExplorerDecorations",
            "setFileExplorerSlots",
            "clearFileExplorerSlots",
            "addVirtualText",
            "removeVirtualText",
            "removeVirtualTextsByPrefix",
            "clearVirtualTexts",
            "clearVirtualTextNamespace",
            "clearVirtualLinesInRange",
            "addVirtualLine",
            "prompt",
            "startPrompt",
            "startPromptWithInitial",
            "setPromptSuggestions",
            "setPromptSelectedIndex",
            "setPromptInputSync",
            "defineMode",
            "setEditorMode",
            "getEditorMode",
            "closeSplit",
            "setSplitBuffer",
            "focusSplit",
            "setSplitScroll",
            "setSplitRatio",
            "setSplitLabel",
            "clearSplitLabel",
            "getSplitByLabel",
            "distributeSplitsEvenly",
            "setBufferCursor",
            "setLineIndicator",
            "clearLineIndicators",
            "setLineNumbers",
            "setIndentationGuide",
            "setViewMode",
            "setViewState",
            "getViewState",
            "createMarker",
            "updateMarker",
            "deleteMarker",
            "queryMarkers",
            "getMarker",
            "setGlobalState",
            "getGlobalState",
            "setLineWrap",
            "createScrollSyncGroup",
            "setScrollSyncAnchors",
            "removeScrollSyncGroup",
            "executeActions",
            "showActionPopup",
            "setLspMenuContributions",
            "disableLspForLanguage",
            "setLspRootUri",
            "getAllDiagnostics",
            "getHandlers",
            "createVirtualBuffer",
            "createVirtualBufferInSplit",
            "createVirtualBufferInExistingSplit",
            "setVirtualBufferContent",
            "getTextPropertiesAtCursor",
            "spawnProcess",
            "spawnProcessWait",
            "spawnHostProcess",
            "httpFetch",
            "setAuthority",
            "clearAuthority",
            "setRemoteIndicatorState",
            "clearRemoteIndicatorState",
            "getBufferText",
            "delay",
            "sendLspRequest",
            "spawnBackgroundProcess",
            "killBackgroundProcess",
            "createTerminal",
            "sendTerminalInput",
            "closeTerminal",
            "signalWindow",
            "refreshLines",
            "getCurrentLocale",
            "loadPlugin",
            "unloadPlugin",
            "reloadPlugin",
            "listPlugins",
            "mountFloatingWidget",
            "updateFloatingWidget",
            "unmountFloatingWidget",
            "floatingPanelControl",
            "setActiveWindowAnimated",
        ];

        let mut missing = Vec::new();
        for method in &expected_methods {
            // Check that the method name appears followed by ( in the API
            let pattern = format!("{}(", method);
            if !api.contains(&pattern) {
                missing.push(*method);
            }
        }

        assert!(
            missing.is_empty(),
            "Missing methods in EditorAPI interface: {:?}",
            missing
        );
    }
}
