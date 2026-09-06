/// <reference path="./lib/fresh.d.ts" />
/// <reference path="./lib/types.ts" />
/// <reference path="./lib/virtual-buffer-factory.ts" />

// Review Diff Plugin
// Magit-style split-panel UI for reviewing and staging code changes.
// Left panel: file list (staged/unstaged/untracked). Right panel: diff.
// Actions: stage/unstage/discard hunks or files, line comments, export.
const editor = getEditor();

import { createVirtualBufferFactory } from "./lib/virtual-buffer-factory.ts";
import {
  type GitCommit,
  buildCommitDetailEntries,
  buildCommitLogEntries,
  buildDetailPlaceholderEntries,
  commitDetailSyntaxRegions,
  fetchCommitShow,
  fetchGitLog,
} from "./lib/git_history.ts";
import { type GitRepo, diffArgs, resolveGitRepo, withDiffArgs } from "./lib/git_repo.ts";
import type { HintEntry, TreeNode, WidgetSpec } from "./lib/widgets.ts";
import {
  WidgetPanel,
  button,
  col,
  flexSpacer,
  hintBar,
  key,
  list,
  raw,
  row,
  spacer,
  styledRow,
  text,
  textInputChar,
  textInputKey,
  tree,
  treeNode,
} from "./lib/widgets.ts";
const VirtualBufferFactory = createVirtualBufferFactory(editor);

/**
 * `editor.t` returns the key itself when a string is missing, so the common
 * `editor.t(key) || fallback` idiom never reaches the fallback — the user sees
 * `status.loading` on screen instead. `tr` does the check the idiom meant to.
 */
function tr(key: string, args?: Record<string, string>): string | null {
    const raw = args ? editor.t(key, args) : editor.t(key);
    if (!raw || raw === key) return null;
    return raw;
}




/**
 * A review comment attached to a specific line in a file
 * Uses file line numbers (not hunk-relative) so comments survive rebases
 */
interface ReviewComment {
  id: string;
  hunk_id: string;            // For grouping, but line numbers are primary
  file: string;               // File path
  text: string;
  timestamp: string;
  // Line positioning using actual file line numbers
  old_line?: number;          // Line number in old file version (for - lines)
  new_line?: number;          // Line number in new file version (for + lines)
  line_content?: string;      // The actual line content for context/matching
  line_type?: 'add' | 'remove' | 'context';  // Type of line
  // Selection range (for multi-line comments)
  selection?: {
    start_line: number;       // Start line in file
    end_line: number;         // End line in file
    version: 'old' | 'new';   // Which file version
  };
}

/**
 * One file's block of `git diff` output, kept as git wrote it. The stream
 * shows these bytes verbatim: the host's diff grammar colours them (file
 * by file, in each file's own language) and its diff gutter numbers them
 * from the hunk headers, so nothing here is laid out per row.
 */
interface RawFile {
  path: string;
  gitStatus: 'staged' | 'unstaged' | 'untracked';
  /** `diff --git` through the end of the last hunk, newline-terminated.
   *  The `diff --git` row is what retargets the highlighter to this
   *  file's language; the stream shows it as the file's header. */
  text: string;
}

/**
 * A diff hunk (block of changes)
 */
interface Hunk {
  id: string;
  file: string;
  range: { start: number; end: number };  // new file line range
  oldRange: { start: number; end: number };  // old file line range
  type: 'add' | 'remove' | 'modify';
  lines: string[];
  status?: string;
  contextHeader: string;
  gitStatus?: 'staged' | 'unstaged' | 'untracked';
  /** The block this hunk came from, and where in it (offsets into
   *  `raw.text`): its `@@` row, its first body row, and the end of its
   *  last one, past the newline. */
  raw: RawFile;
  headerStart: number;
  bodyStart: number;
  end: number;
}

/** A note box spliced into a hunk's body: it follows `hunk.lines[afterLine]`
 *  and takes `rows` rows. */
interface StreamNote {
  afterLine: number;
  rows: number;
  commentId: string;
}

/** A hunk as laid out in the stream. */
interface StreamHunk {
  hunk: Hunk;
  /** 1-indexed row of the `@@` line. */
  headerRow: number;
  /** Rows the hunk spans, header and note boxes included. */
  rowCount: number;
  /** Note boxes in body order. */
  notes: StreamNote[];
}

/**
 * A file entry from git status --porcelain
 */
interface FileEntry {
  path: string;
  status: string;           // 'M', 'A', 'D', 'R', 'C', '?'
  category: 'staged' | 'unstaged' | 'untracked';
  origPath?: string;         // for renames/copies
}

/**
 * Review Session State
 *
 * Scrolling and cursor tracking inside the panel buffers is handled by the
 * editor core natively — this state only mirrors what the plugin needs to
 * know between events (selected file, focused panel, hunk header rows for
 * `n`/`p` jumps).
 */
/**
 * Why the file list is empty. `null` means `state.files` has entries; the
 * other two distinguish "cwd is not a git repo" from "repo is clean" so the
 * panels can show a specific message instead of rendering byte-identically.
 */
type EmptyStateReason = 'not_git' | 'clean' | null;

/**
 * Which slice of history the current review-diff session is inspecting.
 *
 * - `'worktree'`: the default mode — what `git status` reports right now.
 *   No single SHA fingerprints this mode (the working tree is volatile), so
 *   comments are keyed only by repo root and restored on a best-effort basis
 *   using `file`/`old_line`/`new_line`/`line_content`.
 * - `'range'`: reviewing a static slice (single commit or `A..B` range).
 *   The diff is stable, so comments restore 1:1.
 */
type ReviewMode = 'worktree' | 'range';

interface ReviewRange {
  /** `git diff <from>` left-hand-side. */
  from: string;
  /** `git diff ... <to>` right-hand-side. */
  to: string;
  /** Human-readable label for status bar / layout name. */
  label: string;
  /** Optional explicit `git` argv that produces the unified diff (e.g. a
   *  stash: `["stash","show","-p",...]`). When set, used instead of the
   *  default `diff <from>..<to>`. Lets stash/other read-only sources reuse
   *  the whole range pipeline. */
  command?: string[];
}

interface ReviewState {
  hunks: Hunk[];
  comments: ReviewComment[];
  note: string;
  reviewBufferId: number | null;
  /** Review slice: working tree vs. static commit / range. */
  mode: ReviewMode;
  /** Populated when `mode === 'range'`. */
  range: ReviewRange | null;
  /**
   * The resolved git repository for this review, or `null` when not inside
   * one. Resolved once at each review entry point *before* any git command
   * runs, so `gitCwd()` reads the right root. Its `root` is the stable key
   * for persistence.
   */
  repo: GitRepo | null;
  /**
   * Persistence key within the repo's review dir:
   *   `worktree`            — `mode === 'worktree'`
   *   `range-<from>__<to>`  — `mode === 'range'`
   * Filename-safe characters only (see `sanitizeKeySegment`).
   */
  reviewKey: string;
  // Files with changes (used for section grouping + headers in the
  // unified stream). Order matches the order they appear in the diff.
  files: FileEntry[];
  emptyState: EmptyStateReason;
  viewportWidth: number;
  viewportHeight: number;
  focusPanel: 'files' | 'diff' | 'comments';
  // Which composite pane (0 = OLD, 1 = NEW) the side-by-side diff focus is
  // on. Tab steps files → OLD → NEW → comments; tracked here because the
  // host exposes only a "toggle pane" action, not "set pane".
  compositePane: 0 | 1;
  // Index of the focused hunk within the side-by-side composite's file.
  // Tracked synchronously so rapid n/p can't race the async cursor lookup
  // (the composite always opens focused on hunk 0).
  compositeHunkIdx: number;
  groupId: number | null;
  panelBuffers: Record<string, number>;
  // Caches populated each time the unified diff stream is rebuilt —
  // used by `n`/`p` hunk navigation, to translate row numbers into byte
  // positions for `setBufferCursor`, and to draw the cursor-line
  // highlight overlay. `diffLineByteOffsets` has length `(rowCount + 1)`:
  // index `i` is the byte offset of row `i + 1`, and the final entry is
  // the total buffer length.
  hunkHeaderRows: number[];        // 1-indexed row numbers in the unified buffer
  diffLineByteOffsets: number[];
  diffCursorRow: number;           // 1-indexed, last known cursor row in diff buffer
  // Maps file key (`${path}\0${category}`) -> 1-indexed row of the
  // file-header row in the unified stream. Used by mouse/collapse/sticky.
  fileHeaderRows: Record<string, number>;
  // Files that are currently collapsed (`${path}\0${category}` keys).
  // Persists across refreshes within a session; cleared on start_review_diff.
  collapsedFiles: Set<string>;
  // Sections (categories) that are currently collapsed. Same persistence
  // rules as `collapsedFiles`.
  collapsedSections: Set<string>;
  // Directory groups collapsed in the FILES sidebar, keyed by
  // `${category}\0${dir}`.
  collapsedDirs: Set<string>;
  // Hunks that are currently collapsed (`hunk.id` keys). When collapsed,
  // only the hunk header row is emitted; the +/-/context lines are
  // skipped. Same persistence rules as collapsedFiles.
  collapsedHunks: Set<string>;
  // Maps hunk-id -> 1-indexed row of its hunk-header row in the diff
  // stream. Used by mouse + Tab to identify the nearest hunk.
  hunkRowByHunkId: Record<string, number>;
  // Maps comment-id -> 1-indexed row of the *diff line* the comment is
  // attached to (not the comment-display row itself). Lets the comments
  // panel jump the cursor straight to the source line.
  diffLineRowByCommentId: Record<string, number>;
  // The stream's hunks in order, with where each one's rows are. What a
  // row is — which hunk, which line of it, which note — is answered from
  // this and `Hunk.lines` when asked (`streamRowAt`), not from a record
  // built for every row up front.
  streamHunks: StreamHunk[];
  streamHunkById: Map<string, StreamHunk>;
  // The chrome rows, by 1-indexed row: section headers to their category,
  // file headers to their file key.
  sectionByRow: Map<number, string>;
  fileByRow: Map<number, string>;
  // Per file key, the byte range of its `diff --git` row and the label
  // `applyFolds` conceals over it (with the file's collapse glyph).
  fileHeaderConceals: Map<string, { start: number; end: number; label: string }>;
  // The buffer whose language and gutter are already set up for the
  // stream, so the mount does that once per buffer, not per rebuild.
  streamBufferPrepared: number | null;
  // Byte ranges of collapsible bodies, captured at build time. Tab /
  // mouse / z a / z r register these as host folds (see applyFolds)
  // — no buffer rebuild on collapse / expand.
  sectionBodyRange: Record<string, { start: number; end: number }>;
  fileBodyRange: Record<string, { start: number; end: number }>;
  hunkBodyRange: Record<string, { start: number; end: number }>;
  // Maps a category name (`'staged'` etc.) -> 1-indexed row of its
  // section-header row in the unified stream. Used by Tab toggle.
  sectionHeaderRows: Record<string, number>;
  // Maps a 1-indexed row in the files sidebar -> file key. Lets clicks in
  // the sidebar resolve back to a FileEntry.
  filesPanelByRow: Record<number, string>;
  // Maps a sidebar row to a directory-group key (`${category} ${dir}`) so
  // clicking a directory header toggles its collapse.
  filesPanelDirByRow: Record<number, string>;
  // File key currently highlighted in the sidebar (tracks the diff
  // viewport's top file). Lets the scroll handler skip a sidebar repaint
  // when the current file hasn't changed.
  filesCurrentKey: string | null;
  // Whether inline review-note boxes are shown in the diff stream. The
  // `a` key toggles this (hunk-style "agent notes" visibility); the
  // comments side panel is unaffected.
  showComments: boolean;
  // Active file-filter query (the `/` filter). Empty = show all files.
  fileFilter: string;
  // When true, the center panel renders only the focused file
  // (`filesCurrentKey`) instead of every file's hunks. Derived, not a
  // user setting — see `syncFocusMode`: it is the side-by-side composite
  // (per-file by construction) that renders one file. The unified stream
  // lays out the whole changeset, however big.
  focusOnly: boolean;
  // Which of the two optional side panels are on screen. Both start
  // hidden so the diff owns the full width — the reading surface is what
  // a review is about. `F` / `C` (or the toolbar buttons, or the `✕` in a
  // panel header) toggle them; the choice sticks for the editor session.
  panelsVisible: { files: boolean; comments: boolean };
  // Last width in columns the host reported for each panel, keyed by
  // panel name. Used to right-align the `✕` close button in a panel
  // header; falls back to a ratio-derived estimate until the first
  // viewport_changed for that panel arrives.
  panelWidths: Record<string, number>;
  // Last height in rows the host reported for each panel. The panels'
  // own list/tree windows are sized by the host; this is what
  // `refreshViewportDimensions` reads for the diff pane's geometry, and
  // what makes the side panels' relayout fire once per settled size.
  panelHeights: Record<string, number>;
  // Which comment the rail has selected (null = none). The rail is a
  // List widget: the host owns the selected *row*, the plugin owns which
  // comment that row belongs to.
  commentsSelectedId: string | null;
  // Comment-id the diff cursor is sitting on / attached to. Drives the
  // `>` follow-cursor marker in the comments panel.
  commentsHighlightId: string | null;
  // Sticky header current content (for Step 4)
  stickyCurrentFile: string | null;
  // Last known top-visible row in the diff viewport (1-indexed for
  // consistency with hunkHeaderRows, even though the host event delivers
  // 0-indexed). Updated from viewport_changed and cursor_moved.
  diffViewportTopRow: number;
  // Visual line-selection state. Active iff non-null. start and end are
  // 1-indexed rows in the unified stream; hunkId pins the selection to
  // a single hunk (selections that cross hunks are rejected).
  lineSelection: { startRow: number; endRow: number; hunkId: string } | null;
  // --- Composite-center architecture ---
  // The center "diff" panel renders the focused file as a host composite
  // buffer (OLD|NEW source buffers + a hunk-derived alignment), in unified
  // or side-by-side layout. Viewport-only render over real buffers, bounded
  // to one file regardless of changeset size.
  reviewLayout: 'unified' | 'side-by-side';
  centerComposite: {
    fileKey: string;
    compositeBufId: number;
    oldBufId: number;
    newBufId: number;
    absPath: string;
    isUntracked: boolean;
    hunkLineMap: Array<{ oldStart: number; newStart: number }>;
  } | null;
  // The composite for the file the reader left when they switched to the
  // unified stream, kept alive instead of destroyed. Switching back is
  // then a panel swap rather than two `git show` calls, two whole-file
  // buffers over the IPC boundary and an alignment pass. `signature`
  // records what it was built from (see `compositeSignature`), so a
  // review that has changed underneath it rebuilds rather than showing
  // the reader a stale file.
  parkedComposite: {
    fileKey: string;
    compositeBufId: number;
    oldBufId: number;
    newBufId: number;
    absPath: string;
    isUntracked: boolean;
    hunkLineMap: Array<{ oldStart: number; newStart: number }>;
    signature: string;
  } | null;
  // Monotonic token guarding async center rebuilds (file-nav spam / watch).
  centerBuildToken: number;
  // Bumped whenever anything the unified stream is built from changes.
  // The stream costs a second to lay out on a big review, and re-emitting
  // an identical one is not free-but-invisible: the panel swaps to the
  // stream buffer immediately and the content lands a beat later, so the
  // reader watches the old scroll position sit there and then jump.
  streamRevision: number;
  // `streamSignature()` as of the content currently in the stream buffer,
  // or null when nothing has been emitted into it yet.
  streamMountedSignature: string | null;
  // Bumped by `refreshMagitData` — i.e. when the underlying git data is
  // re-read, which is the one thing a hunk-range signature cannot see
  // (a file can change without moving any hunk boundary).
  dataRevision: number;
}

const state: ReviewState = {
  hunks: [],
  comments: [],
  note: '',
  reviewBufferId: null,
  mode: 'worktree',
  range: null,
  repo: null,
  reviewKey: 'worktree',
  files: [],
  emptyState: null,
  viewportWidth: 80,
  viewportHeight: 24,
  focusPanel: 'diff',
  compositePane: 0,
  compositeHunkIdx: 0,
  groupId: null,
  panelBuffers: {},
  hunkHeaderRows: [],
  diffLineByteOffsets: [],
  diffCursorRow: 1,
  fileHeaderRows: {},
  collapsedFiles: new Set(),
  collapsedSections: new Set(),
  collapsedDirs: new Set(),
  collapsedHunks: new Set(),
  hunkRowByHunkId: {},
  diffLineRowByCommentId: {},
  streamHunks: [],
  streamHunkById: new Map(),
  sectionByRow: new Map(),
  fileByRow: new Map(),
  fileHeaderConceals: new Map(),
  streamBufferPrepared: null,
  sectionBodyRange: {},
  fileBodyRange: {},
  hunkBodyRange: {},
  sectionHeaderRows: {},
  filesPanelByRow: {},
  filesPanelDirByRow: {},
  filesCurrentKey: null,
  showComments: true,
  fileFilter: "",
  panelsVisible: { files: false, comments: false },
  panelWidths: {},
  panelHeights: {},
  focusOnly: true,
  commentsSelectedId: null,
  commentsHighlightId: null,
  stickyCurrentFile: null,
  diffViewportTopRow: 0,
  lineSelection: null,
  reviewLayout: 'unified',
  centerComposite: null,
  parkedComposite: null,
  centerBuildToken: 0,
  streamRevision: 0,
  streamMountedSignature: null,
  dataRevision: 0,
};

function fileKey(f: FileEntry): string { return `${f.path}\0${f.category}`; }
function fileKeyOf(path: string, category: string): string { return `${path}\0${category}`; }

// Theme colour for the "cursor line" bar in the panel buffers. The bar
// itself is declared once (`setCursorLineOverlay`) and placed by the host
// from the cursor of the frame being drawn; painting it here from
// `cursor_moved` left it a row behind a held arrow key, because the hook
// only fires after the frame that already moved the caret.
const STYLE_SELECTED_BG: OverlayColorSpec = "editor.selection_bg";

/** Mode carried by the buffers the editor's own cursor moves in — the
 *  unified stream and the side-by-side composite. Same keymap as
 *  `review-mode`, except that cursor motion is bound straight to the
 *  built-in actions instead of taking a round trip through this plugin
 *  (see `DIFF_NATIVE_MOTION`). */
const REVIEW_DIFF_MODE = "review-diff";

// --- Refresh State ---

// --- Colors & Styles ---
// Colors use theme keys where possible, falling back to direct values
const STYLE_BORDER: OverlayColorSpec = "ui.split_separator_fg";
const STYLE_HEADER: OverlayColorSpec = "syntax.keyword";
const STYLE_FILE_NAME: OverlayColorSpec = "syntax.string";
const STYLE_ADD_BG: OverlayColorSpec = "editor.diff_add_bg";
const STYLE_REMOVE_BG: OverlayColorSpec = "editor.diff_remove_bg";
const STYLE_ADD_TEXT: OverlayColorSpec = "diagnostic.info_fg";
const STYLE_REMOVE_TEXT: OverlayColorSpec = "diagnostic.error_fg";

const STYLE_SECTION_HEADER: OverlayColorSpec = "syntax.type";
const STYLE_COMMENT: OverlayColorSpec = "diagnostic.warning_fg";
// Subtle bg for file/section header rows. Uses `editor.current_line_bg`
// which is reliably a notch lighter than editor bg in every theme
// (line_number_bg matches editor bg in Dracula and would render
// invisibly; status_bar_bg is the toolbar accent and is hot pink in
// Dracula). selection_bg is reserved for the cursor-line overlay so
// using it here would blend the two highlights.
const STYLE_FILE_HEADER_BG: OverlayColorSpec = "editor.current_line_bg";
// File-header foreground: brightest reliable foreground in any theme.
// `editor.fg` is white-ish on dark themes and black-ish on light, so it
// always reads as the most prominent text color. Bolded for extra weight.
const STYLE_FILE_HEADER_FG: OverlayColorSpec = "editor.fg";
// "Inverse" pair — swap of editor.bg/fg. Used for full-line-wide section
// dividers (STAGED / UNSTAGED / UNTRACKED) and the Comments panel
// header. Reads as an inverted band in every theme: dark text on light
// bg in dark themes, light text on dark bg in light themes.
const STYLE_INVERSE_FG: OverlayColorSpec = "editor.bg";
const STYLE_INVERSE_BG: OverlayColorSpec = "editor.fg";
/**
 * Calculate UTF-8 byte length of a string manually since TextEncoder is not
 * available.
 *
 * The values are byte offsets the host places overlays at, so the slow path
 * has to stay exact — including the surrogate pair, which is one code point
 * and four bytes, not two characters of three.
 *
 * An all-ASCII string has a byte length equal to its character length, and
 * diff text is overwhelmingly ASCII, so the interpreted per-character loop
 * is skipped whenever a single engine-level regex scan says it can be. The
 * regex is non-global on purpose: a `/g/` one carries `lastIndex` between
 * calls and would start half its scans in the middle of the string.
 */
const NON_ASCII = /[^\x00-\x7F]/;

function getByteLength(str: string): number {
    if (!NON_ASCII.test(str)) return str.length;
    let s = 0;
    for (let i = 0; i < str.length; i++) {
        const code = str.charCodeAt(i);
        if (code <= 0x7f) s += 1;
        else if (code <= 0x7ff) s += 2;
        else if (code >= 0xd800 && code <= 0xdfff) {
            s += 4; i++;
        } else s += 3;
    }
    return s;
}

/** Disclosure triangles for collapsible headers. Collapsing never
 *  rewrites the buffer; `applyFolds` shows the state with conceals. A
 *  section header (and a file header the plugin wrote itself) starts
 *  with `GLYPH_EXPANDED`, replaced by `GLYPH_COLLAPSED` while folded —
 *  both one column and `TRIANGLE_BYTES` long, so the conceal targets a
 *  fixed range at the head of the row. A file with a diff has git's
 *  `diff --git` row for a header, concealed under its label and the
 *  glyph; a hunk header is git's `@@` row, concealed as `▸ @@` while
 *  folded. */
const GLYPH_EXPANDED = '▾';
const GLYPH_COLLAPSED = '▸';
const TRIANGLE_BYTES = getByteLength(GLYPH_EXPANDED);

// --- Persistence ---
//
// Review comments for a given repo are persisted under:
//
//     <data_dir>/audit/<sanitized-repo-root>/<review-key>.json
//
// Where:
//   - `<data_dir>` is the host's `DirectoryContext::data_dir` (exposed via
//     the `getDataDir()` API added for this feature).
//   - `<review-key>` captures the *kind* of review — not every git state is
//     a fingerprint:
//       - `worktree` for `start_review_diff` (working tree review). There
//         is no single fingerprint for the working tree so we just reuse a
//         single slot per repo; line-content + line-number matching on
//         restore prunes comments that no longer apply.
//       - `range-<from>__<to>` for `start_review_range` (commit / branch
//         review). The range is stable, so comments survive re-opening.
//
// Design notes / alternatives that were considered:
//   - Keying worktree comments by the index or HEAD SHA: rejected — the
//     working tree is volatile so the key would change constantly and you
//     couldn't get your comments back after a single edit.
//   - Storing under `.review/` in the working tree: rejected — that bakes
//     the reviewer's state into the repo, which leaks into `git status`.
//   - One big JSON with all review keys per repo: rejected — concurrent
//     edits across review windows could clobber each other. Per-key files
//     keep each review's writes independent.

interface PersistedReview {
    version: number;
    mode: ReviewMode;
    range: ReviewRange | null;
    note: string;
    comments: ReviewComment[];
    updated_at: string;
}

const REVIEW_STORAGE_VERSION = 1;

/**
 * Make a string safe for use as a filename / directory name on all host
 * OSes. Forbidden characters (`/`, `\`, `:`, etc.) collapse to `_`; long
 * tails hash-truncate so path length stays sane.
 */
function sanitizeKeySegment(raw: string): string {
    const replaced = raw.replace(/[^A-Za-z0-9._-]+/g, '_');
    if (replaced.length <= 120) return replaced;
    // Cheap 32-bit FNV-1a so different long segments don't alias after
    // truncation.
    let h = 0x811c9dc5 >>> 0;
    for (let i = 0; i < raw.length; i++) {
        h ^= raw.charCodeAt(i);
        h = Math.imul(h, 0x01000193) >>> 0;
    }
    return replaced.slice(0, 100) + '__' + h.toString(16);
}

/**
 * Build the review-key portion of the storage filename (without the
 * `.json` extension) for the current mode / range.
 */
function buildReviewKey(mode: ReviewMode, range: ReviewRange | null): string {
    if (mode === 'range' && range) {
        return `range-${sanitizeKeySegment(range.from)}__${sanitizeKeySegment(range.to)}`;
    }
    return 'worktree';
}

/** Directory that stores all review files for a given repo. */
function reviewStorageDirFor(repoRoot: string): string | null {
    try {
        const dataDir = (editor as any).getDataDir?.() as string | undefined;
        if (!dataDir) return null;
        return editor.pathJoin(dataDir, "audit", sanitizeKeySegment(repoRoot));
    } catch {
        return null;
    }
}

/** Absolute path of the JSON file backing a review key. */
function reviewStoragePathFor(repoRoot: string, reviewKey: string): string | null {
    const dir = reviewStorageDirFor(repoRoot);
    if (!dir) return null;
    return editor.pathJoin(dir, `${reviewKey}.json`);
}

/**
 * cwd for git commands in the current review. `state.repo` is resolved at the
 * start of every review entry point (before any git call), so this returns the
 * repo root once a review is open; the `getCwd()` fallback only covers stray
 * calls made before a review is bootstrapped.
 */
function gitCwd(): string {
    return state.repo ? state.repo.root : editor.getCwd();
}

/**
 * Persist the current `state.comments` / `state.note` to disk. Best-effort:
 * filesystem errors never surface to the user — the UI is the source of
 * truth during the session and writes are just a cache for restore.
 */
function persistReview(): void {
    if (!state.repo) return;
    const path = reviewStoragePathFor(state.repo.root, state.reviewKey);
    if (!path) return;
    const dir = reviewStorageDirFor(state.repo.root);
    if (dir) {
        try { editor.createDir(editor.localPath(dir)); } catch {}
    }
    const payload: PersistedReview = {
        version: REVIEW_STORAGE_VERSION,
        mode: state.mode,
        range: state.range,
        note: state.note,
        comments: state.comments,
        updated_at: new Date().toISOString(),
    };
    try {
        editor.writeFile(editor.localPath(path), JSON.stringify(payload, null, 2));
    } catch {}
}

/** Read back a persisted review (if any). Returns null on any failure. */
function loadPersistedReview(repoRoot: string, reviewKey: string): PersistedReview | null {
    if (!repoRoot) return null;
    const path = reviewStoragePathFor(repoRoot, reviewKey);
    if (!path) return null;
    if (!editor.fileExists(editor.localPath(path))) return null;
    try {
        const raw = editor.readFile(editor.localPath(path));
        if (!raw) return null;
        const parsed = JSON.parse(raw) as PersistedReview;
        if (!parsed || typeof parsed !== 'object') return null;
        if (!Array.isArray(parsed.comments)) return null;
        return parsed;
    } catch {
        return null;
    }
}

// --- Diff Logic ---

interface DiffPart {
    text: string;
    type: 'added' | 'removed' | 'unchanged';
}

/**
 * Inline word-level diff between two changed lines.
 *
 * Used to highlight the *changed region* inside a -/+ pair, called once per
 * adjacent pair while building a file's diff. The previous implementation
 * was a full O(n*m) LCS that allocated an (n+1)*(m+1) DP table per pair —
 * fast enough for short lines, but for files with hundreds of long-line
 * changes (e.g. `audit_mode.ts` itself) it added hundreds of milliseconds
 * to every diff rebuild and made file-list navigation visibly laggy.
 *
 * This O(n+m) scan finds the longest common prefix and suffix and reports
 * everything in between as the changed region. It misses internal matches
 * (e.g. it can't tell that "abc-xy-def" → "abc-zw-def" only changed the
 * middle "xy"), but for inline highlighting that's fine — the human eye is
 * already drawn to the line as a whole, the highlight just answers "where
 * inside the line did the change happen?". The cost difference is dramatic:
 * for two 200-char lines, ~400 char compares vs. ~40 000.
 */
function diffStrings(oldStr: string, newStr: string): DiffPart[] {
    const n = oldStr.length;
    const m = newStr.length;
    let pre = 0;
    const minLen = Math.min(n, m);
    while (pre < minLen && oldStr.charCodeAt(pre) === newStr.charCodeAt(pre)) pre++;
    let suf = 0;
    while (
        suf < n - pre &&
        suf < m - pre &&
        oldStr.charCodeAt(n - 1 - suf) === newStr.charCodeAt(m - 1 - suf)
    ) {
        suf++;
    }

    // Both walks count UTF-16 units, so a boundary can land between the
    // halves of a surrogate pair — two lines differing only in the low
    // half of an emoji share its high half, and the prefix swallows it.
    // That leaves a lone surrogate at the end of one part and another at
    // the start of the next, and `getByteLength` charges four bytes for
    // each: every offset built from these parts is then a whole character
    // out, and the highlight lands past the end of its row. Pull either
    // boundary back onto a code-point boundary.
    const isHigh = (c: number) => c >= 0xd800 && c <= 0xdbff;
    const isLow = (c: number) => c >= 0xdc00 && c <= 0xdfff;
    if (pre > 0 && isHigh(oldStr.charCodeAt(pre - 1)) && isLow(oldStr.charCodeAt(pre))) pre--;
    if (suf > 0 && isHigh(oldStr.charCodeAt(n - suf - 1)) && isLow(oldStr.charCodeAt(n - suf))) suf--;

    const parts: DiffPart[] = [];
    if (pre > 0) parts.push({ text: oldStr.slice(0, pre), type: 'unchanged' });
    if (pre < n - suf) parts.push({ text: oldStr.slice(pre, n - suf), type: 'removed' });
    if (pre < m - suf) parts.push({ text: newStr.slice(pre, m - suf), type: 'added' });
    if (suf > 0) parts.push({ text: oldStr.slice(n - suf), type: 'unchanged' });
    return parts;
}

/**
 * Split `git diff` output into hunks without taking it apart: each hunk
 * records where it sits in its file's block, and the block is what the
 * stream shows. The pass is an `indexOf` per row and a first-byte test;
 * only `Hunk.lines`, which staging and note anchoring read, is split out.
 */
function parseDiffOutput(stdout: string, gitStatus: 'staged' | 'unstaged' | 'untracked'): Hunk[] {
    if (stdout.length > 0 && !stdout.endsWith('\n')) stdout += '\n';
    const hunks: Hunk[] = [];
    let file: RawFile | null = null;
    let fileStart = 0;
    let hunk: Hunk | null = null;
    const closeHunk = (at: number) => {
        if (hunk === null) return;
        hunk.end = at - fileStart;
        const lines = stdout.slice(fileStart + hunk.bodyStart, at).split('\n');
        lines.pop(); // the terminating newline leaves an empty tail
        hunk.lines = lines;
        hunk = null;
    };
    const closeFile = (at: number) => {
        closeHunk(at);
        if (file === null) return;
        file.text = stdout.slice(fileStart, at);
        file = null;
    };
    const n = stdout.length;
    let pos = 0;
    while (pos < n) {
        let nl = stdout.indexOf('\n', pos);
        if (nl < 0) nl = n;
        const c = stdout.charCodeAt(pos);
        if (c === 0x64 /* d */ && stdout.startsWith('diff --git ', pos)) {
            closeFile(pos);
            const match = stdout.slice(pos, nl).match(/diff --git a\/(.+) b\/(.+)/);
            if (match) {
                fileStart = pos;
                file = { path: match[2], gitStatus, text: '' };
            }
        } else if (c === 0x40 /* @ */ && file !== null && stdout.startsWith('@@ ', pos)) {
            closeHunk(pos);
            const match = stdout.slice(pos, nl).match(/@@ -(\d+),?\d* \+(\d+),?\d* @@(.*)/);
            if (match) {
                const oldStart = parseInt(match[1]);
                const newStart = parseInt(match[2]);
                hunk = {
                    id: `${file.path}:${newStart}:${gitStatus}`,
                    file: file.path,
                    range: { start: newStart, end: newStart },
                    oldRange: { start: oldStart, end: oldStart },
                    type: 'modify',
                    lines: [],
                    status: 'pending',
                    contextHeader: match[3]?.trim() || "",
                    gitStatus,
                    raw: file,
                    headerStart: pos - fileStart,
                    bodyStart: nl + 1 - fileStart,
                    end: nl + 1 - fileStart,
                };
                hunks.push(hunk);
            }
        }
        pos = nl + 1;
    }
    closeFile(n);
    return hunks;
}

// --- Git status detection ---

/**
 * Parse `git status --porcelain -z -uall` output into FileEntry[].
 *
 * Format: each entry is "XY path\0" where X = index status, Y = worktree status.
 * Renames/copies add "origPath\0" after the entry.
 * A file can appear in BOTH staged and unstaged if both X and Y are set.
 */
function parseGitStatusPorcelain(raw: string): FileEntry[] {
    const files: FileEntry[] = [];
    if (!raw) return files;

    // Split on null bytes
    const parts = raw.split('\0');
    let i = 0;
    while (i < parts.length) {
        const entry = parts[i];
        if (entry.length < 3) { i++; continue; }

        const x = entry[0]; // index (staged) status
        const y = entry[1]; // worktree (unstaged) status
        // entry[2] is a space
        const path = entry.slice(3);

        if (!path) { i++; continue; }

        // Check for rename/copy — next part is the original path
        let origPath: string | undefined;
        if (x === 'R' || x === 'C' || y === 'R' || y === 'C') {
            i++;
            origPath = parts[i];
        }

        // Untracked files: XY = '??'
        if (x === '?' && y === '?') {
            files.push({ path, status: '?', category: 'untracked' });
            i++;
            continue;
        }

        // Ignored files: XY = '!!' — skip
        if (x === '!' && y === '!') {
            i++;
            continue;
        }

        // Staged changes: X is not ' ' and not '?'
        if (x !== ' ' && x !== '?') {
            files.push({ path, status: x, category: 'staged', origPath });
        }

        // Unstaged changes: Y is not ' ' and not '?'
        if (y !== ' ' && y !== '?') {
            files.push({ path, status: y, category: 'unstaged', origPath });
        }

        i++;
    }

    // Sort: staged → unstaged → untracked, then by filename
    const categoryOrder: Record<string, number> = { staged: 0, unstaged: 1, untracked: 2 };
    files.sort((a, b) => {
        const orderA = categoryOrder[a.category] ?? 2;
        const orderB = categoryOrder[b.category] ?? 2;
        if (orderA !== orderB) return orderA - orderB;
        return a.path.localeCompare(b.path);
    });

    return files;
}

/**
 * Single source of truth for changed files using `git status --porcelain -z -uall`.
 *
 * `-uall` (`--untracked-files=all`) makes git list every untracked file
 * individually instead of collapsing a brand-new directory into a single
 * `?? dir/` entry. Without it, a new folder of files appears as one blank,
 * contentless row whose files can never be reviewed (see issue #2315). This
 * matches the feature's documentation ("everything staged, unstaged, and
 * untracked in the working tree") and VS Code's Source Control panel, which
 * also lists untracked files with `all` granularity.
 *
 * `emptyReason` distinguishes the two no-content cases so the UI can explain
 * itself instead of rendering a blank pane:
 *   - `'not_git'`: `git status` failed (no repo at cwd).
 *   - `'clean'`: `git status` succeeded but returned no entries.
 *   - `null`: files were found; render them normally.
 */
interface GitStatusResult {
    files: FileEntry[];
    emptyReason: EmptyStateReason;
}

async function getGitStatus(): Promise<GitStatusResult> {
    const cwd = gitCwd();
    // `--no-optional-locks`: plain `git status` refreshes the index and takes
    // `.git/index.lock` to write it back. This runs on a timer while a review
    // is open (#3126), so without it the panel would periodically lose a
    // `git commit` or `git add` in the user's own terminal the race for that
    // lock.
    const result = await editor.spawnProcess(
        "git",
        ["--no-optional-locks", "status", "--porcelain", "-z", "-uall"],
        cwd,
    );
    if (result.exit_code !== 0) {
        return { files: [], emptyReason: 'not_git' };
    }
    const files = parseGitStatusPorcelain(result.stdout);
    return {
        files,
        emptyReason: files.length === 0 ? 'clean' : null,
    };
}

// `diffArgs` / `withDiffArgs` — the argv that pins git's patch output to the
// shape `parseDiffOutput` and `buildHunkPatch` expect — are shared with the
// git log plugin and live in `./lib/git_repo.ts`.

/**
 * Fetch unified diffs for the given file entries.
 * Groups by category to minimize git invocations.
 */
async function fetchDiffsForFiles(files: FileEntry[]): Promise<Hunk[]> {
    const allHunks: Hunk[] = [];
    const cwd = gitCwd();

    const hasStaged = files.some(f => f.category === 'staged');
    const hasUnstaged = files.some(f => f.category === 'unstaged');
    const untrackedFiles = files.filter(f => f.category === 'untracked');

    // Staged diffs
    if (hasStaged) {
        const result = await editor.spawnProcess("git", diffArgs(["diff"], "--cached", "--unified=3"), cwd);
        if (result.exit_code === 0 && result.stdout.trim()) {
            allHunks.push(...parseDiffOutput(result.stdout, 'staged'));
        }
    }

    // Unstaged diffs
    if (hasUnstaged) {
        const result = await editor.spawnProcess("git", diffArgs(["diff"], "--unified=3"), cwd);
        if (result.exit_code === 0 && result.stdout.trim()) {
            allHunks.push(...parseDiffOutput(result.stdout, 'unstaged'));
        }
    }

    // Untracked file diffs
    for (const f of untrackedFiles) {
        const result = await editor.spawnProcess(
            "git",
            diffArgs(["diff"], "--no-index", "--unified=3", "/dev/null", f.path),
            cwd,
        );
        if (result.stdout.trim()) {
            const hunks = parseDiffOutput(result.stdout, 'untracked');
            for (const h of hunks) {
                h.file = f.path;
                h.raw.path = f.path;
                h.id = `${f.path}:${h.range.start}:untracked`;
                h.type = 'add';
            }
            allHunks.push(...hunks);
        }
    }

    // Sort: staged → unstaged → untracked, then by filename
    const statusOrder: Record<string, number> = { staged: 0, unstaged: 1, untracked: 2 };
    allHunks.sort((a, b) => {
        const orderA = statusOrder[a.gitStatus || 'unstaged'];
        const orderB = statusOrder[b.gitStatus || 'unstaged'];
        if (orderA !== orderB) return orderA - orderB;
        return a.file.localeCompare(b.file);
    });

    return allHunks;
}

// --- Derived views of `state.hunks` ---
//
// Everything that needs "the hunks for this file" or "how many lines this
// file changes" used to answer it by scanning every hunk in the review.
// Per file that is O(files x hunks) and, once the line counts are wanted
// too, O(files x total diff lines) — on a large changeset the scans cost
// more than parsing the diff did.
//
// So the answers are derived once, in a single pass, and keyed by
// `fileKey`. The cache is guarded by identity of the array it was built
// from: `state.hunks` is only ever replaced wholesale (hunk objects are
// filled in during parsing, before the array is installed), so a new
// array is exactly the event that invalidates the derivation.

interface HunkIndex {
    /** Hunks per `fileKey`, in `state.hunks` order. */
    byFileKey: Map<string, Hunk[]>;
    /** +added / -removed line counts per `fileKey`. */
    countsByFileKey: Map<string, { added: number; removed: number }>;
    /** The git output block per `fileKey`. */
    rawByFileKey: Map<string, RawFile>;
    /** The largest line number any hunk reaches, on either side. */
    maxLine: number;
}

const NO_HUNKS: readonly Hunk[] = [];
const NO_CHANGES: Readonly<{ added: number; removed: number }> = { added: 0, removed: 0 };

let hunkIndexCache: { source: Hunk[]; index: HunkIndex } | null = null;

function hunkIndex(): HunkIndex {
    const source = state.hunks;
    if (hunkIndexCache !== null && hunkIndexCache.source === source) {
        return hunkIndexCache.index;
    }
    const byFileKey = new Map<string, Hunk[]>();
    const countsByFileKey = new Map<string, { added: number; removed: number }>();
    const rawByFileKey = new Map<string, RawFile>();
    let maxLine = 1;
    for (const h of source) {
        const key = fileKeyOf(h.file, h.gitStatus || 'unstaged');
        let group = byFileKey.get(key);
        let counts = countsByFileKey.get(key);
        if (group === undefined || counts === undefined) {
            group = [];
            counts = { added: 0, removed: 0 };
            byFileKey.set(key, group);
            countsByFileKey.set(key, counts);
            rawByFileKey.set(key, h.raw);
        }
        group.push(h);
        // `counts` runs over the whole file; the reach of *this* hunk is
        // its own lines, so the two are counted apart. Adding the file's
        // running total to one hunk's start overstates the last line by
        // everything changed above it, and sizes the gutter too wide.
        let context = 0;
        let added = 0;
        let removed = 0;
        for (const line of h.lines) {
            if (line[0] === '+') { counts.added++; added++; }
            else if (line[0] === '-') { counts.removed++; removed++; }
            else if (line[0] !== '\\') context++;
        }
        maxLine = Math.max(maxLine, h.oldRange.start + removed + context, h.range.start + added + context);
    }
    const index: HunkIndex = { byFileKey, countsByFileKey, rawByFileKey, maxLine };
    hunkIndexCache = { source, index };
    return index;
}

// The index owns these arrays and count records, so both accessors hand
// back read-only views: a caller that mutated one would corrupt every
// later reader of the same file.

/** The hunks belonging to `key` (a `fileKey`), in review order. */
function hunksForKey(key: string): readonly Hunk[] {
    return hunkIndex().byFileKey.get(key) || NO_HUNKS;
}

/** The hunks belonging to `file`, in review order. */
function hunksForFile(file: FileEntry): readonly Hunk[] {
    return hunksForKey(fileKey(file));
}

/** Compute +N / -M line counts for a file. */
function fileChangeCounts(file: FileEntry): Readonly<{ added: number; removed: number }> {
    return hunkIndex().countsByFileKey.get(fileKey(file)) || NO_CHANGES;
}

/** Columns the host's diff gutter puts in front of every stream row: the
 *  indicator slot, the two line-number columns, and the separator. */
function diffGutterColumns(): number {
    const digits = String(hunkIndex().maxLine).length;
    return 1 + (2 * digits + 1) + 3;
}

// Inline review-note box sizing. The note renders as a bordered, wrapped
// callout anchored under its diff line (hunk-style), instead of a single
// truncated one-line row. See
// docs/internal/REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md §5.6.
const COMMENT_BOX_MIN_W = 24;
const COMMENT_BOX_MAX_W = 100;

/**
 * Greedy word-wrap to `width` columns. Preserves explicit newlines as
 * paragraph breaks and hard-splits any single word longer than `width`.
 * Always returns at least one (possibly empty) line.
 */
function wrapText(text: string, width: number): string[] {
    const out: string[] = [];
    const w = Math.max(1, width);
    for (const para of text.split('\n')) {
        const words = para.split(/\s+/).filter(t => t.length > 0);
        if (words.length === 0) {
            out.push('');
            continue;
        }
        let cur = '';
        for (let word of words) {
            while (word.length > w) {
                if (cur.length > 0) {
                    out.push(cur);
                    cur = '';
                }
                out.push(word.slice(0, w));
                word = word.slice(w);
            }
            if (cur.length === 0) cur = word;
            else if (cur.length + 1 + word.length <= w) cur += ' ' + word;
            else {
                out.push(cur);
                cur = word;
            }
        }
        if (cur.length > 0) out.push(cur);
    }
    return out.length > 0 ? out : [''];
}

/** Width in columns of the center diff panel — the host's last reported
 *  viewport width for it, falling back to the terminal width before the
 *  first `viewport_changed` arrives. */
function diffPanelWidth(): number {
    const known = state.panelWidths["diff"];
    return known && known > 0 ? known : state.viewportWidth;
}

/**
 * The rows of one inline note box: a bordered, word-wrapped callout whose
 * border title is the line reference. The host's gutter sits in front of
 * every row, so the box needs no indent of its own — and none of its rows
 * may start with a space, `+` or `-`, which the diff grammar would read
 * as a diff row.
 */
/** Outer width of a note box, clamped to the visible content area. The
 *  diff panel's own width (not the terminal's) is what the box has to fit
 *  in — with a side panel open they differ, and nothing soft-wraps a box
 *  that overshoots any more; it just gets clipped. */
function noteBoxWidth(): number {
    return Math.max(
        COMMENT_BOX_MIN_W,
        Math.min(COMMENT_BOX_MAX_W, diffPanelWidth() - diffGutterColumns() - 1)
    );
}

function noteBoxRows(comment: ReviewComment): string[] {
    const boxW = noteBoxWidth();
    const innerW = boxW - 4; // "| " + content + " |"
    const lineRef = comment.line_type === 'add'
        ? `+${comment.new_line}`
        : comment.line_type === 'remove'
        ? `-${comment.old_line}`
        : `${comment.new_line}`;
    const rows: string[] = [];
    // Top border carries the line reference as its title.
    const titleSeg = `\u256d\u2500 ${lineRef} `;
    const topFill = '\u2500'.repeat(Math.max(0, boxW - titleSeg.length - 1));
    rows.push(`${titleSeg}${topFill}\u256e`);
    for (const wl of wrapText(comment.text, innerW)) {
        rows.push(`\u2502 ${wl.padEnd(innerW)} \u2502`);
    }
    rows.push(`\u2570${'\u2500'.repeat(Math.max(0, boxW - 2))}\u256f`);
    return rows;
}

/** Whether `c` is attached to the diff row with these line numbers. */
function commentAnchorsAt(
    c: ReviewComment,
    lineType: 'add' | 'remove' | 'context',
    oldLine: number | undefined,
    newLine: number | undefined,
): boolean {
    return c.line_type === lineType && (
        lineType === 'remove' ? c.old_line === oldLine : c.new_line === newLine
    );
}

/** Where each of `comments` attaches in `hunk`: the index into
 *  `hunk.lines` of its anchor row, in body order. Comments whose row is
 *  not in the hunk are left out. */
function noteAnchors(hunk: Hunk, comments: ReviewComment[]): Array<{ comment: ReviewComment; afterLine: number }> {
    const out: Array<{ comment: ReviewComment; afterLine: number }> = [];
    let oldN = hunk.oldRange.start;
    let newN = hunk.range.start;
    for (let i = 0; i < hunk.lines.length; i++) {
        const c = hunk.lines[i][0];
        if (c === '\\') continue;
        const lineType = c === '+' ? 'add' : c === '-' ? 'remove' : 'context';
        for (const comment of comments) {
            if (commentAnchorsAt(comment, lineType, oldN, newN)) out.push({ comment, afterLine: i });
        }
        if (c !== '+') oldN++;
        if (c !== '-') newN++;
    }
    return out;
}

/**
 * Build the full display as exactly viewportHeight lines.
 * Layout:
 *   Row 0:        Toolbar (shortcuts)
 *   Row 1:        Header (left: GIT STATUS, right: DIFF FOR <file>)
 *   Rows 2..H-1:  Main content (left file list, │ divider, right diff)
 */

// Theme colors for toolbar key hints
// Toolbar styling — explicitly NOT using `ui.status_bar_bg` because that
// key is a saturated accent in some themes (Dracula's hot pink). Instead
// we paint the toolbar with `editor.bg` so it visually matches the
// editor content and keys/labels get reliable contrast against it.
//   * Keys: `editor.fg` + bold (white-bold on dark, etc.).
//   * Labels: `editor.line_number_fg` (dim foreground in every theme).
const STYLE_KEY_FG: OverlayColorSpec = "editor.fg";
const STYLE_HINT_FG: OverlayColorSpec = "editor.line_number_fg";
const STYLE_TOOLBAR_BG: OverlayColorSpec = "editor.bg";
const STYLE_TOOLBAR_SEP: OverlayColorSpec = "ui.split_separator_fg";

interface HintItem {
    key: string;
    label: string;
}

/**
 * Build the (two-row) toolbar with all review-diff shortcuts.
 * Row 1 — navigation; row 2 — actions. Identical regardless of which
 * panel currently has focus (no more files-pane vs diff-pane variants).
 */
function buildToolbar(): HintEntry[][] {
    // In range mode, stage / unstage / discard are meaningless (there is
    // no working tree to mutate), so hide them from the hint bar to keep
    // the toolbar honest. The key-bindings themselves are harmless if
    // pressed — `review_stage_scope` no-ops on range-mode hunks because
    // their gitStatus is 'unstaged' and the git commands it invokes
    // target the working tree, which isn't what the user intended. The
    // toolbar is the user-facing surface, so pruning here is the
    // cheapest honest thing to do.
    const inRange = state.mode === 'range';
    const row1: HintItem[][] = [
        // The "how do I move around" group first: focus-cycle, file nav,
        // hunk nav — the keys a newcomer reaches for.
        [{ key: "Tab", label: "focus" }, { key: ", .", label: "file" }],
        [{ key: "n", label: "next hunk" }, { key: "p", label: "prev hunk" }],
        inRange
            ? [{ key: "v", label: "select" }, { key: "c", label: "comment" }]
            : [{ key: "s", label: "stage" }, { key: "u", label: "unstage" }, { key: "d", label: "discard" },
               { key: "c", label: "comment" }],
    ];
    const row2: HintItem[][] = [
        [{ key: "1 2", label: "unified/split" }, { key: "↑↓", label: "move in panel" },
         { key: "Enter", label: "jump" }, { key: "Alt+o", label: "open file" }],
        inRange
            ? [{ key: "/", label: "filter" }, { key: "?", label: "help" },
               { key: "e", label: "export" }, { key: "q", label: "close" }]
            : [{ key: "S U D", label: "file-level" }, { key: "/", label: "filter" },
               { key: "?", label: "help" }, { key: "q", label: "close" }],
    ];
    return [row1, row2].map(groups => groups.flat().map(
        (item): HintEntry => ({ keys: item.key, label: item.label }),
    ));
}

// --- Buffer Group panel content builders ---

// The toolbar is a widget panel: two hint rows (still built as styled
// text entries and wrapped in `raw`) plus the two panel buttons pinned to
// the right of the second row by a `flexSpacer`. The buttons are real
// `Button` widgets, so the host owns their hit-testing, hover styling and
// keyboard activation and the plugin only handles `widget_event`.
const PANEL_BUTTON_KEYS = {
    files: "toolbar.files",
    comments: "toolbar.comments",
} as const;

function panelButtonLabel(panel: 'files' | 'comments'): string {
    const name = panel === 'files'
        ? (editor.t("panel.files") || "Files")
        : (editor.t("panel.comments") || "Comments");
    return `${state.panelsVisible[panel] ? '▾' : '▸'} ${name}`;
}

function panelButton(panel: 'files' | 'comments'): WidgetSpec {
    return button(panelButtonLabel(panel), {
        key: PANEL_BUTTON_KEYS[panel],
        // Mouse-first affordance: the review mode's Tab cycle belongs to
        // the three content panels, and `F` / `C` already drive these
        // from the keyboard.
        focusable: false,
    });
}

/** Render the toolbar panel: hint rows plus the panel buttons. */
function renderToolbar(): void {
    if (toolbarPanel === null) return;
    const rows = buildToolbar();
    toolbarPanel.set(col(
        hintBar(rows[0]),
        row(hintBar(rows[1]), flexSpacer(), panelButton('files'), spacer(1), panelButton('comments')),
    ));
}

/**
 * Build the unified stream: git's own output, file by file, with the
 * plugin's rows — section headers, note boxes, placeholders — spliced in
 * between. A file's `diff --git` row is its header: the host's diff
 * grammar reads the language off it, and the reader sees the file's
 * label concealed over it (see `applyFolds`). The `index` / `---` / `+++`
 * rows that follow it in git's output say nothing the grammar needs and
 * are left out.
 *
 * A row of the plugin's own never starts with a space, `+` or `-`; the
 * grammar would take it for a diff row and the gutter would number it.
 *
 * Populates the row and byte maps the rest of the plugin navigates by:
 * the hunk layout (`streamHunks`), header rows, fold ranges, and one byte
 * offset per row. The offsets come from a newline scan per block — a
 * block that is all ASCII (nearly every one) needs no per-row measuring.
 */
function buildStreamContent(): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];
    const offsets: number[] = [];
    const hunkHeaderRows: number[] = [];
    const fileHeaderRows: Record<string, number> = {};
    const sectionHeaderRows: Record<string, number> = {};
    const hunkRowByHunkId: Record<string, number> = {};
    const diffLineRowByCommentId: Record<string, number> = {};
    const sectionBodyRange: Record<string, { start: number; end: number }> = {};
    const fileBodyRange: Record<string, { start: number; end: number }> = {};
    const hunkBodyRange: Record<string, { start: number; end: number }> = {};
    const fileHeaderConceals = new Map<string, { start: number; end: number; label: string }>();
    const streamHunks: StreamHunk[] = [];
    const streamHunkById = new Map<string, StreamHunk>();
    const sectionByRow = new Map<number, string>();
    const fileByRow = new Map<number, string>();

    let row = 0;  // rows emitted so far; the last one is row `row`
    let byte = 0; // bytes emitted so far
    // Unstyled text is held back and handed over as one entry, so the
    // host gets an entry per styled row, not per row.
    let plain: string[] = [];
    const flushPlain = () => {
        if (plain.length > 0) {
            entries.push({ text: plain.join('') });
            plain = [];
        }
    };
    const pushRow = (text: string, style?: Partial<OverlayOptions>) => {
        const t = text + '\n';
        offsets.push(byte);
        row++;
        byte += getByteLength(t);
        if (style) {
            flushPlain();
            entries.push({ text: t, style });
        } else {
            plain.push(t);
        }
    };
    // Newline-terminated rows of git output, verbatim.
    const pushRaw = (text: string) => {
        if (text.length === 0) return;
        if (!NON_ASCII.test(text)) {
            let pos = 0;
            while (pos < text.length) {
                offsets.push(byte + pos);
                row++;
                pos = text.indexOf('\n', pos) + 1;
                if (pos === 0) break;
            }
            byte += text.length;
        } else {
            let pos = 0;
            while (pos < text.length) {
                let nl = text.indexOf('\n', pos);
                if (nl < 0) nl = text.length - 1;
                offsets.push(byte);
                row++;
                byte += getByteLength(text.slice(pos, nl + 1));
                pos = nl + 1;
            }
        }
        plain.push(text);
    };

    if (state.files.length === 0) {
        if (state.emptyState === 'not_git') {
            pushRow(editor.t("status.not_git_repo") || "Not a git repository",
                { fg: STYLE_SECTION_HEADER, italic: true });
        } else if (state.emptyState === 'clean') {
            pushRow(editor.t("panel.no_changes") || "No changes to review.",
                { fg: STYLE_SECTION_HEADER, italic: true });
        }
    }

    const commentsByHunk = new Map<string, ReviewComment[]>();
    if (state.showComments) {
        for (const c of state.comments) {
            const list = commentsByHunk.get(c.hunk_id);
            if (list === undefined) commentsByHunk.set(c.hunk_id, [c]);
            else list.push(c);
        }
    }

    // The "body" of a section or file is the byte range from the byte
    // after its header's newline up to the byte before the next header
    // that ends it.
    let curSection: string | null = null;
    let curFile: string | null = null;
    let sectionBodyStart = 0;
    let fileBodyStart = 0;
    const closeFile = () => {
        if (curFile !== null) fileBodyRange[curFile] = { start: fileBodyStart, end: byte };
        curFile = null;
    };
    const closeSection = () => {
        closeFile();
        if (curSection !== null) sectionBodyRange[curSection] = { start: sectionBodyStart, end: byte };
        curSection = null;
    };

    let lastCategory: string | undefined;
    for (let fi = 0; fi < state.files.length; fi++) {
        const file = state.files[fi];

        // Honor the active `/` filter — skip non-matching files entirely so
        // the center matches the sidebar.
        if (!fileMatchesFilter(file)) continue;

        // Section header — full-line-wide INVERSE band, uppercase, bold.
        // The strong inverse coloring (editor.bg as fg / editor.fg as bg)
        // makes the band read as a hard divider between Staged /
        // Unstaged / Untracked sections regardless of theme.
        if (file.category !== lastCategory) {
            lastCategory = file.category;
            closeSection();
            let label: string = file.category;
            // Range mode reuses the `unstaged` bucket for every hunk as
            // an impl shortcut — surface the range label so the user
            // isn't told their commit review is "Unstaged".
            if (state.mode === 'range' && state.range) {
                label = state.range.label;
            } else if (file.category === 'staged') label = editor.t("section.staged") || "Staged";
            else if (file.category === 'unstaged') label = editor.t("section.unstaged") || "Unstaged";
            else if (file.category === 'untracked') label = editor.t("section.untracked") || "Untracked";
            const sectionCount = state.files.filter(f => f.category === file.category && fileMatchesFilter(f)).length;
            // Always render the expanded triangle. Collapse state is
            // shown by overlaying a `▸` replacement-conceal on the
            // triangle byte range (see `applyFolds`) — the buffer text
            // never changes, so toggling collapse never has to rebuild.
            // Range labels (e.g. `main..HEAD`) carry case already — don't
            // mangle them with the section uppercase; worktree category
            // names are lowercase words and need the uppercase.
            const displayLabel = state.mode === 'range' ? label : label.toUpperCase();
            pushRow(`${GLYPH_EXPANDED} ${displayLabel}  (${sectionCount})`, {
                fg: STYLE_INVERSE_FG,
                bg: STYLE_INVERSE_BG,
                bold: true,
                extendToLineEnd: true,
            });
            sectionHeaderRows[file.category] = row;
            sectionByRow.set(row, file.category);
            curSection = file.category;
            sectionBodyStart = byte;
        }

        closeFile();
        const counts = fileChangeCounts(file);
        const key = fileKey(file);
        const filename = file.origPath ? `${file.origPath} → ${file.path}` : file.path;
        const label = `${filename}   +${counts.added} / -${counts.removed}`;
        const headerStyle: Partial<OverlayOptions> = {
            fg: STYLE_FILE_HEADER_FG,
            bg: STYLE_FILE_HEADER_BG,
            bold: true,
            extendToLineEnd: true,
        };
        const fileHunks = hunksForKey(key);
        const raw = hunkIndex().rawByFileKey.get(key);
        const hasBlock = fileHunks.length > 0 && raw !== undefined;

        if (hasBlock) {
            // git's own `diff --git` row, with the label concealed over it.
            const headerLine = raw.text.slice(0, raw.text.indexOf('\n'));
            const start = byte;
            pushRow(headerLine, headerStyle);
            fileHeaderConceals.set(key, { start, end: byte - 1, label });
        } else {
            // Nothing for the grammar to read: a row of the plugin's own,
            // whose triangle `applyFolds` conceals when collapsed.
            pushRow(`${GLYPH_EXPANDED} ${label}`, headerStyle);
        }
        fileHeaderRows[key] = row;
        fileByRow.set(row, key);
        curFile = key;
        fileBodyStart = byte;

        // The composite draws one file, so while it is up the stream
        // carries headers only. The unified stream renders every file.
        if (!fileBodyRendered(key)) {
            pushRow('');
            continue;
        }

        if (!hasBlock) {
            if (file.status === 'R' && file.origPath) {
                pushRow(`Renamed from ${file.origPath}`, { fg: STYLE_SECTION_HEADER });
            } else if (file.status === 'D') {
                pushRow("(file deleted)");
            } else if (file.status === 'T') {
                pushRow("(type change: file ↔ symlink)", { fg: STYLE_SECTION_HEADER });
            } else if (file.status === '?' && file.path.endsWith('/')) {
                pushRow("(untracked directory)");
            } else {
                pushRow("(no diff available)");
            }
            pushRow('');
            continue;
        }

        for (const hunk of fileHunks) {
            const headerRow = row + 1;
            hunkHeaderRows.push(headerRow);
            hunkRowByHunkId[hunk.id] = headerRow;
            pushRaw(raw.text.slice(hunk.headerStart, hunk.bodyStart));
            const bodyStart = byte;
            const notes: StreamNote[] = [];
            const hunkComments = commentsByHunk.get(hunk.id);
            if (hunkComments === undefined) {
                pushRaw(raw.text.slice(hunk.bodyStart, hunk.end));
            } else {
                // Cut the body after each annotated row and splice the
                // note box in. `from` and `cut` are offsets into
                // `raw.text`; `lineIdx` is the line starting at `cut`.
                let from = hunk.bodyStart;
                let cut = hunk.bodyStart;
                let lineIdx = 0;
                let anchorRow = row;
                for (const { comment, afterLine } of noteAnchors(hunk, hunkComments)) {
                    while (lineIdx <= afterLine) {
                        cut += hunk.lines[lineIdx].length + 1;
                        lineIdx++;
                    }
                    if (cut > from) {
                        pushRaw(raw.text.slice(from, cut));
                        from = cut;
                        anchorRow = row;
                    }
                    diffLineRowByCommentId[comment.id] = anchorRow;
                    const box = noteBoxRows(comment);
                    for (let i = 0; i < box.length; i++) {
                        pushRow(box[i], { fg: STYLE_COMMENT, italic: i > 0 && i < box.length - 1 });
                    }
                    notes.push({ afterLine, rows: box.length, commentId: comment.id });
                }
                if (hunk.end > from) pushRaw(raw.text.slice(from, hunk.end));
            }
            hunkBodyRange[hunk.id] = { start: bodyStart, end: byte };
            const sh: StreamHunk = { hunk, headerRow, rowCount: row + 1 - headerRow, notes };
            streamHunks.push(sh);
            streamHunkById.set(hunk.id, sh);
        }

        // Blank separator between files
        pushRow('');
    }
    closeSection();
    flushPlain();
    offsets.push(byte);

    state.hunkHeaderRows = hunkHeaderRows;
    state.diffLineByteOffsets = offsets;
    state.fileHeaderRows = fileHeaderRows;
    state.sectionHeaderRows = sectionHeaderRows;
    state.hunkRowByHunkId = hunkRowByHunkId;
    state.diffLineRowByCommentId = diffLineRowByCommentId;
    state.sectionBodyRange = sectionBodyRange;
    state.fileBodyRange = fileBodyRange;
    state.hunkBodyRange = hunkBodyRange;
    state.fileHeaderConceals = fileHeaderConceals;
    state.streamHunks = streamHunks;
    state.streamHunkById = streamHunkById;
    state.sectionByRow = sectionByRow;
    state.fileByRow = fileByRow;
    return entries;
}

// --- Reading the stream back by row ---
//
// A row is identified on demand from the hunk layout: which hunk starts
// at or before it (a binary search), then which of that hunk's rows it
// is, note boxes accounted for. Line numbers come from counting the hunk's
// lines up to it. Nothing is stored per row.

type StreamRow =
    | { kind: 'section'; category: string }
    | { kind: 'file'; key: string }
    | { kind: 'hunk-header'; sh: StreamHunk }
    | { kind: 'line'; sh: StreamHunk; lineIdx: number }
    | { kind: 'note'; sh: StreamHunk; commentId: string; afterLine: number };

/** Index into `state.streamHunks` of the last hunk whose header is at or
 *  before `row`, or -1. */
function streamHunkIndexAtRow(row: number): number {
    const shs = state.streamHunks;
    let lo = 0;
    let hi = shs.length - 1;
    let best = -1;
    while (lo <= hi) {
        const mid = (lo + hi) >> 1;
        if (shs[mid].headerRow <= row) {
            best = mid;
            lo = mid + 1;
        } else {
            hi = mid - 1;
        }
    }
    return best;
}

/** What body row `bodyIdx` (0 = the row after the header) of `sh` is. */
function bodyRowOf(sh: StreamHunk, bodyIdx: number): { lineIdx: number } | { note: StreamNote } {
    let shift = 0;
    for (const note of sh.notes) {
        const noteStart = note.afterLine + 1 + shift;
        if (bodyIdx < noteStart) break;
        if (bodyIdx < noteStart + note.rows) return { note };
        shift += note.rows;
    }
    return { lineIdx: bodyIdx - shift };
}

/** 1-indexed stream row of `sh.hunk.lines[lineIdx]`. */
function rowOfLine(sh: StreamHunk, lineIdx: number): number {
    let shift = 0;
    for (const note of sh.notes) {
        if (note.afterLine >= lineIdx) break;
        shift += note.rows;
    }
    return sh.headerRow + 1 + lineIdx + shift;
}

function streamRowAt(row: number): StreamRow | null {
    const category = state.sectionByRow.get(row);
    if (category !== undefined) return { kind: 'section', category };
    const key = state.fileByRow.get(row);
    if (key !== undefined) return { kind: 'file', key };
    const i = streamHunkIndexAtRow(row);
    if (i < 0) return null;
    const sh = state.streamHunks[i];
    if (row >= sh.headerRow + sh.rowCount) return null;
    if (row === sh.headerRow) return { kind: 'hunk-header', sh };
    const body = bodyRowOf(sh, row - sh.headerRow - 1);
    if ('note' in body) {
        return { kind: 'note', sh, commentId: body.note.commentId, afterLine: body.note.afterLine };
    }
    return { kind: 'line', sh, lineIdx: body.lineIdx };
}

/** Kind and line numbers of `hunk.lines[lineIdx]`; the side a row is
 *  absent from is undefined. A `\ No newline at end of file` marker is
 *  a context row with no numbers. */
function lineNumbersAt(hunk: Hunk, lineIdx: number): {
    lineType: 'add' | 'remove' | 'context'; oldLine?: number; newLine?: number;
} {
    let oldN = hunk.oldRange.start;
    let newN = hunk.range.start;
    for (let i = 0; i < lineIdx; i++) {
        const c = hunk.lines[i][0];
        if (c === '\\') continue;
        if (c !== '+') oldN++;
        if (c !== '-') newN++;
    }
    const c = hunk.lines[lineIdx][0];
    if (c === '\\') return { lineType: 'context' };
    if (c === '+') return { lineType: 'add', newLine: newN };
    if (c === '-') return { lineType: 'remove', oldLine: oldN };
    return { lineType: 'context', oldLine: oldN, newLine: newN };
}

/** The properties of a stream row — type, hunk, file, line numbers —
 *  derived when asked. */
function propsAtRow(row: number): Record<string, unknown> | null {
    const r = streamRowAt(row);
    if (r === null) return null;
    switch (r.kind) {
        case 'section':
            return { type: 'section-header', file: r.category, filePath: r.category };
        case 'file': {
            const fileIndex = state.files.findIndex(f => fileKey(f) === r.key);
            const path = fileIndex >= 0 ? state.files[fileIndex].path : undefined;
            return { type: 'file-header', file: path, filePath: path, fileKey: r.key, fileIndex };
        }
        case 'hunk-header':
            return { type: 'hunk-header', hunkId: r.sh.hunk.id, file: r.sh.hunk.file };
        case 'note':
            return { type: 'comment', commentId: r.commentId, hunkId: r.sh.hunk.id, file: r.sh.hunk.file };
        case 'line': {
            const hunk = r.sh.hunk;
            const n = lineNumbersAt(hunk, r.lineIdx);
            return {
                type: n.lineType, hunkId: hunk.id, file: hunk.file,
                lineType: n.lineType, oldLine: n.oldLine, newLine: n.newLine,
                lineContent: hunk.lines[r.lineIdx],
            };
        }
    }
}

/** Stream row of the diff line an anchor names, if the stream carries it. */
function rowOfAnchorLine(anchor: ReviewAnchor): number | undefined {
    if (anchor.lineType === undefined) return undefined;
    for (const h of hunksForKey(anchor.fileKey)) {
        const sh = state.streamHunkById.get(h.id);
        if (sh === undefined) continue;
        let oldN = h.oldRange.start;
        let newN = h.range.start;
        for (let i = 0; i < h.lines.length; i++) {
            const c = h.lines[i][0];
            if (c === '\\') continue;
            const lineType = c === '+' ? 'add' : c === '-' ? 'remove' : 'context';
            if (lineType === anchor.lineType
                && (lineType === 'remove' ? oldN === anchor.oldLine : newN === anchor.newLine)) {
                return rowOfLine(sh, i);
            }
            if (c !== '+') oldN++;
            if (c !== '-') newN++;
        }
    }
    return undefined;
}

const NS_WORD_DIFF = "review-word-diff";

/** Rows `[first, last]` the word-level highlights currently cover. */
let wordDiffWindow: { first: number; last: number } | null = null;

/**
 * Word-level highlights for the `-`/`+` pairs around `aroundRow` (a
 * 0-indexed viewport row). Painted for the rows on screen and a few
 * screens past them, not for the whole stream: the pairs are found by
 * walking only the hunks that reach into that window, and a move that
 * leaves the window repaints it.
 */
function paintWordDiff(aroundRow: number): void {
    wordDiffWindow = null;
    if (state.groupId === null || state.centerComposite) return;
    const diffId = state.panelBuffers["diff"];
    if (diffId === undefined) return;
    editor.clearNamespace(diffId, NS_WORD_DIFF);
    const height = Math.max(1, state.panelHeights["diff"] ?? state.viewportHeight);
    const first = Math.max(1, aroundRow + 1 - height);
    const last = aroundRow + 1 + 3 * height;
    const shs = state.streamHunks;
    const offsets = state.diffLineByteOffsets;
    for (let i = Math.max(0, streamHunkIndexAtRow(first)); i < shs.length && shs[i].headerRow <= last; i++) {
        const sh = shs[i];
        const lines = sh.hunk.lines;
        for (let li = 0; li + 1 < lines.length; li++) {
            if (lines[li][0] !== '-' || lines[li + 1][0] !== '+') continue;
            const removeRow = rowOfLine(sh, li);
            const addRow = rowOfLine(sh, li + 1);
            if (addRow < first) continue;
            if (removeRow > last) break;
            const parts = diffStrings(lines[li].substring(1), lines[li + 1].substring(1));
            // Past each row's diff marker.
            let rOffset = offsets[removeRow - 1] + 1;
            let aOffset = offsets[addRow - 1] + 1;
            for (const part of parts) {
                const len = getByteLength(part.text);
                if (part.type === 'removed') {
                    editor.addOverlay(diffId, NS_WORD_DIFF, rOffset, rOffset + len,
                        { fg: STYLE_REMOVE_TEXT, bg: STYLE_REMOVE_BG, bold: true });
                    rOffset += len;
                } else if (part.type === 'added') {
                    editor.addOverlay(diffId, NS_WORD_DIFF, aOffset, aOffset + len,
                        { fg: STYLE_ADD_TEXT, bg: STYLE_ADD_BG, bold: true });
                    aOffset += len;
                } else {
                    rOffset += len;
                    aOffset += len;
                }
            }
        }
    }
    wordDiffWindow = { first, last };
}

/**
 * Build the comments navigation panel. Flat list of comments in the
 * order they appear in the unified diff stream. Each row reads
 *   "path:line  snippet"
 * truncated to fit the panel width. Empty state shows a dim "No comments
 * yet." line. Read-only in this step (interaction lands in Step 5/6).
 */

/**
 * Build the file sidebar: one row per changed file, grouped by git
 * category, showing a status glyph, the (left-truncated) path, the
 * +added/-removed counts, and a `*N` badge when the file carries review
 * comments. The row matching the diff viewport's current file is
 * highlighted. Populates `state.filesPanelByRow` so a click resolves back
 * to a file.
 */
/** A one-glyph focus indicator for a panel header: `▸` when that panel holds
 *  keyboard focus, a space otherwise. Keeps the three panels (files / diff /
 *  comments) reading in the same visual language so arrow-key users can see
 *  where input will land. */
function focusMark(panel: 'files' | 'diff' | 'comments'): string {
    return state.focusPanel === panel ? '▸' : ' ';
}

// The toolbar and the two side panels are widget panels: the host owns
// their buttons' hit-testing, hover styling and activation, and the
// plugin only reacts to `widget_event`. Their bodies are still
// entry-based text, wrapped in `raw`.
let toolbarPanel: WidgetPanel | null = null;
let filesPanel: WidgetPanel | null = null;
let commentsPanel: WidgetPanel | null = null;

/** Width in columns of a side panel: the host's last reported viewport
 *  width for it, or the share of the screen `REVIEW_LAYOUT` gives it
 *  until the first `viewport_changed` for that panel arrives. */
function panelWidthOf(panel: 'files' | 'comments'): number {
    const known = state.panelWidths[panel];
    if (known && known > 0) return known;
    return Math.max(12, Math.floor(state.viewportWidth * (panel === 'files' ? FILES_PANEL_RATIO : 0.15)));
}

/** `text` clipped to `width` columns, dropping characters from the left
 *  and marking the cut with `…`. Used for paths, where the tail (the
 *  directory you are in) carries more than the root. */
function elideLeft(text: string, width: number): string {
    if (width <= 1) return text.slice(0, Math.max(0, width));
    return text.length <= width ? text : '…' + text.slice(text.length - (width - 1));
}

/** `text` clipped to `width` columns, marking the cut with `…`. */
function elideRight(text: string, width: number): string {
    if (width <= 1) return text.slice(0, Math.max(0, width));
    return text.length <= width ? text : text.slice(0, width - 1) + '…';
}

/** The close button at the right edge of an open side panel's header.
 *  Clicking it hides that panel (same as `F` / `C`). */
const PANEL_CLOSE_GLYPH = '✕';

const PANEL_CLOSE_KEYS = {
    files: "files.close",
    comments: "comments.close",
} as const;

/**
 * Header row for a side panel: the focus marker and label on the left, a
 * `✕` hard against the right edge. A `flexSpacer` between them is sized
 * by the host against the panel's real width, so the button stays pinned
 * to the edge without the plugin measuring anything — and the `✕` is a
 * real `Button`, so the host owns its hit-testing and hover styling.
 */
function panelHeaderSpec(panel: 'files' | 'comments', label: string): WidgetSpec {
    const base: Partial<OverlayOptions> = {
        fg: STYLE_INVERSE_FG,
        bg: STYLE_INVERSE_BG,
        bold: true,
        extendToLineEnd: true,
    };
    return row(
        raw([styledRow([{ text: `${focusMark(panel)}${label}`, style: base }], { style: base })]),
        flexSpacer(),
        button(PANEL_CLOSE_GLYPH, {
            key: PANEL_CLOSE_KEYS[panel],
            // Mouse-only, like the file explorer's and the orchestrator
            // dock's `✕`: the keyboard has `F` / `C`, and this panel's Tab
            // stop belongs to its list.
            focusable: false,
            bare: true,
            hoverStyle: { fg: "ui.tab_close_hover_fg" },
        }),
    );
}

/** Parent directory of a path (with trailing slash; `./` for repo root). */
function fileDirOf(p: string): string {
    const i = p.lastIndexOf('/');
    return i < 0 ? './' : p.slice(0, i + 1);
}
/** Basename of a path. */
function fileBaseOf(p: string): string {
    const i = p.lastIndexOf('/');
    return i < 0 ? p : p.slice(i + 1);
}

interface FileGroup {
    category: string;
    dir: string;
    dirKey: string;       // `${category} ${dir}` — collapse key
    files: FileEntry[];
}

/** The changed files grouped exactly as the sidebar renders them — category
 *  (first-seen order) → directory (sorted) → file (sorted by basename). This
 *  is the single source of truth: `buildFilesPanelEntries` renders from it and
 *  `visibleFiles` flattens it, so the visual list and `,`/`.`/↑/↓ navigation
 *  always traverse the identical order. */
function fileGroups(): FileGroup[] {
    const categories: string[] = [];
    for (const f of state.files) {
        if (!fileMatchesFilter(f)) continue;
        if (categories.indexOf(f.category) < 0) categories.push(f.category);
    }
    const groups: FileGroup[] = [];
    for (const category of categories) {
        const byDir: Record<string, FileEntry[]> = {};
        for (const f of state.files) {
            if (f.category !== category || !fileMatchesFilter(f)) continue;
            const d = fileDirOf(f.path);
            if (!byDir[d]) byDir[d] = [];
            byDir[d].push(f);
        }
        const dirs = Object.keys(byDir).sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));
        for (const dir of dirs) {
            const files = byDir[dir].slice().sort((a, b) => {
                const an = fileBaseOf(a.path), bn = fileBaseOf(b.path);
                return an < bn ? -1 : an > bn ? 1 : 0;
            });
            groups.push({ category, dir, dirKey: `${category} ${dir}`, files });
        }
    }
    return groups;
}

/** The FILES panel's header label (the panel's `✕` and the focus marker
 *  are drawn by `panelHeaderSpec`). */
function filesHeaderLabel(): string {
    return (editor.t("panel.files") || "Files").toUpperCase();
}

// --- FILES sidebar: a host-owned Tree -------------------------------------
//
// The sidebar is a `Tree` widget, not plugin-drawn rows. The host owns
// what a file list keeps getting wrong when a plugin hand-rolls it:
// selection, expand/collapse, scrolling the selection into view, clipping
// rows to the panel, and routing clicks. The plugin's job is to emit the
// hierarchy (category → directory → file) and to react to `select` /
// `activate` / `expand` events.

const FILES_TREE_KEY = "files-tree";
const FILES_FILTER_KEY = "files-filter";

/** Share of the review's width the FILES sidebar opens at.
 *
 *  Read by both `REVIEW_LAYOUT` (the host's initial split) and
 *  `panelWidthOf` (the plugin's own laying-out before the first
 *  `viewport_changed` arrives) — they describe the same panel, so they
 *  have to agree or the first paint is laid out to a width the panel
 *  does not have. Once the user drags the divider the host's reported
 *  width wins and this is no longer consulted.
 *
 *  Paths are long and the tree nests, so the old 0.16 left filenames
 *  elided to a few characters on a typical terminal. */
const FILES_PANEL_RATIO = 0.22;

/** Columns of indent per tree level in the FILES sidebar.
 *
 *  One, not the host's default of two: this tree nests a directory chain
 *  several levels deep in a panel only a couple of dozen columns wide, so
 *  every column spent on indent comes straight off the filenames. One
 *  column still reads as a level because each row also carries a
 *  disclosure glyph (or the two spaces standing in for one). */
const FILES_TREE_INDENT = 1;

interface FilesTree {
    nodes: TreeNode[];
    keys: string[];
    /** Node key → file key, for the file rows only. */
    fileByNodeKey: Record<string, string>;
    /** Index in `nodes` of each file row, so the selection can be
     *  restored from `state.filesCurrentKey` on every rebuild. */
    indexByFileKey: Record<string, number>;
    /** Every category / directory key, the initial expanded set. */
    groupKeys: string[];
}

/** Build the sidebar's node list: one row per category, per directory,
 *  and per file, depth-first. The tree draws its own disclosure glyphs
 *  and indents by depth, so the rows carry text only. */
/** One directory in the sidebar's hierarchy. Built per category from the
 *  changed files' paths, so the panel shows `crates` → `fresh-editor` →
 *  `src` as nested rows carrying short names — the file explorer's shape —
 *  rather than one row repeating the whole path per group. */
interface DirNode {
    /** Path segment shown on the row (empty for a category's root). */
    name: string;
    /** Full path from the repo root, with a trailing slash. */
    path: string;
    children: Map<string, DirNode>;
    files: FileEntry[];
    /** Added / removed across everything beneath, so an ancestor row
     *  totals its subtree the way its own files' rows total themselves. */
    added: number;
    removed: number;
}

function newDirNode(name: string, path: string): DirNode {
    return { name, path, children: new Map(), files: [], added: 0, removed: 0 };
}

/** Insert `file` under its directory chain, creating the ancestors it
 *  needs and adding its counts to each of them. */
function insertIntoDirTree(root: DirNode, file: FileEntry): void {
    const counts = fileChangeCounts(file);
    const dir = fileDirOf(file.path);
    let node = root;
    node.added += counts.added;
    node.removed += counts.removed;
    if (dir !== './') {
        let prefix = '';
        for (const segment of dir.split('/')) {
            if (segment === '') continue;
            prefix += `${segment}/`;
            let child = node.children.get(segment);
            if (!child) {
                child = newDirNode(segment, prefix);
                node.children.set(segment, child);
            }
            child.added += counts.added;
            child.removed += counts.removed;
            node = child;
        }
    }
    node.files.push(file);
}

/**
 * Collapse runs of directories that only ever contain one another into a
 * single row, the way the file explorer does: `crates/fresh-editor/src/app`
 * is one row rather than four, because none of the intermediate levels has
 * a file or a second child of its own to show. A level earns its own row as
 * soon as it holds a file or branches.
 *
 * Counts need no fixing up — every level of a chain totals the same
 * subtree — and the surviving node keeps the deepest `path`, so its key and
 * its expand state stay stable as long as the chain does.
 */
function compressDirChains(node: DirNode): void {
    for (const [key, child] of [...node.children]) {
        let merged = child;
        while (merged.files.length === 0 && merged.children.size === 1) {
            const only = [...merged.children.values()][0];
            merged = {
                name: `${merged.name}/${only.name}`,
                path: only.path,
                children: only.children,
                files: only.files,
                added: only.added,
                removed: only.removed,
            };
        }
        if (merged !== child) node.children.set(key, merged);
        compressDirChains(merged);
    }
}

function buildFilesTree(): FilesTree {
    const out: FilesTree = {
        nodes: [], keys: [], fileByNodeKey: {}, indexByFileKey: {}, groupKeys: [],
    };
    const commentCounts: Record<string, number> = {};
    for (const c of state.comments) commentCounts[c.file] = (commentCounts[c.file] || 0) + 1;

    // `fileGroups()` stays the ordering authority (category order, then
    // directory, then file) — this only re-shapes it into a hierarchy.
    const groups = fileGroups();
    const catCounts: Record<string, number> = {};
    const roots: Array<{ category: string; root: DirNode }> = [];
    for (const g of groups) {
        catCounts[g.category] = (catCounts[g.category] || 0) + g.files.length;
        let entry = roots.find(r => r.category === g.category);
        if (!entry) {
            entry = { category: g.category, root: newDirNode('', '') };
            roots.push(entry);
        }
        for (const f of g.files) insertIntoDirTree(entry.root, f);
    }
    for (const r of roots) compressDirChains(r.root);
    // With one category (the usual worktree review: everything unstaged)
    // a category row would be a header over the whole tree and one wasted
    // indent level in a panel that has ~24 columns to work with.
    const showCategories = roots.length > 1;
    const baseDepth = showCategories ? 1 : 0;
    const W = panelWidthOf('files');

    const pushDir = (category: string, node: DirNode, depth: number): void => {
        const key = `dir:${category} ${node.path}`;
        out.groupKeys.push(key);
        out.keys.push(key);
        // The host indents `FILES_TREE_INDENT` columns per depth level and
        // draws a disclosure glyph; a segment is short, so this only bites
        // on a deeply nested path in a narrow panel.
        const room = Math.max(4, W - depth * FILES_TREE_INDENT - 2);
        out.nodes.push(treeNode(
            { text: elideRight(node.name, room), style: { fg: STYLE_SECTION_HEADER } },
            { depth, hasChildren: true },
        ));
    };

    const pushFile = (file: FileEntry, depth: number): void => {
        const key = fileKey(file);
        const badge = commentCounts[file.path] ? ` *${commentCounts[file.path]}` : '';
        const nodeKey = `file:${key}`;
        out.fileByNodeKey[nodeKey] = key;
        out.indexByFileKey[key] = out.nodes.length;
        out.keys.push(nodeKey);
        // The status letter rides on the right with the counts, not in
        // front of the name. The host already aligns a leaf's text with
        // its sibling directories' *names* (a leaf gets two spaces where
        // the disclosure glyph would be), so a leading `M ` pushed every
        // filename two columns past its siblings — and a file following a
        // collapsed directory then read as that directory's contents. The
        // file explorer this sidebar mirrors right-aligns status for the
        // same reason.
        const status = file.status ? ` ${file.status}` : '';
        const stats = `${badge}${status}`;
        const room = Math.max(4, W - depth * FILES_TREE_INDENT - 2 - stats.length);
        out.nodes.push(treeNode(
            {
                text: `${elideRight(fileBaseOf(file.path), room)}${stats}`,
                properties: { type: "file", fileKey: key, filePath: file.path },
            },
            { depth },
        ));
    };

    /** Depth-first: a directory's own rows, then its children, then its
     *  files — the file explorer's order. */
    const emit = (category: string, node: DirNode, depth: number): void => {
        const childNames = [...node.children.keys()].sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));
        for (const name of childNames) {
            const child = node.children.get(name)!;
            pushDir(category, child, depth);
            emit(category, child, depth + 1);
        }
        for (const file of node.files) pushFile(file, depth);
    };

    for (const { category, root } of roots) {
        if (showCategories) {
            let label: string = category;
            if (state.mode === 'range' && state.range) label = state.range.label;
            else if (category === 'staged') label = editor.t("section.staged") || "Staged";
            else if (category === 'unstaged') label = editor.t("section.unstaged") || "Changes";
            else if (category === 'untracked') label = editor.t("section.untracked") || "Untracked";
            const display = state.mode === 'range' ? label : label.toUpperCase();
            const key = `cat:${category}`;
            out.groupKeys.push(key);
            out.keys.push(key);
            out.nodes.push(treeNode(
                {
                    text: `${display} (${catCounts[category]})`,
                    style: { fg: STYLE_SECTION_HEADER, bold: true },
                },
                { depth: 0, hasChildren: true },
            ));
        }
        emit(category, root, baseDepth);
    }
    return out;
}

/** The node the sidebar tree has selected — `file:…`, `dir:…` or
 *  `cat:…`. Mirrored from the host's `select` events so Enter knows what
 *  it is acting on. */
let filesSelectedNodeKey = "";

/** The sidebar tree as last built — the map from a `select` event's node
 *  key back to a file. */
let filesTree: FilesTree = {
    nodes: [], keys: [], fileByNodeKey: {}, indexByFileKey: {}, groupKeys: [],
};

/** The FILES panel spec: header, the filter field while `/` is open, and
 *  the file tree. */
function buildFilesPanelSpec(): WidgetSpec {
    filesTree = buildFilesTree();
    // The spec's `selectedIndex` is authoritative on every render, so it
    // has to agree with where the host's own navigation left the
    // selection — otherwise each repaint drags the selection back to the
    // current file and folding a directory (which selects a directory
    // row first) never gets to happen.
    const fileNodeKey = state.filesCurrentKey !== null ? `file:${state.filesCurrentKey}` : "";
    const trackedKey = filesSelectedNodeKey.startsWith("file:")
        ? fileNodeKey                       // a file row: the review's current file wins
        : filesSelectedNodeKey;             // a directory / category row: keep it
    let selected = trackedKey ? filesTree.keys.indexOf(trackedKey) : -1;
    if (selected < 0 && fileNodeKey) selected = filesTree.keys.indexOf(fileNodeKey);
    const parts: WidgetSpec[] = [
        panelHeaderSpec('files', filesHeaderLabel()),
        // Always present, whether or not it holds focus: a filter you
        // cannot see is a filter you forget is on. `/` focuses it, and so
        // does clicking it.
        text({
            value: state.fileFilter,
            cursorByte: filterCursor,
            placeholder: editor.t("prompt.filter_files") || "Filter files",
            fullWidth: true,
            key: FILES_FILTER_KEY,
        }),
    ];
    if (filesTree.nodes.length === 0) {
        parts.push(raw([{
            text: ` ${(state.fileFilter
                ? editor.t("status.filter_no_match") || "No files match"
                : editor.t("panel.no_changes") || "No changes.")}\n`,
            style: { fg: STYLE_SECTION_HEADER, italic: true },
        }]));
    } else {
        parts.push(tree({
            nodes: filesTree.nodes,
            itemKeys: filesTree.keys,
            selectedIndex: selected,
            // No `visibleRows`: the host sizes the tree to the panel's
            // live height minus the rows its siblings occupy, and re-runs
            // that on every resize. A plugin-side budget was a copy of the
            // height taken from the last `viewport_changed`, and a grown
            // panel kept the old, shorter window until some unrelated
            // event happened to repaint it — the rows below the window
            // stayed blank with files still to show.
            expandedKeys: filesTree.groupKeys,
            indentCols: FILES_TREE_INDENT,
            key: FILES_TREE_KEY,
        }));
    }
    return col(...parts);
}

/** The COMMENTS panel's header label. */
function commentsHeaderLabel(): string {
    return (editor.t("panel.comments") || "Comments").toUpperCase();
}

// --- COMMENTS rail: a host-owned List ------------------------------------
//
// One list item per rendered row — a comment's location row plus the rows
// its note wraps onto — all keyed back to the same comment. The host owns
// selection, scrolling and click routing; the plugin maps the selected
// item back to a comment and snaps the selection to that comment's
// location row, so stepping with ↑↓ moves comment by comment while the
// note still renders in full.

const COMMENTS_LIST_KEY = "comments-list";

interface CommentsList {
    items: TextPropertyEntry[];
    keys: string[];
    /** Item index of each comment's location row. */
    indexById: Record<string, number>;
}

/** Comments in stream order: by file, then by line. */
function commentsInStreamOrder(): ReviewComment[] {
    const fileIndex = (file: string): number => {
        for (let i = 0; i < state.files.length; i++) {
            if (state.files[i].path === file) return i;
        }
        return Number.MAX_SAFE_INTEGER;
    };
    return [...state.comments].sort((a, b) => {
        const fa = fileIndex(a.file);
        const fb = fileIndex(b.file);
        if (fa !== fb) return fa - fb;
        return (a.new_line ?? a.old_line ?? 0) - (b.new_line ?? b.old_line ?? 0);
    });
}

function buildCommentsList(): CommentsList {
    const out: CommentsList = { items: [], keys: [], indexById: {} };
    const width = Math.max(12, panelWidthOf('comments') - 1);
    for (const c of commentsInStreamOrder()) {
        const lineRef = c.new_line ?? c.old_line ?? 0;
        const path = c.file.split('/').pop() || c.file;
        const isCurrent = c.id === state.commentsHighlightId;
        const marker = isCurrent ? '>' : ' ';
        const locText = `${marker} ${path}:${lineRef}`;
        out.indexById[c.id] = out.items.length;
        out.keys.push(`c:${c.id}#loc`);
        out.items.push({
            text: elideRight(locText, width),
            style: { bold: true },
            inlineOverlays: [{ start: 2, end: getByteLength(elideRight(locText, width)), style: { fg: STYLE_KEY_FG } }],
            properties: { type: "comment-nav", commentId: c.id, file: c.file, line: lineRef },
        });
        const body = c.text.replace(/\s+/g, ' ').trim();
        const lines = wrapText(body, Math.max(4, width - 3));
        for (let i = 0; i < lines.length; i++) {
            out.keys.push(`c:${c.id}#${i}`);
            out.items.push({
                text: `   ${lines[i]}`,
                style: isCurrent ? undefined : { fg: STYLE_COMMENT },
                properties: { type: "comment-nav", commentId: c.id, file: c.file, line: lineRef },
            });
        }
    }
    return out;
}

/** The rail as last built, for mapping a selection back to a comment. */
let commentsList: CommentsList = { items: [], keys: [], indexById: {} };

/** The COMMENTS panel spec: header plus the comment list. */
function buildCommentsPanelSpec(): WidgetSpec {
    commentsList = buildCommentsList();
    if (commentsList.items.length === 0) {
        return col(
            panelHeaderSpec('comments', commentsHeaderLabel()),
            raw([{
                text: ` ${editor.t("panel.no_comments") || "No comments yet."}\n`,
                style: { fg: STYLE_SECTION_HEADER, italic: true },
            }]),
        );
    }
    const selected = state.commentsSelectedId !== null
        ? (commentsList.indexById[state.commentsSelectedId] ?? -1)
        : -1;
    return col(
        panelHeaderSpec('comments', commentsHeaderLabel()),
        list({
            items: commentsList.items,
            itemKeys: commentsList.keys,
            selectedIndex: selected,
            // Host-auto-sized, like the FILES tree above.
            focusable: true,
            key: COMMENTS_LIST_KEY,
        }),
    );
}

/**
 * Decide whether the center renders one file or a stream of them.
 *
 * Side-by-side builds a composite of one file's OLD/NEW buffers and is
 * per-file by construction. Unified is a single scrollable stream and
 * lays out the whole changeset — a 100-commit range included.
 *
 * There used to be a line budget here, above which the stream rendered
 * one file at a time and marked the rest "not loaded". It was
 * compensating for per-frame work in the host that grew with the buffer's
 * decorations; with that fixed (the plugin state snapshot deep-copied
 * every text property on every tick, and rainbow brackets republished
 * across the whole buffer on every frame) a fully laid-out 100k-line diff
 * costs what a 20k-line one does per keystroke, so the budget bought
 * nothing but a confusing review.
 */
function syncFocusMode(): void {
    state.focusOnly = state.reviewLayout === 'side-by-side';
}

/** Whether the center carries this file's hunk rows: everything in the
 *  unified stream, and only the focused file while the composite is up. */
function fileBodyRendered(key: string): boolean {
    return !state.focusOnly || key === state.filesCurrentKey;
}

/** Whether `panel` is currently on screen. The diff and its sticky header
 *  are never hidden; the two side panels are. */
function panelVisible(panel: 'files' | 'diff' | 'comments'): boolean {
    if (panel === 'diff') return true;
    return state.panelsVisible[panel];
}

/** Repaint the FILES sidebar, if it is on screen. Hidden panels are not
 *  rendered, so rebuilding their content is work nobody sees —
 *  `setReviewPanelVisible` repaints on the way back in. */
function renderFilesPanel(): void {
    if (filesPanel === null || !panelVisible('files')) return;
    filesPanel.set(buildFilesPanelSpec());
    pointSidebarAtCurrentFile();
}

/** Move the sidebar's selection onto the review's current file.
 *
 *  A tree's selected row is host state after its first render — the
 *  `selectedIndex` in a rebuilt spec is a seed the host ignores — so
 *  repainting the panel with a new current file left the highlight
 *  wherever the sidebar's own navigation had last put it. Reading down
 *  the stream then walked the cursor through file after file with the
 *  sidebar still pointing at the one you started in. This is the
 *  host-side setter, so the selection actually moves (and scrolls itself
 *  into view).
 *
 *  A directory or section row the user selected stays put: those are the
 *  sidebar's own navigation, and folding one is a gesture the diff cursor
 *  has no opinion about. */
function pointSidebarAtCurrentFile(): void {
    if (filesPanel === null || !panelVisible('files')) return;
    if (state.filesCurrentKey === null) return;
    if (filesSelectedNodeKey.startsWith("dir:") || filesSelectedNodeKey.startsWith("cat:")) return;
    const nodeKey = `file:${state.filesCurrentKey}`;
    if (nodeKey === filesSelectedNodeKey) return;
    const idx = filesTree.keys.indexOf(nodeKey);
    if (idx < 0) return;
    filesSelectedNodeKey = nodeKey;
    filesPanel.setSelectedIndex(FILES_TREE_KEY, idx);
}

/** Repaint the COMMENTS rail, if it is on screen. */
function renderCommentsPanel(): void {
    if (commentsPanel === null || !panelVisible('comments')) return;
    commentsPanel.set(buildCommentsPanelSpec());
    if (state.focusPanel === 'comments') commentsPanel.setFocusKey(COMMENTS_LIST_KEY);
}

/** Push the plugin's visibility state into the host's group layout. */
function applyPanelVisibility(): void {
    if (state.groupId === null) return;
    for (const panel of ['files', 'comments'] as const) {
        editor.setBufferGroupPanelVisible(state.groupId, panel, state.panelsVisible[panel]);
    }
}

/**
 * Show or hide one of the side panels. Hiding the panel that holds
 * keyboard focus hands focus back to the diff (the host refuses to focus
 * an unrendered panel, so leaving `focusPanel` pointing at it would
 * strand the arrow keys).
 */
function setReviewPanelVisible(panel: 'files' | 'comments', visible: boolean): void {
    if (state.groupId === null) return;
    if (state.panelsVisible[panel] === visible) return;
    state.panelsVisible[panel] = visible;
    editor.setBufferGroupPanelVisible(state.groupId, panel, visible);
    if (visible) {
        if (panel === 'files') {
            renderFilesPanel();
        } else {
            renderCommentsPanel();
        }
        // You asked for the panel; the keys go there.
        reviewSetFocus(panel);
    } else if (state.focusPanel === panel) {
        // Focus cannot stay on a panel that is no longer drawn.
        reviewSetFocus('diff');
    }
    if (!visible && state.focusPanel === panel) state.focusPanel = 'diff';
    // The panel appearing or vanishing is its own feedback — no status
    // message — but the toolbar button carries the open/closed marker.
    renderToolbar();
}

function review_toggle_files_panel(): void {
    setReviewPanelVisible('files', !state.panelsVisible.files);
}
registerHandler("review_toggle_files_panel", review_toggle_files_panel);

function review_toggle_comments_panel(): void {
    setReviewPanelVisible('comments', !state.panelsVisible.comments);
}
registerHandler("review_toggle_comments_panel", review_toggle_comments_panel);

/** Buttons in the toolbar and in the two panel headers. The host does the
 *  hit-testing and hands us the widget key. */
editor.on("widget_event", (data) => {
    if (state.groupId === null) return;

    // --- FILES tree: selection, activation, and the filter field --------
    if (data.widget_key === FILES_TREE_KEY) {
        // Focus moved off the field and onto the tree — back to the
        // panel's command keys.
        if (data.event_type === "focus") {
            leaveFilterMode();
            return;
        }
        const nodeKey = String((data.payload as Record<string, unknown>)?.["key"] ?? "");
        if (data.event_type === "select") {
            onFilesTreeSelect(nodeKey);
            return;
        }
        if (data.event_type === "activate") {
            // Double-click on a file row: same as Enter — go to it.
            if (nodeKey.startsWith("file:")) {
                filesSelectedNodeKey = nodeKey;
                onFilesTreeSelect(nodeKey);
                reviewSetFocus('diff');
            }
            return;
        }
        return; // `expand` is host-owned; nothing to mirror.
    }
    if (data.widget_key === FILES_FILTER_KEY) {
        if (data.event_type === "focus") {
            enterFilterMode();
            return;
        }
        if (data.event_type === "change") {
            const payload = (data.payload ?? {}) as Record<string, unknown>;
            if (typeof payload["value"] === "string") state.fileFilter = payload["value"];
            if (typeof payload["cursorByte"] === "number") filterCursor = payload["cursorByte"];
            scheduleFileFilter();
            return;
        }
        if (data.event_type === "activate" || data.event_type === "cancel") {
            closeFileFilter(data.event_type === "cancel");
            return;
        }
        return;
    }
    // --- COMMENTS list --------------------------------------------------
    if (data.widget_key === COMMENTS_LIST_KEY) {
        const itemKey = String((data.payload as Record<string, unknown>)?.["key"] ?? "");
        const commentId = itemKey.startsWith("c:") ? itemKey.slice(2).split("#")[0] : "";
        if (!commentId) return;
        if (data.event_type === "select") {
            state.commentsSelectedId = commentId;
            renderCommentsPanel();
            return;
        }
        if (data.event_type === "activate") {
            state.commentsSelectedId = commentId;
            jumpToComment(commentId);
            renderCommentsPanel();
            return;
        }
        return;
    }

    if (data.event_type !== "activate") return;
    switch (data.widget_key) {
        case PANEL_BUTTON_KEYS.files:
            review_toggle_files_panel();
            return;
        case PANEL_BUTTON_KEYS.comments:
            review_toggle_comments_panel();
            return;
        case PANEL_CLOSE_KEYS.files:
            setReviewPanelVisible('files', false);
            return;
        case PANEL_CLOSE_KEYS.comments:
            setReviewPanelVisible('comments', false);
            return;
    }
});

/** A row of the FILES tree became the selection — by arrow key or click.
 *  Selecting a file moves the review to it; selecting a category or
 *  directory row is just navigation. */
function onFilesTreeSelect(nodeKey: string): void {
    filesSelectedNodeKey = nodeKey;
    const key = filesTree.fileByNodeKey[nodeKey];
    if (key === undefined || key === state.filesCurrentKey) return;
    // Asked before the assignment: in side-by-side `fileBodyRendered` is
    // `filesCurrentKey` itself, so the answer changes as we assign.
    const needsLayout = !fileBodyRendered(key);
    state.filesCurrentKey = key;
    if (needsLayout) {
        // Side-by-side: the composite draws one file, so it has to be
        // rebuilt around this one.
        refreshFocusedFile();
        return;
    }
    // The stream already holds this file — just go there. No rebuild, so
    // walking the sidebar with ↑↓ stays instant on a big diff.
    const headerRow = state.fileHeaderRows[key];
    if (headerRow !== undefined) jumpDiffCursorToRow(headerRow);
}

/** Declare that the unified stream's content is out of date: the next
 *  render of the stream has to lay it out again.
 *
 *  Every caller that changes what the stream says goes through
 *  `updateMagitDisplay` or `applyFileFilter`, so those two mark it — a
 *  layout flip, which changes only *which* view is mounted, does not. */
function markStreamDirty(): void {
    state.streamRevision++;
}

/** What the content currently in the stream buffer was built from. The
 *  focused file is part of it because the composite's stand-in stream
 *  carries only that file's body (`fileBodyRendered`).
 *
 *  The panel's width counts only while notes are on screen: comment boxes
 *  are the one thing wrapped to it (`noteBoxWidth`), and the host
 *  reports the panel's real width a moment after the stream is first laid
 *  out — so treating every width as significant put a full relayout in
 *  the reader's way seconds into a review that had no notes in it. */
function streamSignature(): string {
    const widthShapesTheStream = state.showComments && state.comments.length > 0;
    return [
        state.streamRevision,
        state.focusOnly ? state.filesCurrentKey : '*',
        // The width the boxes were actually laid out to, not the panel's:
        // a wide panel clamps them to the same width whatever it reports,
        // and a relayout of the whole stream is not free.
        widthShapesTheStream ? noteBoxWidth() : '*',
    ].join('|');
}

/**
 * Full refresh — rebuild all three panels. Called on data changes
 * (refreshMagitData, comment add/edit, note edit, resize). NOT called on
 * scroll: scrolling is handled natively by the editor in the panel buffers.
 */
function updateMagitDisplay(): void {
    markStreamDirty();
    refreshViewportDimensions();
    if (state.groupId === null) return;
    syncFocusMode();
    ensureFocusFile();
    renderToolbar();
    renderCenter();
    renderCommentsPanel();
    renderFilesPanel();
    refreshStickyHeader(0);
}

/** Conceal namespace owning the collapsed-header triangles, so
 *  `applyFolds` can drop the whole set in one host call. */
const NS_COLLAPSE_TRIANGLE = "review-collapse-triangle";
/** Conceal namespace of the file labels drawn over `diff --git` rows. */
const NS_FILE_HEADER = "review-file-header";

/** Byte range of the `▾` in a section header row, or a file header row
 *  of the plugin's own, given its 1-indexed row. Every such header is
 *  emitted as `"▾ …"`, so the glyph is the row's first `TRIANGLE_BYTES`
 *  bytes. Returns null when the row
 *  index has no recorded offset (stale state between rebuilds). */
function headerTriangleRange(row1: number | undefined): { start: number; end: number } | null {
    if (row1 === undefined) return null;
    const start = state.diffLineByteOffsets[row1 - 1];
    if (start === undefined) return null;
    return { start, end: start + TRIANGLE_BYTES };
}

/**
 * Apply collapse state via the host's folding infrastructure. Folds
 * are designed exactly for "header line stays visible, body lines
 * skipped by the renderer". A fold range covers `[bodyStart, bodyEnd)`
 * — the line containing `bodyStart - 1` (the header) stays visible,
 * everything inside the range gets elided.
 *
 * The header keeps its `▾` in the buffer text and a replacement conceal
 * turns it into `▸` while collapsed, so the triangle agrees with the
 * fold instead of pointing open over a closed body — the sidebar's
 * directory rows rotate theirs, and a row that says "expanded" while
 * showing nothing is a contradiction the host's `…` marker doesn't
 * resolve. Conceals are rendering-only: the buffer text never changes,
 * so this keeps collapse a rebuild-free operation.
 *
 * Toggling collapse costs one `addConceal` per file with a diff (the
 * header labels are re-issued whole, collapsed or not) plus one
 * `addFold` + `addConceal` per collapsed item. `clearFolds` /
 * `clearConcealNamespace` each drop a whole set in one host call.
 */
function applyFolds(): void {
    if (state.groupId === null) return;
    const diffId = state.panelBuffers["diff"];
    if (diffId === undefined) return;
    editor.clearFolds(diffId);
    editor.clearConcealNamespace(diffId, NS_COLLAPSE_TRIANGLE);
    editor.clearConcealNamespace(diffId, NS_FILE_HEADER);
    // A file header is git's `diff --git` row wearing the file's label.
    for (const [key, header] of state.fileHeaderConceals) {
        const glyph = state.collapsedFiles.has(key) ? GLYPH_COLLAPSED : GLYPH_EXPANDED;
        editor.addConceal(diffId, NS_FILE_HEADER, header.start, header.end, `${glyph} ${header.label}`);
    }
    const collapseTriangle = (row1: number | undefined): void => {
        const range = headerTriangleRange(row1);
        if (range) {
            editor.addConceal(diffId, NS_COLLAPSE_TRIANGLE, range.start, range.end, GLYPH_COLLAPSED);
        }
    };
    for (const cat of state.collapsedSections) {
        const body = state.sectionBodyRange[cat];
        if (body && body.end > body.start) editor.addFold(diffId, body.start, body.end);
        collapseTriangle(state.sectionHeaderRows[cat]);
    }
    for (const key of state.collapsedFiles) {
        const body = state.fileBodyRange[key];
        if (body && body.end > body.start) editor.addFold(diffId, body.start, body.end);
        if (!state.fileHeaderConceals.has(key)) collapseTriangle(state.fileHeaderRows[key]);
    }
    for (const id of state.collapsedHunks) {
        const body = state.hunkBodyRange[id];
        if (body && body.end > body.start) editor.addFold(diffId, body.start, body.end);
        // A hunk header is git's `@@ … @@` row: its leading `@@` reads as
        // `▸ @@` while the body is folded.
        const row = state.hunkRowByHunkId[id];
        const start = row !== undefined ? state.diffLineByteOffsets[row - 1] : undefined;
        if (start !== undefined) {
            editor.addConceal(diffId, NS_COLLAPSE_TRIANGLE, start, start + 2, `${GLYPH_COLLAPSED} @@`);
        }
    }
    // Folding changes which rows are on screen without a scroll.
    paintWordDiff(state.diffViewportTopRow);
}

/**
 * Render the sticky panel for `topVisibleRow` (0-indexed line at the top
 * of the diff viewport). Shows the file whose header row is the largest
 * ≤ topVisibleRow, with its category as a dim prefix. Falls back to a
 * neutral summary when nothing is above the cursor.
 */
function refreshStickyHeader(topVisibleRow: number): void {
    if (state.groupId === null) return;
    const stickyId = state.panelBuffers["sticky"];
    if (stickyId === undefined) return;

    // The sticky row spans the diff pane, so it is clipped to the diff
    // pane's width — not the review's overall viewport, which is the
    // focused split's and may be a side panel.
    const W = diffPanelWidth();
    let text: string;
    let style: Partial<OverlayOptions> = { fg: STYLE_HEADER, bold: true };

    // topVisibleRow is 0-indexed; fileHeaderRows are 1-indexed.
    let bestFile: FileEntry | null = null;
    if (state.focusOnly) {
        // Focus mode: the current file is the explicit nav target
        // (`filesCurrentKey`), not whatever happens to be scrolled to the
        // top. Re-deriving from scroll here fought file navigation (after
        // `,`/`.` the top-visible header would clobber the focus) and broke
        // the composite center, which has no in-buffer file headers at all.
        bestFile = state.files.find(f => fileKey(f) === state.filesCurrentKey) ?? null;
    } else {
        const top1 = topVisibleRow + 1;
        let bestRow = 0;
        for (const f of state.files) {
            const row = state.fileHeaderRows[fileKey(f)];
            if (row !== undefined && row <= top1 && row > bestRow) {
                bestRow = row;
                bestFile = f;
            }
        }
    }

    if (!bestFile) {
        if (state.files.length === 0) {
            text = ` ${editor.t("status.review_empty") || "Review Diff"}`;
        } else {
            const totals = state.files.reduce(
                (acc, f) => {
                    const c = fileChangeCounts(f);
                    acc.added += c.added;
                    acc.removed += c.removed;
                    return acc;
                },
                { added: 0, removed: 0 }
            );
            const rangeSuffix = state.mode === 'range' && state.range
                ? ` (${state.range.label})`
                : '';
            text = ` Review Diff${rangeSuffix} — ${state.files.length} files, +${totals.added} / -${totals.removed}`;
            style = { fg: STYLE_SECTION_HEADER, italic: true };
        }
    } else {
        const counts = fileChangeCounts(bestFile);
        let section: string = bestFile.category;
        // In range mode every hunk is bucketed as 'unstaged' as an impl
        // detail; "UNSTAGED" would be misleading, so display the range
        // label instead.
        if (state.mode === 'range' && state.range) {
            section = state.range.label;
        } else if (bestFile.category === 'staged') section = (editor.t("section.staged") || "Staged").toUpperCase();
        else if (bestFile.category === 'unstaged') section = (editor.t("section.unstaged") || "Changes").toUpperCase();
        else if (bestFile.category === 'untracked') section = (editor.t("section.untracked") || "Untracked").toUpperCase();
        const filename = bestFile.origPath ? `${bestFile.origPath} → ${bestFile.path}` : bestFile.path;
        text = ` ${section} · ${filename}   +${counts.added} / -${counts.removed}`;
    }

    // Prefix a focus marker so the diff panel reads as "focused" in the same
    // visual language as the FILES / COMMENTS headers.
    text = `${focusMark('diff')}${text.replace(/^ /, '')}`;
    const padded = (text.length > W ? text.slice(0, W) : text).padEnd(W) + "\n";
    editor.setPanelContent(state.groupId, "sticky", [{
        text: padded,
        // Same band-bg as file/section headers — keeps the sticky visually
        // tied to the headers it summarizes and avoids the toolbar's
        // status_bar_bg, which is a saturated accent in some themes
        // (Dracula's is hot pink — clashes badly with the diff content).
        style: { ...style, bg: STYLE_FILE_HEADER_BG, extendToLineEnd: true },
        properties: { type: "sticky-header" },
    }]);

    // Expanded (multi-file) stream: the sidebar highlight follows the
    // *cursor's* file, not the top-visible one. Deriving it from the scroll
    // fought navigation — `.` jumped the cursor into the next file, the
    // resulting scroll put a different file at the top, and the highlight
    // (and with it the next `.`) snapped back to that one. In focus mode
    // the current file is explicit navigation, so it isn't re-derived at all.
    if (!state.focusOnly) {
        const cursorFile = currentFileFromCursor();
        const curKey = cursorFile ? fileKey(cursorFile) : state.filesCurrentKey;
        if (curKey !== state.filesCurrentKey) {
            state.filesCurrentKey = curKey;
            renderFilesPanel();
        }
    }
}



/**
 * Helper: jump the diff cursor to the file's first hunk (or its file
 * header if it has no hunks). Auto-expands the file if collapsed.
 */
function jumpToFile(file: FileEntry): void {
    const key = fileKey(file);
    // Collapse is a fold, so revealing the file is one too — no relayout.
    if (state.collapsedFiles.delete(key)) applyFolds();
    // Prefer this file's first hunk row; fall back to the file header.
    // Read the row from the build's own map rather than counting hunks:
    // the count used to skip collapsed files, but collapse is a conceal
    // (see `applyFolds`) and their hunk headers are still in the stream,
    // so the Nth counted hunk was not the Nth row — clicking the sticky
    // header with a collapsed file above landed inside that file instead.
    const firstHunk = hunksForKey(key)[0];
    if (firstHunk) {
        const row = state.hunkRowByHunkId[firstHunk.id];
        if (row !== undefined) { jumpDiffCursorToRow(row); return; }
    }
    const headerRow = state.fileHeaderRows[key];
    if (headerRow !== undefined) jumpDiffCursorToRow(headerRow);
}

/**
 * Mouse click handler. Routes clicks to the appropriate behavior:
 *   * Diff buffer file-header row → toggle that file's collapse state.
 *   * Sticky panel → jump to the currently-pinned file's first hunk.
 *   * Comments panel row → jump diff cursor to that comment's location
 *     (auto-expanding the file when collapsed) and select the row.
 */
function on_review_mouse_click(data: {
    column: number; row: number; button: string; modifiers: string;
    content_x: number; content_y: number;
    buffer_id: number | null; buffer_row: number | null; buffer_col: number | null;
}): void {
    if (state.groupId === null) return;
    if (data.buffer_id === null || data.buffer_row === null) return;

    const diffId = state.panelBuffers["diff"];
    const stickyId = state.panelBuffers["sticky"];
    const commentsId = state.panelBuffers["comments"];

    // Clicks on the toolbar's and the panel headers' buttons arrive as
    // `widget_event`, not here.

    // A click is a focus gesture: the panel you clicked takes the keys.
    // The host moves its own focus to the clicked buffer; mirroring it
    // here keeps `state.focusPanel` — which decides where ↑↓ / ←→ /
    // Home / End go — from pointing at the panel you just left.
    const clickedPanel: 'files' | 'diff' | 'comments' | null =
        data.buffer_id === state.panelBuffers["files"] ? 'files'
            : data.buffer_id === commentsId ? 'comments'
                : (data.buffer_id === diffId || data.buffer_id === stickyId
                    || (state.centerComposite
                        && data.buffer_id === state.centerComposite.compositeBufId)) ? 'diff'
                    : null;
    if (clickedPanel && clickedPanel !== state.focusPanel && panelVisible(clickedPanel)) {
        reviewSetFocus(clickedPanel);
    }

    // Click in the diff buffer: section headers and file headers are
    // both interactive — clicking either toggles its fold state.
    if (data.buffer_id === diffId) {
        const targetRow1 = data.buffer_row + 1;
        // Section header click: toggle the whole category.
        for (const cat of Object.keys(state.sectionHeaderRows)) {
            if (state.sectionHeaderRows[cat] === targetRow1) {
                if (state.collapsedSections.has(cat)) state.collapsedSections.delete(cat);
                else state.collapsedSections.add(cat);
                applyFolds();
                const sectionRow = state.sectionHeaderRows[cat];
                if (sectionRow !== undefined) jumpDiffCursorToRow(sectionRow, { recenter: false });
                return;
            }
        }
        // File header click.
        for (const f of state.files) {
            if (state.fileHeaderRows[fileKey(f)] === targetRow1) {
                const key = fileKey(f);
                if (state.collapsedFiles.has(key)) state.collapsedFiles.delete(key);
                else state.collapsedFiles.add(key);
                applyFolds();
                const headerRow = state.fileHeaderRows[key];
                if (headerRow !== undefined) jumpDiffCursorToRow(headerRow, { recenter: false });
                return;
            }
        }
        // Hunk header click: toggle the single hunk.
        for (const hunkId of Object.keys(state.hunkRowByHunkId)) {
            if (state.hunkRowByHunkId[hunkId] === targetRow1) {
                if (state.collapsedHunks.has(hunkId)) state.collapsedHunks.delete(hunkId);
                else state.collapsedHunks.add(hunkId);
                applyFolds();
                const hunkRow = state.hunkRowByHunkId[hunkId];
                if (hunkRow !== undefined) jumpDiffCursorToRow(hunkRow, { recenter: false });
                return;
            }
        }
        return;
    }

    // Click on the sticky pinned-header: jump to the pinned file's first hunk.
    if (data.buffer_id === stickyId) {
        // Re-derive the pinned file from current viewport top.
        const top1 = state.diffCursorRow; // approximation; sticky tracks topmost visible
        let bestFile: FileEntry | null = null;
        let bestRow = 0;
        for (const f of state.files) {
            const row = state.fileHeaderRows[fileKey(f)];
            if (row !== undefined && row <= top1 && row > bestRow) {
                bestRow = row;
                bestFile = f;
            }
        }
        if (bestFile) jumpToFile(bestFile);
        return;
    }

    // The FILES sidebar is a Tree widget: its rows, disclosure glyphs and
    // header button all report through `widget_event`, so a raw click in
    // that buffer has nothing left to do here.
    if (data.buffer_id === state.panelBuffers["files"]) return;

    // Click in the comments panel: jump to the comment's location and
    // hand focus to the diff so the user can immediately keep navigating.
    // The COMMENTS rail is a List widget: its rows report through
    // `widget_event`, so a raw click there has nothing left to do here.
    if (data.buffer_id === commentsId) return;
}
registerHandler("on_review_mouse_click", on_review_mouse_click);

/**
 * Jump the diff cursor to the line associated with a comment, auto-
 * expanding the comment's file if it is currently collapsed.
 */
function jumpToComment(commentId: string): void {
    const comment = state.comments.find(c => c.id === commentId);
    if (!comment) return;
    const hunk = state.hunks.find(h => h.id === comment.hunk_id);
    if (!hunk) return;
    const cFile = state.files.find(f => f.path === hunk.file && f.category === hunk.gitStatus);

    // Side-by-side: position the composite on the comment's hunk (the unified
    // jumpDiffCursorToRow path is inert when the composite is showing). Switch
    // to the comment's file if needed and rebuild focused on that hunk.
    if (state.centerComposite && cFile) {
        const fileHunks = hunksForKey(fileKeyOf(hunk.file, hunk.gitStatus || 'unstaged'));
        const idx = Math.max(0, fileHunks.findIndex(h => h.id === hunk.id));
        state.filesCurrentKey = fileKey(cFile);
        state.commentsHighlightId = commentId;
        void (async () => {
            await buildCenterComposite(idx);
            if (state.groupId !== null && state.panelBuffers["files"] !== undefined) {
                renderFilesPanel();
            }
            if (state.groupId !== null) {
                renderCommentsPanel();
            }
            refreshStickyHeader(0);
        })();
        return;
    }

    // Auto-expand whatever's between the cursor and this comment. Two
    // different costs hide here: revealing a collapse is a fold change,
    // while changing which file the centre carries is a real relayout.
    let revealed = false;
    let needRebuild = false;
    if (hunk.gitStatus) revealed = state.collapsedSections.delete(hunk.gitStatus) || revealed;
    const file = state.files.find(f => f.path === hunk.file && f.category === hunk.gitStatus);
    if (file) {
        const key = fileKey(file);
        // The comment may live in a file the center isn't carrying — the
        // composite draws one file. Make it the current file so the anchor
        // row exists after the rebuild.
        if (!fileBodyRendered(key)) {
            state.filesCurrentKey = key;
            needRebuild = true;
        }
        revealed = state.collapsedFiles.delete(key) || revealed;
    }
    revealed = state.collapsedHunks.delete(hunk.id) || revealed;
    if (needRebuild) updateMagitDisplay();
    else if (revealed) applyFolds();
    // Pin this comment as the highlighted one BEFORE jumping. Any
    // subsequent cursor_moved event that re-derives the highlight
    // will recompute the same id; doing it eagerly avoids a flicker
    // (and works even when the cursor lands on a row whose props
    // don't directly carry a comment id).
    const prevHighlight = state.commentsHighlightId;
    state.commentsHighlightId = commentId;
    if (state.groupId !== null && prevHighlight !== commentId) {
        renderCommentsPanel();
    }
    // Prefer the diff line the comment is anchored to (line-based);
    // fall back to the hunk header if the lookup hasn't seen the
    // comment yet (race / first render).
    const lineRow = state.diffLineRowByCommentId[commentId];
    if (lineRow !== undefined) { jumpDiffCursorToRow(lineRow); return; }
    const hunkRow = state.hunkRowByHunkId[hunk.id];
    if (hunkRow !== undefined) jumpDiffCursorToRow(hunkRow);
}

/** Milliseconds of quiet before a resized side panel is repainted.
 *
 *  A divider drag delivers a `viewport_changed` per column crossed. The
 *  host has already re-laid the panes out by then — this only defers the
 *  panel's *content* rebuild, so the drag itself stays smooth and the
 *  content catches up the moment the pointer settles. */
const PANEL_RELAYOUT_DEBOUNCE_MS = 60;

const panelRelayoutTimers: { files: number | null; comments: number | null; diff: number | null } = {
    files: null,
    comments: null,
    diff: null,
};

const RELAYOUT_HANDLERS: Record<'files' | 'comments' | 'diff', string> = {
    files: "review_relayout_files",
    comments: "review_relayout_comments",
    diff: "review_relayout_diff",
};

/** Repaint `panel` once its size stops changing. */
function schedulePanelRelayout(panel: 'files' | 'comments' | 'diff'): void {
    const pending = panelRelayoutTimers[panel];
    if (pending !== null) editor.clearInterval(pending);
    panelRelayoutTimers[panel] = editor.setTimeout(
        PANEL_RELAYOUT_DEBOUNCE_MS,
        RELAYOUT_HANDLERS[panel],
    );
}

function review_relayout_files(): void {
    panelRelayoutTimers.files = null;
    if (state.groupId === null) return;
    renderFilesPanel();
}
registerHandler("review_relayout_files", review_relayout_files);

function review_relayout_comments(): void {
    panelRelayoutTimers.comments = null;
    if (state.groupId === null) return;
    renderCommentsPanel();
}
registerHandler("review_relayout_comments", review_relayout_comments);

/** The stream's inline comment boxes are wrapped to the diff panel's
 *  width, so a width the layout did not know about leaves them the wrong
 *  shape. Re-emit the content once the width settles — and *only* the
 *  content: this runs off a timer, at a moment nobody asked for, so it
 *  must not swap which buffer the panel shows or move focus the way a
 *  full `renderCenter` does. Firing that in the middle of a drill-down
 *  would pull the reader out of the composite they just opened. The
 *  signature check inside `mountStreamContent` makes it a no-op when the
 *  width is what the content was already built to. */
function review_relayout_diff(): void {
    panelRelayoutTimers.diff = null;
    if (state.groupId === null || state.reviewLayout === 'side-by-side') return;
    if (state.centerComposite !== null) return;
    mountStreamContent();
}
registerHandler("review_relayout_diff", review_relayout_diff);

function on_review_viewport_changed(data: { split_id: number; buffer_id: number; top_byte: number; top_line: number | null; width: number; height: number }): void {
    if (state.groupId === null) return;
    // Side panels: remember how wide the host actually made them, and
    // repaint once when that changes so the header's `✕` lands on the
    // right edge instead of a guessed column.
    for (const panel of ['files', 'comments'] as const) {
        if (data.buffer_id !== state.panelBuffers[panel]) continue;
        if (state.panelWidths[panel] === data.width
            && state.panelHeights[panel] === data.height) return;
        state.panelWidths[panel] = data.width;
        state.panelHeights[panel] = data.height;
        // Record the size synchronously — everything laid out against it
        // (`panelWidthOf`, the header's `✕` column) must see the new width
        // at once — but coalesce the repaint. Dragging the divider walks
        // through every intervening width, and each one would otherwise
        // rebuild the whole tree and ship it across the IPC boundary; the
        // intermediate widths are never worth painting, only the one the
        // user stops on.
        schedulePanelRelayout(panel);
        return;
    }
    if (data.buffer_id !== state.panelBuffers["diff"]) return;
    // Inline comment boxes are laid out to this width (see
    // `diffPanelWidth`), so it is part of the stream's signature: record
    // it synchronously, then re-render once the width settles. Dragging a
    // divider walks through every intervening width and none of them is
    // worth a layout of the whole stream.
    const widthChanged = state.panelWidths["diff"] !== data.width;
    state.panelWidths["diff"] = data.width;
    if (widthChanged) schedulePanelRelayout('diff');
    // Height too, so `refreshViewportDimensions` has an authoritative
    // size for the diff pane and never has to trust whichever split
    // happens to hold focus.
    state.panelHeights["diff"] = data.height;
    // Prefer top_line when the host provides it. Virtual buffers may not
    // have line metadata, in which case top_line is null — fall back to
    // converting top_byte using our own row-byte index.
    const topRow = data.top_line ?? rowFromByte(data.top_byte);
    state.diffViewportTopRow = topRow;
    refreshStickyHeader(topRow);
    if (wordDiffWindow === null || topRow + 1 < wordDiffWindow.first
        || topRow + data.height > wordDiffWindow.last) {
        paintWordDiff(topRow);
    }
}
registerHandler("on_review_viewport_changed", on_review_viewport_changed);

/**
 * Binary-search `state.diffLineByteOffsets` for the 0-indexed row
 * whose byte offset is the largest one ≤ topByte.
 */
function rowFromByte(topByte: number): number {
    const offs = state.diffLineByteOffsets;
    if (offs.length === 0) return 0;
    let lo = 0;
    let hi = offs.length - 1;
    while (lo < hi) {
        const mid = (lo + hi + 1) >> 1;
        if (offs[mid] <= topByte) lo = mid;
        else hi = mid - 1;
    }
    return lo;
}

/**
 * Ask the host for the "cursor line" bar in the diff panel.
 *
 * The panel buffer is created with show_cursors=true so the editor moves
 * the cursor natively, but a single-line bg bar on the cursor row gives a
 * much more visible "you are here" indicator than the bare caret — which
 * matches the magit-style aesthetic and is what the user expects.
 *
 * Declared once, not repainted: the host re-derives the bar's row from the
 * cursor of the frame it is drawing. Painting it here from `cursor_moved`
 * instead left it one row behind for as long as an arrow key repeated —
 * the hook only fires after the frame that already moved the caret, so
 * every repaint answered the previous frame's cursor.
 */
function declareCursorLineBar(): void {
    const bufId = state.panelBuffers["diff"];
    if (bufId === undefined) return;
    editor.setCursorLineOverlay(bufId, {
        bg: STYLE_SELECTED_BG,
        extendToLineEnd: true,
    });
}

function review_refresh() {
    // Synchronously acknowledge the keypress before kicking off the
    // async `git status` + `git diff`. Those calls take long enough on
    // a non-trivial repo that, without this immediate status update,
    // the sticky-header totals visibly lag the new content: users
    // press `r`, see the old `+N / -M`, conclude the keystroke was
    // dropped, and press `r` again — which then "appears" to work
    // because the first refresh has by then landed. See #2036.
    //
    // In range mode the refresh is intentionally a no-op for
    // working-tree edits (the diff is always between two refs); the
    // range-specific message explains that up front so the user
    // doesn't think `r` is broken when their unstaged changes don't
    // show up.
    if (state.mode === 'range' && state.range) {
        editor.setStatus(
            editor.t("status.refreshing_range", { range: state.range.label }) ||
                `Refreshing ${state.range.label}... (working tree not included)`
        );
    } else {
        editor.setStatus(editor.t("status.refreshing") || "Refreshing review diff...");
    }
    void refreshMagitData();
}
registerHandler("review_refresh", review_refresh);

// --- Cursor-driven navigation ---
//
// In the unified-stream layout the diff panel owns the editor's native
// cursor; j/k/Up/Down/PageUp/PageDown/Home/End delegate directly to the
// editor's built-in motion actions via `executeAction`. The plugin only
// observes `cursor_moved` events to repaint the cursor-line overlay and
// keep `state.diffCursorRow` in sync.

/**
 * Derive the "current file" (FileEntry) from the cursor row in the unified
 * diff stream — the file whose header row is the largest one ≤ the cursor
 * row. Returns null if no file header is at or above the cursor (cursor
 * sits in the empty preamble or there are no files).
 */
function currentFileFromCursor(): FileEntry | null {
    let bestFile: FileEntry | null = null;
    let bestRow = 0;
    for (const f of state.files) {
        const row = state.fileHeaderRows[fileKey(f)];
        if (row !== undefined && row <= state.diffCursorRow && row > bestRow) {
            bestRow = row;
            bestFile = f;
        }
    }
    return bestFile;
}

/** The properties of the cursor's current row, derived from the hunk
 *  layout (`propsAtRow`) — exact, unlike `editor.getTextPropertiesAtCursor`,
 *  which can return the previous row's properties when the cursor sits at
 *  a row boundary. */
function propsAtCursorRow(): Record<string, unknown> | null {
    return propsAtRow(state.diffCursorRow);
}

function sectionUnderCursor(): string | null {
    const props = propsAtCursorRow();
    if (!props || props["type"] !== 'section-header') return null;
    const filePath = props["filePath"];
    return typeof filePath === 'string' ? filePath : null;
}

/**
 * Tab dispatches to the *nearest ancestor* of the cursor's row:
 *   * Section header → toggle the section.
 *   * File header   → toggle the file.
 *   * Anywhere inside a hunk (header, body, inline comment) → toggle
 *     the hunk.
 *   * Blank line above any file header (i.e. cursor inside a file's
 *     diff before its first hunk) → toggle that file.
 *   * Cursor in the comments panel → swap focus back to the diff.
 */
function review_toggle_file_collapse() {
    if (state.groupId === null) return;
    if (state.focusPanel === 'comments') {
        editor.focusBufferGroupPanel(state.groupId, "diff");
        return;
    }
    if (state.files.length === 0) return;

    // Section header → toggle whole section.
    const section = sectionUnderCursor();
    if (section) {
        if (state.collapsedSections.has(section)) state.collapsedSections.delete(section);
        else state.collapsedSections.add(section);
        applyFolds();
        const sectionRow = state.sectionHeaderRows[section];
        if (sectionRow !== undefined) jumpDiffCursorToRow(sectionRow, { recenter: false });
        return;
    }

    // File header → toggle whole file.
    const headerFile = fileHeaderUnderCursor();
    if (headerFile) {
        const key = fileKey(headerFile);
        if (state.collapsedFiles.has(key)) state.collapsedFiles.delete(key);
        else state.collapsedFiles.add(key);
        applyFolds();
        const headerRow = state.fileHeaderRows[key];
        if (headerRow !== undefined) jumpDiffCursorToRow(headerRow, { recenter: false });
        return;
    }

    // Hunk (header / body / inline comment) → toggle that hunk.
    const hunk = getHunkAtDiffCursor();
    if (hunk) {
        if (state.collapsedHunks.has(hunk.id)) state.collapsedHunks.delete(hunk.id);
        else state.collapsedHunks.add(hunk.id);
        applyFolds();
        const hunkRow = state.hunkRowByHunkId[hunk.id];
        if (hunkRow !== undefined) jumpDiffCursorToRow(hunkRow, { recenter: false });
        return;
    }

    // Fall back to the parent file if cursor is in a no-man's-land (e.g.
    // blank separator after the last hunk of a file).
    const fallbackFile = currentFileFromCursor();
    if (!fallbackFile) return;
    const key = fileKey(fallbackFile);
    if (state.collapsedFiles.has(key)) state.collapsedFiles.delete(key);
    else state.collapsedFiles.add(key);
    applyFolds();
    const headerRow = state.fileHeaderRows[key];
    if (headerRow !== undefined) jumpDiffCursorToRow(headerRow, { recenter: false });
}
registerHandler("review_toggle_file_collapse", review_toggle_file_collapse);

/**
 * Order comments the same way the comments panel does — by file order
 * in the unified stream, then by line number. Keeping the ordering
 * here in sync with `buildCommentsPanelEntries` is important so that
 * keyboard navigation lands on the same row the user sees.
 *
 * Builds an O(F) path -> index map once per call instead of doing a
 * linear scan of state.files for every comment in the sort comparator.
 */
function commentsInPanelOrder(): ReviewComment[] {
    const fileIdx: Record<string, number> = {};
    for (let i = 0; i < state.files.length; i++) fileIdx[state.files[i].path] = i;
    return [...state.comments].sort((a, b) => {
        const fa = fileIdx[a.file] ?? Number.MAX_SAFE_INTEGER;
        const fb = fileIdx[b.file] ?? Number.MAX_SAFE_INTEGER;
        if (fa !== fb) return fa - fb;
        return (a.new_line ?? a.old_line ?? 0) - (b.new_line ?? b.old_line ?? 0);
    });
}

function selectAndJumpToComment(c: ReviewComment) {
    if (state.groupId === null) return;
    jumpToComment(c.id);
    state.commentsSelectedId = c.id;
    renderCommentsPanel();
}

/** Index of the selected comment among the comments, or -1. */
function selectedCommentIndex(): number {
    if (state.commentsSelectedId === null) return -1;
    return commentsInPanelOrder().findIndex(c => c.id === state.commentsSelectedId);
}

function review_next_comment() {
    if (state.comments.length === 0) {
        editor.setStatus(editor.t("status.no_comments") || "No comments");
        return;
    }
    const sorted = commentsInPanelOrder();
    const currentIdx = selectedCommentIndex();
    const nextIdx = Math.min(sorted.length - 1, currentIdx + 1);
    if (nextIdx === currentIdx && currentIdx >= 0) return;
    selectAndJumpToComment(sorted[nextIdx >= 0 ? nextIdx : 0]);
}
registerHandler("review_next_comment", review_next_comment);

function review_prev_comment() {
    if (state.comments.length === 0) {
        editor.setStatus(editor.t("status.no_comments") || "No comments");
        return;
    }
    const sorted = commentsInPanelOrder();
    const cur = selectedCommentIndex();
    const prevIdx = Math.max(0, (cur < 0 ? sorted.length : cur) - 1);
    selectAndJumpToComment(sorted[prevIdx]);
}
registerHandler("review_prev_comment", review_prev_comment);

/**
 * Focus the comments panel. Uses native focus-swap so the buffer's
 * native cursor takes the keystrokes (j/k/Enter handled by the
 * comments-mode keybindings).
 */
function review_focus_comments() {
    if (state.groupId === null) return;
    // Asking for the comments panel is asking to see it.
    setReviewPanelVisible('comments', true);
    reviewSetFocus('comments');
    // Ensure the selection highlight shows immediately.
    if (state.commentsSelectedId === null && state.comments.length > 0) {
        state.commentsSelectedId = commentsInPanelOrder()[0].id;
    }
    renderCommentsPanel();
}
registerHandler("review_focus_comments", review_focus_comments);

/** The Tab/BackTab focus ring: file list → diff → comments → (wrap).
 *  A hidden panel is not in the ring — there is nothing on screen to move
 *  a cursor into. Comments join it only when there are comments to step
 *  through. */
function reviewFocusOrder(): Array<'files' | 'diff' | 'comments'> {
    const order: Array<'files' | 'diff' | 'comments'> = [];
    if (panelVisible('files')) order.push('files');
    order.push('diff');
    if (panelVisible('comments') && state.comments.length > 0) order.push('comments');
    return order;
}

/** Move keyboard focus to `panel` (native focus so mouse-click focus and
 *  Tab focus share one source of truth) and refresh the focus markers. */
function reviewSetFocus(panel: 'files' | 'diff' | 'comments'): void {
    if (state.groupId === null) return;
    editor.focusBufferGroupPanel(state.groupId, panel);
    // Set eagerly too: the buffer_activated event that also sets this is
    // async, but a key handler firing immediately after must see the new
    // focus to route the next arrow correctly.
    state.focusPanel = panel;
    // A freshly focused sidebar needs a selected row for its keys to act
    // on, and the row scrolled into view.
    if (panel === 'files') {
        selectedSidebarFile();
        renderFilesPanel();
        if (filesPanel !== null) {
            filesPanel.setFocusKey(filterEditing ? FILES_FILTER_KEY : FILES_TREE_KEY);
        }
    }
    refreshFocusIndicators();
}

/** Focus the diff panel and make sure the composite's focused pane is `p`
 *  (0 = OLD, 1 = NEW). The host only exposes a pane *toggle*, so with two
 *  panes one InsertTab flips between them; `state.compositePane` tracks it. */
function ensureCompositePane(p: 0 | 1): void {
    if (!state.centerComposite) return;
    if (state.focusPanel !== 'diff') reviewSetFocus('diff');
    if (state.compositePane !== p) {
        editor.executeAction("insert_tab"); // composite_focus_next (OLD<->NEW)
        state.compositePane = p;
    }
    refreshFocusIndicators();
}

function review_focus_next(): void {
    if (state.groupId === null) return;
    // Side-by-side: step OLD -> NEW within the diff before leaving it.
    if (state.focusPanel === 'diff' && state.centerComposite && state.compositePane === 0) {
        ensureCompositePane(1);
        return;
    }
    const order = reviewFocusOrder();
    let i = order.indexOf(state.focusPanel);
    if (i < 0) i = 0;
    const next = order[(i + 1) % order.length];
    reviewSetFocus(next);
    // Entering the diff from the file list lands on OLD (the left pane).
    if (next === 'diff' && state.centerComposite) ensureCompositePane(0);
}
registerHandler("review_focus_next", review_focus_next);

function review_focus_prev(): void {
    if (state.groupId === null) return;
    // Side-by-side: step NEW -> OLD within the diff before leaving it.
    if (state.focusPanel === 'diff' && state.centerComposite && state.compositePane === 1) {
        ensureCompositePane(0);
        return;
    }
    const order = reviewFocusOrder();
    let i = order.indexOf(state.focusPanel);
    if (i < 0) i = 0;
    const prev = order[(i - 1 + order.length) % order.length];
    reviewSetFocus(prev);
    // Entering the diff from the right (comments/files-wrap) lands on NEW.
    if (prev === 'diff' && state.centerComposite) ensureCompositePane(1);
}
registerHandler("review_focus_prev", review_focus_prev);

/**
 * Activate the currently-selected comment in the comments panel:
 * jump the diff cursor to it (auto-expanding the file if collapsed).
 */
function review_open_selected_comment() {
    if (state.commentsSelectedId === null) return;
    jumpToComment(state.commentsSelectedId);
}
registerHandler("review_open_selected_comment", review_open_selected_comment);

function review_comments_select_next() {
    if (state.groupId === null || state.comments.length === 0) return;
    const sorted = commentsInPanelOrder();
    const next = Math.min(sorted.length - 1, selectedCommentIndex() + 1);
    state.commentsSelectedId = sorted[Math.max(0, next)].id;
    renderCommentsPanel();
}
registerHandler("review_comments_select_next", review_comments_select_next);

function review_enter_dispatch() {
    if (state.focusPanel === 'comments') {
        review_open_selected_comment();
        return;
    }
    // FILES: Enter is "take me there". The selection already moved the
    // diff to that file, so Enter hands focus to it — the same place Tab
    // would put you, without the keystroke reaching the diff buffer (where
    // it used to land on the file header and fold it). On a directory row
    // it goes to the tree, which folds or unfolds it.
    if (state.focusPanel === 'files') {
        if (filesSelectedNodeKey.startsWith("file:")) reviewSetFocus('diff');
        else filesKey("Enter");
        return;
    }
    // Side-by-side center: Enter opens the file version under the cursor
    // (read-only HEAD on the OLD pane, working file on the NEW pane). Only
    // when the diff panel actually holds focus — otherwise the composite
    // isn't the active buffer and the cursor lookup is meaningless.
    if (state.centerComposite && state.focusPanel === 'diff') {
        void review_center_open_at_cursor();
        return;
    }
    const props = propsAtCursorRow();
    if (!props) return;
    const t = props["type"];
    // On a file or section header, Enter doubles as Tab: toggle the
    // header's collapse state. Matches the intuition that a header is a
    // disclosure widget — pressing the primary key on it should expand
    // or fold the thing it owns, not drill down.
    if (t === 'file-header' || t === 'section-header') {
        review_toggle_file_collapse();
        return;
    }
    // Inside a file's diff content, drill down to side-by-side view.
    // Blank separators and comment rows are quietly ignored to avoid
    // drilling into whatever file the cursor happens to be adjacent to.
    if (t === 'add' || t === 'remove' || t === 'context' || t === 'hunk-header') {
        review_drill_down();
    }
}
registerHandler("review_enter_dispatch", review_enter_dispatch);

/**
 * Alt+O in the unified review-diff stream: open the editable working-tree
 * file at the line under the cursor. Mirrors the side-by-side Alt+O so the
 * shortcut is uniform across both review surfaces.
 *
 * The working-tree line is the row's `newLine` (added / context rows). For
 * a pure-removed row (`newLine` undefined) we scan forward to the next row
 * that carries a `newLine`, landing the cursor where the deletion happened.
 */
function review_open_working_file() {
    if (state.groupId === null) return;
    const file = currentFileFromCursor();
    if (!file) return;
    // Untracked files have no on-disk-vs-HEAD distinction, but the working
    // file still exists — open it. Deleted files have no working copy.
    if (file.status === 'D') {
        editor.setStatus(editor.t("status.file_deleted_no_open") || "File was deleted — no working copy to open");
        return;
    }
    const r = streamRowAt(state.diffCursorRow);
    let line: number | undefined;
    if (r !== null && r.kind === 'line') {
        line = lineNumbersAt(r.sh.hunk, r.lineIdx).newLine;
        if (line === undefined) {
            // Pure-removed row: the deletion happened where the next row
            // the working file still has sits — in this hunk, or in a
            // later hunk of the same file.
            const shs = state.streamHunks;
            let li = r.lineIdx + 1;
            for (let i = streamHunkIndexAtRow(state.diffCursorRow);
                line === undefined && i < shs.length && shs[i].hunk.raw === r.sh.hunk.raw;
                i++, li = 0) {
                const h = shs[i].hunk;
                for (; li < h.lines.length; li++) {
                    const c = h.lines[li][0];
                    if (c === ' ' || c === '+') { line = lineNumbersAt(h, li).newLine; break; }
                }
            }
        }
    }
    const absPath = state.repo ? editor.pathJoin(state.repo.root, file.path) : file.path;
    editor.openFile(absPath, line ?? 1, 1);
}
registerHandler("review_open_working_file", review_open_working_file);

function review_comments_select_prev() {
    if (state.groupId === null || state.comments.length === 0) return;
    const sorted = commentsInPanelOrder();
    const cur = selectedCommentIndex();
    state.commentsSelectedId = sorted[Math.max(0, (cur < 0 ? sorted.length : cur) - 1)].id;
    renderCommentsPanel();
}
registerHandler("review_comments_select_prev", review_comments_select_prev);

/** Home / End in the COMMENTS rail: first / last comment. */
function review_comments_select_first() {
    if (state.groupId === null || state.comments.length === 0) return;
    state.commentsSelectedId = commentsInPanelOrder()[0].id;
    renderCommentsPanel();
}

function review_comments_select_last() {
    if (state.groupId === null || state.comments.length === 0) return;
    const sorted = commentsInPanelOrder();
    state.commentsSelectedId = sorted[sorted.length - 1].id;
    renderCommentsPanel();
}

/**
 * Visual line-selection mode. Activates a multi-row selection rooted
 * at the cursor's hunk; j/k extend it; Esc cancels. The selection is
 * rendered as an inverted background overlay across the selected rows.
 */
function review_visual_start() {
    if (state.groupId === null) return;
    const props = propsAtCursorRow();
    if (!props) return;
    const hunkId = props["hunkId"];
    const lineType = props["lineType"];
    if (typeof hunkId !== 'string' || (lineType !== 'add' && lineType !== 'remove' && lineType !== 'context')) {
        editor.setStatus(editor.t("status.visual_no_diff_line") || "Visual selection requires a diff line");
        return;
    }
    state.lineSelection = {
        startRow: state.diffCursorRow,
        endRow: state.diffCursorRow,
        hunkId,
    };
    paintLineSelectionOverlay();
    editor.setStatus(editor.t("status.visual_started") || "Visual: j/k extend, s/u/d apply, Esc cancel");
}
registerHandler("review_visual_start", review_visual_start);

function review_visual_cancel() {
    state.lineSelection = null;
    if (state.groupId !== null) {
        const diffId = state.panelBuffers["diff"];
        if (diffId !== undefined) editor.clearNamespace(diffId, "review-line-selection");
    }
}
registerHandler("review_visual_cancel", review_visual_cancel);

const LINE_SELECTION_NS = "review-line-selection";

function paintLineSelectionOverlay() {
    if (state.groupId === null) return;
    const diffId = state.panelBuffers["diff"];
    if (diffId === undefined) return;
    editor.clearNamespace(diffId, LINE_SELECTION_NS);
    if (!state.lineSelection) return;
    const { startRow, endRow } = state.lineSelection;
    const lo = Math.min(startRow, endRow);
    const hi = Math.max(startRow, endRow);
    for (let r = lo; r <= hi; r++) {
        const idx = r - 1;
        if (idx < 0 || idx + 1 >= state.diffLineByteOffsets.length) continue;
        const start = state.diffLineByteOffsets[idx];
        const end = state.diffLineByteOffsets[idx + 1];
        if (end <= start) continue;
        editor.addOverlay(diffId, LINE_SELECTION_NS, start, end, {
            bg: STYLE_SELECTED_BG,
            extendToLineEnd: true,
        });
    }
}

/**
 * Translate the active line-selection's (startRow, endRow) into a
 * lineRange (inclusive 0-indexed indices into `hunk.lines`) by walking
 * the rows of the unified stream that belong to the selection's hunk.
 *
 * Returns `null` if the selection crosses out of its hunk (which can't
 * happen given how j/k extend, but defensively guarded), or the hunk
 * can't be found, or the selection contains only context lines (which
 * makes stage/unstage a no-op).
 */
function selectionLineRange(): { hunk: Hunk; range: { start: number; end: number } } | null {
    if (!state.lineSelection) return null;
    const sel = state.lineSelection;
    const hunk = state.hunks.find(h => h.id === sel.hunkId);
    if (!hunk) return null;
    // Row of this hunk's header in the unified stream. `hunkRowByHunkId` is
    // populated for exactly the hunks emitted in the current render, keyed by
    // id — so it stays correct regardless of focus mode (which paints only the
    // focused file's body), collapsed files, or staged/unstaged section order.
    // The previous approach counted hunks across *all* files to index
    // `hunkHeaderRows`, which overshot whenever the focused hunk wasn't the
    // first one rendered (e.g. a line in the second file), yielding a null
    // range and the spurious "no add/remove lines" error.
    const sh = state.streamHunkById.get(hunk.id);
    if (sh === undefined) return null;

    const lo = Math.min(sel.startRow, sel.endRow);
    const hi = Math.max(sel.startRow, sel.endRow);
    // A selection edge on a note box takes the line next to it, inward.
    const lineAt = (row: number, roundDown: boolean): number | null => {
        if (row <= sh.headerRow || row >= sh.headerRow + sh.rowCount) return null;
        const body = bodyRowOf(sh, row - sh.headerRow - 1);
        if ('lineIdx' in body) return body.lineIdx;
        return roundDown ? body.note.afterLine : body.note.afterLine + 1;
    };
    const startInHunk = lineAt(lo, false);
    const endInHunk = lineAt(hi, true);
    if (startInHunk === null || endInHunk === null) return null;
    if (startInHunk > endInHunk || endInHunk >= hunk.lines.length) return null;

    // Reject context-only selections.
    let hasChange = false;
    for (let i = startInHunk; i <= endInHunk; i++) {
        const ch = hunk.lines[i][0];
        if (ch === '+' || ch === '-') { hasChange = true; break; }
    }
    if (!hasChange) return null;

    return { hunk, range: { start: startInHunk, end: endInHunk } };
}

async function applyLineSelection(action: 'stage' | 'unstage' | 'discard') {
    const sel = selectionLineRange();
    if (!sel) {
        editor.setStatus(editor.t("status.visual_invalid") || "Selection has no add/remove lines or crosses hunk boundary");
        return;
    }
    const { hunk, range } = sel;
    const patch = buildHunkPatch(hunk.file, hunk, range);
    let flags: string[];
    if (action === 'stage') flags = ["--cached", "--unidiff-zero"];
    else if (action === 'unstage') flags = ["--cached", "--reverse", "--unidiff-zero"];
    else flags = ["--reverse", "--unidiff-zero"];

    rememberPendingHunkAnchor(hunk.id);
    const ok = await applyHunkPatch(patch, flags);
    if (!ok) return;
    review_visual_cancel();
    // Past tense per action — don't synthesize it by appending `d`, which
    // works for "stage"/"unstage" but yields "discardd" for "discard" and
    // leaks the untranslated i18n key into the status bar (#2420).
    const pastTense: Record<typeof action, string> = {
        stage: "staged",
        unstage: "unstaged",
        discard: "discarded",
    };
    const past = pastTense[action];
    await refreshMagitData();
    setReviewConfirmation(editor.t(`status.lines_${past}`) || `Lines ${past}`);
}

function review_collapse_all() {
    // Remember which file the cursor is in so we can land on its
    // header row after every file collapses.
    const cur = currentFileFromCursor();
    state.collapsedFiles = new Set(state.files.map(fileKey));
    applyFolds();
    if (cur) {
        const headerRow = state.fileHeaderRows[fileKey(cur)];
        if (headerRow !== undefined) jumpDiffCursorToRow(headerRow);
    }
}
registerHandler("review_collapse_all", review_collapse_all);

function review_expand_all() {
    // Same intuition for unfold-all: keep the cursor on the file it was
    // in (rows shift as collapsed files/hunks re-emit their content).
    const cur = currentFileFromCursor();
    state.collapsedFiles.clear();
    state.collapsedSections.clear();
    state.collapsedHunks.clear();
    applyFolds();
    if (cur) {
        const headerRow = state.fileHeaderRows[fileKey(cur)];
        if (headerRow !== undefined) jumpDiffCursorToRow(headerRow);
    }
}
registerHandler("review_expand_all", review_expand_all);

// The diff panel's own mode binds ↑/↓ straight to the built-in motions
// (see `DIFF_NATIVE_MOTION`), so these two run only for a keystroke that
// arrived while a side panel held focus. The line-selection follow-up that
// used to live here now hangs off `cursor_moved`, which sees every motion
// including the native ones.
function review_nav_up() {
    if (state.focusPanel === 'comments') { review_comments_select_prev(); return; }
    if (state.focusPanel === 'files') { filesKey("Up"); return; }
    editor.executeAction("move_up");
}
registerHandler("review_nav_up", review_nav_up);

function review_nav_down() {
    if (state.focusPanel === 'comments') { review_comments_select_next(); return; }
    if (state.focusPanel === 'files') { filesKey("Down"); return; }
    editor.executeAction("move_down");
}
registerHandler("review_nav_down", review_nav_down);

/** Left / Right belong to whichever panel has focus. In the diff they pan
 *  the (unwrapped) stream horizontally; in the FILES sidebar the host's
 *  tree folds and unfolds with them — a sidebar keystroke must never
 *  reach through and scroll the diff behind it. */
function review_nav_left() {
    if (state.focusPanel === 'files') { filesKey("Left"); return; }
    if (state.focusPanel === 'comments') return;
    editor.executeAction("move_left");
}
registerHandler("review_nav_left", review_nav_left);

function review_nav_right() {
    if (state.focusPanel === 'files') { filesKey("Right"); return; }
    if (state.focusPanel === 'comments') return;
    editor.executeAction("move_right");
}
registerHandler("review_nav_right", review_nav_right);

/** Hand a keystroke to the FILES panel's focused widget. The host's
 *  smart-key dispatch does the rest: Up/Down move the tree's selection,
 *  Left/Right fold and unfold, PageUp/PageDown page it, Enter activates —
 *  and it scrolls the selection into view itself. */
function filesKey(name: string): void {
    if (filesPanel === null) return;
    filesPanel.command(key(name));
}

/** The file the sidebar points at, falling back to the first visible one.
 *  In the expanded stream the selection tracks the diff cursor, which sits
 *  on a section header at startup — with no fallback the sidebar's own keys
 *  would have nothing to act on until you scrolled into a file. */
function selectedSidebarFile(): FileEntry | null {
    const current = state.files.find(f => fileKey(f) === state.filesCurrentKey);
    if (current) return current;
    const vis = visibleFiles();
    if (vis.length === 0) return null;
    state.filesCurrentKey = fileKey(vis[0]);
    return vis[0];
}

/** Home / End in a side panel jump to its first / last row; in the diff
 *  they keep the editor's start-of-line / end-of-line meaning. */
function review_nav_home() {
    if (state.focusPanel === 'files') { filesKey("Home"); return; }
    if (state.focusPanel === 'comments') { review_comments_select_first(); return; }
    editor.executeAction("move_line_start");
}
registerHandler("review_nav_home", review_nav_home);

function review_nav_end() {
    if (state.focusPanel === 'files') { filesKey("End"); return; }
    if (state.focusPanel === 'comments') { review_comments_select_last(); return; }
    editor.executeAction("move_line_end");
}
registerHandler("review_nav_end", review_nav_end);

function review_page_up() {
    if (state.focusPanel === 'comments') { review_comments_select_prev(); return; }
    if (state.focusPanel === 'files') { filesKey("PageUp"); return; }
    editor.executeAction("move_page_up");
}
registerHandler("review_page_up", review_page_up);

function review_page_down() {
    if (state.focusPanel === 'comments') { review_comments_select_next(); return; }
    if (state.focusPanel === 'files') { filesKey("PageDown"); return; }
    editor.executeAction("move_page_down");
}
registerHandler("review_page_down", review_page_down);
// Home / End intentionally NOT overridden — the editor's native
// "move to start/end of line" is exactly what we want here. Mapping
// them to move_document_start/end (as the old layout did when Home/
// End served as files-pane shortcuts) made them useless on a unified
// stream.

// --- Real git stage/unstage/discard actions (Step 4) ---

/**
 * Build a minimal unified diff patch for a single hunk.
 *
 * When `lineRange` is provided, only the +/- lines whose indices fall
 * inside the inclusive range are kept; +/- lines outside the range are
 * converted to context lines so that the patch still applies cleanly
 * to the file. Context lines are always preserved.
 */
function buildHunkPatch(filePath: string, hunk: Hunk, lineRange?: { start: number; end: number }): string {
    const filtered: string[] = [];
    let oldCount = 0;
    let newCount = 0;
    // Whether the preceding diff line produced output. A trailing
    // "\ No newline at end of file" marker annotates the line just above
    // it, so it is only meaningful when that line was kept.
    let lastEmitted = false;

    for (let i = 0; i < hunk.lines.length; i++) {
        const line = hunk.lines[i];
        const ch = line[0];
        if (ch === '\\') {
            // "\ No newline at end of file": travels with its annotated
            // line and never counts toward the @@ line totals.
            if (lastEmitted) filtered.push(line);
            continue;
        }
        const inRange = !lineRange || (i >= lineRange.start && i <= lineRange.end);
        if (ch === '+') {
            if (inRange) {
                filtered.push(line);
                newCount++;
                lastEmitted = true;
            } else {
                // An out-of-range '+' line means: this addition isn't being
                // applied, so it shouldn't appear in either side. Drop it
                // entirely (don't convert to context — there's nothing to
                // match in the source file).
                lastEmitted = false;
            }
        } else if (ch === '-') {
            if (inRange) {
                filtered.push(line);
                oldCount++;
            } else {
                // An out-of-range '-' line: this deletion isn't applied,
                // so the line still exists on both sides — render as context.
                filtered.push(' ' + line.substring(1));
                oldCount++;
                newCount++;
            }
            lastEmitted = true;
        } else {
            filtered.push(line);
            oldCount++;
            newCount++;
            lastEmitted = true;
        }
    }

    const header = `@@ -${hunk.oldRange.start},${oldCount} +${hunk.range.start},${newCount} @@`;
    return [
        `diff --git a/${filePath} b/${filePath}`,
        `--- a/${filePath}`,
        `+++ b/${filePath}`,
        header,
        ...filtered,
        ''
    ].join('\n');
}

/**
 * Write a patch to a temp file and apply it with the given flags.
 * Returns true on success.
 */
async function applyHunkPatch(patch: string, flags: string[]): Promise<boolean> {
    const tmpDir = editor.getTempDir();
    const patchPath = editor.pathJoin(tmpDir, `fresh-review-${Date.now()}.patch`);
    editor.writeFile(editor.localPath(patchPath), patch);
    const cwd = gitCwd();
    // Validate first
    const check = await editor.spawnProcess("git", ["apply", "--check", ...flags, patchPath], cwd);
    if (check.exit_code !== 0) {
        editor.setStatus("Patch failed: " + (check.stderr || "").trim());
        return false;
    }
    const result = await editor.spawnProcess("git", ["apply", ...flags, patchPath], cwd);
    return result.exit_code === 0;
}

/**
 * Get the hunk under the cursor in the diff panel, or null.
 *
 * Reads the row's `hunkId` (see `propsAtRow`). Falls
 * back to the first hunk of the selected file when the cursor is somewhere
 * without a hunkId (e.g. the panel header) so commands like `s` still do
 * something useful.
 */
/** The hunk under the cursor, working in both the text and composite center.
 *  Async because the composite cursor is queried asynchronously. */
async function getHunkAtCursor(): Promise<Hunk | null> {
    const cc = state.centerComposite;
    if (cc) {
        const info = await getCompositeLineInfo();
        if (info) {
            const h = state.hunks.find(x => x.id === info.hunkId);
            if (h) return h;
        }
        // Fallback: the focused file's first hunk.
        const file = state.files.find(f => fileKey(f) === cc.fileKey);
        if (file) {
            return hunksForFile(file)[0] || null;
        }
        return null;
    }
    return getHunkAtDiffCursor();
}

function getHunkAtDiffCursor(): Hunk | null {
    const props = propsAtCursorRow();
    const hunkId = props ? props["hunkId"] : undefined;
    if (typeof hunkId === 'string') {
        const found = state.hunks.find(h => h.id === hunkId);
        if (found) return found;
    }
    // Fallback: first hunk for the file under the cursor (if any).
    const cur = currentFileFromCursor();
    if (!cur) return null;
    return hunksForFile(cur)[0] || null;
}

/**
 * Determine if the cursor is on a file-header row. Returns the FileEntry
 * if so, otherwise null.
 *
 * Looks up by `fileKey` (path + category) — looking up by `path` alone
 * is wrong when the same file appears in both Staged and Unstaged: the
 * `state.files.find(... === path)` would always return the first
 * matching entry (typically the staged one), so Tab on the unstaged
 * file header would silently act on the staged file instead.
 */
function fileHeaderUnderCursor(): FileEntry | null {
    const props = propsAtCursorRow();
    if (!props || props["type"] !== 'file-header') return null;
    const key = props["fileKey"];
    if (typeof key !== 'string') return null;
    return state.files.find(f => fileKey(f) === key) || null;
}

/**
 * Stage at the appropriate scope based on cursor context:
 *   * file header  → stage the whole file
 *   * hunk         → stage just that hunk
 */
async function review_stage_scope() {
    if (state.files.length === 0) return;
    if (state.lineSelection) { await applyLineSelection('stage'); return; }
    if (state.centerComposite) { await stageHunk(await getHunkAtCursor()); return; }
    const headerFile = fileHeaderUnderCursor();
    if (headerFile) {
        await stageFileEntry(headerFile);
        return;
    }
    await stageHunk(getHunkAtDiffCursor());
}
registerHandler("review_stage_scope", review_stage_scope);

async function review_unstage_scope() {
    if (state.files.length === 0) return;
    if (state.lineSelection) { await applyLineSelection('unstage'); return; }
    if (state.centerComposite) { await unstageHunk(await getHunkAtCursor()); return; }
    const headerFile = fileHeaderUnderCursor();
    if (headerFile) {
        await unstageFileEntry(headerFile);
        return;
    }
    await unstageHunk(getHunkAtDiffCursor());
}
registerHandler("review_unstage_scope", review_unstage_scope);

/**
 * Always-file-level staging (S / U). Acts on the file the cursor is
 * currently inside, regardless of whether it's on a header or a hunk.
 */
async function review_stage_file() {
    if (state.files.length === 0) return;
    const f = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!f) return;
    await stageFileEntry(f);
}
registerHandler("review_stage_file", review_stage_file);

async function review_unstage_file() {
    if (state.files.length === 0) return;
    const f = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!f) return;
    await unstageFileEntry(f);
}
registerHandler("review_unstage_file", review_unstage_file);

async function stageFileEntry(f: FileEntry) {
    rememberPendingHunkAnchor(null);
    const cwd = gitCwd();
    await editor.spawnProcess("git", ["add", "--", f.path], cwd);
    await refreshMagitData();
}

async function unstageFileEntry(f: FileEntry) {
    rememberPendingHunkAnchor(null);
    const cwd = gitCwd();
    await editor.spawnProcess("git", ["reset", "HEAD", "--", f.path], cwd);
    await refreshMagitData();
}

async function stageHunk(hunk: Hunk | null) {
    if (!hunk || !hunk.file) return;
    rememberPendingHunkAnchor(hunk.id);
    if (hunk.gitStatus === 'untracked') {
        const cwd = gitCwd();
        await editor.spawnProcess("git", ["add", "--", hunk.file], cwd);
    } else {
        const patch = buildHunkPatch(hunk.file, hunk);
        const ok = await applyHunkPatch(patch, ["--cached"]);
        if (!ok) return;
    }
    await refreshMagitData();
    setReviewConfirmation(editor.t("status.hunk_staged") || "Hunk staged");
}

async function unstageHunk(hunk: Hunk | null) {
    if (!hunk || !hunk.file || hunk.gitStatus !== 'staged') {
        editor.setStatus("Can only unstage staged hunks");
        return;
    }
    rememberPendingHunkAnchor(hunk.id);
    const patch = buildHunkPatch(hunk.file, hunk);
    const ok = await applyHunkPatch(patch, ["--cached", "--reverse"]);
    if (!ok) return;
    await refreshMagitData();
    setReviewConfirmation(editor.t("status.hunk_unstaged") || "Hunk unstaged");
}

/**
 * Cursor continuity: remember the hunk-id we just acted on so that
 * after the rebuild we can land the cursor back on the same hunk
 * (which may have moved between sections), or on the nearest survivor.
 */
let pendingHunkAnchor: { hunkId: string | null; section: string | null; row: number } | null = null;
function rememberPendingHunkAnchor(hunkId: string | null) {
    const cur = getHunkAtDiffCursor();
    pendingHunkAnchor = {
        hunkId,
        section: cur?.gitStatus ?? null,
        row: state.diffCursorRow,
    };
}

let pendingDiscardFile: FileEntry | null = null;

/**
 * Every path a file-level discard has to put back: the file itself, plus
 * the pre-rename path when git reports the change as a rename (`R`). A
 * staged rename is one change spread over two pathspecs, and restoring
 * only the new one leaves the old one deleted in the index.
 */
function discardPathsOf(f: FileEntry): string[] {
    return f.origPath && f.origPath !== f.path ? [f.path, f.origPath] : [f.path];
}

/** First line of a git/`rm` stderr, for a one-line status message. */
function firstErrorLine(raw: string): string {
    const line = (raw || "").split("\n").map(s => s.trim()).find(s => s.length > 0);
    return line ?? "";
}

/**
 * Does this file still have changes git knows about? Asked *after* a
 * discard, of git rather than of our own state, because the whole point
 * of #2318 is that the plugin used to announce a destructive action it
 * had not performed.
 */
async function fileIsClean(paths: string[]): Promise<boolean | null> {
    const res = await editor.spawnProcess(
        "git",
        ["--no-optional-locks", "status", "--porcelain", "-z", "-uall", "--", ...paths],
        gitCwd(),
    );
    if (res.exit_code !== 0) return null;
    return res.stdout.split('\0').every(entry => entry.trim() === '');
}

/**
 * File-level discard: put the file back to HEAD — index *and* working
 * tree — and report only what actually happened.
 *
 * The old implementation ran `git checkout -- <path>`, which rewrites the
 * working tree *from the index*. For a fully-staged change the two already
 * agree, so it was a no-op — and the caller announced `Discarded: <file>`
 * regardless (#2318). `git restore --source=HEAD --staged --worktree`
 * covers every shape the panel can show: a plain modification, a staged
 * add (which `git checkout HEAD -- <path>` cannot touch, the path not
 * being in HEAD at all), a staged delete, and a rename's two paths.
 *
 * The result is then verified against `git status` rather than against the
 * command's exit code alone, so a discard that silently changes nothing
 * can never again be reported as success.
 */
async function discardFileToHead(f: FileEntry): Promise<{ ok: boolean; detail: string }> {
    const cwd = gitCwd();
    const paths = discardPathsOf(f);

    if (f.category === 'untracked') {
        const rm = await editor.spawnProcess("rm", ["--", ...paths], cwd);
        if (rm.exit_code !== 0) return { ok: false, detail: firstErrorLine(rm.stderr) };
    } else {
        const res = await editor.spawnProcess(
            "git",
            ["restore", "--source=HEAD", "--staged", "--worktree", "--", ...paths],
            cwd,
        );
        if (res.exit_code !== 0) return { ok: false, detail: firstErrorLine(res.stderr) };
    }

    const clean = await fileIsClean(paths);
    if (clean === null) return { ok: false, detail: tr("status.discard_unverified") ?? "could not verify" };
    if (!clean) return { ok: false, detail: tr("status.discard_still_changed") ?? "the change is still there" };
    return { ok: true, detail: "" };
}

/**
 * The prompt for `D`.
 *
 * The wording ("Discard changes in <file>", "Permanently lose changes",
 * "This cannot be undone") describes a discard of the *file*, and that is
 * now what the action does — index and working tree both go back to HEAD.
 * When the file also has staged content the description spells that out,
 * because a file can be listed under UNSTAGED while carrying a staged
 * change the user would not expect this key to touch.
 */
function startDiscardFilePrompt(f: FileEntry): void {
    pendingDiscardFile = f;
    // A file the repo has never had — untracked, or added to the index —
    // is not restored by this, it is removed. `git restore --source=HEAD`
    // on a path HEAD does not carry deletes it, so the prompt has to say
    // "Delete": "discard changes in" describes an edit surviving as a
    // file, which is not what happens.
    //
    // Asked of the *path*, not of the row under the cursor. `git status`
    // reports an added-then-edited file (`AM`) as two entries, and the
    // unstaged one carries `M` — so a row-local test called `D` on it a
    // discard of changes and then deleted the file, which is the same
    // dialog/action mismatch #2318 is about, one row over.
    const removesFile = f.category === 'untracked'
        || state.files.some(o => o.path === f.path && o.category === 'staged'
            && (o.status === 'A' || o.status === 'C'));
    const action = removesFile ? "Delete" : "Discard changes in";
    // `git status` reports a file changed on both sides as two entries, so
    // the same path can sit under STAGED and under UNSTAGED. Whichever row
    // the cursor is on, the discard takes both — so the warning has to name
    // the *other* row, the one the user is not looking at. (Comparing
    // against `f.category` rather than a fixed 'staged' also stops the
    // staged row matching itself and warning about what the user just
    // asked for.)
    const collateral = f.category === 'untracked'
        ? null
        : state.files.find(o => o.path === f.path && o.category !== f.category
            && o.category !== 'untracked')?.category ?? null;
    const description = collateral === 'staged'
        ? (tr("prompt.discard_file_scope") ?? "Discards the staged changes too — back to HEAD")
        : collateral === 'unstaged'
            ? (tr("prompt.discard_file_scope_unstaged")
                ?? "Discards the unstaged changes too — back to HEAD")
            : (tr("prompt.discard_file_lose") ?? "Permanently lose changes");
    editor.startPrompt(`${action} "${f.path}"? This cannot be undone.`, "review-discard-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: `${action} file`, description, value: "discard" },
        { text: "Cancel", description: "Keep the file as-is", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
}

/** Always-file-level discard (D). Acts on the file the cursor is in. */
function review_discard_file_only() {
    if (state.files.length === 0) return;
    const f = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!f) return;
    rememberPendingHunkAnchor(null);
    startDiscardFilePrompt(f);
}
registerHandler("review_discard_file_only", review_discard_file_only);

function review_discard_file() {
    if (state.files.length === 0) return;
    if (state.lineSelection) { void applyLineSelection('discard'); return; }
    const headerFile = fileHeaderUnderCursor();
    const f = headerFile ?? currentFileFromCursor();
    if (!headerFile) {
        // No file-header under cursor → hunk-level discard
        const hunk = getHunkAtDiffCursor();
        if (!hunk || !hunk.file) return;
        rememberPendingHunkAnchor(hunk.id);
        editor.startPrompt(
            editor.t("prompt.discard_hunk", { file: hunk.file }) ||
            `Discard this hunk in "${hunk.file}"? This cannot be undone.`,
            "review-discard-hunk-confirm"
        );
        const suggestions: PromptSuggestion[] = [
            { text: "Discard hunk", description: "Permanently lose this change", value: "discard" },
            { text: "Cancel", description: "Keep the hunk as-is", value: "cancel" },
        ];
        editor.setPromptSuggestions(suggestions);
        return;
    }
    if (!f) return;

    // Show confirmation prompt — discard is destructive and irreversible
    rememberPendingHunkAnchor(null);
    startDiscardFilePrompt(f);
}
registerHandler("review_discard_file", review_discard_file);





/**
 * A confirmation the status bar holds until the user moves on ("Lines
 * discarded", "Hunk staged", …).
 *
 * Every stage/unstage/discard used to emit its confirmation and then `await
 * refreshMagitData()`, whose tail calls `updateReviewStatus()` — so the
 * summary overwrote the confirmation within the same burst and the user saw
 * it for at most a frame, or not at all (#2420). Holding it here instead of
 * racing the refresh means `updateReviewStatus` re-renders the confirmation
 * rather than clobbering it, and there is exactly one place that decides when
 * it expires.
 *
 * `row` anchors the confirmation to the diff row it was issued on. The cursor
 * restore inside the refresh emits its own `cursor_moved` *after* the
 * confirmation is set; that echo carries the same row, so it doesn't count as
 * the user moving on. A real navigation keystroke carries a different row and
 * clears it.
 */
let reviewConfirmation: { text: string; row: number } | null = null;

/** Emit a confirmation and hold it. Call *after* the refresh that follows the
 * action, so the rebuild's own status update can't land on top of it. */
function setReviewConfirmation(text: string): void {
    reviewConfirmation = { text, row: state.diffCursorRow };
    editor.setStatus(text);
}

/**
 * An incremental djb2 over a sequence of fields, with a separator mixed in
 * after each so `["ab","c"]` and `["a","bc"]` don't collide. Values are only
 * ever compared against other values from the same builder.
 */
function makeHasher(): { field(s: string): void; value(): string } {
    let h = 5381;
    return {
        field(s: string): void {
            for (let i = 0; i < s.length; i++) h = ((h * 33) ^ s.charCodeAt(i)) >>> 0;
            h = ((h * 33) ^ 0x1f) >>> 0;
        },
        value(): string { return String(h); },
    };
}

/**
 * A fingerprint of everything the stream draws from: which files are in
 * which section, and the exact text of every hunk line.
 *
 * The auto-refresh poll re-reads git on a timer and has to decide whether
 * anything actually moved. File paths and `+N / -M` counts are not enough
 * — an external edit can rewrite a line without changing either — so the
 * hunk bodies go into the hash as well. A hash rather than the strings
 * themselves because this is held across ticks for the lifetime of the
 * session.
 */
function worktreeDataSignature(
    files: FileEntry[],
    hunks: Hunk[],
    emptyState: EmptyStateReason,
): string {
    const h = makeHasher();
    h.field(emptyState ?? '');
    for (const f of files) {
        h.field(f.path); h.field(f.category); h.field(f.status); h.field(f.origPath ?? '');
    }
    for (const hk of hunks) {
        h.field(hk.id); h.field(hk.file); h.field(hk.gitStatus ?? '');
        for (const line of hk.lines) h.field(line);
    }
    return h.value();
}

/** Signature of the data the stream currently shows; see `worktreeDataSignature`. */
let lastDataSignature: string | null = null;

/**
 * The refresh queue.
 *
 * Every refresh re-reads git, then assigns `state.files`/`state.hunks`
 * wholesale, rebuilds, and consumes the single `pendingHunkAnchor`. Two
 * overlapping ones therefore interleave badly whichever way round they
 * start: the one that finishes last repaints from *its* snapshot, so a
 * poll that began before a stage can put the pre-stage diff back on screen
 * and record its signature as current, and the first to finish eats the
 * anchor, dropping the other's cursor at hunk 0.
 *
 * A flag that only the poll consulted could not fix that — the poll
 * yielded to a keystroke, but a keystroke never yielded to an in-flight
 * poll. So the runs are serialized instead: every caller chains onto the
 * last, and only one `refreshMagitDataInner` is ever in progress.
 * `refreshPending` counts callers rather than tracking a single run, so a
 * refresh finishing cannot clear the flag out from under another that is
 * still queued behind it.
 */
let refreshQueue: Promise<unknown> = Promise.resolve();
let refreshPending = 0;

/**
 * Refresh file list and diffs using the new git status approach, then re-render.
 *
 * `onlyIfChanged` is for the auto-refresh poll: it still re-reads git (that
 * is the only way to find out), but leaves the panel — and the reader's
 * cursor, folds and status line — completely alone when the answer is the
 * same as last time. Returns whether the display was rebuilt.
 */
async function refreshMagitData(opts?: { onlyIfChanged?: boolean }): Promise<boolean> {
    refreshPending++;
    const run = refreshQueue.then(
        () => refreshMagitDataInner(opts),
        // A predecessor that threw must not cancel the refreshes behind it.
        () => refreshMagitDataInner(opts),
    );
    refreshQueue = run.then(() => undefined, () => undefined);
    try {
        return await run;
    } finally {
        refreshPending--;
    }
}

async function refreshMagitDataInner(opts?: { onlyIfChanged?: boolean }): Promise<boolean> {
    let files: FileEntry[];
    let hunks: Hunk[];
    let emptyState: EmptyStateReason;
    if (state.mode === 'range' && state.range) {
        const range = await fetchRangeDiff(state.range);
        files = range.files;
        hunks = range.hunks;
        emptyState = null;
    } else {
        const status = await getGitStatus();
        files = status.files;
        emptyState = status.emptyReason;
        hunks = await fetchDiffsForFiles(status.files);
    }
    const signature = worktreeDataSignature(files, hunks, emptyState);
    if (opts?.onlyIfChanged && signature === lastDataSignature) return false;
    lastDataSignature = signature;

    // A rebuild supersedes whatever the last action confirmed: `r` and the
    // watch-driven refreshes should land on the summary, not on a stale
    // "Lines discarded" from several actions ago.
    reviewConfirmation = null;
    state.files = files;
    state.hunks = hunks;
    state.emptyState = emptyState;
    state.diffCursorRow = 1;
    // The hunks and files under every cached view have just been replaced.
    // A hunk-range signature can miss a file whose content changed without
    // moving a boundary, so re-reading the data is itself the invalidation.
    state.dataRevision++;
    discardParkedComposite();
    updateMagitDisplay();
    restoreCursorAfterRebuild();
    updateReviewStatus();
    return true;
}

/**
 * After a rebuild caused by stage/unstage/discard, try to land the cursor
 * back on the same hunk (now possibly in a different section), or the
 * nearest survivor in the original section, or the first hunk overall.
 */
function restoreCursorAfterRebuild() {
    const anchor = pendingHunkAnchor;
    pendingHunkAnchor = null;
    if (!anchor) return;
    if (anchor.hunkId) {
        // Find the hunk by id in the new state.
        const found = state.hunks.findIndex(h => h.id === anchor.hunkId);
        if (found >= 0) {
            // Compute its visible row (auto-expanding if needed).
            jumpToGlobalHunk(found);
            return;
        }
    }
    // Hunk vanished — fall back to the next hunk in the same section,
    // else the previous one, else the first hunk overall.
    if (anchor.section) {
        const idx = state.hunks.findIndex(h => h.gitStatus === anchor.section);
        if (idx >= 0) {
            jumpToGlobalHunk(idx);
            return;
        }
    }
    if (state.hunks.length > 0) jumpToGlobalHunk(0);
}

// --- Resize handler ---

/**
 * Refresh viewport dimensions from the actual split viewport.
 * This accounts for sidebars (file explorer) that reduce available width,
 * unlike the terminal-level resize event which reports full terminal size.
 */
function refreshViewportDimensions(): boolean {
    // `getViewport()` reports the *focused* split only. With a side panel
    // focused — which is exactly where `r` leaves you after picking a file
    // — that is the sidebar's geometry, and recording it as the review's
    // viewport shrinks everything laid out against `viewportWidth` (the
    // sticky header was being sliced to the sidebar's width). The host
    // reports each panel's real rect through `on_review_viewport_changed`,
    // so prefer the diff pane's own recorded size and fall back to the
    // focused split only before the group has been laid out once.
    const width = state.panelWidths["diff"];
    const height = state.panelHeights["diff"];
    const viewport = width && height && width > 0 && height > 0
        ? { width, height }
        : editor.getViewport();
    if (viewport) {
        const changed = viewport.width !== state.viewportWidth || viewport.height !== state.viewportHeight;
        state.viewportWidth = viewport.width;
        state.viewportHeight = viewport.height;
        return changed;
    }
    return false;
}

function onReviewDiffResize(_data: { width: number; height: number }): void {
    if (state.reviewBufferId === null) return;
    refreshViewportDimensions();
    updateMagitDisplay();
}
registerHandler("onReviewDiffResize", onReviewDiffResize);

let activeDiffViewState: { lSplit: number, rSplit: number } | null = null;

/**
 * Find line number for a given byte offset using binary search
 */
function findLineForByte(lineByteOffsets: number[], topByte: number): number {
    let low = 0;
    let high = lineByteOffsets.length - 1;
    while (low < high) {
        const mid = Math.floor((low + high + 1) / 2);
        if (lineByteOffsets[mid] <= topByte) {
            low = mid;
        } else {
            high = mid - 1;
        }
    }
    return low;
}

function on_viewport_changed(data: any) {
    // This handler is now a no-op - scroll sync is handled by the core
    // using the anchor-based ScrollSyncGroup system.
    // Keeping the handler for backward compatibility if core sync fails.
    if (!activeDiffViewState || !activeSideBySideState) return;

    // Skip if core scroll sync is active (we have a scrollSyncGroupId)
    if (activeSideBySideState.scrollSyncGroupId !== null) return;

    const { oldSplitId, newSplitId, oldLineByteOffsets, newLineByteOffsets } = activeSideBySideState;

    if (data.splitId === oldSplitId && newLineByteOffsets.length > 0) {
        // OLD pane scrolled - find which line it's on and sync NEW pane to same line
        const lineNum = findLineForByte(oldLineByteOffsets, data.top_byte);
        const targetByte = newLineByteOffsets[Math.min(lineNum, newLineByteOffsets.length - 1)];
        (editor as any).setSplitScroll(newSplitId, targetByte);
    } else if (data.splitId === newSplitId && oldLineByteOffsets.length > 0) {
        // NEW pane scrolled - find which line it's on and sync OLD pane to same line
        const lineNum = findLineForByte(newLineByteOffsets, data.top_byte);
        const targetByte = oldLineByteOffsets[Math.min(lineNum, oldLineByteOffsets.length - 1)];
        (editor as any).setSplitScroll(oldSplitId, targetByte);
    }
}
registerHandler("on_viewport_changed", on_viewport_changed);

/**
 * Represents an aligned line pair for side-by-side diff display
 */
interface AlignedLine {
    oldLine: string | null;  // null means filler line
    newLine: string | null;  // null means filler line
    oldLineNum: number | null;
    newLineNum: number | null;
    changeType: 'unchanged' | 'added' | 'removed' | 'modified';
}

/**
 * Parse git diff and compute fully aligned line pairs for side-by-side display.
 * Shows the complete files with proper alignment through all hunks.
 */
function computeFullFileAlignedDiff(oldContent: string, newContent: string, hunks: Hunk[]): AlignedLine[] {
    const oldLines = oldContent.split('\n');
    const newLines = newContent.split('\n');
    const aligned: AlignedLine[] = [];

    // Build a map of changes from all hunks for this file
    // Key: old line number (1-based), Value: { type, newLineNum, content }
    interface ChangeInfo {
        type: 'removed' | 'added' | 'modified' | 'context';
        oldContent?: string;
        newContent?: string;
        newLineNum?: number;
    }

    // Parse all hunks for this file
    const allHunkChanges: { oldStart: number, newStart: number, changes: { type: 'add' | 'remove' | 'context', content: string }[] }[] = [];
    for (const hunk of hunks) {
        const changes: { type: 'add' | 'remove' | 'context', content: string }[] = [];
        for (const line of hunk.lines) {
            if (line.startsWith('+')) {
                changes.push({ type: 'add', content: line.substring(1) });
            } else if (line.startsWith('-')) {
                changes.push({ type: 'remove', content: line.substring(1) });
            } else if (line.startsWith(' ')) {
                changes.push({ type: 'context', content: line.substring(1) });
            }
        }
        allHunkChanges.push({
            oldStart: hunk.oldRange.start,
            newStart: hunk.range.start,
            changes
        });
    }

    // Sort hunks by old line start
    allHunkChanges.sort((a, b) => a.oldStart - b.oldStart);

    // Process the file line by line
    let oldIdx = 0;  // 0-based index into oldLines
    let newIdx = 0;  // 0-based index into newLines
    let hunkIdx = 0;

    while (oldIdx < oldLines.length || newIdx < newLines.length || hunkIdx < allHunkChanges.length) {
        // Check if we're at a hunk boundary
        const currentHunk = hunkIdx < allHunkChanges.length ? allHunkChanges[hunkIdx] : null;

        if (currentHunk && oldIdx + 1 === currentHunk.oldStart) {
            // Process this hunk
            let changeIdx = 0;
            while (changeIdx < currentHunk.changes.length) {
                const change = currentHunk.changes[changeIdx];

                if (change.type === 'context') {
                    aligned.push({
                        oldLine: oldLines[oldIdx],
                        newLine: newLines[newIdx],
                        oldLineNum: oldIdx + 1,
                        newLineNum: newIdx + 1,
                        changeType: 'unchanged'
                    });
                    oldIdx++;
                    newIdx++;
                    changeIdx++;
                } else if (change.type === 'remove') {
                    // Look ahead to see if next is an 'add' (modification)
                    if (changeIdx + 1 < currentHunk.changes.length &&
                        currentHunk.changes[changeIdx + 1].type === 'add') {
                        // Modified line
                        aligned.push({
                            oldLine: oldLines[oldIdx],
                            newLine: newLines[newIdx],
                            oldLineNum: oldIdx + 1,
                            newLineNum: newIdx + 1,
                            changeType: 'modified'
                        });
                        oldIdx++;
                        newIdx++;
                        changeIdx += 2;
                    } else {
                        // Pure removal
                        aligned.push({
                            oldLine: oldLines[oldIdx],
                            newLine: null,
                            oldLineNum: oldIdx + 1,
                            newLineNum: null,
                            changeType: 'removed'
                        });
                        oldIdx++;
                        changeIdx++;
                    }
                } else if (change.type === 'add') {
                    // Pure addition
                    aligned.push({
                        oldLine: null,
                        newLine: newLines[newIdx],
                        oldLineNum: null,
                        newLineNum: newIdx + 1,
                        changeType: 'added'
                    });
                    newIdx++;
                    changeIdx++;
                }
            }
            hunkIdx++;
        } else if (oldIdx < oldLines.length && newIdx < newLines.length) {
            // Not in a hunk - add unchanged line
            aligned.push({
                oldLine: oldLines[oldIdx],
                newLine: newLines[newIdx],
                oldLineNum: oldIdx + 1,
                newLineNum: newIdx + 1,
                changeType: 'unchanged'
            });
            oldIdx++;
            newIdx++;
        } else if (oldIdx < oldLines.length) {
            // Only old lines left (shouldn't happen normally)
            aligned.push({
                oldLine: oldLines[oldIdx],
                newLine: null,
                oldLineNum: oldIdx + 1,
                newLineNum: null,
                changeType: 'removed'
            });
            oldIdx++;
        } else if (newIdx < newLines.length) {
            // Only new lines left
            aligned.push({
                oldLine: null,
                newLine: newLines[newIdx],
                oldLineNum: null,
                newLineNum: newIdx + 1,
                changeType: 'added'
            });
            newIdx++;
        } else {
            break;
        }
    }

    return aligned;
}

interface HighlightTask {
    range: [number, number];
    fg: OverlayColorSpec;
    bg?: OverlayColorSpec;
    bold?: boolean;
    italic?: boolean;
    extend_to_line_end?: boolean;
}

/**
 * Generate virtual buffer content with diff highlighting for one side.
 * Returns entries, highlight tasks, and line byte offsets for scroll sync.
 */
function generateDiffPaneContent(
    alignedLines: AlignedLine[],
    side: 'old' | 'new'
): { entries: TextPropertyEntry[], highlights: HighlightTask[], lineByteOffsets: number[] } {
    const entries: TextPropertyEntry[] = [];
    const highlights: HighlightTask[] = [];
    const lineByteOffsets: number[] = [];
    let currentByte = 0;

    for (const line of alignedLines) {
        lineByteOffsets.push(currentByte);
        const content = side === 'old' ? line.oldLine : line.newLine;
        const lineNum = side === 'old' ? line.oldLineNum : line.newLineNum;
        const isFiller = content === null;

        // Format: "│ NNN │ content" or "│     │ ~~~~~~~~" for filler
        let lineNumStr: string;
        if (lineNum !== null) {
            lineNumStr = lineNum.toString().padStart(4, ' ');
        } else {
            lineNumStr = '    ';
        }

        // Gutter marker based on change type
        let gutterMarker = ' ';
        if (line.changeType === 'added' && side === 'new') gutterMarker = '+';
        else if (line.changeType === 'removed' && side === 'old') gutterMarker = '-';
        else if (line.changeType === 'modified') gutterMarker = '~';

        let lineText: string;
        if (isFiller) {
            // Filler line for alignment
            lineText = `│${gutterMarker}${lineNumStr} │ ${"░".repeat(40)}\n`;
        } else {
            lineText = `│${gutterMarker}${lineNumStr} │ ${content}\n`;
        }

        const lineLen = getByteLength(lineText);
        const prefixLen = getByteLength(`│${gutterMarker}${lineNumStr} │ `);

        entries.push({
            text: lineText,
            properties: {
                type: 'diff-line',
                changeType: line.changeType,
                lineNum: lineNum,
                side: side
            }
        });

        // Apply colors based on change type
        // Border color
        highlights.push({ range: [currentByte, currentByte + 1], fg: STYLE_BORDER });
        highlights.push({ range: [currentByte + prefixLen - 3, currentByte + prefixLen - 1], fg: STYLE_BORDER });

        // Line number color
        highlights.push({
            range: [currentByte + 2, currentByte + 6],
            fg: "editor.line_number_fg",
        });

        if (isFiller) {
            // Filler styling - extend to full line width
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: "editor.line_number_fg",
                bg: "editor.line_number_bg",
                extend_to_line_end: true
            });
        } else if (line.changeType === 'added' && side === 'new') {
            // Added line (green) - extend to full line width
            highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_ADD_TEXT, bold: true }); // gutter marker
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: STYLE_ADD_TEXT,
                bg: STYLE_ADD_BG,
                extend_to_line_end: true
            });
        } else if (line.changeType === 'removed' && side === 'old') {
            // Removed line (red) - extend to full line width
            highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_REMOVE_TEXT, bold: true }); // gutter marker
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: STYLE_REMOVE_TEXT,
                bg: STYLE_REMOVE_BG,
                extend_to_line_end: true
            });
        } else if (line.changeType === 'modified') {
            // Modified line - show word-level diff
            const oldText = line.oldLine || '';
            const newText = line.newLine || '';
            const diffParts = diffStrings(oldText, newText);

            let offset = currentByte + prefixLen;
            if (side === 'old') {
                highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_REMOVE_TEXT, bold: true });
                // Highlight removed parts in old line
                for (const part of diffParts) {
                    const partLen = getByteLength(part.text);
                    if (part.type === 'removed') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_REMOVE_TEXT,
                            bg: STYLE_REMOVE_BG,
                            bold: true
                        });
                    } else if (part.type === 'unchanged') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_REMOVE_TEXT
                        });
                    }
                    if (part.type !== 'added') {
                        offset += partLen;
                    }
                }
            } else {
                highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_ADD_TEXT, bold: true });
                // Highlight added parts in new line
                for (const part of diffParts) {
                    const partLen = getByteLength(part.text);
                    if (part.type === 'added') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_ADD_TEXT,
                            bg: STYLE_ADD_BG,
                            bold: true
                        });
                    } else if (part.type === 'unchanged') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_ADD_TEXT
                        });
                    }
                    if (part.type !== 'removed') {
                        offset += partLen;
                    }
                }
            }
        }

        currentByte += lineLen;
    }

    return { entries, highlights, lineByteOffsets };
}

// State for active side-by-side diff view
interface SideBySideDiffState {
    oldSplitId: number;
    newSplitId: number;
    oldBufferId: number;
    newBufferId: number;
    alignedLines: AlignedLine[];
    oldLineByteOffsets: number[];
    newLineByteOffsets: number[];
    scrollSyncGroupId: number | null;  // Core scroll sync group ID
}

let activeSideBySideState: SideBySideDiffState | null = null;
let nextScrollSyncGroupId = 1;

// State for composite buffer-based diff view
interface CompositeDiffState {
    compositeBufferId: number;
    oldBufferId: number;
    newBufferId: number;
    filePath: string;        // path relative to the git root
    gitRoot: string;         // absolute git top-level dir
    absPath: string;         // absolute path of the working-tree file
    // True when there is no HEAD version of this file (untracked / added).
    // Opening the OLD (HEAD) side is then a no-op with a status message.
    isUntracked: boolean;
    // 1-indexed (old, new) line of each hunk's first line, used to map an
    // OLD-side line with no NEW counterpart (a pure deletion) onto the
    // nearest working-tree line for Alt+O. Sorted by old line ascending.
    hunkLineMap: Array<{ oldStart: number; newStart: number }>;
}

let activeCompositeDiffState: CompositeDiffState | null = null;

// =============================================================================
// Composite-center architecture
// =============================================================================
// The center "diff" panel shows the focused file as a host composite buffer
// (OLD|NEW source buffers + a hunk-derived alignment), in unified or
// side-by-side layout. Rendering is viewport-only over real buffers, so the
// center stays responsive on changesets of any size.

/** Per-line ops string for a hunk in git order (' ' context, '-' old, '+'
 *  new), so the host aligns unchanged lines identically instead of zipping
 *  positionally. */
function hunkOps(fh: Hunk): string {
    let ops = '';
    for (const line of fh.lines) {
        if (line.startsWith('-')) ops += '-';
        else if (line.startsWith('+')) ops += '+';
        else if (line.startsWith(' ')) ops += ' ';
        // '\' (no-newline) and blanks are skipped
    }
    return ops;
}

/** Split file content into display lines, dropping the single empty trailing
 *  "line" produced by a final newline. That phantom line has no ViewLine in
 *  the composite, so leaving it in logs "ViewLine missing" when scrolled into
 *  view at end-of-file. */
function contentToLines(content: string): string[] {
    const lines = content.split('\n');
    if (lines.length > 1 && lines[lines.length - 1] === '') lines.pop();
    return lines;
}

/** Build a composite source buffer's content from a file's text. Entries
 *  are spans, not lines, so the whole file is one of them: the composite
 *  reads its line numbers from the buffer itself (`getCompositeCursorInfo`)
 *  and never from per-line properties, and shipping one span instead of
 *  one per line is what a large file's side-by-side switch was paying for.
 *
 *  The trailing newline is dropped so the buffer's line count matches the
 *  number of real lines — otherwise it adds a phantom empty line with no
 *  ViewLine, which logs "ViewLine missing" when scrolled to the bottom. */
function contentToEntries(content: string): TextPropertyEntry[] {
    const text = contentToLines(content).join('\n');
    return text.length > 0 ? [{ text }] : [];
}

function compositeHunksForFile(fileHunks: readonly Hunk[]): TsCompositeHunk[] {
    return fileHunks.map(fh => {
        let oldCount = 0, newCount = 0;
        for (const line of fh.lines) {
            if (line.startsWith('-')) oldCount++;
            else if (line.startsWith('+')) newCount++;
            else if (line.startsWith(' ')) { oldCount++; newCount++; }
        }
        return {
            oldStart: Math.max(0, fh.oldRange.start - 1),
            oldCount: oldCount || 1,
            newStart: Math.max(0, fh.range.start - 1),
            newCount: newCount || 1,
            ops: hunkOps(fh),
        };
    });
}

function teardownCenterComposite(): void {
    closeComposite(state.centerComposite);
    state.centerComposite = null;
}

/** Close a composite and the two file buffers behind it. */
function closeComposite(cc: { compositeBufId: number; oldBufId: number; newBufId: number } | null): void {
    if (!cc) return;
    try {
        editor.closeCompositeBuffer(cc.compositeBufId);
        editor.closeBuffer(cc.oldBufId);
        editor.closeBuffer(cc.newBufId);
    } catch { /* already gone */ }
}

/** What a composite for `file` was built from. Two `git show` calls, two
 *  whole-file buffers and an alignment pass are worth skipping when none
 *  of this has moved — and worth redoing the moment any of it has. */
function compositeSignature(file: FileEntry): string {
    const ranges = hunksForFile(file)
        .map(h => `${h.oldRange.start}-${h.oldRange.end}:${h.range.start}-${h.range.end}`)
        .join(',');
    // The comment count is in the pane label, so it is part of what was built.
    return `${state.dataRevision}|${fileKey(file)}|${commentCountForFile(file)}|${ranges}`;
}

/** Keep the current composite alive off-screen so a flip back to
 *  side-by-side is a panel swap. Only one is parked: the reader has one
 *  place they left. */
function parkCenterComposite(): void {
    const cc = state.centerComposite;
    state.centerComposite = null;
    if (!cc) return;
    const file = state.files.find(f => fileKey(f) === cc.fileKey);
    if (!file) {
        closeComposite(cc);
        return;
    }
    if (state.parkedComposite && state.parkedComposite.compositeBufId !== cc.compositeBufId) {
        closeComposite(state.parkedComposite);
    }
    state.parkedComposite = { ...cc, signature: compositeSignature(file) };
}

/** Drop the parked composite (if any). Called when the review data is
 *  replaced or the session ends — anything holding buffers open past the
 *  thing they render is a leak. */
function discardParkedComposite(): void {
    closeComposite(state.parkedComposite);
    state.parkedComposite = null;
}

/** Read both sides of `file`. The two reads are independent, so they run
 *  together: fetching one version at a time made opening side-by-side
 *  wait out two full `git show` round trips back to back, and that pair
 *  is the largest single cost of the switch. */
async function fetchFileVersions(file: FileEntry): Promise<{ oldContent: string; newContent: string; absPath: string }> {
    const root = state.repo ? state.repo.root : (editor.getCwd() || "");
    const absPath = root ? editor.pathJoin(root, file.path) : file.path;
    const cwd = root || editor.getCwd();
    const gitShow = async (rev: string): Promise<string> => {
        const shown = await editor.spawnProcess("git", ["show", `${rev}:${file.path}`], cwd);
        return shown.exit_code === 0 ? shown.stdout : "";
    };
    if (state.mode === 'range' && state.range) {
        const [oldContent, newContent] = await Promise.all([
            gitShow(state.range.from),
            gitShow(state.range.to),
        ]);
        return { oldContent, newContent, absPath };
    }
    // Only the `git show` is a round trip; reading the working file is a
    // synchronous host call, so there is nothing to overlap it with.
    const oldContent = file.category !== 'untracked' && file.status !== 'A'
        ? await gitShow("HEAD")
        : "";
    const newContent = file.status !== 'D'
        ? (editor.readFile(editor.authorityPath(absPath)) ?? "")
        : "";
    return { oldContent, newContent, absPath };
}

/** How many review comments are anchored in `file`. Shown in the
 *  side-by-side pane labels: the composite renders two real file buffers
 *  with nowhere to put an inline comment box, so the label says the
 *  comments are there and the COMMENTS rail (opened by
 *  `review_set_layout` on the way in) carries the text. */
function commentCountForFile(file: FileEntry): number {
    let n = 0;
    for (const c of state.comments) if (c.file === file.path) n++;
    return n;
}

/** `label` with a `· N comments` suffix when the file carries any, and a
 *  pointer at the rail while the rail is closed. */
function paneLabelWithComments(label: string, count: number): string {
    if (count === 0) return label;
    const where = panelVisible('comments') ? '' : ' — see COMMENTS (C)';
    return `${label}  ·  ${count} comment${count === 1 ? '' : 's'}${where}`;
}

async function buildCenterComposite(focusHunkIdx: number = 0): Promise<void> {
    if (state.groupId === null) return;
    ensureFocusFile();
    const token = ++state.centerBuildToken;

    const key = state.filesCurrentKey;
    const file = key ? state.files.find(f => fileKey(f) === key) : undefined;
    if (!file) {
        teardownCenterComposite();
        discardParkedComposite();
        if (state.panelBuffers["diff"] !== undefined) {
            editor.setBufferGroupPanelBuffer(state.groupId, "diff", state.panelBuffers["diff"]);
            mountStreamContent();
        }
        return;
    }

    // The composite the reader left behind, still describing this file as
    // it stands: mount it instead of rebuilding it. Rebuilding costs two
    // `git show` calls, two whole files across the IPC boundary and an
    // alignment pass — seconds on a large file, every single flip.
    const parked = state.parkedComposite;
    if (parked && parked.fileKey === key && parked.signature === compositeSignature(file)) {
        state.parkedComposite = null;
        teardownCenterComposite();
        const { signature: _signature, ...composite } = parked;
        state.centerComposite = composite;
        state.compositePane = 0;
        editor.setBufferGroupPanelBuffer(state.groupId, "diff", composite.compositeBufId);
        if (reviewGroupIsActive()) {
            editor.focusBufferGroupPanel(state.groupId, "diff");
            if (state.focusPanel !== 'diff' && panelVisible(state.focusPanel)) {
                editor.focusBufferGroupPanel(state.groupId, state.focusPanel);
            }
        }
        editor.flushLayout();
        return;
    }
    // Not reusable — and only one composite is ever parked, so whatever is
    // sitting there is now dead weight.
    discardParkedComposite();

    const { oldContent, newContent, absPath } = await fetchFileVersions(file);
    if (token !== state.centerBuildToken || state.groupId === null) return;

    const fileHunks = hunksForFile(file);
    const compositeHunks = compositeHunksForFile(fileHunks);

    const oldEntries: TextPropertyEntry[] = contentToEntries(oldContent);
    const newEntries: TextPropertyEntry[] = contentToEntries(newContent);

    const oldRes = await editor.createVirtualBuffer({
        name: `*OLD:${file.path}*`, mode: "normal", readOnly: true,
        entries: oldEntries, showLineNumbers: true, editingDisabled: true, hiddenFromTabs: true,
    });
    const newRes = await editor.createVirtualBuffer({
        name: `*NEW:${file.path}*`, mode: "normal", readOnly: true,
        entries: newEntries, showLineNumbers: true, editingDisabled: true, hiddenFromTabs: true,
    });
    if (token !== state.centerBuildToken || state.groupId === null) {
        try { editor.closeBuffer(oldRes.bufferId); editor.closeBuffer(newRes.bufferId); } catch {}
        return;
    }

    const layoutCfg = state.reviewLayout === 'side-by-side'
        ? { type: "side-by-side", ratios: [0.5, 0.5], showSeparator: true }
        : { type: "unified", showSeparator: false };

    const compositeBufId = await editor.createCompositeBuffer({
        name: `*Review: ${file.path}*`,
        mode: REVIEW_DIFF_MODE,
        layout: layoutCfg as never,
        sources: [
            { bufferId: oldRes.bufferId, label: "OLD (HEAD)", editable: false, style: { gutterStyle: "diff-markers" } },
            {
                bufferId: newRes.bufferId,
                label: paneLabelWithComments("NEW (Working)", commentCountForFile(file)),
                editable: false,
                style: { gutterStyle: "diff-markers" },
            },
        ],
        hunks: compositeHunks.length > 0 ? compositeHunks : null,
        initialFocusHunk: compositeHunks.length > 0
            ? Math.max(0, Math.min(focusHunkIdx, compositeHunks.length - 1)) : undefined,
    });
    if (token !== state.centerBuildToken || state.groupId === null) {
        try {
            editor.closeCompositeBuffer(compositeBufId);
            editor.closeBuffer(oldRes.bufferId); editor.closeBuffer(newRes.bufferId);
        } catch {}
        return;
    }

    // Swap the NEW composite into the panel FIRST, then tear down the OLD.
    // Closing the old before swapping leaves the panel momentarily pointing
    // at a closed buffer — that's the empty-panel flicker and the stray
    // "[No Name]" tab (auto-created when the active panel buffer is closed).
    const prev = state.centerComposite;
    // A freshly-created composite focuses pane 0 (OLD); the hunk index tracks
    // the requested initial focus hunk so n/p continue from there.
    state.compositePane = 0;
    state.compositeHunkIdx = compositeHunks.length > 0
        ? Math.max(0, Math.min(focusHunkIdx, compositeHunks.length - 1)) : 0;
    state.centerComposite = {
        fileKey: key!,
        compositeBufId,
        oldBufId: oldRes.bufferId,
        newBufId: newRes.bufferId,
        absPath,
        isUntracked: file.category === 'untracked',
        hunkLineMap: fileHunks
            .map(fh => ({ oldStart: fh.oldRange.start, newStart: fh.range.start }))
            .sort((a, b) => a.oldStart - b.oldStart),
    };
    editor.setBufferGroupPanelBuffer(state.groupId, "diff", compositeBufId);
    // createCompositeBuffer registers the composite as the active buffer of
    // the host split; re-focus the group panel so the review group stays the
    // active tab and the sidebar/comments/toolbar remain visible — then hand
    // focus back to whichever panel the user was actually in. (Which is
    // also why no refresh the user did not ask for is allowed to reach
    // here while a composite is up — see the watch handlers.)
    if (reviewGroupIsActive()) {
        editor.focusBufferGroupPanel(state.groupId, "diff");
        if (state.focusPanel !== 'diff' && panelVisible(state.focusPanel)) {
            editor.focusBufferGroupPanel(state.groupId, state.focusPanel);
        }
    }
    if (prev) {
        try {
            editor.closeCompositeBuffer(prev.compositeBufId);
            editor.closeBuffer(prev.oldBufId);
            editor.closeBuffer(prev.newBufId);
        } catch { /* already gone */ }
    }
    editor.flushLayout();
}

/** Render the center panel for the current layout: side-by-side uses the
 *  host composite (efficient, full-file context); unified uses the focused-
 *  file text buffer (interleaved, syntax-highlighted, inline comment boxes).
 *  Comment-add / staging dispatch on `state.centerComposite` (set iff the
 *  composite is showing). */
/**
 * Is the review the thing the user is looking at right now?
 *
 * `state.focusPanel` says which of the review's own panels holds the keys
 * *within* the review; it knows nothing about whether the review's tab is
 * the active one. Claiming focus for the diff panel while the user is in
 * another buffer activates the review's tab — which is how a refresh that
 * fires on every save (the watch is on by default) yanked people out of
 * the file they had just saved. So before any refresh-driven focus call,
 * ask the host which buffer is active and only proceed if it is one of
 * ours.
 */
function reviewGroupIsActive(): boolean {
    const active = editor.getActiveBufferId();
    for (const id of Object.values(state.panelBuffers)) {
        if (id === active) return true;
    }
    const cc = state.centerComposite;
    if (cc && (active === cc.compositeBufId || active === cc.oldBufId || active === cc.newBufId)) {
        return true;
    }
    return false;
}

function renderCenter(): void {
    if (state.groupId === null) return;
    syncFocusMode();
    if (state.reviewLayout === 'side-by-side') {
        void buildCenterComposite();
        return;
    }
    // Unified (default): the focused-file plugin-text buffer.
    // Bump the build token so any in-flight side-by-side build is superseded
    // and won't swap a composite back in after we switch to unified.
    state.centerBuildToken++;
    parkCenterComposite();
    if (state.panelBuffers["diff"] !== undefined) {
        editor.setBufferGroupPanelBuffer(state.groupId, "diff", state.panelBuffers["diff"]);
        mountStreamContent();
        // Only claim focus when the diff is where focus belongs. Rebuilding
        // the centre happens for reasons that have nothing to do with focus
        // — a filter keystroke in the sidebar, a comment added, a save in
        // some other buffer that the watch noticed — and stealing it there
        // sends the next keystroke to the wrong panel, or to the wrong tab.
        if (state.focusPanel === 'diff' && reviewGroupIsActive()) {
            editor.focusBufferGroupPanel(state.groupId, "diff");
        }
    }
}

/** Put the unified stream's content into the diff panel buffer — unless
 *  the content already there was built from the same state, in which case
 *  the buffer is exactly what a rebuild would produce.
 *
 *  Laying out a large review takes a noticeable beat, and it lands as one
 *  host command *after* the panel has already swapped to the stream. The
 *  reader therefore sees the stream at its old scroll position, waits,
 *  and then watches it jump — for a rebuild that changed nothing. Flipping
 *  between the two layouts is exactly that case. */
function mountStreamContent(): void {
    if (state.groupId === null) return;
    const signature = streamSignature();
    if (state.streamMountedSignature === signature) return;
    const diffId = state.panelBuffers["diff"];
    if (diffId !== undefined && state.streamBufferPrepared !== diffId) {
        // The stream is git's own output: the host's diff grammar colours
        // it, file by file, and its diff gutter numbers the rows from the
        // hunk headers. Both are set before the first content lands.
        editor.setBufferLanguage(diffId, "review.diff");
        editor.setBufferDiffGutter(diffId, true);
        state.streamBufferPrepared = diffId;
    }
    editor.setPanelContent(state.groupId, "diff", buildStreamContent());
    state.streamMountedSignature = signature;
    // Fresh content, so the host's folds went with the old rows.
    applyFolds();
}

/** Light refresh after the focused file changes (nav / sidebar click):
 *  rebuild only the center + sidebar highlight + sticky, not the static
 *  toolbar or the file-ordered comments panel. */
function refreshFocusedFile(): void {
    if (state.groupId === null) return;
    // renderCenter() focuses the diff panel as part of swapping buffers. When
    // the user is driving file navigation from the FILES panel (arrows while
    // it holds focus), keep focus there afterwards so the next arrow keeps
    // moving the file selection instead of suddenly scrolling the diff.
    const keepFocus = state.focusPanel;
    renderCenter();
    if (state.panelBuffers["files"] !== undefined) {
        renderFilesPanel();
    }
    // Unified: scroll the newly-focused file into position and put the cursor
    // on it, so navigating files actually moves the view to that file's diff
    // (instead of leaving the cursor parked at the top while only the
    // collapsed/expanded set changes). Side-by-side handles its own initial
    // position via the composite's focus-hunk logic.
    if (state.reviewLayout !== 'side-by-side' && state.filesCurrentKey
        && state.fileHeaderRows[state.filesCurrentKey] !== undefined) {
        jumpDiffCursorToRow(state.fileHeaderRows[state.filesCurrentKey]);
    } else {
        refreshStickyHeader(0);
    }
    // Restore FILES-panel focus if that's where the user was (renderCenter
    // focuses the diff). The diff cursor still scrolled to the new file above.
    if (keepFocus === 'files' && state.panelBuffers["files"] !== undefined) {
        editor.focusBufferGroupPanel(state.groupId, "files");
        state.focusPanel = 'files';
        refreshFocusIndicators();
    }
}

async function review_drill_down() {
    // In focus mode the sidebar's selected file is authoritative (the
    // cursor may be sitting on a header row); otherwise use the file the
    // cursor is within.
    if (state.files.length === 0) return;
    let selectedFile: FileEntry | null = null;
    if (state.focusOnly && state.filesCurrentKey) {
        selectedFile = state.files.find(f => fileKey(f) === state.filesCurrentKey) ?? null;
    }
    if (!selectedFile) selectedFile = currentFileFromCursor();
    if (!selectedFile) return;

    // Create a minimal hunk-like reference for the rest of the function
    const h = { file: selectedFile.path, gitStatus: selectedFile.category };

    editor.setStatus(editor.t("status.loading_diff"));

    // Get all hunks for this file
    const fileHunks = state.hunks.filter(hunk => hunk.file === h.file);
    if (fileHunks.length === 0) return;

    // Get git root to construct absolute path
    const cwd = gitCwd();
    const gitRootResult = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"], cwd);
    if (gitRootResult.exit_code !== 0) {
        editor.setStatus(editor.t("status.not_git_repo"));
        return;
    }
    const gitRoot = gitRootResult.stdout.trim();
    const absoluteFilePath = editor.pathJoin(gitRoot, h.file);

    // Get old (HEAD) and new (working) file content
    let oldContent: string;
    const gitShow = await editor.spawnProcess("git", ["show", `HEAD:${h.file}`], cwd);
    if (gitShow.exit_code !== 0) {
        oldContent = "";
    } else {
        oldContent = gitShow.stdout;
    }

    // Read new file content (use absolute path for readFile)
    // For deleted files the path no longer exists — use empty content
    let newContent: string;
    if (selectedFile.status === 'D') {
        newContent = "";
    } else {
        const readResult = await editor.readFile(editor.authorityPath(absoluteFilePath));
        if (readResult === null) {
            editor.setStatus(editor.t("status.failed_new_version"));
            return;
        }
        newContent = readResult;
    }

    // Close any existing side-by-side views (old split-based approach)
    if (activeSideBySideState) {
        try {
            if (activeSideBySideState.scrollSyncGroupId !== null) {
                (editor as any).removeScrollSyncGroup(activeSideBySideState.scrollSyncGroupId);
            }
            editor.closeBuffer(activeSideBySideState.oldBufferId);
            editor.closeBuffer(activeSideBySideState.newBufferId);
        } catch {}
        activeSideBySideState = null;
    }

    // Close any existing composite diff view
    if (activeCompositeDiffState) {
        try {
            editor.closeCompositeBuffer(activeCompositeDiffState.compositeBufferId);
            editor.closeBuffer(activeCompositeDiffState.oldBufferId);
            editor.closeBuffer(activeCompositeDiffState.newBufferId);
        } catch {}
        activeCompositeDiffState = null;
    }

    // Create virtual buffers for old and new content
    const oldEntries: TextPropertyEntry[] = contentToEntries(oldContent);
    const newEntries: TextPropertyEntry[] = contentToEntries(newContent);

    // Create source buffers (hidden from tabs, used by composite)
    const oldResult = await editor.createVirtualBuffer({
        name: `*OLD:${h.file}*`,
        mode: "normal",
        readOnly: true,
        entries: oldEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const oldBufferId = oldResult.bufferId;

    const newResult = await editor.createVirtualBuffer({
        name: `*NEW:${h.file}*`,
        mode: "normal",
        readOnly: true,
        entries: newEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const newBufferId = newResult.bufferId;

    // Convert hunks to composite buffer format (parse counts from git diff)
    const compositeHunks: TsCompositeHunk[] = fileHunks.map(fh => {
        let oldCount = 0, newCount = 0;
        for (const line of fh.lines) {
            if (line.startsWith('-')) oldCount++;
            else if (line.startsWith('+')) newCount++;
            else if (line.startsWith(' ')) { oldCount++; newCount++; }
        }
        return {
            oldStart: Math.max(0, fh.oldRange.start - 1),
            oldCount: oldCount || 1,
            newStart: Math.max(0, fh.range.start - 1),
            newCount: newCount || 1,
            ops: hunkOps(fh)
        };
    });

    // Create composite buffer with side-by-side layout
    const compositeBufferId = await editor.createCompositeBuffer({
        name: `*Diff: ${h.file}*`,
        mode: "diff-view",
        layout: {
            type: "side-by-side",
            ratios: [0.5, 0.5],
            showSeparator: true
        },
        sources: [
            {
                bufferId: oldBufferId,
                label: "OLD (HEAD)  [Enter] open this version  [n/p] hunks  [q] close",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            },
            {
                bufferId: newBufferId,
                label: "NEW (Working)  [Enter/Alt+o] open file",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            }
        ],
        hunks: compositeHunks.length > 0 ? compositeHunks : null,
        initialFocusHunk: compositeHunks.length > 0 ? 0 : undefined
    });

    // Store state for cleanup + the Enter/Alt+O "open on disk" actions.
    activeCompositeDiffState = {
        compositeBufferId,
        oldBufferId,
        newBufferId,
        filePath: h.file,
        gitRoot,
        absPath: absoluteFilePath,
        isUntracked: selectedFile.category === 'untracked',
        hunkLineMap: fileHunks
            .map(fh => ({ oldStart: fh.oldRange.start, newStart: fh.range.start }))
            .sort((a, b) => a.oldStart - b.oldStart),
    };

    // Show the composite buffer (replaces the review diff buffer)
    editor.showBuffer(compositeBufferId);

    const addedCount = fileHunks.reduce((sum, fh) => {
        return sum + fh.lines.filter(l => l.startsWith('+')).length;
    }, 0);
    const removedCount = fileHunks.reduce((sum, fh) => {
        return sum + fh.lines.filter(l => l.startsWith('-')).length;
    }, 0);
    const modifiedCount = Math.min(addedCount, removedCount);

    editor.setStatus(editor.t("status.diff_summary", { added: String(addedCount), removed: String(removedCount), modified: String(modifiedCount) }));
}
registerHandler("review_drill_down", review_drill_down);

// --- Layout toggle: stack (unified) <-> split (side-by-side) ---
//
// `hunk`-style 1/2/0 layout keys. Stack is the unified review buffer;
// split is the per-file side-by-side composite (reusing the verified
// drill-down). Auto picks split on wide terminals, stack otherwise. The
// full multi-file split-of-the-whole-stream is future work; today split
// shows the file under the cursor, which is what the reviewer is reading.
// See docs/internal/REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md §5.1.
const AUTO_SPLIT_MIN_WIDTH = 140;

/** Where the reader is: the file and the file-line under the cursor, in
 *  whichever layout is showing. Carried across a layout switch so the
 *  other view opens on the same line rather than at the top of the file. */
interface ReviewAnchor {
    fileKey: string;
    /** Absent when the cursor was not on a diff line — a hunk header, a
     *  file header, a comment box. Those rows are where `n` and `,`/`.`
     *  leave you, so they are exactly where a layout switch is likely to
     *  happen from. */
    lineType?: 'add' | 'remove' | 'context';
    oldLine?: number;
    newLine?: number;
    /** The hunk the row belongs to, when it belongs to one. Carries a
     *  header row across the switch on its own: it is enough to open the
     *  other layout on that hunk and to find the row again coming back. */
    hunkId?: string;
}

/** The anchor for the current cursor position. Reads the composite's
 *  cursor in side-by-side and the unified stream's row properties
 *  otherwise.
 *
 *  A row that is not a diff line still anchors: pressing `n` leaves the
 *  cursor on a hunk header, and requiring a `+`/`-`/context line meant
 *  flipping the layout from there threw the reader's place away and
 *  reopened the file at the top. */
async function currentReviewAnchor(): Promise<ReviewAnchor | null> {
    const info = state.centerComposite
        ? await getCompositeLineInfo()
        : getCurrentLineInfo();
    if (info && info.lineType) {
        const file = state.files.find(f => f.path === info.file);
        if (file) {
            return {
                fileKey: fileKey(file),
                lineType: info.lineType,
                oldLine: info.oldLine,
                newLine: info.newLine,
                hunkId: info.hunkId,
            };
        }
    }
    // Not on a diff line. In the stream the row still says which hunk (or
    // at least which file) it belongs to.
    if (state.centerComposite) return null;
    const props = propsAtCursorRow();
    if (!props) return null;
    const path = typeof props["file"] === 'string' ? props["file"] as string : null;
    const file = path !== null
        ? state.files.find(f => f.path === path)
        : state.files.find(f => fileKey(f) === state.filesCurrentKey);
    if (!file) return null;
    const hunkId = typeof props["hunkId"] === 'string' ? props["hunkId"] as string : undefined;
    return { fileKey: fileKey(file), hunkId };
}

/** The hunk an anchor names, if it names one. */
function anchorHunk(anchor: ReviewAnchor): Hunk | undefined {
    return anchor.hunkId !== undefined
        ? state.hunks.find(h => h.id === anchor.hunkId)
        : undefined;
}

/** Index, within its file's hunks, of the hunk holding the anchor's line —
 *  what `buildCenterComposite` wants as its initial focus. 0 when the line
 *  sits outside every hunk. */
function anchorHunkIndex(anchor: ReviewAnchor): number {
    const file = state.files.find(f => fileKey(f) === anchor.fileKey);
    if (!file) return 0;
    const fileHunks = hunksForFile(file);
    // The hunk the anchor names outright wins: a header row has no line
    // to place inside a range.
    if (anchor.hunkId !== undefined) {
        const named = fileHunks.findIndex(h => h.id === anchor.hunkId);
        if (named >= 0) return named;
    }
    const idx = fileHunks.findIndex(h =>
        (anchor.newLine !== undefined
            && anchor.newLine >= h.range.start && anchor.newLine <= h.range.end)
        || (anchor.oldLine !== undefined
            && anchor.oldLine >= h.oldRange.start && anchor.oldLine <= h.oldRange.end)
    );
    return idx < 0 ? 0 : idx;
}

/** Put the freshly-built center on `anchor`. In unified that is the stream
 *  row carrying the same file + line; in side-by-side it is the composite
 *  row showing that line of OLD (pane 0) or NEW (pane 1). */
function restoreReviewAnchor(anchor: ReviewAnchor): void {
    if (state.reviewLayout === 'side-by-side') {
        const cc = state.centerComposite;
        if (!cc) return;
        const hunk = anchorHunk(anchor);
        // A header row names a hunk but no line: open on where that hunk
        // starts. Its NEW side exists for everything but a pure deletion.
        const pane = anchor.lineType === 'remove'
            || (anchor.lineType === undefined && hunk !== undefined && hunk.range.start === 0)
            ? 0 : 1;
        const line = anchor.lineType === 'remove'
            ? anchor.oldLine
            : anchor.lineType !== undefined
                ? anchor.newLine
                : (pane === 0 ? hunk?.oldRange.start : hunk?.range.start);
        if (line === undefined) return;
        editor.setCompositeCursorLine(cc.compositeBufId, pane, line - 1);
        return;
    }
    if (anchor.lineType === undefined) {
        // Came from a header row: its own row in the stream is the place
        // to land, not some line inside the hunk.
        const hunkRow = anchor.hunkId !== undefined
            ? state.hunkRowByHunkId[anchor.hunkId]
            : undefined;
        if (hunkRow !== undefined) {
            jumpDiffCursorToRow(hunkRow);
            return;
        }
        const fileRow = state.fileHeaderRows[anchor.fileKey];
        if (fileRow !== undefined) jumpDiffCursorToRow(fileRow);
        return;
    }
    const lineRow = rowOfAnchorLine(anchor);
    if (lineRow !== undefined) {
        jumpDiffCursorToRow(lineRow);
        return;
    }
    // The line isn't in the stream at all. Side-by-side shows the whole
    // file, so the cursor can sit a long way from any change; unified
    // only carries the hunks and their context. Land on the change
    // nearest that line — the one just above it, or the one just below
    // when there is nothing above — instead of falling back to the file
    // header, which reads as "jumped somewhere random".
    const hunkRow = nearestHunkRowToAnchor(anchor);
    if (hunkRow !== undefined) {
        jumpDiffCursorToRow(hunkRow);
        return;
    }
    const headerRow = state.fileHeaderRows[anchor.fileKey];
    if (headerRow !== undefined) jumpDiffCursorToRow(headerRow);
}

/** Row of the hunk closest to `anchor`'s line within its file: the last
 *  one ending at or before it, else the first one starting after it.
 *  `undefined` when the file has no hunks in the stream. */
function nearestHunkRowToAnchor(anchor: ReviewAnchor): number | undefined {
    const file = state.files.find(f => fileKey(f) === anchor.fileKey);
    if (!file) return undefined;
    const line = anchor.lineType === 'remove' ? anchor.oldLine : anchor.newLine;
    if (line === undefined) return undefined;
    const useOld = anchor.lineType === 'remove';
    let before: Hunk | undefined;
    let after: Hunk | undefined;
    for (const h of hunksForFile(file)) {
        const start = useOld ? h.oldRange.start : h.range.start;
        const end = useOld ? h.oldRange.end : h.range.end;
        if (end <= line) before = h;              // hunks come in file order
        else if (start >= line && !after) after = h;
    }
    const pick = before && after
        ? ((line - (useOld ? before.oldRange.end : before.range.end))
            <= ((useOld ? after.oldRange.start : after.range.start) - line) ? before : after)
        : (before ?? after);
    return pick ? state.hunkRowByHunkId[pick.id] : undefined;
}

async function review_set_layout(layout: 'unified' | 'side-by-side'): Promise<void> {
    if (state.reviewLayout !== layout) {
        // Read where the reader is *before* the center is rebuilt. On the
        // way back from a side-by-side the reader never moved in, the row
        // they left the stream on is a better answer than anything the
        // composite's cursor can say: the composite has no header rows, so
        // a hunk header becomes "the hunk's first line" on the round trip.
        const anchor = (layout === 'unified' && await composedCursorUnmoved())
            ? layoutReturn!.anchor
            : await currentReviewAnchor();
        layoutReturn = null;
        state.reviewLayout = layout;
        // Unified expands every file, side-by-side renders one — the
        // center rebuild below has to see the new mode.
        syncFocusMode();
        if (anchor) state.filesCurrentKey = anchor.fileKey;
        if (layout === 'side-by-side') {
            // Await the build (renderCenter fires it off unawaited) so the
            // composite exists before the cursor is placed on it, and open
            // it on the anchor's hunk so the exact-line move below is a
            // nudge rather than a jump.
            await buildCenterComposite(anchor ? anchorHunkIndex(anchor) : 0);
        } else {
            renderCenter();
        }
        if (anchor) restoreReviewAnchor(anchor);
        if (layout === 'side-by-side' && anchor) {
            const placed = await editor.getCompositeCursorInfo();
            if (placed) {
                layoutReturn = {
                    anchor,
                    pane: placed.focusedPane,
                    line: placed.lines[placed.focusedPane] ?? null,
                };
            }
        }
        // The sticky names the current file in side-by-side and the
        // top-of-view file in unified — either way it has just changed.
        refreshStickyHeader(state.diffViewportTopRow);
    }
    editor.setStatus(
        layout === 'side-by-side'
            ? (editor.t("status.split_view") || "Side-by-side view")
            : (editor.t("status.unified_view") || "Unified view")
    );
}
/** Where the switch into side-by-side put the composite's cursor, and the
 *  stream row it came from. Cleared as soon as it is used or superseded. */
let layoutReturn: { anchor: ReviewAnchor; pane: number; line: number | null } | null = null;

/** True when the composite's cursor is still exactly where switching into
 *  side-by-side put it — i.e. the reader looked and came back without
 *  moving, so the row they left is still the row they mean. */
async function composedCursorUnmoved(): Promise<boolean> {
    if (layoutReturn === null || state.centerComposite === null) return false;
    const info = await editor.getCompositeCursorInfo();
    if (!info) return false;
    return info.focusedPane === layoutReturn.pane
        && (info.lines[info.focusedPane] ?? null) === layoutReturn.line;
}

async function review_layout_split() { await review_set_layout('side-by-side'); }
registerHandler("review_layout_split", review_layout_split);

async function review_layout_stack() { await review_set_layout('unified'); }
registerHandler("review_layout_stack", review_layout_stack);

function review_layout_auto() {
    review_set_layout(state.viewportWidth >= AUTO_SPLIT_MIN_WIDTH ? 'side-by-side' : 'unified');
}
registerHandler("review_layout_auto", review_layout_auto);

// --- View toggle: inline review-note visibility (hunk-style `a`) ---
function review_toggle_agent_notes() {
    state.showComments = !state.showComments;
    updateMagitDisplay();
    editor.setStatus(
        state.showComments
            ? (editor.t("status.notes_shown") || "Notes shown")
            : (editor.t("status.notes_hidden") || "Notes hidden")
    );
}
registerHandler("review_toggle_agent_notes", review_toggle_agent_notes);

// --- Help overlay (hunk-style `?`) ---
// Built from English literals to match the existing toolbar hint bar,
// which is likewise non-localized. Opens a read-only buffer the user
// dismisses with `q`.
async function review_help() {
    const rows: string[] = [
        " Review Diff — keyboard reference",
        "",
        " Focus       Tab / S-Tab cycle focus: files → diff → comments",
        "             ↑ ↓        move within the focused panel (j / k too)",
        "             ← →        pan the diff / fold a directory in FILES",
        "             Home End   line ends in the diff, first / last row in a panel",
        " Navigate    n / p      next / prev hunk",
        "             , / .      prev / next file",
        "             ] / [      next / prev comment",
        "             z a / z r  fold all / unfold all (Enter folds one)",
        " Layout      1 / 2 / 0  stack (unified) / split (side-by-side) / auto",
        " Panels      F / C      show / hide the files sidebar / comments rail",
        "                        (both start hidden; ✕ in a header closes it)",
        " View        a          show / hide inline notes",
        "             /          filter files (empty to clear)",
        "             W          watch: auto-refresh on changes (on by default)",
        " Review      c          add comment        x   delete comment",
        "             s / u / d  stage / unstage / discard (hunk or file)",
        "             S / U / D  stage / unstage / discard the whole file",
        "             v          start line selection",
        " Open        Enter      side-by-side, or open the comment under cursor",
        "             Alt+o      open the working-tree file at this line",
        " Session     r          refresh        e   export        q   close",
        "",
        " Press q to close this help.",
    ];
    const entries: TextPropertyEntry[] = rows.map(r => ({
        text: r + "\n",
        properties: { type: "help" },
    }));
    const res = await editor.createVirtualBuffer({
        name: "*Review Keys*",
        mode: "review-help",
        readOnly: true,
        entries,
        editingDisabled: true,
    });
    editor.showBuffer(res.bufferId);
}
registerHandler("review_help", review_help);

// --- Focus-file selection + file navigation ---

/** True if `file` passes the active `/` filter (case-insensitive substring
 *  on the path). Empty filter matches everything. */
function fileMatchesFilter(file: FileEntry): boolean {
    if (!state.fileFilter) return true;
    return file.path.toLowerCase().includes(state.fileFilter.toLowerCase());
}

/** Files visible under the active filter, in display order. */
function visibleFiles(): FileEntry[] {
    // Flatten the shared grouping so navigation order == the rendered order.
    const out: FileEntry[] = [];
    for (const g of fileGroups()) for (const f of g.files) out.push(f);
    return out;
}

/** Nearest diff row (add/remove/context) to the cursor, or null if the
 *  current view has no commentable line. Used so `c` off a diff line lands
 *  the user on a real line instead of leaving keystrokes to execute as
 *  commands. */
function nearestDiffRow(): number | null {
    const cur = state.diffCursorRow;
    const r = streamRowAt(cur);
    if (r !== null && r.kind === 'line') return cur;
    // Off a diff line, the nearest one is at the edge of the hunk the
    // cursor is in or next to — or, on a note box, the line it follows.
    const shs = state.streamHunks;
    const i = streamHunkIndexAtRow(cur);
    const candidates: number[] = [];
    const edges = (sh: StreamHunk) => {
        if (sh.hunk.lines.length === 0) return;
        candidates.push(rowOfLine(sh, 0), rowOfLine(sh, sh.hunk.lines.length - 1));
    };
    if (r !== null && r.kind === 'note') {
        candidates.push(rowOfLine(r.sh, r.afterLine));
        if (r.afterLine + 1 < r.sh.hunk.lines.length) candidates.push(rowOfLine(r.sh, r.afterLine + 1));
    }
    if (i >= 0) edges(shs[i]);
    if (i > 0) edges(shs[i - 1]);
    if (i + 1 < shs.length) edges(shs[i + 1]);
    let best: number | null = null;
    for (const row of candidates) {
        if (best === null || Math.abs(row - cur) < Math.abs(best - cur)) best = row;
    }
    return best;
}

/** Ensure `filesCurrentKey` names a visible file (exists + passes filter). */
function ensureFocusFile() {
    if (!state.focusOnly) return;
    const valid = state.filesCurrentKey !== null
        && state.files.some(f => fileKey(f) === state.filesCurrentKey && fileMatchesFilter(f));
    if (!valid) {
        const vis = visibleFiles();
        state.filesCurrentKey = vis.length > 0 ? fileKey(vis[0]) : null;
    }
}

/** Move the focused file by `delta` (clamped) across the visible
 *  (filtered) files and rebuild the center. */
function review_goto_file(delta: number) {
    ensureFocusFile();
    const vis = visibleFiles();
    if (vis.length === 0) return;
    let idx = vis.findIndex(f => fileKey(f) === state.filesCurrentKey);
    if (idx < 0) idx = 0;
    // Clamped, not bailed: `,`/`.` still stop at the ends, and Home / End
    // can ask for "as far as it goes" with one big delta.
    const next = Math.max(0, Math.min(vis.length - 1, idx + delta));
    if (next === idx) return;
    state.filesCurrentKey = fileKey(vis[next]);
    // Light refresh: only the center + sidebar highlight + sticky change on a
    // file switch — rebuilding the toolbar/comments panels too would add
    // avoidable flicker.
    refreshFocusedFile();
}
function review_goto_next_file() { review_goto_file(1); }
function review_goto_prev_file() { review_goto_file(-1); }
registerHandler("review_goto_next_file", review_goto_next_file);
registerHandler("review_goto_prev_file", review_goto_prev_file);

// --- File filter: a field in the FILES panel -----------------------------
//
// `/` opens the sidebar with a Text widget under its header and puts the
// panel into `review-filter` mode, where every printable key is text
// rather than a review command. The host owns the field — caret,
// selection, editing — and reports each edit as a `change` event; the
// plugin only re-filters the tree. ↑↓ still walk the tree while the field
// holds focus (the host forwards them), so you can type and pick without
// leaving the panel.

/** True while the filter field is open. */
let filterEditing = false;
/** Caret byte offset inside the field, mirrored from the host. */
let filterCursor = 0;
/** The filter as it was when the field opened, so Esc can put it back. */
let filterBeforeEdit = "";

const REVIEW_FILTER_MODE = "review-filter";

/** Put the panel into text-entry mode: while the filter field holds
 *  focus its single-key commands (`s`, `c`, `n`, …) are letters, not
 *  commands, so the FILES buffer switches to a mode that says so. */
function enterFilterMode(): void {
    if (filterEditing) return;
    filterEditing = true;
    filterBeforeEdit = state.fileFilter;
    const filesBuf = state.panelBuffers["files"];
    if (filesBuf !== undefined) editor.setBufferMode(filesBuf, REVIEW_FILTER_MODE);
}

/** Focus has left the field — the panel's command keys are back. */
function leaveFilterMode(): void {
    if (!filterEditing) return;
    filterEditing = false;
    const filesBuf = state.panelBuffers["files"];
    if (filesBuf !== undefined) editor.setBufferMode(filesBuf, "review-mode");
}

function review_filter_files() {
    if (state.groupId === null) return;
    setReviewPanelVisible('files', true);
    filterCursor = getByteLength(state.fileFilter);
    enterFilterMode();
    reviewSetFocus('files');
    if (filesPanel !== null) filesPanel.setFocusKey(FILES_FILTER_KEY);
}
registerHandler("review_filter_files", review_filter_files);

/** Milliseconds of quiet before a typed filter is actually applied.
 *
 *  Long enough that a burst of typing costs one rebuild instead of one
 *  per character, short enough to feel immediate after the last key. */
const FILTER_DEBOUNCE_MS = 90;

let filterApplyTimer: number | null = null;

/** Apply the current filter, coalescing bursts of typing into one pass.
 *
 *  `applyFileFilter` rebuilds the whole unified diff — the stream renders
 *  only matching files, so the centre genuinely changes — and on a large
 *  review that is far too much to do between keystrokes. The field itself
 *  stays responsive regardless: the host owns its text and echoes each
 *  character immediately, so debouncing only defers the tree and centre.
 *
 *  Every keystroke cancels the pending pass, so intermediate queries are
 *  never rendered at all — the abandoned work is dropped rather than
 *  raced. Only the final query is applied. */
function scheduleFileFilter(): void {
    if (filterApplyTimer !== null) editor.clearInterval(filterApplyTimer);
    filterApplyTimer = editor.setTimeout(FILTER_DEBOUNCE_MS, "review_apply_file_filter");
}

/** Timer target for `scheduleFileFilter`. */
function review_apply_file_filter(): void {
    filterApplyTimer = null;
    if (state.groupId === null) return;
    applyFileFilter();
}
registerHandler("review_apply_file_filter", review_apply_file_filter);

/** Run a debounced filter pass now, if one is pending.
 *
 *  Anything that depends on the filter having been applied — closing the
 *  field, navigating off it — flushes first so it never observes a tree
 *  built from a stale query. */
function flushPendingFileFilter(): void {
    if (filterApplyTimer === null) return;
    editor.clearInterval(filterApplyTimer);
    filterApplyTimer = null;
    applyFileFilter();
}

/** Re-filter after an edit: the tree, the centre (the stream renders only
 *  matching files) and the status line. */
function applyFileFilter(): void {
    // The stream renders only matching files, so the filter changes it.
    markStreamDirty();
    ensureFocusFile();
    renderFilesPanel();
    renderCenter();
    const vis = visibleFiles().length;
    editor.setStatus(
        state.fileFilter
            ? (editor.t("status.filter_active", { n: String(vis), q: state.fileFilter })
                || `Filter "${state.fileFilter}" — ${vis} file(s)`)
            : (editor.t("status.filter_cleared") || "Filter cleared")
    );
}

/** Close the field. `revert` puts the query back to what it was when the
 *  field opened (Esc); otherwise the typed query stands (Enter). */
function closeFileFilter(revert: boolean): void {
    if (!filterEditing) return;
    if (revert && state.fileFilter !== filterBeforeEdit) {
        // Drop any pass still owed to the abandoned query — it would rebuild
        // to the query being reverted away from, and then land *after* this
        // one.
        if (filterApplyTimer !== null) {
            editor.clearInterval(filterApplyTimer);
            filterApplyTimer = null;
        }
        state.fileFilter = filterBeforeEdit;
        filterCursor = filterBeforeEdit.length;
        applyFileFilter();
        // The host owns the field's text, and a plain re-render carries the
        // widget's own value forward — so the restored query has to be
        // pushed back explicitly, or Esc leaves the box reading the
        // abandoned text over a tree that already reverted.
        filesPanel?.setValue(FILES_FILTER_KEY, filterBeforeEdit, filterCursor);
    }
    // Enter: the typed query stands, so anything still owed has to land
    // before the field closes — otherwise the tree behind it is one
    // keystroke stale until the timer happens to fire.
    flushPendingFileFilter();
    leaveFilterMode();
    // Focus lands on the tree — you filtered to pick a file.
    if (filesPanel !== null) filesPanel.setFocusKey(FILES_TREE_KEY);
    renderFilesPanel();
}

function review_filter_accept() { closeFileFilter(false); }
registerHandler("review_filter_accept", review_filter_accept);

function review_filter_cancel() { closeFileFilter(true); }
registerHandler("review_filter_cancel", review_filter_cancel);

/** Printable keys while the field is open. The host applies them to the
 *  focused Text widget and reports the new value back as a `change`. */
function review_filter_text_input(args: { text: string }): void {
    if (!filterEditing || filesPanel === null || !args?.text) return;
    filesPanel.command(textInputChar(args.text));
}
// The host dispatches unbound printable keys in an `allowTextInput` mode
// as the `mode_text_input` action, qualified by the mode name so it
// reaches the plugin that defined the mode — here, only while
// `REVIEW_FILTER_MODE` is active. The `filterEditing` guard covers the
// unqualified legacy dispatch, which other plugins also answer to.
registerHandler("mode_text_input", review_filter_text_input);

/** Editing keys (Backspace, arrows, …) and the tree-walking keys, both
 *  handed to the host's smart-key dispatch for the focused field. */
function review_filter_key(name: string): void {
    if (filesPanel === null) return;
    filesPanel.command(name === "Backspace" || name === "Delete"
        ? textInputKey(name)
        : key(name));
}
registerHandler("review_filter_backspace", () => review_filter_key("Backspace"));
registerHandler("review_filter_delete", () => review_filter_key("Delete"));
/** ↑ / ↓ from the field step into the results: focus moves to the tree
 *  (which puts the panel back in command mode) and the key walks it. */
function review_filter_step(name: "Up" | "Down"): void {
    if (filesPanel === null) return;
    filesPanel.setFocusKey(FILES_TREE_KEY);
    leaveFilterMode();
    filesPanel.command(key(name));
}
registerHandler("review_filter_up", () => review_filter_step("Up"));
registerHandler("review_filter_down", () => review_filter_step("Down"));
registerHandler("review_filter_left", () => review_filter_key("Left"));
registerHandler("review_filter_right", () => review_filter_key("Right"));

editor.defineMode(REVIEW_FILTER_MODE, [
    ["Esc", "review_filter_cancel"],
    ["Enter", "review_filter_accept"],
    ["Backspace", "review_filter_backspace"],
    ["Delete", "review_filter_delete"],
    ["Up", "review_filter_up"],
    ["Down", "review_filter_down"],
    ["Left", "review_filter_left"],
    ["Right", "review_filter_right"],
    ["Tab", "review_filter_accept"],
], true, true, false);

// --- Watch / auto-refresh (on by default, toggled with `W`) ---
//
// A review of the working tree is a view of state nothing in the editor
// owns: the change under review can come from a save in Fresh, from a
// `git` command, or from an agent editing files in another terminal
// (#3126). So the panel watches on two channels, both gated by the same
// `W` toggle:
//
//   * The editor's own `after_file_save` / `after_file_revert` events —
//     immediate (debounced by a generation counter), free, and covering
//     the common review-while-editing loop.
//   * A `git status` poll on a timer, for everything that happens outside
//     Fresh. There is no cheaper signal: a filesystem watch over the
//     worktree would have to be recursive over the whole repo and would
//     still need the same `git` calls to say what changed. The tick only
//     re-renders when the data actually differs (see `refreshMagitData`'s
//     `onlyIfChanged`), so the steady-state cost of a quiet repo is the
//     three `git` invocations a manual `r` would run, once per interval.
//
// It starts on, because a stale diff is not a neutral default — it is a
// panel quietly disagreeing with the repository. `W` turns both channels
// off for anyone who would rather not pay for the poll.
let reviewWatchEnabled = true;
let reviewWatchGen = 0;
const WATCH_DEBOUNCE_MS = 200;

/** How often an open worktree review re-reads git looking for outside changes. */
const WATCH_POLL_MS = 2000;

/**
 * The share of wall-clock time the watch is allowed to spend inside `git`.
 *
 * A tick is not a fixed cost. `fetchDiffsForFiles` batches the staged and
 * unstaged diffs into one process each, but an untracked file gets its own
 * `git diff --no-index` — and `git status -uall` lists every untracked file
 * individually, so an un-ignored `node_modules` or `target/` turns one tick
 * into hundreds of sequential processes. At a fixed period, once a tick
 * outlasts its interval the next starts the moment it ends and the editor
 * sits in a permanent spawn loop, with nothing on screen saying why.
 *
 * So the next tick is scheduled from the last one's measured duration
 * rather than from a constant: a tick that took `d` waits at least
 * `d * WATCH_POLL_DUTY_DIVISOR`. A normal repo (a few milliseconds a tick)
 * keeps the 2s cadence; a pathological one degrades to refreshing rarely
 * instead of to occupying the machine.
 */
const WATCH_POLL_DUTY_DIVISOR = 4;
/** Never back off past this, however slow the repo is. */
const WATCH_POLL_MAX_MS = 60_000;

let watchPollTimer: number | null = null;
/** A tick can outlast its period on a big repo; ticks are not queued. */
let watchPollInFlight = false;

function review_toggle_watch() {
    reviewWatchEnabled = !reviewWatchEnabled;
    if (reviewWatchEnabled) startWatchPoll(); else stopWatchPoll();
    editor.setStatus(
        reviewWatchEnabled
            ? (editor.t("status.watch_on") || "Watching for changes")
            : (editor.t("status.watch_off") || "Watch off")
    );
}
registerHandler("review_toggle_watch", review_toggle_watch);

/**
 * One poll tick: re-read git, and rebuild only if what came back differs
 * from what the panel is showing.
 *
 * Skipped while the reader is in the middle of something a rebuild would
 * yank out from under them — a line selection, the filter field, or the
 * discard confirmation.
 */
async function review_watch_poll(): Promise<void> {
    watchPollTimer = null;
    // Whatever this tick decides to do — including deciding to do nothing —
    // it owes the next one a slot, or the watch stops for good.
    const startedAt = Date.now();
    const rearm = (): void => {
        if (!reviewWatchEnabled || state.groupId === null || state.mode === 'range') return;
        const spent = Math.max(0, Date.now() - startedAt);
        scheduleWatchPoll(Math.min(
            WATCH_POLL_MAX_MS,
            Math.max(WATCH_POLL_MS, spent * WATCH_POLL_DUTY_DIVISOR),
        ));
    };
    try {
        await review_watch_tick();
    } finally {
        rearm();
    }
}
registerHandler("review_watch_poll", review_watch_poll);

async function review_watch_tick(): Promise<void> {
    if (!reviewWatchEnabled || state.groupId === null) return;
    // Range reviews are ref-to-ref; the working tree doesn't affect them.
    if (state.mode === 'range') return;
    if (watchPollInFlight || refreshPending > 0) return;
    // Anything the reader is in the middle of that a rebuild would yank out
    // from under them: a line selection, the filter field, an open comment,
    // or the discard confirmation.
    if (state.lineSelection !== null || filterEditing
        || pendingDiscardFile !== null || pendingCommentInfo !== null) return;
    // A composite is the strongest form of "in the middle of something".
    // A rebuild bumps `dataRevision`, which `compositeSignature` embeds, so
    // the composite cannot be reused: it is torn down and rebuilt from two
    // `git show` calls and dropped back at its first hunk — losing the
    // reader's place in a file that a change elsewhere never touched.
    // `review_relayout_diff` already refuses to run here for this reason;
    // a tick nobody asked for has even less claim. The next refresh the
    // reader *does* ask for (`r`, a stage, leaving the composite) picks the
    // change up.
    if (state.centerComposite !== null) return;
    watchPollInFlight = true;
    try {
        // Keep the reader where they were: the rebuild's cursor restore
        // uses this anchor, so an external change three files away does
        // not scroll the panel back to the top.
        //
        // Only when there *is* a hunk under the cursor. An anchor with no
        // hunk id and no section — which is what every header, summary and
        // filler row produces — falls through `restoreCursorAfterRebuild`
        // to `jumpToGlobalHunk(0)`, so parking on a file header and letting
        // an agent touch any file would scroll the reader to the top of the
        // review. No anchor means no jump.
        const atCursor = getHunkAtDiffCursor();
        if (atCursor) rememberPendingHunkAnchor(atCursor.id);
        const rebuilt = await refreshMagitData({ onlyIfChanged: true });
        if (!rebuilt) pendingHunkAnchor = null;
    } finally {
        watchPollInFlight = false;
    }
}

function startWatchPoll(): void {
    if (watchPollTimer !== null || !reviewWatchEnabled) return;
    if (state.groupId === null || state.mode === 'range') return;
    scheduleWatchPoll(WATCH_POLL_MS);
}

/** Arm the next tick. One-shot, re-armed by the tick itself, so the delay
 *  can answer to what the last one cost. */
function scheduleWatchPoll(delayMs: number): void {
    if (watchPollTimer !== null) editor.clearInterval(watchPollTimer);
    watchPollTimer = editor.setTimeout(delayMs, "review_watch_poll");
}

function stopWatchPoll(): void {
    if (watchPollTimer === null) return;
    editor.clearInterval(watchPollTimer);
    watchPollTimer = null;
}

// A save and an external reset (auto-revert reload, e.g. `git checkout
// <ref> -- <file>` in another terminal) both change the working tree the
// review diffs against, so they share one refresh handler. This is the
// low-latency channel; the poll above is the one that catches everything
// that never passes through the editor at all.
for (const event of ["after_file_save", "after_file_revert"] as const) {
    editor.on(event, () => {
        if (!reviewWatchEnabled || state.groupId === null) return true;
        // Range reviews are ref-to-ref; working-tree changes don't affect them.
        if (state.mode === 'range') return true;
        const myGen = ++reviewWatchGen;
        void editor.delay(WATCH_DEBOUNCE_MS).then(() => {
            // Superseded by a later change, or the review closed / watch
            // turned off while we waited.
            if (myGen !== reviewWatchGen || !reviewWatchEnabled || state.groupId === null) return;
            // A composite is rebuilt from scratch by a refresh, and the host
            // makes the new one the active buffer — so a save in another
            // tab would land the user back in the review. Same stand-down
            // as the poll; `r` or leaving the composite picks the change up.
            if (state.centerComposite !== null) return;
            void refreshMagitData({ onlyIfChanged: true });
        });
        return true;
    });
}

// --- Hunk navigation for side-by-side diff view ---

/**
 * Move the diff panel's native cursor to the given 1-indexed row.
 *
 * `options.recenter` controls whether the viewport is re-centered on the
 * target row. The default is `true` for user-initiated navigation (next
 * hunk, jump-to-comment, jump-to-file) — there the caller wants the
 * target to land at a predictable position in the viewport. Callers
 * that merely re-anchor the cursor to a nearby header (e.g. after a
 * collapse/expand toggle) should pass `recenter: false` so the viewport
 * stays put; `setBufferCursor` still runs `ensure_cursor_visible`, so
 * the cursor is scrolled into view only when it would otherwise move
 * off-screen. Without this opt-out every fold toggle re-centers the
 * cursor's row at ~1/3 from the top of the viewport, which makes the
 * diff jump around whenever the user is reading anywhere else.
 */
function jumpDiffCursorToRow(row: number, options?: { recenter?: boolean }): void {
    // In side-by-side the unified diff text buffer isn't mounted (the
    // composite occupies the diff panel), so moving its cursor is a no-op
    // that logs a "no splits found" warning. Skip it.
    if (state.centerComposite) return;
    const diffId = state.panelBuffers["diff"];
    if (diffId === undefined) return;
    const idx = row - 1;
    if (idx < 0 || idx >= state.diffLineByteOffsets.length) return;

    const byteOffset = state.diffLineByteOffsets[idx];
    editor.setBufferCursor(diffId, byteOffset);
    if (options?.recenter !== false) {
        editor.scrollBufferToLine(diffId, idx);
    }
    state.diffCursorRow = row;
    // The scroll this jump triggers reports back through
    // `viewport_changed`, which is what re-derives the sticky header.
    refreshStickyHeader(state.diffViewportTopRow);
    updateReviewStatus();
}

/**
 * Compute the 1-indexed global hunk number that corresponds to the current
 * diff-panel cursor row. Returns null when no hunk is "current".
 */
function currentGlobalHunkIndex(): number | null {
    if (state.hunkHeaderRows.length === 0) return null;
    let within = -1;
    for (let i = 0; i < state.hunkHeaderRows.length; i++) {
        if (state.hunkHeaderRows[i] <= state.diffCursorRow) within = i;
        else break;
    }
    if (within < 0) return null;
    return within + 1;
}

/**
 * Refresh the status-bar summary for review-diff mode. Shows "Hunk N of M"
 * when a current hunk is known, falls back to the bare hunk count otherwise.
 */
function updateReviewStatus(): void {
    if (state.groupId === null) return;
    // A held confirmation outranks the summary until the user moves off the
    // row it was issued on — see `reviewConfirmation`.
    if (reviewConfirmation !== null) {
        editor.setStatus(reviewConfirmation.text);
        return;
    }
    const total = state.hunkHeaderRows.length;
    const current = currentGlobalHunkIndex();
    // Range reviews fundamentally don't include working-tree edits; the
    // suffix makes that visible from the status bar at all times rather
    // than only flashing past during a refresh. Without it users hit `r`,
    // see their unsaved changes don't appear, and conclude the refresh is
    // broken (#2036).
    const rangeNote = state.mode === 'range' && state.range
        ? ` · ${editor.t("status.working_tree_not_included") || "working tree not included"}`
        : '';
    if (current !== null) {
        editor.setStatus(editor.t("status.review_summary_indexed", {
            current: String(current),
            count: String(total),
        }) + rangeNote);
    } else {
        editor.setStatus(editor.t("status.review_summary", { count: String(total) }) + rangeNote);
    }
}

/**
 * Find the global index in `state.hunks` of the hunk the cursor is on or
 * below. Reads the build's own row map rather than counting rendered
 * hunks: a collapsed file still emits its hunk headers, so the Nth
 * rendered hunk is not the Nth hunk of `state.hunks`.
 * Returns -1 if no rendered hunk is at or before the cursor.
 */
function visibleHunkIndexAtCursor(): number {
    let best = -1;
    let bestRow = 0;
    for (let i = 0; i < state.hunks.length; i++) {
        const row = state.hunkRowByHunkId[state.hunks[i].id];
        if (row !== undefined && row <= state.diffCursorRow && row >= bestRow) {
            bestRow = row;
            best = i;
        }
    }
    return best;
}

function jumpToGlobalHunk(globalIdx: number) {
    if (globalIdx < 0 || globalIdx >= state.hunks.length) return;
    const target = state.hunks[globalIdx];
    const targetFileKey = fileKeyOf(target.file, target.gitStatus || 'unstaged');
    // Always expand any collapse on the target's section / file / hunk so
    // n/p never silently lands on an invisible row. Collapsing is a host
    // fold over rows the stream already carries (see `applyFolds`), so
    // revealing them is a fold change — re-laying-out the stream for it
    // put a second of work behind every `n` on a large review, most of
    // them for a target that was not collapsed in the first place.
    let revealed = false;
    if (target.gitStatus) revealed = state.collapsedSections.delete(target.gitStatus) || revealed;
    revealed = state.collapsedFiles.delete(targetFileKey) || revealed;
    revealed = state.collapsedHunks.delete(target.id) || revealed;
    if (!fileBodyRendered(targetFileKey)) {
        // Side-by-side: the composite draws one file, so the target has to
        // become the current file before its hunk row exists. This is how
        // `n`/`p` cross file boundaries there.
        state.filesCurrentKey = targetFileKey;
        refreshFocusedFile();
    } else if (revealed) {
        applyFolds();
    }
    // Look up the target hunk's row directly — much simpler than counting.
    const row = state.hunkRowByHunkId[target.id];
    if (row !== undefined) jumpDiffCursorToRow(row);
}

/** Global index into `state.hunks` of the hunk at/before the cursor.
 *  In focus-only mode only the focused file's hunks are rendered, so this
 *  works off that file's hunks and falls back to "just before its first
 *  hunk" when the cursor sits above them or the file is collapsed — so the
 *  next `n` lands on the focused file's first hunk. Returns -1 when there
 *  are no hunks at all. */
function currentHunkIndexForNav(): number {
    if (state.hunks.length === 0) return -1;
    if (!state.focusOnly) return visibleHunkIndexAtCursor();
    // Indices of the focused file's hunks, in global order.
    const focusIdxs: number[] = [];
    for (let i = 0; i < state.hunks.length; i++) {
        const h = state.hunks[i];
        if (fileKeyOf(h.file, h.gitStatus || 'unstaged') === state.filesCurrentKey) focusIdxs.push(i);
    }
    if (focusIdxs.length === 0) return -1;
    // The focused file's hunk whose rendered row is the largest <= cursor.
    let best = -1;
    for (const gi of focusIdxs) {
        const row = state.hunkRowByHunkId[state.hunks[gi].id];
        if (row !== undefined && row <= state.diffCursorRow) best = gi;
    }
    return best >= 0 ? best : focusIdxs[0] - 1;
}

function review_next_hunk() {
    if (state.groupId === null) return;
    // Side-by-side: the active diff buffer is the composite. Navigate within
    // it, advancing to the next file when the focused file has no more hunks.
    if (state.centerComposite) {
        enqueueCompositeNav(1);
        return;
    }
    // Walk the global hunk list (focus-aware), auto-expanding/refocusing the
    // target file as needed — so `n` crosses file boundaries and reveals a
    // collapsed file's hunks rather than no-op'ing.
    if (state.hunks.length === 0) return;
    const cur = currentHunkIndexForNav();
    const next = cur + 1;
    if (next >= state.hunks.length) return;
    jumpToGlobalHunk(next);
}
registerHandler("review_next_hunk", review_next_hunk);

function review_prev_hunk() {
    if (state.groupId === null) return;
    if (state.centerComposite) {
        enqueueCompositeNav(-1);
        return;
    }
    if (state.hunks.length === 0) return;
    const cur = currentHunkIndexForNav();
    if (cur > 0) jumpToGlobalHunk(cur - 1);
}
registerHandler("review_prev_hunk", review_prev_hunk);

// --- Open the real file from the side-by-side diff view ---
//
// Two entry points, both keyed off the composite cursor:
//   * Enter — side-aware. On the NEW (working) pane it opens the editable
//     on-disk file at that line. On the OLD (HEAD) pane it opens that
//     historical version read-only at the old line ("jump to THAT version").
//   * Alt+O — uniform. Always opens the editable working-tree file at the
//     corresponding line, regardless of which pane the cursor is on.
//
// The composite's panes are always [OLD (HEAD), NEW (working)], so pane
// index 0 is the historical side and the last pane is the working side.

interface CompositeCursor {
    focusedPane: number;
    paneCount: number;
    lines: Array<number | null>;  // 0-indexed source line per pane (null = blank side)
}

/** Map an OLD-side line (1-indexed) to the nearest working-tree line using
 *  the hunk offsets, for the case where a pure deletion has no NEW line. */
function mapOldLineToWorking(oldLine: number, st: CompositeDiffState): number {
    let delta = 0;
    for (const h of st.hunkLineMap) {
        if (h.oldStart <= oldLine) delta = h.newStart - h.oldStart;
        else break;
    }
    return Math.max(1, oldLine + delta);
}

/** Resolve the working-tree line (1-indexed) the cursor maps to, preferring
 *  the NEW pane's line and falling back to mapping the OLD pane's line. */
function workingLineFromCursor(info: CompositeCursor, st: CompositeDiffState): number | null {
    const newPane = info.paneCount - 1;  // working side is the last pane
    const newLine0 = info.lines[newPane];
    if (newLine0 !== null && newLine0 !== undefined) return newLine0 + 1;
    const oldLine0 = info.lines[0];
    if (oldLine0 !== null && oldLine0 !== undefined) return mapOldLineToWorking(oldLine0 + 1, st);
    return null;
}

/** Open the editable working-tree file at the cursor's mapped line. */
async function openWorkingFileAtCursor(info: CompositeCursor, st: CompositeDiffState): Promise<void> {
    const line = workingLineFromCursor(info, st);
    if (line === null) {
        editor.setStatus(editor.t("status.open_no_line") || "No corresponding line on disk");
        return;
    }
    editor.openFile(st.absPath, line, 1);
}

/** Open the HEAD version of the file read-only, at the given 1-indexed line. */
async function openHeadVersionReadOnly(st: CompositeDiffState, oldLine: number): Promise<void> {
    if (st.isUntracked) {
        editor.setStatus(editor.t("status.no_head_version") || "No HEAD version (file is untracked)");
        return;
    }
    const gitShow = await editor.spawnProcess("git", ["-C", st.gitRoot, "show", `HEAD:${st.filePath}`]);
    if (gitShow.exit_code !== 0) {
        editor.setStatus(editor.t("status.no_head_version") || "No HEAD version of this file");
        return;
    }
    const content = gitShow.stdout;
    const lines = content.split('\n');
    const entries: TextPropertyEntry[] = lines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 },
    }));
    // Name ends with the file path so the host detects syntax from the
    // trailing extension (same convention git_log uses for its revision
    // views).
    const view = await editor.createVirtualBuffer({
        name: `*HEAD:${st.filePath}*`,
        mode: "normal",
        readOnly: true,
        entries,
        showLineNumbers: true,
        editingDisabled: true,
    });
    if (!view) {
        editor.setStatus(editor.t("status.no_head_version") || "No HEAD version of this file");
        return;
    }
    // createVirtualBuffer makes the new buffer active, so setBufferCursor
    // lands on it directly and scrolls the line into view via the host's
    // ensure-cursor-visible pass — no showBuffer / delay / extra scroll
    // needed. The byte offset is computed from the content we already have,
    // so it doesn't depend on host line-lookup timing.
    const targetLine = Math.max(1, Math.min(lines.length, oldLine));
    let byteOffset = 0;
    for (let i = 0; i < targetLine - 1; i++) byteOffset += getByteLength(lines[i] + '\n');
    editor.setBufferCursor(view.bufferId, byteOffset);
    editor.setStatus(editor.t("status.opened_head_version", { line: String(targetLine) })
        || `Opened HEAD version (read-only) at line ${targetLine}`);
}

/** Enter in the side-by-side view: open the file for the side under the
 *  cursor — working file (editable) on the NEW pane, HEAD version
 *  (read-only) on the OLD pane. */
async function review_diff_open_at_cursor() {
    const st = activeCompositeDiffState;
    if (!st) return;
    const info = await editor.getCompositeCursorInfo();
    if (!info) return;
    const onOldPane = info.focusedPane === 0;
    if (onOldPane) {
        const oldLine0 = info.lines[0];
        if (oldLine0 === null || oldLine0 === undefined) {
            // Blank OLD side (a pure insertion) — fall back to the working file.
            await openWorkingFileAtCursor(info, st);
            return;
        }
        await openHeadVersionReadOnly(st, oldLine0 + 1);
    } else {
        await openWorkingFileAtCursor(info, st);
    }
}
registerHandler("review_diff_open_at_cursor", review_diff_open_at_cursor);

/** Alt+O in the side-by-side view: always open the editable working file. */
async function review_diff_open_working_at_cursor() {
    const st = activeCompositeDiffState;
    if (!st) return;
    const info = await editor.getCompositeCursorInfo();
    if (!info) return;
    await openWorkingFileAtCursor(info, st);
}
registerHandler("review_diff_open_working_at_cursor", review_diff_open_working_at_cursor);

/** A `CompositeDiffState` view over the in-panel center composite, so the
 *  side-by-side open helpers (HEAD-version / working-file) can be reused for
 *  Enter in the embedded review side-by-side. */
function centerCompositeDiffState(): CompositeDiffState | null {
    const cc = state.centerComposite;
    if (!cc) return null;
    const file = state.files.find(f => fileKey(f) === cc.fileKey);
    if (!file) return null;
    return {
        compositeBufferId: cc.compositeBufId,
        oldBufferId: cc.oldBufId,
        newBufferId: cc.newBufId,
        filePath: file.path,
        gitRoot: state.repo?.root ?? '',
        absPath: cc.absPath,
        isUntracked: cc.isUntracked,
        hunkLineMap: cc.hunkLineMap,
    };
}

/** Enter in the in-panel side-by-side center: open the file version for the
 *  side under the cursor — read-only HEAD version on the OLD pane, the
 *  editable working file on the NEW pane — exactly like the standalone
 *  side-by-side view. */
async function review_center_open_at_cursor(): Promise<void> {
    const st = centerCompositeDiffState();
    if (!st) return;
    const info = await editor.getCompositeCursorInfo();
    if (!info) return;
    if (info.focusedPane === 0) {
        const oldLine0 = info.lines[0];
        if (oldLine0 === null || oldLine0 === undefined) {
            // Blank OLD side (a pure insertion) — open the working file.
            await openWorkingFileAtCursor(info, st);
            return;
        }
        await openHeadVersionReadOnly(st, oldLine0 + 1);
    } else {
        await openWorkingFileAtCursor(info, st);
    }
}
registerHandler("review_center_open_at_cursor", review_center_open_at_cursor);

/** Side-by-side n/p: move to the next/prev hunk within the focused file's
 *  composite, advancing to the next/prev file when there are no more hunks in
 *  that direction. */
// Serialize side-by-side hunk navigation. compositeHunkNav reads the
// composite cursor asynchronously (getCompositeCursorInfo); if rapid n/p
// presses ran it concurrently they'd all observe the SAME stale cursor,
// each conclude "no more hunks", and skip whole files. Chaining the calls
// guarantees each press sees the cursor left by the previous one.
let compositeNavChain: Promise<void> = Promise.resolve();
function enqueueCompositeNav(dir: 1 | -1): void {
    compositeNavChain = compositeNavChain.then(() => compositeHunkNav(dir)).catch(() => {});
}

async function compositeHunkNav(dir: 1 | -1): Promise<void> {
    const cc = state.centerComposite;
    if (cc) {
        const file = state.files.find(f => fileKey(f) === cc.fileKey);
        const fileHunks = file ? hunksForFile(file) : NO_HUNKS;
        const target = state.compositeHunkIdx + dir;
        if (target >= 0 && target < fileHunks.length) {
            // Within the focused file: step the composite hunk cursor and
            // advance our tracked index in lockstep (synchronous — no race).
            state.compositeHunkIdx = target;
            if (dir > 0) editor.compositeNextHunk(cc.compositeBufId);
            else editor.compositePrevHunk(cc.compositeBufId);
            return;
        }
    }
    // No more hunks in this file → move to the next/prev file. Await the
    // rebuild so a following (serialized) n/p sees the new composite, not the
    // old one (the source of the file-skipping race).
    ensureFocusFile();
    const vis = visibleFiles();
    if (vis.length === 0) return;
    let idx = vis.findIndex(f => fileKey(f) === state.filesCurrentKey);
    if (idx < 0) idx = 0;
    const next = idx + dir;
    if (next < 0 || next >= vis.length) return;
    state.filesCurrentKey = fileKey(vis[next]);
    await buildCenterComposite(); // rebuilds the composite, resets compositeHunkIdx = 0
    if (state.groupId !== null && state.panelBuffers["files"] !== undefined) {
        renderFilesPanel();
    }
    refreshStickyHeader(0);
}

// Define the diff-view mode for the side-by-side composite buffer.
//
// Close (q) and hunk navigation (n/p/]/[) are provided by the core
// CompositeBuffer keymap, so they are intentionally NOT bound here — only
// the keys the core leaves free are added: Enter and Alt+O, which open the
// real file under the cursor. Enter is side-aware (working file on the NEW
// pane, read-only HEAD version on the OLD pane); Alt+O always opens the
// editable working-tree file.
editor.defineMode("diff-view", [
    ["Enter", "review_diff_open_at_cursor"],
    ["M-o", "review_diff_open_working_at_cursor"],
], true);

// The `?` help reference is a read-only buffer; `q` closes it (matches the
// "Press q to close" hint and the review's own `q` = close).
editor.defineMode("review-help", [
    ["q", "close"],
], true);

// --- Review Comment Actions ---

function getCurrentHunkId(): string | null {
    if (state.files.length === 0) return null;
    const hunk = getHunkAtDiffCursor();
    return hunk?.id || null;
}



interface PendingCommentInfo {
    hunkId: string;
    file: string;
    lineType?: 'add' | 'remove' | 'context';
    oldLine?: number;
    newLine?: number;
    lineContent?: string;
}

/**
 * Get the line under the cursor for comment attachment. Returns null
 * unless the cursor is on a real diff line (`add` / `remove` / `context`)
 * — comments are always line-based, never hunk-level.
 */
/** Composite-center equivalent of getCurrentLineInfo: maps the composite
 *  cursor (old/new source line) onto the focused file's hunk + line. */
async function getCompositeLineInfo(): Promise<PendingCommentInfo | null> {
    const cc = state.centerComposite;
    if (!cc) return null;
    const file = state.files.find(f => fileKey(f) === cc.fileKey);
    if (!file) return null;
    const info = await editor.getCompositeCursorInfo();
    if (!info) return null;
    const oldL = info.lines[0];
    const newL = info.lines[1];
    const oldLine = (oldL !== null && oldL !== undefined) ? oldL + 1 : undefined;
    const newLine = (newL !== null && newL !== undefined) ? newL + 1 : undefined;
    if (oldLine === undefined && newLine === undefined) return null;
    const lineType: 'add' | 'remove' | 'context' =
        (newLine !== undefined && oldLine === undefined) ? 'add'
            : (oldLine !== undefined && newLine === undefined) ? 'remove'
                : 'context';
    const fileHunks = hunksForFile(file);
    let hunk = fileHunks.find(h =>
        (newLine !== undefined && newLine >= h.range.start && newLine <= h.range.end) ||
        (oldLine !== undefined && oldLine >= h.oldRange.start && oldLine <= h.oldRange.end)
    );
    if (!hunk && fileHunks.length > 0) {
        // The side-by-side composite shows the whole file, so the cursor can
        // sit on a context line outside every hunk. Allow commenting there by
        // anchoring to the nearest hunk (the comment is still recorded against
        // a real line and listed in the comments panel) — "comment anywhere".
        const pos = newLine ?? oldLine ?? 0;
        hunk = fileHunks.reduce((best, h) => {
            const hStart = newLine !== undefined ? h.range.start : h.oldRange.start;
            const bStart = newLine !== undefined ? best.range.start : best.oldRange.start;
            return Math.abs(hStart - pos) < Math.abs(bStart - pos) ? h : best;
        }, fileHunks[0]);
    }
    if (!hunk) return null;
    return { hunkId: hunk.id, file: file.path, lineType, oldLine, newLine, lineContent: undefined };
}

function getCurrentLineInfo(): PendingCommentInfo | null {
    if (state.files.length === 0) return null;
    const props = propsAtCursorRow();
    if (!props) return null;
    const hunkId = props["hunkId"];
    const lineType = props["lineType"];
    if (typeof hunkId !== 'string') return null;
    if (lineType !== 'add' && lineType !== 'remove' && lineType !== 'context') return null;
    const file = typeof props["file"] === 'string' ? props["file"] as string : '';
    const oldLine = typeof props["oldLine"] === 'number' ? props["oldLine"] as number : undefined;
    const newLine = typeof props["newLine"] === 'number' ? props["newLine"] as number : undefined;
    const lineContent = typeof props["lineContent"] === 'string' ? props["lineContent"] as string : undefined;
    return { hunkId, file, lineType: lineType as 'add' | 'remove' | 'context', oldLine, newLine, lineContent };
}

// Pending prompt state for event-based prompt handling
let pendingCommentInfo: PendingCommentInfo | null = null;
let editingCommentId: string | null = null; // non-null when editing an existing comment

/**
 * Find an existing comment at the current diff cursor position, either on the
 * comment display line itself or on the diff line it's attached to.
 */
function findCommentAtCursor(): ReviewComment | null {
    const props = propsAtCursorRow();
    if (!props) return null;

    // Cursor sits directly on a comment display line.
    const commentId = props["commentId"];
    if (typeof commentId === 'string') {
        return state.comments.find(c => c.id === commentId) || null;
    }

    // Cursor sits on a diff line — match by hunk + line type + line number.
    const hunkId = props["hunkId"];
    const lineType = props["lineType"];
    if (typeof hunkId !== 'string') return null;
    if (lineType !== 'add' && lineType !== 'remove' && lineType !== 'context') return null;
    const oldLine = typeof props["oldLine"] === 'number' ? props["oldLine"] as number : undefined;
    const newLine = typeof props["newLine"] === 'number' ? props["newLine"] as number : undefined;
    return state.comments.find(c =>
        c.hunk_id === hunkId && (
            (c.line_type === 'add' && c.new_line === newLine) ||
            (c.line_type === 'remove' && c.old_line === oldLine) ||
            (c.line_type === 'context' && c.new_line === newLine)
        )
    ) || null;
}

async function review_add_comment() {
    // Composite center: map the composite cursor onto the focused file's
    // hunk/line and add (or edit) a comment there.
    if (state.centerComposite) {
        const info = await getCompositeLineInfo();
        if (!info) {
            editor.setStatus(
                editor.t("status.comment_needs_line") ||
                    "Position cursor on a diff line to add a comment"
            );
            return;
        }
        const existing = state.comments.find(c =>
            c.hunk_id === info.hunkId && (
                (c.line_type === 'add' && c.new_line === info.newLine) ||
                (c.line_type === 'remove' && c.old_line === info.oldLine) ||
                (c.line_type === 'context' && c.new_line === info.newLine)
            )
        ) || null;
        pendingCommentInfo = info;
        editingCommentId = existing?.id || null;
        const lineRef = info.lineType === 'add' && info.newLine ? `+${info.newLine}`
            : info.lineType === 'remove' && info.oldLine ? `-${info.oldLine}`
            : `${info.newLine ?? info.oldLine}`;
        const label = existing
            ? (editor.t("prompt.edit_comment", { line: lineRef }) || `Edit comment on ${lineRef}: `)
            : editor.t("prompt.comment", { line: lineRef });
        if (existing) editor.startPromptWithInitial(label, "review-comment", existing.text);
        else editor.startPrompt(label, "review-comment");
        return;
    }
    // If the cursor is sitting on an existing comment row, edit it
    // directly — `c` doubles as "edit this comment" so the user
    // doesn't have to first move back to the diff line.
    const props = propsAtCursorRow();
    if (props && props["type"] === 'comment' && typeof props["commentId"] === 'string') {
        const existing = state.comments.find(c => c.id === props["commentId"]);
        if (existing) {
            editingCommentId = existing.id;
            pendingCommentInfo = {
                hunkId: existing.hunk_id,
                file: existing.file,
                lineType: existing.line_type,
                oldLine: existing.old_line,
                newLine: existing.new_line,
                lineContent: existing.line_content,
            };
            const lineRef =
                existing.line_type === 'add' && existing.new_line ? `+${existing.new_line}`
                : existing.line_type === 'remove' && existing.old_line ? `-${existing.old_line}`
                : existing.new_line ? `L${existing.new_line}`
                : existing.old_line ? `L${existing.old_line}` : 'line';
            const label =
                editor.t("prompt.edit_comment", { line: lineRef }) ||
                `Edit comment on ${lineRef}: `;
            editor.startPromptWithInitial(label, "review-comment", existing.text);
            return;
        }
    }

    let info = getCurrentLineInfo();
    if (!info) {
        // Cursor isn't on a diff line. Rather than no-op (which, in a modal
        // buffer, leaves the user's next keystrokes to execute as commands),
        // hop to the nearest diff line and comment there.
        const row = nearestDiffRow();
        if (row !== null) {
            jumpDiffCursorToRow(row, { recenter: false });
            info = getCurrentLineInfo();
        }
    }
    if (!info) {
        editor.setStatus(
            editor.t("status.comment_needs_line") ||
                "Position cursor on a diff line to add a comment"
        );
        return;
    }

    // Check for existing comment on this diff line to edit
    const existing = findCommentAtCursor();

    pendingCommentInfo = info;
    editingCommentId = existing?.id || null;

    let lineRef = 'line';
    if (info.lineType === 'add' && info.newLine) {
        lineRef = `+${info.newLine}`;
    } else if (info.lineType === 'remove' && info.oldLine) {
        lineRef = `-${info.oldLine}`;
    } else if (info.newLine) {
        lineRef = `L${info.newLine}`;
    } else if (info.oldLine) {
        lineRef = `L${info.oldLine}`;
    }

    const label = existing
        ? (editor.t("prompt.edit_comment", { line: lineRef }) || `Edit comment on ${lineRef}: `)
        : editor.t("prompt.comment", { line: lineRef });

    if (existing) {
        editor.startPromptWithInitial(label, "review-comment", existing.text);
    } else {
        editor.startPrompt(label, "review-comment");
    }
}
registerHandler("review_add_comment", review_add_comment);

let pendingDeleteCommentId: string | null = null;

async function review_delete_comment() {
    const target: ReviewComment | null = findCommentAtCursor();

    if (!target) {
        editor.setStatus("No comment to delete");
        return;
    }

    pendingDeleteCommentId = target.id;
    const preview = target.text.length > 40 ? target.text.substring(0, 37) + '...' : target.text;
    editor.startPrompt(`Delete "${preview}"?`, "review-delete-comment-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: "Delete", description: "Remove this comment", value: "delete" },
        { text: "Cancel", description: "Keep the comment", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
}
registerHandler("review_delete_comment", review_delete_comment);



// Prompt event handlers




// Register prompt event handlers
editor.on("prompt_confirmed", (args) => {
    if (args.prompt_type !== "review-comment") {
        return true;
    }

    // Remember the cursor row from before the rebuild so we can put the
    // user back where they were. Inserting a comment row shifts later
    // rows down by one, but the line the user was on keeps its row
    // number — so saving the row pre-rebuild and restoring it after
    // lands the cursor on the same diff line.
    const cursorRowBeforeRebuild = state.diffCursorRow;

    if (editingCommentId) {
        // Edit mode: update existing comment (empty text keeps the comment unchanged)
        if (args.input && args.input.trim()) {
            const existing = state.comments.find(c => c.id === editingCommentId);
            if (existing) {
                existing.text = args.input.trim();
                existing.timestamp = new Date().toISOString();
                persistReview();
                updateMagitDisplay();
                jumpDiffCursorToRow(cursorRowBeforeRebuild);
                editor.setStatus("Comment updated");
            }
        } else {
            editor.setStatus("Comment unchanged (use x to delete)");
        }
        editingCommentId = null;
        pendingCommentInfo = null;
        return true;
    }

    // New comment mode
    if (pendingCommentInfo && args.input && args.input.trim()) {
        const comment: ReviewComment = {
            id: `comment-${Date.now()}`,
            hunk_id: pendingCommentInfo.hunkId,
            file: pendingCommentInfo.file,
            text: args.input.trim(),
            timestamp: new Date().toISOString(),
            old_line: pendingCommentInfo.oldLine,
            new_line: pendingCommentInfo.newLine,
            line_content: pendingCommentInfo.lineContent,
            line_type: pendingCommentInfo.lineType
        };
        state.comments.push(comment);
        persistReview();
        updateMagitDisplay();
        jumpDiffCursorToRow(cursorRowBeforeRebuild);
        let lineRef = 'hunk';
        if (comment.line_type === 'add' && comment.new_line) {
            lineRef = `line +${comment.new_line}`;
        } else if (comment.line_type === 'remove' && comment.old_line) {
            lineRef = `line -${comment.old_line}`;
        } else if (comment.new_line) {
            lineRef = `line ${comment.new_line}`;
        } else if (comment.old_line) {
            lineRef = `line ${comment.old_line}`;
        }
        editor.setStatus(editor.t("status.comment_added", { line: lineRef }));
    }
    pendingCommentInfo = null;
    return true;
});
editor.on("prompt_confirmed", async (args) => {
    if (args.prompt_type !== "review-discard-confirm") return true;

    const response = args.input.trim().toLowerCase();
    if (response === "discard" || args.selected_index === 0) {
        const f = pendingDiscardFile;
        if (f) {
            const outcome = await discardFileToHead(f);
            await refreshMagitData();
            // Report what git actually did. A failed discard says so —
            // and says why — instead of the blanket "Discarded" that
            // used to follow a no-op (#2318).
            if (outcome.ok) {
                setReviewConfirmation(
                    tr("status.file_discarded", { file: f.path }) ?? `Discarded: ${f.path}`,
                );
            } else {
                const failed = tr("status.discard_failed", { file: f.path })
                    ?? `Discard failed: ${f.path}`;
                setReviewConfirmation(outcome.detail ? `${failed} — ${outcome.detail}` : failed);
            }
        }
    } else {
        editor.setStatus("Discard cancelled");
    }
    pendingDiscardFile = null;
    return false;
});
editor.on("prompt_confirmed", async (args) => {
    if (args.prompt_type !== "review-discard-hunk-confirm") return true;
    const response = args.input.trim().toLowerCase();
    if (response === "discard" || args.selected_index === 0) {
        const hunk = getHunkAtDiffCursor();
        if (hunk && hunk.file) {
            const patch = buildHunkPatch(hunk.file, hunk);
            const ok = await applyHunkPatch(patch, ["--reverse"]);
            if (ok) {
                await refreshMagitData();
                setReviewConfirmation(editor.t("status.hunk_discarded") || "Hunk discarded");
            }
        }
    } else {
        editor.setStatus("Discard cancelled");
    }
    return false;
});
editor.on("prompt_confirmed", (args) => {
    if (args.prompt_type !== "review-edit-note") return true;
    if (args.input && args.input.trim()) {
        state.note = args.input.trim();
        persistReview();
        updateMagitDisplay();
        editor.setStatus(state.note ? "Note saved" : "Note cleared");
    } else {
        // Empty submission: keep existing note unchanged (use x to delete)
        if (state.note) {
            editor.setStatus("Note unchanged (use x to delete)");
        }
    }
    return true;
});
editor.on("prompt_confirmed", (args) => {
    if (args.prompt_type !== "review-delete-comment-confirm") return true;
    const response = args.input.trim().toLowerCase();
    if ((response === "delete" || args.selected_index === 0) && pendingDeleteCommentId) {
        if (pendingDeleteCommentId === '__note__') {
            state.note = '';
        } else {
            state.comments = state.comments.filter(c => c.id !== pendingDeleteCommentId);
        }
        persistReview();
        updateMagitDisplay();
        editor.setStatus("Deleted");
    } else {
        editor.setStatus("Delete cancelled");
    }
    pendingDeleteCommentId = null;
    return false;
});
editor.on("prompt_cancelled", (args) => {
    if (args.prompt_type === "review-comment") {
        pendingCommentInfo = null;
        editingCommentId = null;
        editor.setStatus(editor.t("status.comment_cancelled"));
    }
    // Escape on the discard dialog has to clear the pending file too. The
    // auto-refresh poll stands down while it is set (a rebuild would move
    // the ground under an open confirmation), so leaving it set after a
    // cancel silently stops the panel following the working tree for the
    // rest of the session — the #3126 symptom, reintroduced.
    if (args.prompt_type === "review-discard-confirm") {
        pendingDiscardFile = null;
        editor.setStatus("Discard cancelled");
    }
    return true;
});

async function review_edit_note() {
    const label = editor.t("prompt.overall_comment") || "Note: ";
    if (state.note) {
        editor.startPromptWithInitial(label, "review-edit-note", state.note);
    } else {
        editor.startPrompt(label, "review-edit-note");
    }
}
registerHandler("review_edit_note", review_edit_note);



async function review_export_session() {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");

    let md = `# Code Review Session\n`;
    md += `Date: ${new Date().toISOString()}\n\n`;

    if (state.note) {
        md += `## Note\n${state.note}\n\n`;
    }

    // Summary
    const filesWithComments = new Set(state.comments.map(c => c.file)).size;
    md += `## Summary\n`;
    md += `- Files: ${state.files.length}\n`;
    md += `- Hunks: ${state.hunks.length}\n`;
    if (filesWithComments > 0) {
        md += `- Files with comments: ${filesWithComments}\n`;
    }
    md += `\n`;

    // Group comments by file
    const fileComments: Record<string, ReviewComment[]> = {};
    for (const c of state.comments) {
        const file = c.file || 'unknown';
        if (!fileComments[file]) fileComments[file] = [];
        fileComments[file].push(c);
    }

    for (const [file, comments] of Object.entries(fileComments)) {
        md += `## ${file}\n\n`;
        for (const c of comments) {
            let lineRef = '';
            if (c.line_type === 'add' && c.new_line) {
                lineRef = `line +${c.new_line}`;
            } else if (c.line_type === 'remove' && c.old_line) {
                lineRef = `line -${c.old_line}`;
            } else if (c.new_line) {
                lineRef = `line ${c.new_line}`;
            } else if (c.old_line) {
                lineRef = `line ${c.old_line}`;
            }
            if (lineRef) {
                md += `- **${lineRef}**: ${c.text}\n`;
            } else {
                md += `- ${c.text}\n`;
            }
            if (c.line_content) {
                md += `  \`${c.line_content.trim()}\`\n`;
            }
        }
        md += `\n`;
    }

    const filePath = editor.pathJoin(reviewDir, "session.md");
    await editor.writeFile(editor.authorityPath(filePath), md);
    editor.setStatus(editor.t("status.exported", { path: filePath }));
}
registerHandler("review_export_session", review_export_session);

async function review_export_json() {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");

    const session = {
        version: "2.0",
        timestamp: new Date().toISOString(),
        note: state.note || null,
        comments: state.comments.map(c => ({
            file: c.file,
            text: c.text,
            line_type: c.line_type || null,
            old_line: c.old_line || null,
            new_line: c.new_line || null,
            line_content: c.line_content || null
        }))
    };

    const filePath = editor.pathJoin(reviewDir, "session.json");
    await editor.writeFile(editor.authorityPath(filePath), JSON.stringify(session, null, 2));
    editor.setStatus(editor.t("status.exported", { path: filePath }));
}
registerHandler("review_export_json", review_export_json);

/**
 * Reset the slice of `state` that tracks per-session cursor / fold / row
 * indices. Keeps `state.comments` and `state.note` untouched so the
 * caller can populate them (either freshly, or from disk).
 */
function resetPerSessionState(): void {
    state.diffCursorRow = 1;
    state.hunkHeaderRows = [];
    state.diffLineByteOffsets = [];
    state.streamHunks = [];
    state.streamHunkById = new Map();
    state.sectionByRow = new Map();
    state.fileByRow = new Map();
    state.fileHeaderConceals = new Map();
    state.fileHeaderRows = {};
    state.collapsedFiles = new Set();
    state.collapsedSections = new Set();
    state.collapsedHunks = new Set();
    state.commentsSelectedId = null;
    state.focusPanel = 'diff';
    state.commentsHighlightId = null;
    state.stickyCurrentFile = null;
    state.lineSelection = null;
}

const REVIEW_LAYOUT = JSON.stringify({
    type: "split",
    direction: "v",
    ratio: 0.05,
    first: { type: "fixed", id: "toolbar", height: 2 },
    // Below the toolbar: a left file sidebar, then the diff (with its
    // sticky header) and the comments panel.
    second: {
        type: "split",
        direction: "h",
        ratio: FILES_PANEL_RATIO,
        // Both side panels are widget panels whose list/tree owns its
        // own scroll window, so the buffer under them is pinned
        // (`scrollable: false`, as search_replace.ts does for the same
        // reason). Left user-scrollable, a wheel that walked off the end
        // of the tree fell through to the pane and scrolled the panel
        // itself: the header slid off the top and a blank row appeared at
        // the bottom, with no way to scroll it back.
        first: { type: "scrollable", id: "files", scrollable: false },
        second: {
            type: "split",
            direction: "h",
            // Diff takes the bulk; the comments rail is narrow by default
            // (~15% of the total width, given the 16% file sidebar).
            ratio: 0.82,
            first: {
                type: "split",
                direction: "v",
                ratio: 0.05,
                first: { type: "fixed", id: "sticky", height: 1 },
                second: { type: "scrollable", id: "diff" },
            },
            second: { type: "scrollable", id: "comments", scrollable: false },
        },
    },
});

/**
 * Create the review-diff buffer group (toolbar / sticky / diff / comments)
 * and wire up the standard review-mode event listeners. Returns true if
 * the panels were created, false on failure.
 */
async function openReviewPanels(groupName: string): Promise<boolean> {
    const viewport = editor.getViewport();
    if (viewport) {
        state.viewportWidth = viewport.width;
        state.viewportHeight = viewport.height;
    }
    editor.setContext("review-mode", true);
    const groupResult = await editor.createBufferGroup(groupName, "review-mode", REVIEW_LAYOUT);
    state.groupId = groupResult.groupId;
    state.panelBuffers = groupResult.panels;
    state.reviewBufferId = groupResult.panels["diff"];
    // A brand-new stream buffer holds nothing yet, whatever the last
    // session left recorded.
    state.streamMountedSignature = null;

    if (state.panelBuffers["diff"] !== undefined) {
        (editor as any).setBufferShowCursors(state.panelBuffers["diff"], true);
        // The stream is where the editor's cursor lives, so it takes the
        // motion-native copy of the keymap; the side panels keep
        // `review-mode`, whose ↑/↓ drive their widgets instead.
        editor.setBufferMode(state.panelBuffers["diff"], REVIEW_DIFF_MODE);
        declareCursorLineBar();
    }

    // Mount the widget panels over the group's toolbar / sidebar / rail
    // buffers. Everything with a button on it goes through the widget
    // runtime; the diff and its sticky header stay plain text panels.
    toolbarPanel = state.panelBuffers["toolbar"] !== undefined
        ? new WidgetPanel(state.panelBuffers["toolbar"]) : null;
    filesPanel = state.panelBuffers["files"] !== undefined
        ? new WidgetPanel(state.panelBuffers["files"]) : null;
    commentsPanel = state.panelBuffers["comments"] !== undefined
        ? new WidgetPanel(state.panelBuffers["comments"]) : null;

    // Both of these describe the data the caller fetched, and both have to
    // be in place *before* the first render:
    //   - the signature, or the first watch tick reads "no signature yet",
    //     takes that for a change, and rebuilds a panel nobody touched;
    //   - the revision, because per-session memos key off it, so a reopened
    //     review must not read the previous session's entry for the same
    //     file.
    lastDataSignature = worktreeDataSignature(state.files, state.hunks, state.emptyState);
    state.dataRevision++;

    // The group is created with every panel in its layout; the two side
    // panels are then hidden (the session default) before anything is
    // drawn, so the diff opens at full width without a visible reflow.
    applyPanelVisibility();

    updateMagitDisplay();

    editor.focusBufferGroupPanel(state.groupId!, "diff");

    editor.on("resize", onReviewDiffResize);
    updateReviewStatus();
    // Worktree reviews watch git for changes made outside the editor
    // (#3126); range reviews are ref-to-ref and have nothing to watch.
    startWatchPoll();
    editor.on("buffer_activated", on_review_buffer_activated);
    editor.on("buffer_closed", on_review_buffer_closed);
    editor.on("cursor_moved", on_review_cursor_moved);
    editor.on("viewport_changed", on_review_viewport_changed);
    editor.on("mouse_click", on_review_mouse_click);
    return true;
}

/**
 * Drop any comments whose anchor lines can no longer be found in the
 * current hunks. Applied on restore so stale worktree-mode comments from
 * a long-since-rewritten file don't pile up. For range mode this is a
 * no-op because comments should always match.
 */
function pruneOrphanComments(comments: ReviewComment[], hunks: Hunk[]): ReviewComment[] {
    const byHunk = new Map<string, Hunk>();
    for (const h of hunks) byHunk.set(h.id, h);
    const fileSet = new Set(hunks.map(h => h.file));
    return comments.filter(c => {
        // Keep comments whose hunk still exists or whose file is still
        // part of the diff and whose anchor line is present in some hunk.
        if (byHunk.has(c.hunk_id)) return true;
        if (!fileSet.has(c.file)) return false;
        const fileHunks = hunks.filter(h => h.file === c.file);
        for (const h of fileHunks) {
            const lt = c.line_type;
            if (!lt) continue;
            let oldN = h.oldRange.start - 1;
            let newN = h.range.start - 1;
            for (const raw of h.lines) {
                if (raw.startsWith('+')) {
                    newN++;
                    if (lt === 'add' && c.new_line === newN) return true;
                } else if (raw.startsWith('-')) {
                    oldN++;
                    if (lt === 'remove' && c.old_line === oldN) return true;
                } else {
                    oldN++; newN++;
                    if (lt === 'context' && c.new_line === newN) return true;
                }
            }
        }
        return false;
    });
}

async function start_review_diff() {
    // Already reviewing the working tree: refresh that panel and focus it.
    // Opening a second one left the first orphaned — the plugin tracks a
    // single session, so the old group's tab stayed in the tab bar with
    // nothing driving it, and repeating the command piled up
    // `*Review Diff* 1`, `*Review Diff* 2`, … (#3126).
    if (state.groupId !== null && state.mode === 'worktree') {
        editor.focusBufferGroupPanel(state.groupId, 'diff');
        review_refresh();
        return;
    }
    // A *different* review (a range or a stash) is open: that session ends
    // here rather than leaking, for the same one-session-at-a-time reason.
    if (state.groupId !== null) stop_review_diff();

    editor.setStatus(editor.t("status.generating"));

    // Resolve the repo *before* any git call: getGitStatus/fetchDiffsForFiles
    // read gitCwd(), which derives from state.repo.
    state.repo = await resolveGitRepo(editor);

    // Fetch data using the git status approach.
    const status = await getGitStatus();
    state.files = status.files;
    state.emptyState = status.emptyReason;
    state.hunks = await fetchDiffsForFiles(status.files);

    // Persistence setup: worktree mode keyed by repo root.
    state.mode = 'worktree';
    state.range = null;
    state.reviewKey = buildReviewKey(state.mode, state.range);

    // Restore persisted comments (if any). We drop orphans so the UI
    // doesn't display comments that no longer point at visible lines.
    const loaded = loadPersistedReview(state.repo?.root ?? '', state.reviewKey);
    state.comments = loaded ? pruneOrphanComments(loaded.comments, state.hunks) : [];
    state.note = loaded?.note ?? '';

    resetPerSessionState();
    await openReviewPanels("*Review Diff*");
}
registerHandler("start_review_diff", start_review_diff);

function stop_review_diff() {
    teardownCenterComposite();
    discardParkedComposite();
    state.streamMountedSignature = null;
    state.streamBufferPrepared = null;
    // Unmount before the buffers go away, so the host drops the panels'
    // widget state instead of holding it against dead buffer ids.
    for (const panel of [toolbarPanel, filesPanel, commentsPanel]) panel?.unmount();
    toolbarPanel = null;
    filesPanel = null;
    commentsPanel = null;
    if (state.groupId !== null) {
        editor.closeBufferGroup(state.groupId);
        state.groupId = null;
        state.panelBuffers = {};
    }
    state.reviewBufferId = null;
    stopWatchPoll();
    reviewWatchEnabled = true;
    lastDataSignature = null;
    editor.setContext("review-mode", false);
    editor.off("resize", onReviewDiffResize);
    editor.off("buffer_activated", on_review_buffer_activated);
    editor.off("buffer_closed", on_review_buffer_closed);
    editor.off("cursor_moved", on_review_cursor_moved);
    editor.off("viewport_changed", on_review_viewport_changed);
    editor.off("mouse_click", on_review_mouse_click);
    editor.setStatus(editor.t("status.stopped"));
}
registerHandler("stop_review_diff", stop_review_diff);

// =============================================================================
// Range / commit review (Task 2)
// =============================================================================
//
// `start_review_diff` reviews the working tree. `start_review_range` reviews
// a flattened diff between two git refs — the user types:
//
//     HEAD~3..HEAD     (a span of commits)
//     main..HEAD       (a whole branch)
//     <sha>            (a single commit — rewritten to `<sha>^..<sha>`)
//
// Alternatives considered for the picker UI:
//   - A dedicated two-panel picker (from / to). Clean but adds a big new
//     UI surface for a small benefit.
//   - The existing `start_branch_log` commit list (inline, Enter-to-
//     select). Rejected because that view is commit-by-commit and we
//     specifically want a *flattened* diff for batch commenting.
//   - Single prompt with a small suggestion list. Chosen — matches the
//     tone of the existing branch-log prompt and lets power
//     users type arbitrary revspecs without a multi-step UI.

/**
 * Parse a range string typed into the picker. Accepts:
 *   `A..B`, `A...B` — two-dot / three-dot ranges.
 *   `<ref>`         — single commit, rewritten to `<ref>^..<ref>`.
 *
 * Returns `null` on invalid input (empty string).
 */
function parseRangeInput(input: string): ReviewRange | null {
    const raw = input.trim();
    if (!raw) return null;
    const threeDot = raw.indexOf("...");
    if (threeDot > 0) {
        const from = raw.slice(0, threeDot).trim();
        const to = raw.slice(threeDot + 3).trim();
        if (!from || !to) return null;
        return { from, to, label: `${from}...${to}` };
    }
    const twoDot = raw.indexOf("..");
    if (twoDot > 0) {
        const from = raw.slice(0, twoDot).trim();
        const to = raw.slice(twoDot + 2).trim();
        if (!from || !to) return null;
        return { from, to, label: `${from}..${to}` };
    }
    // Single ref -> single-commit review.
    return { from: `${raw}^`, to: raw, label: raw };
}

/**
 * Fetch a flattened unified diff for the given range and convert it to
 * the same Hunk + FileEntry shape the worktree path produces. All hunks
 * are assigned `gitStatus: 'unstaged'` so the existing section grouping
 * still works; untracked / staged categories are meaningless here.
 */
async function fetchRangeDiff(range: ReviewRange): Promise<{ hunks: Hunk[]; files: FileEntry[] }> {
    // The pinning flags go on *after* the override so every `range.command`
    // inherits them; a source that spells out its own argv (the stash review)
    // otherwise has to remember the whole list for itself.
    const args = withDiffArgs(range.command || ["diff", "--unified=3", `${range.from}..${range.to}`]);
    const cwd = gitCwd();
    const result = await editor.spawnProcess("git", args, cwd);
    if (result.exit_code !== 0) {
        return { hunks: [], files: [] };
    }
    const hunks = parseDiffOutput(result.stdout, 'unstaged');
    // Rewrite hunk ids so they include the range — avoids id collisions
    // when a user opens multiple range reviews in the same session.
    for (const h of hunks) {
        h.id = `${range.label}|${h.file}:${h.range.start}`;
    }
    // Derive a FileEntry list from the hunks, preserving first-seen order.
    const seen = new Set<string>();
    const files: FileEntry[] = [];
    for (const h of hunks) {
        if (!seen.has(h.file)) {
            seen.add(h.file);
            files.push({ path: h.file, status: 'M', category: 'unstaged' });
        }
    }
    return { hunks, files };
}

/**
 * Build a short list of revspec suggestions to prefill the picker. Falls
 * back gracefully if any of the helper git calls fail — the prompt still
 * accepts arbitrary input.
 */
async function buildRangeSuggestions(): Promise<PromptSuggestion[]> {
    const suggestions: PromptSuggestion[] = [];
    // HEAD last commit.
    suggestions.push({ text: "HEAD", description: "Review last commit", value: "HEAD" });
    // Current-branch-vs-main style ranges.
    const tryRange = async (base: string) => {
        const cwd = gitCwd();
        const exists = await editor.spawnProcess("git", ["rev-parse", "--verify", base], cwd);
        if (exists.exit_code === 0) {
            suggestions.push({
                text: `${base}..HEAD`,
                description: `Review all commits on current branch vs ${base}`,
                value: `${base}..HEAD`,
            });
        }
    };
    await tryRange("main");
    await tryRange("master");
    // Recent commits for one-off review.
    try {
        const cwd = gitCwd();
        const log = await editor.spawnProcess("git", [
            "log", "-n", "5", "--pretty=format:%h %s",
        ], cwd);
        if (log.exit_code === 0) {
            for (const line of log.stdout.split('\n')) {
                const m = line.match(/^([0-9a-f]+)\s+(.*)$/);
                if (m) {
                    suggestions.push({
                        text: m[1],
                        description: `Review commit: ${m[2]}`,
                        value: m[1],
                    });
                }
            }
        }
    } catch {}
    return suggestions;
}

async function start_review_range(): Promise<void> {
    // If a review is already open, swap it out rather than stacking two.
    if (state.groupId !== null) {
        stop_review_diff();
    }

    // Resolve the repo before buildRangeSuggestions(), which reads gitCwd().
    state.repo = await resolveGitRepo(editor);
    const suggestions = await buildRangeSuggestions();
    const label = editor.t("prompt.review_range") || "Review range (A..B or commit):";
    editor.startPromptWithInitial(label, "review-range", "HEAD");
    if (suggestions.length > 0) {
        editor.setPromptSuggestions(suggestions);
    }
}
registerHandler("start_review_range", start_review_range);


editor.on("prompt_confirmed", (args) => {
    if (args.prompt_type !== "review-range") return true;
    const range = parseRangeInput(args.input);
    if (!range) {
        editor.setStatus(tr("status.cancelled") ?? "Cancelled");
        return true;
    }
    // Kick off the async bootstrap; the prompt is already dismissed so we
    // can return immediately.
    bootstrapRangeReview(range);
    return true;
});

async function bootstrapRangeReview(range: ReviewRange): Promise<void> {
    editor.setStatus(editor.t("status.generating") || "Generating diff…");
    // Resolve the repo before fetchRangeDiff, which reads gitCwd().
    state.repo = await resolveGitRepo(editor);
    const { hunks, files } = await fetchRangeDiff(range);
    if (hunks.length === 0) {
        editor.setStatus(
            editor.t("status.review_range_empty", { range: range.label }) ||
                `No changes in ${range.label}`,
        );
        return;
    }
    // One review session at a time — see `start_review_diff`. Done after
    // the empty-range check above so a typo'd range doesn't close the
    // review the user is in the middle of.
    if (state.groupId !== null) stop_review_diff();

    state.mode = 'range';
    state.range = range;
    state.hunks = hunks;
    state.files = files;
    state.emptyState = null;
    state.reviewKey = buildReviewKey(state.mode, state.range);

    // Load persisted comments for this exact range — the diff is static
    // so they always line up.
    const loaded = loadPersistedReview(state.repo?.root ?? '', state.reviewKey);
    state.comments = loaded ? loaded.comments : [];
    state.note = loaded?.note ?? '';

    resetPerSessionState();
    await openReviewPanels(`*Review ${range.label}*`);
}

editor.registerCommand(
    "%cmd.review_range",
    "%cmd.review_range_desc",
    "start_review_range",
    null,
);

// --- Stash review (hunk-style `hunk stash show`) -------------------------
// Reuses the range pipeline (read-only, ref-labelled) via an explicit
// `git stash show -p` command override.
async function start_review_stash(): Promise<void> {
    if (state.groupId !== null) stop_review_diff();
    const label = editor.t("prompt.review_stash") || "Review stash (e.g. stash@{0}):";
    editor.startPromptWithInitial(label, "review-stash", "stash@{0}");
}
registerHandler("start_review_stash", start_review_stash);

editor.on("prompt_confirmed", (args) => {
    if (args.prompt_type !== "review-stash") return true;
    const ref = (args.input || "").trim() || "stash@{0}";
    bootstrapRangeReview({
        from: ref,
        to: ref,
        label: ref,
        command: ["stash", "show", "-p", "--unified=3", ref],
    });
    return true;
});

editor.registerCommand(
    "%cmd.review_stash",
    "%cmd.review_stash_desc",
    "start_review_stash",
    null,
);


/**
 * React to a buffer becoming active. Used here purely to track which review
 * panel currently has focus (Tab and mouse clicks both fire buffer_activated).
 * The focus state drives toolbar hint rendering and the `review_nav_*`
 * handlers' files-vs-diff branching.
 *
 * Note: this used to call `refreshMagitData()` on every activation, which
 * spawned several `git` subprocesses every time the user switched panels.
 * The user has a dedicated `r` key for that — auto-refresh was too aggressive.
 */
function on_review_buffer_activated(data: { buffer_id: number }): void {
    if (state.groupId === null) return;
    const diffId = state.panelBuffers["diff"];
    const commentsId = state.panelBuffers["comments"];
    const filesId = state.panelBuffers["files"];
    // In side-by-side the diff panel hosts the composite (its own buffer id),
    // so treat that as the diff panel too.
    const compositeId = state.centerComposite ? state.centerComposite.compositeBufId : -1;
    let newPanel: 'files' | 'diff' | 'comments' | null = null;
    if (data.buffer_id === diffId || data.buffer_id === compositeId) newPanel = 'diff';
    else if (data.buffer_id === commentsId) newPanel = 'comments';
    else if (data.buffer_id === filesId) newPanel = 'files';
    // A hidden panel's buffer can still be "activated" (repainting it
    // touches the buffer). It is not on screen, so it must not take the
    // arrow keys — that stranded `j` / `Down` on an invisible file list.
    if (newPanel !== null && !panelVisible(newPanel)) return;
    if (newPanel === null || newPanel === state.focusPanel) return;
    state.focusPanel = newPanel;
    refreshFocusIndicators();
}
registerHandler("on_review_buffer_activated", on_review_buffer_activated);

/** Re-render the three panel headers so the `▸` focus marker tracks the
 *  currently-focused panel. Cheap: only header rows change. */
function refreshFocusIndicators(): void {
    if (state.groupId === null) return;
    if (state.panelBuffers["files"] !== undefined) {
        renderFilesPanel();
    }
    if (state.panelBuffers["comments"] !== undefined) {
        renderCommentsPanel();
    }
    // The diff's "header" is the sticky bar; refresh it in place. It
    // names what sits at the *top of the view*, so it is driven by the
    // viewport, never by the cursor — feeding it the cursor row made it
    // name a file that wasn't on screen.
    refreshStickyHeader(state.diffViewportTopRow);
}

/**
 * React to native cursor movement inside review panels.
 *
 * Diff panel: keeps `state.diffCursorRow` in sync and re-paints the
 * cursor-line highlight overlay.
 *
 * Files panel: when the cursor moves (e.g. via mouse click), read the
 * `fileIndex` text property at the new position and select that file.
 * This makes click-to-select work even though the files panel hides its
 * native cursor (`show_cursors = false` blocks keyboard-driven movement
 * but mouse clicks still move the cursor).
 */
/**
 * Determine the "current comment" — the one the diff cursor is sitting
 * on (a comment-display row) or attached to (a +/-/context line).
 * Returns null if the cursor is not associated with any comment.
 */
function currentCommentIdAtCursor(): string | null {
    const props = propsAtCursorRow();
    if (!props) return null;
    if (props["type"] === 'comment' && typeof props["commentId"] === 'string') {
        return props["commentId"] as string;
    }
    const hunkId = props["hunkId"];
    const lineType = props["lineType"];
    if (typeof hunkId !== 'string') return null;
    if (lineType !== 'add' && lineType !== 'remove' && lineType !== 'context') return null;
    const oldLine = typeof props["oldLine"] === 'number' ? (props["oldLine"] as number) : undefined;
    const newLine = typeof props["newLine"] === 'number' ? (props["newLine"] as number) : undefined;
    const found = state.comments.find(c =>
        c.hunk_id === hunkId && (
            (c.line_type === 'add' && c.new_line === newLine) ||
            (c.line_type === 'remove' && c.old_line === oldLine) ||
            (c.line_type === 'context' && c.new_line === newLine)
        )
    );
    return found ? found.id : null;
}

function on_review_cursor_moved(data: {
    buffer_id: number;
    cursor_id: number;
    old_position: number;
    new_position: number;
    line: number;
    text_properties: Array<Record<string, unknown>>;
}): void {
    if (state.groupId === null) return;

    // Diff panel: track cursor row + repaint the cursor-line overlay.
    if (data.buffer_id === state.panelBuffers["diff"]) {
        const prevHighlight = state.commentsHighlightId;
        // Moving to a different row is the user moving on from the last
        // action's confirmation. Same-row events are the echo of our own
        // post-action cursor restore, which must not expire it.
        if (reviewConfirmation !== null && data.line !== reviewConfirmation.row) {
            reviewConfirmation = null;
        }
        state.diffCursorRow = data.line;
        // A visual line selection follows the cursor wherever it goes.
        // Extending it from the ↑/↓ handlers only covered the keys those
        // handlers still see; the cursor's own move event covers every
        // motion, native ones included.
        if (state.lineSelection && state.lineSelection.endRow !== data.line) {
            state.lineSelection.endRow = Math.max(1, data.line);
            paintLineSelectionOverlay();
        }
        // Use the cursor row as a sticky-header anchor too — viewport_changed
        // doesn't always fire reliably for plugin-managed virtual buffers
        // (top_line can be null). Tracking the cursor row gives a snappy
        // "what file am I in" indicator regardless.
        refreshStickyHeader(Math.max(0, data.line - 1));
        // The viewport event is the usual trigger; a jump the host reports
        // late (or not at all, for a virtual buffer) still gets its rows
        // painted from here.
        if (wordDiffWindow === null || data.line < wordDiffWindow.first || data.line > wordDiffWindow.last) {
            paintWordDiff(Math.max(0, data.line - 1));
        }
        updateReviewStatus();
        // Re-render the comments panel only when the highlighted comment
        // actually changes — avoids re-emitting the panel on every
        // cursor tick.
        const newHighlight = currentCommentIdAtCursor();
        if (newHighlight !== prevHighlight) {
            state.commentsHighlightId = newHighlight;
            renderCommentsPanel();
        }
        return;
    }
}
registerHandler("on_review_cursor_moved", on_review_cursor_moved);

function on_review_buffer_closed(data: any) {
    if (data.buffer_id === state.reviewBufferId) stop_review_diff();
}
registerHandler("on_review_buffer_closed", on_review_buffer_closed);

// Side-by-side diff for current file using composite buffers
async function side_by_side_diff_current_file() {
    const bid = editor.getActiveBufferId();
    const absolutePath = editor.getBufferPath(bid);

    if (!absolutePath) {
        editor.setStatus(editor.t("status.no_file_open"));
        return;
    }

    editor.setStatus(editor.t("status.loading_diff"));

    // Get the file's directory and name for running git commands
    const fileDir = editor.pathDirname(absolutePath);
    const fileName = editor.pathBasename(absolutePath);

    // Run git commands from the file's directory to avoid path format issues on Windows
    const gitRootResult = await editor.spawnProcess("git", ["-C", fileDir, "rev-parse", "--show-toplevel"]);
    if (gitRootResult.exit_code !== 0) {
        editor.setStatus(editor.t("status.not_git_repo"));
        return;
    }
    const gitRoot = gitRootResult.stdout.trim();

    // Get relative path from git root using git itself (handles Windows paths correctly)
    // `-z`: without it a path with non-ASCII (or `"`, `\`) bytes comes back
    // quoted and octal-escaped, and no later git call would match it.
    const relPathResult = await editor.spawnProcess("git", ["-C", fileDir, "ls-files", "-z", "--full-name", fileName]);
    const relPath = relPathResult.stdout.split("\0")[0] ?? "";
    let filePath: string;
    if (relPathResult.exit_code === 0 && relPath) {
        filePath = relPath;
    } else {
        // File might be untracked, compute relative path manually
        // Normalize paths: replace backslashes with forward slashes for comparison
        const normAbsPath = absolutePath.replace(/\\/g, '/');
        const normGitRoot = gitRoot.replace(/\\/g, '/');
        if (normAbsPath.toLowerCase().startsWith(normGitRoot.toLowerCase())) {
            filePath = normAbsPath.substring(normGitRoot.length + 1);
        } else {
            // Fallback to just the filename
            filePath = fileName;
        }
    }

    // Check if the file is untracked
    const isTrackedResult = await editor.spawnProcess("git", ["-C", gitRoot, "ls-files", "--", filePath]);
    const isUntracked = isTrackedResult.exit_code !== 0 || !isTrackedResult.stdout.trim();

    // Get hunks for this specific file
    let diffOutput: string;
    if (isUntracked) {
        // For untracked files, use --no-index to diff against /dev/null
        const result = await editor.spawnProcess("git", ["-C", gitRoot, ...diffArgs(["diff"], "--no-index", "--unified=3", "--", "/dev/null", filePath)]);
        // git diff --no-index exits with 1 when there are differences, which is expected
        diffOutput = result.stdout || "";
    } else {
        // For tracked files, use normal diff against HEAD
        const result = await editor.spawnProcess("git", ["-C", gitRoot, ...diffArgs(["diff"], "HEAD", "--unified=3", "--", filePath)]);
        if (result.exit_code !== 0) {
            editor.setStatus(editor.t("status.failed_git_diff"));
            return;
        }
        diffOutput = result.stdout;
    }

    // Parse hunks from diff output. This view aligns whole files, so it
    // wants each hunk's real extent on both sides.
    const fileHunks = parseDiffOutput(diffOutput, 'unstaged');
    for (const h of fileHunks) {
        h.id = `${filePath}:${h.range.start}`;
        h.file = filePath;
        h.type = isUntracked ? 'add' : 'modify';
        h.lines = h.lines.filter(l => l[0] !== '\\');
        let oldCount = 0;
        let newCount = 0;
        for (const l of h.lines) {
            if (l[0] !== '+') oldCount++;
            if (l[0] !== '-') newCount++;
        }
        h.oldRange.end = h.oldRange.start + oldCount - 1;
        h.range.end = h.range.start + newCount - 1;
    }

    if (fileHunks.length === 0) {
        editor.setStatus(editor.t("status.no_changes"));
        return;
    }

    // Get old (HEAD) and new (working) file content (use -C gitRoot since filePath is relative to git root)
    let oldContent: string;
    if (isUntracked) {
        // For untracked files, old content is empty (file didn't exist before)
        oldContent = "";
    } else {
        const gitShow = await editor.spawnProcess("git", ["-C", gitRoot, "show", `HEAD:${filePath}`]);
        if (gitShow.exit_code !== 0) {
            editor.setStatus(editor.t("status.failed_old_new_file"));
            return;
        }
        oldContent = gitShow.stdout;
    }

    // Read new file content (use absolute path for readFile)
    const newContent = await editor.readFile(editor.authorityPath(absolutePath));
    if (newContent === null) {
        editor.setStatus(editor.t("status.failed_new_version"));
        return;
    }

    // Close any existing side-by-side views
    if (activeSideBySideState) {
        try {
            if (activeSideBySideState.scrollSyncGroupId !== null) {
                (editor as any).removeScrollSyncGroup(activeSideBySideState.scrollSyncGroupId);
            }
            editor.closeBuffer(activeSideBySideState.oldBufferId);
            editor.closeBuffer(activeSideBySideState.newBufferId);
        } catch {}
        activeSideBySideState = null;
    }

    // Close any existing composite diff view
    if (activeCompositeDiffState) {
        try {
            editor.closeCompositeBuffer(activeCompositeDiffState.compositeBufferId);
            editor.closeBuffer(activeCompositeDiffState.oldBufferId);
            editor.closeBuffer(activeCompositeDiffState.newBufferId);
        } catch {}
        activeCompositeDiffState = null;
    }

    // Create virtual buffers for old and new content
    const oldEntries: TextPropertyEntry[] = contentToEntries(oldContent);
    const newEntries: TextPropertyEntry[] = contentToEntries(newContent);

    // Create source buffers (hidden from tabs, used by composite)
    const oldResult = await editor.createVirtualBuffer({
        name: `*OLD:${filePath}*`,
        mode: "normal",
        readOnly: true,
        entries: oldEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const oldBufferId = oldResult.bufferId;

    const newResult = await editor.createVirtualBuffer({
        name: `*NEW:${filePath}*`,
        mode: "normal",
        readOnly: true,
        entries: newEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const newBufferId = newResult.bufferId;

    // Convert hunks to composite buffer format
    const compositeHunks: TsCompositeHunk[] = fileHunks.map(h => ({
        oldStart: Math.max(0, h.oldRange.start - 1),  // Convert to 0-indexed (0 for new files)
        oldCount: Math.max(1, h.oldRange.end - h.oldRange.start + 1),
        newStart: Math.max(0, h.range.start - 1),     // Convert to 0-indexed
        newCount: h.range.end - h.range.start + 1,
        ops: hunkOps(h)
    }));

    // Create composite buffer with side-by-side layout
    const compositeBufferId = await editor.createCompositeBuffer({
        name: `*Diff: ${filePath}*`,
        mode: "diff-view",
        layout: {
            type: "side-by-side",
            ratios: [0.5, 0.5],
            showSeparator: true
        },
        sources: [
            {
                bufferId: oldBufferId,
                label: "OLD (HEAD)  [Enter] open this version  [n/p] hunks  [q] close",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            },
            {
                bufferId: newBufferId,
                label: "NEW (Working)  [Enter/Alt+o] open file",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            }
        ],
        hunks: compositeHunks.length > 0 ? compositeHunks : null
    });

    // Store state for cleanup + the Enter/Alt+O "open on disk" actions.
    activeCompositeDiffState = {
        compositeBufferId,
        oldBufferId,
        newBufferId,
        filePath,
        gitRoot,
        absPath: absolutePath,
        isUntracked,
        hunkLineMap: fileHunks
            .map(h => ({ oldStart: h.oldRange.start, newStart: h.range.start }))
            .sort((a, b) => a.oldStart - b.oldStart),
    };

    // Show the composite buffer
    editor.showBuffer(compositeBufferId);

    const addedCount = fileHunks.reduce((sum, h) => {
        return sum + h.lines.filter(l => l.startsWith('+')).length;
    }, 0);
    const removedCount = fileHunks.reduce((sum, h) => {
        return sum + h.lines.filter(l => l.startsWith('-')).length;
    }, 0);
    const modifiedCount = Math.min(addedCount, removedCount);

    editor.setStatus(editor.t("status.diff_summary", { added: String(addedCount), removed: String(removedCount), modified: String(modifiedCount) }));
}
registerHandler("side_by_side_diff_current_file", side_by_side_diff_current_file);

// =============================================================================
// Git Log: PR Branch
//
// A git-log view scoped to a PR branch: the commits in `base..HEAD` rather
// than the working-tree changes `start_review_diff` shows. It is named into
// the git_log plugin's `Git Log: …` family because that is what it is; the
// `Review Diff: …` family is the review tool. It opens a buffer
// group with the commit history on the left (rendered by the shared
// `lib/git_history.ts` helpers the git_log plugin uses) and a live-updating
// `git show` of the selected commit on the right. This reuses the same
// rendering pipeline so both plugins stay visually consistent and respect
// theme keys in one place.
//
// This is a *browser*, not a review session: it opens no review buffers and
// takes no comments. To code-review the same commits, use `Review Diff:
// Range` with `base..HEAD`, which flattens them into a single reviewable
// diff.
// =============================================================================

interface BranchLogState {
    isOpen: boolean;
    groupId: number | null;
    logBufferId: number | null;
    detailBufferId: number | null;
    commits: GitCommit[];
    selectedIndex: number;
    baseRef: string;
    detailCache: { hash: string; output: string } | null;
    pendingDetailId: number;
    /** Byte offset of each row in the log panel; final entry = buffer length. */
    logRowByteOffsets: number[];
}

const branchState: BranchLogState = {
    isOpen: false,
    groupId: null,
    logBufferId: null,
    detailBufferId: null,
    commits: [],
    selectedIndex: 0,
    // Empty means "not yet detected"; start_branch_log fills this in
    // from the repo's actual default branch (main, master, or whatever
    // origin/HEAD points at) before showing the prompt.
    baseRef: "",
    detailCache: null,
    pendingDetailId: 0,
    logRowByteOffsets: [],
};

/**
 * Best-effort detection of the repo's default branch. Checks, in order:
 *   1. `origin/HEAD` (the remote's notion of the default branch)
 *   2. local `main`
 *   3. local `master`
 * Falls back to `main` if none match, so the prompt still has a sensible
 * default in an empty / unusual repo.
 */
async function detectDefaultBranch(): Promise<string> {
    const cwd = gitCwd();
    try {
        const r = await editor.spawnProcess("git", [
            "symbolic-ref", "--short", "refs/remotes/origin/HEAD",
        ], cwd);
        if (r.exit_code === 0) {
            const name = r.stdout.trim();
            // Output looks like "origin/main"; strip the remote prefix.
            const slash = name.indexOf("/");
            const branch = slash >= 0 ? name.slice(slash + 1) : name;
            if (branch) return branch;
        }
    } catch { /* fall through */ }
    for (const candidate of ["main", "master"]) {
        try {
            const r = await editor.spawnProcess("git", [
                "show-ref", "--verify", "--quiet", `refs/heads/${candidate}`,
            ], cwd);
            if (r.exit_code === 0) return candidate;
        } catch { /* fall through */ }
    }
    return "main";
}

// UTF-8 byte length helper, local copy so audit_mode doesn't pull in the one
// from git_history (keeps the import list tiny).
function branchUtf8Len(s: string): number {
    let b = 0;
    for (let i = 0; i < s.length; i++) {
        const c = s.charCodeAt(i);
        if (c <= 0x7f) b += 1;
        else if (c <= 0x7ff) b += 2;
        else if (c >= 0xd800 && c <= 0xdfff) { b += 4; i++; }
        else b += 3;
    }
    return b;
}

function branchRowFromByte(bytePos: number): number {
    const offs = branchState.logRowByteOffsets;
    if (offs.length === 0) return 0;
    let lo = 0;
    let hi = offs.length - 1;
    while (lo < hi) {
        const mid = (lo + hi + 1) >> 1;
        if (offs[mid] <= bytePos) lo = mid;
        else hi = mid - 1;
    }
    return lo;
}

function branchIndexFromCursor(bytePos: number): number {
    const row = branchRowFromByte(bytePos);
    const idx = row - 1; // row 0 is the header
    if (idx < 0) return 0;
    if (idx >= branchState.commits.length) return branchState.commits.length - 1;
    return idx;
}

function branchRenderLog(): void {
    if (branchState.groupId === null) return;
    const header = tr("panel.branch_log_header", { base: branchState.baseRef })
        ?? `Commits (${branchState.baseRef}..HEAD)`;
    const footer = tr("panel.branch_log_footer")
        ?? "j/k: navigate · Enter: focus detail · r: refresh · q: close";
    const entries = buildCommitLogEntries(branchState.commits, {
        selectedIndex: branchState.selectedIndex,
        header,
        footer,
        propertyType: "branch-commit",
    });
    const offsets: number[] = [];
    let running = 0;
    for (const e of entries) {
        offsets.push(running);
        running += branchUtf8Len(e.text);
    }
    offsets.push(running);
    branchState.logRowByteOffsets = offsets;
    editor.setPanelContent(branchState.groupId, "log", entries);
}

function branchByteOffsetOfFirstCommit(): number {
    return branchState.logRowByteOffsets.length > 1 ? branchState.logRowByteOffsets[1] : 0;
}

/** Tell the host where the detail panel's rows are code, and in what
 *  (#2871): the `+` / `-` / context rows of each hunk, in the language
 *  of the file the hunk belongs to. */
function branchDeclareDetailSyntax(entries: TextPropertyEntry[]): void {
    if (branchState.detailBufferId === null) return;
    editor.setSyntaxRegions(branchState.detailBufferId, commitDetailSyntaxRegions(entries));
}

async function branchRefreshDetail(): Promise<void> {
    if (branchState.groupId === null) return;
    if (branchState.commits.length === 0) {
        const msg = tr("status.branch_log_empty") ?? "No commits in the selected range.";
        editor.setPanelContent(
            branchState.groupId,
            "detail",
            buildDetailPlaceholderEntries(msg),
        );
        return;
    }
    const idx = Math.max(0, Math.min(branchState.selectedIndex, branchState.commits.length - 1));
    const commit = branchState.commits[idx];
    if (!commit) return;

    if (branchState.detailCache && branchState.detailCache.hash === commit.hash) {
        const entries = buildCommitDetailEntries(commit, branchState.detailCache.output, {});
        editor.setPanelContent(branchState.groupId, "detail", entries);
        branchDeclareDetailSyntax(entries);
        return;
    }
    const myId = ++branchState.pendingDetailId;
    editor.setPanelContent(
        branchState.groupId,
        "detail",
        buildDetailPlaceholderEntries(
            tr("status.loading_commit", { hash: commit.shortHash }) ?? `Loading ${commit.shortHash}…`,
        ),
    );
    const output = await fetchCommitShow(editor, commit.hash, gitCwd());
    if (myId !== branchState.pendingDetailId) return;
    if (branchState.groupId === null) return;
    branchState.detailCache = { hash: commit.hash, output };
    const entries = buildCommitDetailEntries(commit, output, {});
    editor.setPanelContent(branchState.groupId, "detail", entries);
    branchDeclareDetailSyntax(entries);
}

async function start_branch_log(): Promise<void> {
    if (branchState.isOpen) {
        editor.setStatus(tr("status.branch_log_already_open") ?? "Branch log already open");
        return;
    }
    // Prompt for the base ref so the user can review any PR, not just
    // one branched off main. The default offered is either what the user
    // picked last time in this session, or the repo's actual default
    // branch (main/master/etc.) on first use.
    // Resolve the repo before any git call in the branch-review flow.
    state.repo = await resolveGitRepo(editor);
    const suggested = branchState.baseRef || await detectDefaultBranch();
    const rawPromptText = editor.t("prompt.branch_base", { default: suggested });
    const promptText = (rawPromptText && !rawPromptText.startsWith("prompt."))
        ? rawPromptText
        : `Base ref to compare against (default: ${suggested}):`;
    const input = await editor.prompt(promptText + " ", suggested);
    if (input === null) {
        editor.setStatus(tr("status.cancelled") ?? "Cancelled");
        return;
    }
    const base = input.trim() || suggested;
    branchState.baseRef = base;

    editor.setStatus(tr("status.branch_log_loading") ?? "Loading commits…");
    branchState.commits = await fetchGitLog(editor, { range: `${base}..HEAD`, maxCommits: 500, cwd: gitCwd() });
    if (branchState.commits.length === 0) {
        editor.setStatus(
            tr("status.branch_log_empty", { base }) ?? `No commits in ${base}..HEAD`,
        );
        return;
    }

    const layout = JSON.stringify({
        type: "split",
        direction: "h",
        ratio: 0.4,
        first: { type: "scrollable", id: "log" },
        second: { type: "scrollable", id: "detail" },
    });
    // `createBufferGroup` is a runtime-only binding (not in the generated
    // EditorAPI type); cast to `any` so the type-checker doesn't complain.
    const group = await (editor as any).createBufferGroup(
        `*Git Log: ${base}..HEAD*`,
        "branch-log",
        layout,
    );
    branchState.groupId = group.groupId as number;
    branchState.logBufferId = (group.panels["log"] as number | undefined) ?? null;
    branchState.detailBufferId = (group.panels["detail"] as number | undefined) ?? null;
    branchState.selectedIndex = 0;
    branchState.detailCache = null;
    branchState.isOpen = true;

    if (branchState.logBufferId !== null) {
        editor.setBufferShowCursors(branchState.logBufferId, true);
    }
    if (branchState.detailBufferId !== null) {
        editor.setBufferShowCursors(branchState.detailBufferId, true);
    }

    branchRenderLog();
    if (branchState.logBufferId !== null && branchState.commits.length > 0) {
        editor.setBufferCursor(branchState.logBufferId, branchByteOffsetOfFirstCommit());
    }
    await branchRefreshDetail();

    if (branchState.groupId !== null) {
        editor.focusBufferGroupPanel(branchState.groupId, "log");
    }
    editor.on("cursor_moved", on_branch_log_cursor_moved);

    editor.setStatus(
        tr("status.branch_log_ready", {
            count: String(branchState.commits.length),
            base,
        }) ?? `${branchState.commits.length} commits in ${base}..HEAD`,
    );
}
registerHandler("start_branch_log", start_branch_log);

function stop_branch_log(): void {
    if (!branchState.isOpen) return;
    if (branchState.groupId !== null) editor.closeBufferGroup(branchState.groupId);
    editor.off("cursor_moved", on_branch_log_cursor_moved);
    branchState.isOpen = false;
    branchState.groupId = null;
    branchState.logBufferId = null;
    branchState.detailBufferId = null;
    branchState.commits = [];
    branchState.selectedIndex = 0;
    branchState.detailCache = null;
    editor.setStatus(tr("status.branch_log_closed") ?? "Branch log closed");
}
registerHandler("stop_branch_log", stop_branch_log);

async function branch_log_refresh(): Promise<void> {
    if (!branchState.isOpen) return;
    const base = branchState.baseRef;
    branchState.commits = await fetchGitLog(editor, { range: `${base}..HEAD`, maxCommits: 500, cwd: gitCwd() });
    branchState.detailCache = null;
    if (branchState.selectedIndex >= branchState.commits.length) {
        branchState.selectedIndex = Math.max(0, branchState.commits.length - 1);
    }
    branchRenderLog();
    await branchRefreshDetail();
}
registerHandler("branch_log_refresh", branch_log_refresh);

/** Is the detail panel the currently-focused buffer? */
function isBranchLogDetailFocused(): boolean {
    return (
        branchState.detailBufferId !== null &&
        editor.getActiveBufferId() === branchState.detailBufferId
    );
}

/** The currently-selected commit in the log panel, or null. */
function selectedBranchLogCommit(): GitCommit | null {
    if (branchState.commits.length === 0) return null;
    const i = Math.max(
        0,
        Math.min(branchState.selectedIndex, branchState.commits.length - 1),
    );
    return branchState.commits[i] ?? null;
}

/**
 * Enter: on the log panel jumps focus into the detail panel; on the detail
 * panel opens the file at the cursor position at the selected commit (if any).
 */
function branch_log_enter(): void {
    if (branchState.groupId === null) return;
    if (isBranchLogDetailFocused()) {
        void branch_log_detail_open_file();
        return;
    }
    editor.focusBufferGroupPanel(branchState.groupId, "detail");
}
registerHandler("branch_log_enter", branch_log_enter);

/**
 * Open the file at the cursor's `(file, line)` text-properties at the
 * currently-selected commit, in a read-only virtual buffer. Mirrors the
 * git-log plugin's `git_log_detail_open_file` so users get the same
 * drill-down from the branch-log detail panel.
 */
async function branch_log_detail_open_file(): Promise<void> {
    if (branchState.detailBufferId === null) return;
    const commit = selectedBranchLogCommit();
    if (!commit) return;

    const props = editor.getTextPropertiesAtCursor(branchState.detailBufferId);
    if (props.length === 0) {
        editor.setStatus(editor.t("status.move_to_diff"));
        return;
    }
    const file = props[0].file as string | undefined;
    const line = (props[0].line as number | undefined) ?? 1;
    if (!file) {
        editor.setStatus(editor.t("status.move_to_diff_with_context"));
        return;
    }

    editor.setStatus(
        editor.t("status.file_loading", { file, hash: commit.shortHash }),
    );
    const result = await editor.spawnProcess("git", [
        "show",
        `${commit.hash}:${file}`,
    ], gitCwd());
    if (result.exit_code !== 0) {
        editor.setStatus(
            editor.t("status.file_not_found", { file, hash: commit.shortHash }),
        );
        return;
    }

    const lines = result.stdout.split("\n");
    const entries: TextPropertyEntry[] = lines.map((l, i) => ({
        text: l + (i < lines.length - 1 ? "\n" : ""),
        properties: { type: "content", line: i + 1 },
    }));

    // `*<hash>:<path>*` matches the virtual-name convention the host uses
    // to detect syntax from the trailing filename's extension.
    const name = `*${commit.shortHash}:${file}*`;
    const view = await editor.createVirtualBuffer({
        name,
        mode: "branch-log-file-view",
        readOnly: true,
        editingDisabled: true,
        showLineNumbers: true,
        entries,
    });
    if (view) {
        const byte = await editor.getLineStartPosition(Math.max(0, line - 1));
        if (byte !== null) editor.setBufferCursor(view.bufferId, byte);
        editor.setStatus(
            editor.t("status.file_view_ready", {
                file,
                hash: commit.shortHash,
                line: String(line),
            }),
        );
    } else {
        editor.setStatus(editor.t("status.failed_open_file", { file }));
    }
}
registerHandler(
    "branch_log_detail_open_file",
    branch_log_detail_open_file,
);

/** Tab: toggle focus between the log and detail panels. */
function branch_log_tab(): void {
    if (branchState.groupId === null) return;
    editor.focusBufferGroupPanel(
        branchState.groupId,
        isBranchLogDetailFocused() ? "log" : "detail",
    );
}
registerHandler("branch_log_tab", branch_log_tab);

/** q/Escape: focus-back from detail, or close when already on log. */
function branch_log_close_or_back(): void {
    if (branchState.groupId === null) return;
    const active = editor.getActiveBufferId();
    if (branchState.detailBufferId !== null && active === branchState.detailBufferId) {
        editor.focusBufferGroupPanel(branchState.groupId, "log");
        return;
    }
    stop_branch_log();
}
registerHandler("branch_log_close_or_back", branch_log_close_or_back);

function on_branch_log_cursor_moved(data: {
    buffer_id: number;
    cursor_id: number;
    old_position: number;
    new_position: number;
}): void {
    if (!branchState.isOpen) return;
    if (data.buffer_id !== branchState.logBufferId) return;
    const idx = branchIndexFromCursor(data.new_position);
    if (idx === branchState.selectedIndex) return;
    branchState.selectedIndex = idx;
    branchRenderLog();
    branchRefreshDetail();
}
registerHandler("on_branch_log_cursor_moved", on_branch_log_cursor_moved);

editor.defineMode(
    "branch-log",
    [
        // vi-style aliases for Up/Down. Everything else (arrows,
        // Page{Up,Down}, Home/End, selection motion, …) is inherited
        // from the Normal keymap via `inheritNormalBindings: true`.
        ["k", "move_up"],
        ["j", "move_down"],
        // Enter: from the log, focus the detail panel; from the detail
        // panel, open the file at the cursor at the selected commit.
        ["Return", "branch_log_enter"],
        // Tab: toggle focus between the log and detail panels.
        ["Tab", "branch_log_tab"],
        ["r", "branch_log_refresh"],
        ["q", "branch_log_close_or_back"],
        ["Escape", "branch_log_close_or_back"],
    ],
    true, // readOnly
    false, // allowTextInput — keeps plain letters from inserting into the RO buffer
    true, // inheritNormalBindings — PageUp/PageDown/arrows/Home/End come from Normal
);

/** Close the file-view virtual buffer opened from the branch-log detail panel. */
function branch_log_file_view_close(): void {
    const id = editor.getActiveBufferId();
    if (id) editor.closeBuffer(id);
}
registerHandler("branch_log_file_view_close", branch_log_file_view_close);

// Mode for the read-only "git show <hash>:<file>" buffer opened from the
// branch-log detail panel. Mirrors git-log's `git-log-file-view`:
// q/Escape close the view, j/k alias Up/Down, and all other Normal
// bindings (arrows, PageUp/Down, Home/End, Ctrl+C copy) are inherited so
// unbound keys don't fall through to edit actions and trip the
// `editing_disabled` status message (see #566).
editor.defineMode(
    "branch-log-file-view",
    [
        ["k", "move_up"],
        ["j", "move_down"],
        ["q", "branch_log_file_view_close"],
        ["Escape", "branch_log_file_view_close"],
    ],
    true, // read-only
    false, // allow_text_input
    true, // inherit Normal-context bindings for unbound keys
);

// Register Modes and Commands
//
// Two families, each under one prefix. `Review Diff: …` is the code review
// tool — the working tree (`Review Diff` itself), a range or branch flattened
// into one diff, a stash entry, and the commands that act on an open review
// session. `Git Log: …` is the commit browser the git_log plugin owns; the PR
// branch log below joins that family because that is what it is.
editor.registerCommand("%cmd.review_diff", "%cmd.review_diff_desc", "start_review_diff", null);
editor.registerCommand("%cmd.stop_review_diff", "%cmd.stop_review_diff_desc", "stop_review_diff", "review-mode");
editor.registerCommand("%cmd.refresh_review_diff", "%cmd.refresh_review_diff_desc", "review_refresh", "review-mode");
editor.registerCommand("%cmd.side_by_side_diff", "%cmd.side_by_side_diff_desc", "side_by_side_diff_current_file", null);

// Git Log: PR Branch (a git log scoped to `base..HEAD`, not a review session)
editor.registerCommand("%cmd.branch_log", "%cmd.branch_log_desc", "start_branch_log", null);
editor.registerCommand("%cmd.branch_log_close", "%cmd.branch_log_close_desc", "stop_branch_log", "branch-log");
editor.registerCommand("%cmd.branch_log_refresh", "%cmd.branch_log_refresh_desc", "branch_log_refresh", "branch-log");

// Review Comment Commands
editor.registerCommand("%cmd.add_comment", "%cmd.add_comment_desc", "review_add_comment", "review-mode");
editor.registerCommand("%cmd.edit_note", "%cmd.edit_note_desc", "review_edit_note", "review-mode");
editor.registerCommand("%cmd.export_markdown", "%cmd.export_markdown_desc", "review_export_session", "review-mode");
editor.registerCommand("%cmd.export_json", "%cmd.export_json_desc", "review_export_json", "review-mode");

// Handler for when buffers are closed - cleans up scroll sync groups and composite buffers


editor.on("buffer_closed", (data) => {
    // If one of the diff view buffers is closed, clean up the scroll sync group
    if (activeSideBySideState) {
        if (data.buffer_id === activeSideBySideState.oldBufferId ||
            data.buffer_id === activeSideBySideState.newBufferId) {
            // Remove scroll sync group
            if (activeSideBySideState.scrollSyncGroupId !== null) {
                try {
                    (editor as any).removeScrollSyncGroup(activeSideBySideState.scrollSyncGroupId);
                } catch {}
            }
            activeSideBySideState = null;
            activeDiffViewState = null;
        }
    }

    // Clean up composite diff state if the composite buffer is closed
    if (activeCompositeDiffState) {
        if (data.buffer_id === activeCompositeDiffState.compositeBufferId) {
            // Close the source buffers
            try {
                editor.closeBuffer(activeCompositeDiffState.oldBufferId);
                editor.closeBuffer(activeCompositeDiffState.newBufferId);
            } catch {}
            activeCompositeDiffState = null;
        }
    }
});

const REVIEW_MODE_BINDINGS: string[][] = [
    // Native cursor motion in the unified diff stream.
    ["Up", "review_nav_up"], ["Down", "review_nav_down"],
    ["k", "review_nav_up"], ["j", "review_nav_down"],
    ["PageUp", "review_page_up"], ["PageDown", "review_page_down"],
    // Home / End — start / end of line in the diff (the editor's normal
    // meaning), first / last row in a focused side panel. Mode bindings
    // replace globals, so these are bound explicitly.
    ["Home", "review_nav_home"], ["End", "review_nav_end"],
    // Left / Right pan the unified stream horizontally. Nothing wraps
    // in the diff panel, so a long line runs past the right edge; the
    // cursor walking off it is what scrolls the viewport across, the
    // same way it does in a normal buffer (Shift+wheel pans too). In the
    // FILES sidebar they fold / unfold the selected directory instead.
    ["Left", "review_nav_left"], ["Right", "review_nav_right"],
    // Hunk navigation across the unified stream.
    ["n", "review_next_hunk"], ["p", "review_prev_hunk"],
    // File navigation (hunk-style): focus the prev / next file.
    [",", "review_goto_prev_file"], [".", "review_goto_next_file"],
    // Layout toggle: 1 = stack (the unified stream), 2 = split
    // (side-by-side of the file under the cursor — two columns, two
    // sides), 0 = auto by terminal width.
    // 1 = one column (the unified stream), 2 = two columns (side-by-side).
    ["1", "review_layout_stack"],
    ["2", "review_layout_split"],
    ["0", "review_layout_auto"],
    // Show / hide the two side panels (both start hidden, so the diff
    // gets the full width until you ask for them).
    ["F", "review_toggle_files_panel"],
    ["C", "review_toggle_comments_panel"],
    // Toggle inline review-note visibility; filter files; watch; help.
    ["a", "review_toggle_agent_notes"],
    ["/", "review_filter_files"],
    ["W", "review_toggle_watch"],
    ["?", "review_help"],
    // Tab / Shift-Tab cycle keyboard focus between the file list, the diff,
    // and the comments panel; arrows then act on the focused panel.
    ["Tab", "review_focus_next"],
    ["BackTab", "review_focus_prev"],
    // Fold: `z a` collapses every file; `z r` reveals (expands) every file;
    // Enter on a file/section header toggles just that one.
    ["z a", "review_collapse_all"],
    ["z r", "review_expand_all"],
    // Visual line-selection mode for line-level stage/unstage/discard.
    ["v", "review_visual_start"],
    ["Esc", "review_visual_cancel"],
    // Drill-down to side-by-side view of the file under the cursor —
    // unless focus is in the comments panel, in which case Enter opens
    // the selected comment.
    ["Enter", "review_enter_dispatch"],
    // Open the editable working-tree file at the line under the cursor.
    // Uniform with the side-by-side view's Alt+O.
    ["M-o", "review_open_working_file"],
    // Comments-nav: cycle through comments, jump diff cursor, expand
    // the file if needed. Works regardless of which panel has focus.
    ["]", "review_next_comment"],
    ["[", "review_prev_comment"],
    // Focus the comments panel (use j/k/Enter inside).
    ["`", "review_focus_comments"],
    // Stage/unstage/discard — context-sensitive. s/u/d act on the file
    // (when cursor is on a file header) or the hunk under the cursor.
    // Capital S/U/D always act on the enclosing file.
    ["s", "review_stage_scope"], ["u", "review_unstage_scope"],
    ["d", "review_discard_file"],
    ["S", "review_stage_file"], ["U", "review_unstage_file"],
    ["D", "review_discard_file_only"],
    ["r", "review_refresh"],
    // Comments
    ["c", "review_add_comment"],
    ["N", "review_edit_note"],
    ["x", "review_delete_comment"],
    // Close & export
    ["q", "close"],
    ["e", "review_export_session"],
];

/** Motion handlers that exist only to forward to a built-in action once
 *  the diff panel has focus. In the diff's own mode they *are* the
 *  built-in action.
 *
 *  A plugin action is a round trip: the host hands the key to the plugin
 *  thread, the handler asks for `move_down`, and only a later frame moves
 *  the caret. That is a whole frame of lag on every repeat of a held
 *  arrow key — for a keystroke whose entire job is to move the cursor one
 *  row. Bound directly, the move lands in the frame the key arrived in.
 *
 *  The side panels keep the plugin handlers: there ↑/↓ drive a widget's
 *  selection, which the editor's own cursor motion cannot do. */
const DIFF_NATIVE_MOTION: Record<string, string> = {
    review_nav_up: "move_up",
    review_nav_down: "move_down",
    review_page_up: "move_page_up",
    review_page_down: "move_page_down",
    review_nav_home: "move_line_start",
    review_nav_end: "move_line_end",
    review_nav_left: "move_left",
    review_nav_right: "move_right",
};

editor.defineMode("review-mode", REVIEW_MODE_BINDINGS, true);
/** The diff panel's copy of the map. Same keys, same commands — only the
 *  motions differ (see `DIFF_NATIVE_MOTION`). The buffer carrying this
 *  mode is the one the editor's cursor lives in: the unified stream, and
 *  the side-by-side composite. */
editor.defineMode(
    REVIEW_DIFF_MODE,
    REVIEW_MODE_BINDINGS.map(([key, action]) => [key, DIFF_NATIVE_MOTION[action] ?? action]),
    true,
);

editor.debug("Review Diff plugin loaded with review comments support");
