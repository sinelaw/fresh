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
  fetchCommitShow,
  fetchGitLog,
} from "./lib/git_history.ts";
const VirtualBufferFactory = createVirtualBufferFactory(editor);



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
  byteOffset: number; // Position in the virtual buffer
  gitStatus?: 'staged' | 'unstaged' | 'untracked';
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
  /** Absolute path to the git repo root — stable key for persistence. */
  repoRoot: string;
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
  focusPanel: 'diff' | 'comments';
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
  // Maps 1-indexed row -> the entry's properties. Lets handlers look up
  // type / hunkId / fileKey / etc. by cursor row directly, bypassing
  // editor.getTextPropertiesAtCursor (which can return the previous
  // row's props when the cursor sits at a row-boundary byte).
  entryPropsByRow: Record<number, Record<string, unknown>>;
  // Byte ranges of collapsible bodies, captured at build time. Tab /
  // mouse / z a / z r register these as host folds (see applyFolds)
  // — no buffer rebuild on collapse / expand.
  sectionBodyRange: Record<string, { start: number; end: number }>;
  fileBodyRange: Record<string, { start: number; end: number }>;
  hunkBodyRange: Record<string, { start: number; end: number }>;
  // Maps a category name (`'staged'` etc.) -> 1-indexed row of its
  // section-header row in the unified stream. Used by Tab toggle.
  sectionHeaderRows: Record<string, number>;
  // Maps a 1-indexed row in the comments panel -> comment id
  commentsByRow: Record<number, string>;
  // Maps a 1-indexed row in the files sidebar -> file key. Lets clicks in
  // the sidebar resolve back to a FileEntry.
  filesPanelByRow: Record<number, string>;
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
  // When true (default), the center panel renders only the focused file
  // (`filesCurrentKey`) instead of every file's hunks. The sidebar is the
  // multi-file navigator; this keeps the center buffer small and fast even
  // on huge changesets. `,`/`.` move the focus between files.
  focusOnly: boolean;
  // When true, the focused file renders as a two-column side-by-side
  // (OLD | NEW) in the center panel instead of the unified stream. The
  // `1`/`2` keys toggle it; the sidebar and other panels stay put.
  splitView: boolean;
  // Current selection in the comments panel (1-indexed row, 0 means none)
  commentsSelectedRow: number;
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
}

const state: ReviewState = {
  hunks: [],
  comments: [],
  note: '',
  reviewBufferId: null,
  mode: 'worktree',
  range: null,
  repoRoot: '',
  reviewKey: 'worktree',
  files: [],
  emptyState: null,
  viewportWidth: 80,
  viewportHeight: 24,
  focusPanel: 'diff',
  groupId: null,
  panelBuffers: {},
  hunkHeaderRows: [],
  diffLineByteOffsets: [],
  diffCursorRow: 1,
  fileHeaderRows: {},
  collapsedFiles: new Set(),
  collapsedSections: new Set(),
  collapsedHunks: new Set(),
  hunkRowByHunkId: {},
  diffLineRowByCommentId: {},
  entryPropsByRow: {},
  sectionBodyRange: {},
  fileBodyRange: {},
  hunkBodyRange: {},
  sectionHeaderRows: {},
  commentsByRow: {},
  filesPanelByRow: {},
  filesCurrentKey: null,
  showComments: true,
  fileFilter: "",
  focusOnly: true,
  splitView: false,
  commentsSelectedRow: 0,
  commentsHighlightId: null,
  stickyCurrentFile: null,
  diffViewportTopRow: 0,
  lineSelection: null,
};

function fileKey(f: FileEntry): string { return `${f.path}\0${f.category}`; }
function fileKeyOf(path: string, category: string): string { return `${path}\0${category}`; }

// Theme colour for the synthetic "cursor line" highlight in the panel
// buffers. Reintroduced after the per-line bg overlay was deleted from the
// builders — `applyCursorLineOverlay` writes it on every cursor_moved event.
const STYLE_SELECTED_BG: OverlayColorSpec = "editor.selection_bg";
const CURSOR_LINE_NS = "review-cursor-line";

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
const STYLE_HUNK_HEADER_BG: OverlayColorSpec = "editor.current_line_bg";
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
// Dim foreground for the per-row old/new line-number gutter. Same key
// the editor uses for its own gutter — already chosen per-theme to be
// readable but visibly subordinate to content fg.
const STYLE_LINE_NUM_FG: OverlayColorSpec = "editor.line_number_fg";

// Width of each line-number column. 4 chars fits up to 9999 lines —
// past that we just let the number overflow rather than expanding the
// gutter (extremely rare in review-diff context).
const LINE_NUM_W = 4;

/** Format the per-row "OLD  NEW " prefix (with trailing space). Either
 *  side passes `undefined` for blank — removed lines blank the new
 *  column, added lines blank the old column. */
function lineNumPrefix(oldNum: number | undefined, newNum: number | undefined): string {
    const o = oldNum !== undefined ? String(oldNum).padStart(LINE_NUM_W) : ' '.repeat(LINE_NUM_W);
    const n = newNum !== undefined ? String(newNum).padStart(LINE_NUM_W) : ' '.repeat(LINE_NUM_W);
    return ` ${o} ${n} `;
}


/**
 * Calculate UTF-8 byte length of a string manually since TextEncoder is not available
 */
function getByteLength(str: string): number {
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
 * Resolve the git top-level for `editor.getCwd()`. Returns `''` when the
 * cwd isn't inside a repo — callers then skip persistence.
 */
async function detectRepoRoot(): Promise<string> {
    try {
        const result = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"]);
        if (result.exit_code === 0) {
            return result.stdout.trim();
        }
    } catch {
        // fall through
    }
    return '';
}

/**
 * Persist the current `state.comments` / `state.note` to disk. Best-effort:
 * filesystem errors never surface to the user — the UI is the source of
 * truth during the session and writes are just a cache for restore.
 */
function persistReview(): void {
    if (!state.repoRoot) return;
    const path = reviewStoragePathFor(state.repoRoot, state.reviewKey);
    if (!path) return;
    const dir = reviewStorageDirFor(state.repoRoot);
    if (dir) {
        try { editor.createDir(dir); } catch {}
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
        editor.writeFile(path, JSON.stringify(payload, null, 2));
    } catch {}
}

/** Read back a persisted review (if any). Returns null on any failure. */
function loadPersistedReview(repoRoot: string, reviewKey: string): PersistedReview | null {
    if (!repoRoot) return null;
    const path = reviewStoragePathFor(repoRoot, reviewKey);
    if (!path) return null;
    if (!editor.fileExists(path)) return null;
    try {
        const raw = editor.readFile(path);
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

    const parts: DiffPart[] = [];
    if (pre > 0) parts.push({ text: oldStr.slice(0, pre), type: 'unchanged' });
    if (pre < n - suf) parts.push({ text: oldStr.slice(pre, n - suf), type: 'removed' });
    if (pre < m - suf) parts.push({ text: newStr.slice(pre, m - suf), type: 'added' });
    if (suf > 0) parts.push({ text: oldStr.slice(n - suf), type: 'unchanged' });
    return parts;
}

function parseDiffOutput(stdout: string, gitStatus: 'staged' | 'unstaged' | 'untracked'): Hunk[] {
    const lines = stdout.split('\n');
    const hunks: Hunk[] = [];
    let currentFile = "";
    let currentHunk: Hunk | null = null;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        if (line.startsWith('diff --git')) {
            const match = line.match(/diff --git a\/(.+) b\/(.+)/);
            if (match) {
                currentFile = match[2];
                currentHunk = null;
            }
        } else if (line.startsWith('@@')) {
            const match = line.match(/@@ -(\d+),?\d* \+(\d+),?\d* @@(.*)/);
            if (match && currentFile) {
                const oldStart = parseInt(match[1]);
                const newStart = parseInt(match[2]);
                currentHunk = {
                    id: `${currentFile}:${newStart}:${gitStatus}`,
                    file: currentFile,
                    range: { start: newStart, end: newStart },
                    oldRange: { start: oldStart, end: oldStart },
                    type: 'modify',
                    lines: [],
                    status: 'pending',
                    contextHeader: match[3]?.trim() || "",
                    byteOffset: 0,
                    gitStatus
                };
                hunks.push(currentHunk);
            }
        } else if (currentHunk && (line.startsWith('+') || line.startsWith('-') || line.startsWith(' ') || line.startsWith('\\'))) {
            // Keep the "\ No newline at end of file" marker (starts with
            // a backslash) so buildHunkPatch can reproduce the exact
            // end-of-file newline state; without it git refuses to apply
            // the reconstructed patch (e.g. discarding a hunk that adds an
            // unterminated final line).
            if (!line.startsWith('---') && !line.startsWith('+++')) {
                 currentHunk.lines.push(line);
            }
        }
    }
    return hunks;
}

// --- Git status detection ---

/**
 * Parse `git status --porcelain -z` output into FileEntry[].
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
 * Single source of truth for changed files using `git status --porcelain -z`.
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
    const result = await editor.spawnProcess("git", ["status", "--porcelain", "-z"]);
    if (result.exit_code !== 0) {
        return { files: [], emptyReason: 'not_git' };
    }
    const files = parseGitStatusPorcelain(result.stdout);
    return {
        files,
        emptyReason: files.length === 0 ? 'clean' : null,
    };
}

/**
 * Fetch unified diffs for the given file entries.
 * Groups by category to minimize git invocations.
 */
async function fetchDiffsForFiles(files: FileEntry[]): Promise<Hunk[]> {
    const allHunks: Hunk[] = [];

    const hasStaged = files.some(f => f.category === 'staged');
    const hasUnstaged = files.some(f => f.category === 'unstaged');
    const untrackedFiles = files.filter(f => f.category === 'untracked');

    // Staged diffs
    if (hasStaged) {
        const result = await editor.spawnProcess("git", ["diff", "--cached", "--unified=3"]);
        if (result.exit_code === 0 && result.stdout.trim()) {
            allHunks.push(...parseDiffOutput(result.stdout, 'staged'));
        }
    }

    // Unstaged diffs
    if (hasUnstaged) {
        const result = await editor.spawnProcess("git", ["diff", "--unified=3"]);
        if (result.exit_code === 0 && result.stdout.trim()) {
            allHunks.push(...parseDiffOutput(result.stdout, 'unstaged'));
        }
    }

    // Untracked file diffs
    for (const f of untrackedFiles) {
        const result = await editor.spawnProcess("git", [
            "diff", "--no-index", "--unified=3", "/dev/null", f.path
        ]);
        if (result.stdout.trim()) {
            const hunks = parseDiffOutput(result.stdout, 'untracked');
            for (const h of hunks) {
                h.file = f.path;
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

// --- New magit-style rendering (Step 2 of rewrite) ---

const STYLE_DIVIDER: OverlayColorSpec = "ui.split_separator_fg";
const STYLE_FOOTER: OverlayColorSpec = "ui.status_bar_fg";
const STYLE_HUNK_HEADER: OverlayColorSpec = "syntax.keyword";

interface ListLine {
    text: string;
    type: 'section-header' | 'file';
    fileIndex?: number;     // index into state.files[]
    style?: Partial<OverlayOptions>;
    inlineOverlays?: InlineOverlay[];
}

interface DiffLine {
    text: string;
    type: 'hunk-header' | 'add' | 'remove' | 'context' | 'empty' | 'comment' | 'file-header' | 'section-header';
    filePath?: string;   // for file-header rows
    fileKey?: string;    // for file-header rows
    fileIndex?: number;  // for file-header rows
    style?: Partial<OverlayOptions>;
    inlineOverlays?: InlineOverlay[];
    // Line metadata for comment attachment
    hunkId?: string;
    file?: string;
    lineType?: 'add' | 'remove' | 'context';
    oldLine?: number;
    newLine?: number;
    lineContent?: string;
    commentId?: string;
}

/** Compute +N / -M line counts for a file. */
function fileChangeCounts(file: FileEntry): { added: number; removed: number } {
    let added = 0;
    let removed = 0;
    for (const h of state.hunks) {
        if (h.file === file.path && h.gitStatus === file.category) {
            for (const line of h.lines) {
                if (line[0] === '+') added++;
                else if (line[0] === '-') removed++;
            }
        }
    }
    return { added, removed };
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

/**
 * Push inline comment box rows for a given diff line into the lines array.
 * Each comment becomes a bordered, word-wrapped callout whose border title
 * is the line reference. Every row carries the same `commentId` so cursor
 * hit-testing, deletion, and comment navigation resolve from any box row.
 */
function pushLineComments(
    lines: DiffLine[], hunk: Hunk,
    lineType: 'add' | 'remove' | 'context',
    oldLine: number | undefined, newLine: number | undefined
) {
    if (!state.showComments) return;
    const lineComments = state.comments.filter(c =>
        c.hunk_id === hunk.id && (
            (c.line_type === 'add' && c.new_line === newLine) ||
            (c.line_type === 'remove' && c.old_line === oldLine) ||
            (c.line_type === 'context' && c.new_line === newLine)
        )
    );
    if (lineComments.length === 0) return;
    // Indent the box so its left border aligns with the diff content
    // column (just past the OLD/NEW number gutter and the +/- indicator).
    const commentIndent = ' '.repeat(LINE_NUM_W + 1 + LINE_NUM_W + 1 + 1 + 1);
    // Box outer width, clamped to the visible content area.
    const boxW = Math.max(
        COMMENT_BOX_MIN_W,
        Math.min(COMMENT_BOX_MAX_W, state.viewportWidth - commentIndent.length - 1)
    );
    const innerW = boxW - 4; // "| " + content + " |"
    for (const comment of lineComments) {
        const lineRef = comment.line_type === 'add'
            ? `+${comment.new_line}`
            : comment.line_type === 'remove'
            ? `-${comment.old_line}`
            : `${comment.new_line}`;
        const pushRow = (text: string, italic: boolean) => lines.push({
            text: commentIndent + text,
            type: 'comment',
            commentId: comment.id,
            style: { fg: STYLE_COMMENT, italic },
        });
        // Top border carries the line reference as its title.
        const titleSeg = `\u256d\u2500 ${lineRef} `;
        const topFill = '\u2500'.repeat(Math.max(0, boxW - titleSeg.length - 1));
        pushRow(`${titleSeg}${topFill}\u256e`, false);
        for (const wl of wrapText(comment.text, innerW)) {
            pushRow(`\u2502 ${wl.padEnd(innerW)} \u2502`, true);
        }
        pushRow(`\u2570${'\u2500'.repeat(Math.max(0, boxW - 2))}\u256f`, false);
    }
}

/**
 * Render a single hunk as two side-by-side columns (OLD | NEW) into
 * `lines`. Removed lines sit on the left, added on the right, aligned
 * row-by-row; context lines appear on both sides. Per-column add/remove
 * backgrounds are applied via inline overlays so each side keeps its own
 * tint around the central separator. Used when `state.splitView` is on.
 */
function pushSideBySideHunk(lines: DiffLine[], hunk: Hunk) {
    const centerW = Math.max(40, Math.floor(state.viewportWidth * 0.6));
    const colW = Math.max(12, Math.floor((centerW - 3) / 2));
    const SEP = ' │ ';
    const gutterLen = LINE_NUM_W + 1;

    const cell = (num: number | undefined, text: string): string => {
        const g = num !== undefined ? String(num).padStart(LINE_NUM_W) : ' '.repeat(LINE_NUM_W);
        let body = `${g} ${text}`;
        body = body.length > colW ? body.slice(0, colW) : body.padEnd(colW);
        return body;
    };

    let oldN = hunk.oldRange.start;
    let newN = hunk.range.start;
    let rem: { n: number; t: string }[] = [];
    let add: { n: number; t: string }[] = [];

    const pushRow = (
        left: string, right: string,
        leftFilled: boolean, rightFilled: boolean,
        oldLine: number | undefined, newLine: number | undefined,
    ) => {
        const text = left + SEP + right;
        const rightStart = getByteLength(left + SEP);
        const overlays: InlineOverlay[] = [
            { start: 0, end: gutterLen, style: { fg: STYLE_LINE_NUM_FG } },
            { start: rightStart, end: rightStart + gutterLen, style: { fg: STYLE_LINE_NUM_FG } },
        ];
        if (leftFilled && oldLine !== undefined && newLine === undefined) {
            overlays.push({ start: 0, end: getByteLength(left), style: { bg: STYLE_REMOVE_BG } });
        }
        if (rightFilled && newLine !== undefined && oldLine === undefined) {
            overlays.push({ start: rightStart, end: rightStart + getByteLength(right), style: { bg: STYLE_ADD_BG } });
        }
        const type: DiffLine['type'] = leftFilled && !rightFilled ? 'remove'
            : rightFilled && !leftFilled ? 'add' : 'context';
        lines.push({
            text, type,
            hunkId: hunk.id, file: hunk.file,
            lineType: type === 'add' ? 'add' : type === 'remove' ? 'remove' : 'context',
            oldLine, newLine,
            inlineOverlays: overlays,
        });
    };

    const flush = () => {
        const n = Math.max(rem.length, add.length);
        for (let i = 0; i < n; i++) {
            const l = rem[i];
            const r = add[i];
            const left = l ? cell(l.n, l.t) : ' '.repeat(colW);
            const right = r ? cell(r.n, r.t) : ' '.repeat(colW);
            pushRow(left, right, !!l, !!r, l ? l.n : undefined, r ? r.n : undefined);
            if (r) pushLineComments(lines, hunk, 'add', undefined, r.n);
            else if (l) pushLineComments(lines, hunk, 'remove', l.n, undefined);
        }
        rem = [];
        add = [];
    };

    for (const raw of hunk.lines) {
        const p = raw[0];
        const content = raw.substring(1);
        if (p === '-') {
            rem.push({ n: oldN++, t: content });
        } else if (p === '+') {
            add.push({ n: newN++, t: content });
        } else if (p === '\\') {
            // "\ No newline at end of file" — not a real line.
        } else {
            flush();
            pushRow(cell(oldN, content), cell(newN, content), true, true, oldN, newN);
            pushLineComments(lines, hunk, 'context', oldN, newN);
            oldN++;
            newN++;
        }
    }
    flush();
}

/**
 * Build the diff lines for the unified stream.
 * Emits one file-header row per file, followed by its hunks inline.
 * When the file is collapsed, only the header is emitted.
 */
function buildDiffLines(_rightWidth: number): DiffLine[] {
    const lines: DiffLine[] = [];
    if (state.files.length === 0) {
        if (state.emptyState === 'not_git') {
            lines.push({
                text: editor.t("status.not_git_repo") || "Not a git repository",
                type: 'empty',
                style: { fg: STYLE_SECTION_HEADER, italic: true },
            });
        } else if (state.emptyState === 'clean') {
            lines.push({
                text: editor.t("panel.no_changes") || "No changes to review.",
                type: 'empty',
                style: { fg: STYLE_SECTION_HEADER, italic: true },
            });
        }
        return lines;
    }

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
            // Always render expanded triangle (▾). Collapse state is
            // shown by overlaying a `▸` replacement-conceal on the
            // triangle byte range — the buffer text never changes, so
            // toggling collapse never has to rebuild.
            // Range labels (e.g. `main..HEAD`) carry case already — don't
            // mangle them with the section uppercase; worktree category
            // names are lowercase words and need the uppercase.
            const displayLabel = state.mode === 'range' ? label : label.toUpperCase();
            lines.push({
                text: ` ▾ ${displayLabel}  (${sectionCount})`,
                type: 'section-header',
                file: file.category, // store category in 'file' field for reuse
                filePath: file.category,
                style: {
                    fg: STYLE_INVERSE_FG,
                    bg: STYLE_INVERSE_BG,
                    bold: true,
                    extendToLineEnd: true,
                },
            });
        }

        // File header — always emit the expanded triangle; conceal
        // overlays handle the collapsed view.
        const counts = fileChangeCounts(file);
        const key = fileKey(file);
        const filename = file.origPath ? `${file.origPath} → ${file.path}` : file.path;
        const headerText = ` ▾ ${filename}   +${counts.added} / -${counts.removed}`;
        lines.push({
            text: headerText,
            type: 'file-header',
            file: file.path,
            filePath: file.path,
            fileKey: key,
            fileIndex: fi,
            style: {
                fg: STYLE_FILE_HEADER_FG,
                bg: STYLE_FILE_HEADER_BG,
                bold: true,
                extendToLineEnd: true,
            },
        });

        // Focus mode: emit the header for every file (so the stream stays
        // a navigable overview) but build the hunk body only for the
        // focused file. This keeps the center buffer small — and thus
        // responsive — on large changesets without losing the file list.
        if (state.focusOnly && state.filesCurrentKey && key !== state.filesCurrentKey) {
            lines.push({ text: '', type: 'empty' });
            continue;
        }

        // Find hunks for this file
        const fileHunks = state.hunks.filter(
            h => h.file === file.path && h.gitStatus === file.category
        );

        if (fileHunks.length === 0) {
            if (file.status === 'R' && file.origPath) {
                lines.push({ text: `  Renamed from ${file.origPath}`, type: 'empty', style: { fg: STYLE_SECTION_HEADER } });
            } else if (file.status === 'D') {
                lines.push({ text: "  (file deleted)", type: 'empty' });
            } else if (file.status === 'T') {
                lines.push({ text: "  (type change: file ↔ symlink)", type: 'empty', style: { fg: STYLE_SECTION_HEADER } });
            } else if (file.status === '?' && file.path.endsWith('/')) {
                lines.push({ text: "  (untracked directory)", type: 'empty' });
            } else {
                lines.push({ text: "  (no diff available)", type: 'empty' });
            }
            lines.push({ text: '', type: 'empty' });
            continue;
        }

        for (const hunk of fileHunks) {
        // Hunk header — always emit expanded triangle; collapse
        // overlays a `▸` replacement-conceal.
        const headerInner = hunk.contextHeader
            ? `@@ ${hunk.contextHeader} @@`
            : `@@ -${hunk.oldRange.start} +${hunk.range.start} @@`;
        const header = ` ▾ ${headerInner}`;

        lines.push({
            text: header,
            type: 'hunk-header',
            hunkId: hunk.id,
            file: hunk.file,
            style: {
                fg: STYLE_HUNK_HEADER,
                bg: STYLE_HUNK_HEADER_BG,
                bold: true,
                extendToLineEnd: true,
            },
        });

        // (Body always emitted — collapse is handled by overlay
        // conceals on the body's byte range.)

        // (Comments are line-based — they appear under their attached
        // diff line via pushLineComments below, never as hunk-level.)

        // Side-by-side layout: render this hunk as two columns and skip
        // the unified per-line emission below.
        if (state.splitView) {
            pushSideBySideHunk(lines, hunk);
            continue;
        }

        // Track actual file line numbers as we iterate
        let oldLineNum = hunk.oldRange.start;
        let newLineNum = hunk.range.start;

        // Diff content lines with word-level highlighting for adjacent -/+ pairs
        for (let li = 0; li < hunk.lines.length; li++) {
            const line = hunk.lines[li];
            if (line[0] === '\\') {
                // "\ No newline at end of file": informational marker, not a
                // real line. Render it dim with no line numbers and don't
                // advance the line counters. It still occupies exactly one
                // row so hunk.lines stays 1:1 with displayed rows (which
                // selectionLineRange relies on).
                lines.push({
                    text: line, type: 'context',
                    hunkId: hunk.id, file: hunk.file,
                    lineType: 'context', oldLine: undefined, newLine: undefined,
                    lineContent: line,
                    style: { fg: STYLE_LINE_NUM_FG },
                });
                continue;
            }
            const nextLine = hunk.lines[li + 1];
            const prefix = line[0];
            const lineType: 'add' | 'remove' | 'context' =
                prefix === '+' ? 'add' : prefix === '-' ? 'remove' : 'context';
            const curOldLine = lineType !== 'add' ? oldLineNum : undefined;
            const curNewLine = lineType !== 'remove' ? newLineNum : undefined;

            // Detect adjacent -/+ pair for word-level diff
            if (prefix === '-' && nextLine && nextLine[0] === '+') {
                const oldContent = line.substring(1);
                const newContent = nextLine.substring(1);
                const parts = diffStrings(oldContent, newContent);

                // Removed-side line: " OLD       -content"
                const removePrefix = lineNumPrefix(curOldLine, undefined);
                const removeText = removePrefix + line;
                const removePrefixLen = getByteLength(removePrefix);
                const removeOverlays: InlineOverlay[] = [
                    { start: 0, end: removePrefixLen, style: { fg: STYLE_LINE_NUM_FG } },
                ];
                let rOffset = removePrefixLen + getByteLength(line[0]); // skip diff prefix
                for (const part of parts) {
                    const pLen = getByteLength(part.text);
                    if (part.type === 'removed') {
                        removeOverlays.push({ start: rOffset, end: rOffset + pLen, style: { fg: STYLE_REMOVE_TEXT, bg: STYLE_REMOVE_BG, bold: true } });
                    }
                    if (part.type !== 'added') rOffset += pLen;
                }
                lines.push({
                    text: removeText, type: 'remove',
                    style: { bg: STYLE_REMOVE_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType: 'remove', oldLine: curOldLine, newLine: undefined, lineContent: line,
                    inlineOverlays: removeOverlays,
                });
                // Inline comments for the removed line
                pushLineComments(lines, hunk, 'remove', curOldLine, undefined);
                oldLineNum++;

                // Added-side line: "      NEW +content"
                const addPrefix = lineNumPrefix(undefined, newLineNum);
                const addText = addPrefix + nextLine;
                const addPrefixLen = getByteLength(addPrefix);
                const addOverlays: InlineOverlay[] = [
                    { start: 0, end: addPrefixLen, style: { fg: STYLE_LINE_NUM_FG } },
                ];
                let aOffset = addPrefixLen + getByteLength(nextLine[0]);
                for (const part of parts) {
                    const pLen = getByteLength(part.text);
                    if (part.type === 'added') {
                        addOverlays.push({ start: aOffset, end: aOffset + pLen, style: { fg: STYLE_ADD_TEXT, bg: STYLE_ADD_BG, bold: true } });
                    }
                    if (part.type !== 'removed') aOffset += pLen;
                }
                lines.push({
                    text: addText, type: 'add',
                    style: { bg: STYLE_ADD_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType: 'add', oldLine: undefined, newLine: newLineNum, lineContent: nextLine,
                    inlineOverlays: addOverlays,
                });
                pushLineComments(lines, hunk, 'add', undefined, newLineNum);
                newLineNum++;
                li++; // skip the + line we already processed
                continue;
            }

            const numPrefix = lineNumPrefix(curOldLine, curNewLine);
            const decoratedText = numPrefix + line;
            const numPrefixLen = getByteLength(numPrefix);
            const dimNumOverlay: InlineOverlay = {
                start: 0, end: numPrefixLen, style: { fg: STYLE_LINE_NUM_FG },
            };

            if (prefix === '+') {
                lines.push({
                    text: decoratedText, type: 'add',
                    style: { bg: STYLE_ADD_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                    inlineOverlays: [dimNumOverlay],
                });
                newLineNum++;
            } else if (prefix === '-') {
                lines.push({
                    text: decoratedText, type: 'remove',
                    style: { bg: STYLE_REMOVE_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                    inlineOverlays: [dimNumOverlay],
                });
                oldLineNum++;
            } else {
                lines.push({
                    text: decoratedText, type: 'context',
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                    inlineOverlays: [dimNumOverlay],
                });
                oldLineNum++;
                newLineNum++;
            }

            // Render inline comments attached to this line
            pushLineComments(lines, hunk, lineType, curOldLine, curNewLine);
        }
        }

        // Blank separator between files
        lines.push({ text: '', type: 'empty' });
    }

    return lines;
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
 * Build a styled toolbar entry with highlighted key hints.
 * Keys get bold + keyword color; labels get dim text; groups separated by │.
 */
function buildToolbarRow(W: number, groups: HintItem[][]): TextPropertyEntry {
    const overlays: InlineOverlay[] = [];
    let text = " ";
    let bytePos = getByteLength(" ");
    let done = false;

    for (let g = 0; g < groups.length && !done; g++) {
        if (g > 0) {
            const sep = " │ ";
            if (text.length + sep.length > W) { done = true; break; }
            overlays.push({ start: bytePos, end: bytePos + getByteLength(sep), style: { fg: STYLE_TOOLBAR_SEP } });
            text += sep;
            bytePos += getByteLength(sep);
        }
        for (let h = 0; h < groups[g].length && !done; h++) {
            const item = groups[g][h];
            const gap = h > 0 ? "  " : "";
            // Bracket-style key hint: "[key] label" — the brackets make
            // the keys legible without a saturated bg, which works in
            // every theme (no Dracula hot-pink toolbar problem). When
            // the key itself is `[` or `]`, drop the brackets so we
            // don't render `[[]` / `[]]`.
            const isBracket = item.key === '[' || item.key === ']';
            const keyDisplay = isBracket ? item.key : `[${item.key}]`;
            const fullLen = gap.length + keyDisplay.length + 1 + item.label.length;
            const keyOnlyLen = gap.length + keyDisplay.length;

            if (text.length + fullLen <= W) {
                if (gap) { text += gap; bytePos += getByteLength(gap); }
                const keyLen = getByteLength(keyDisplay);
                overlays.push({ start: bytePos, end: bytePos + keyLen, style: { fg: STYLE_KEY_FG, bold: true } });
                text += keyDisplay;
                bytePos += keyLen;
                const labelText = " " + item.label;
                const labelLen = getByteLength(labelText);
                overlays.push({ start: bytePos, end: bytePos + labelLen, style: { fg: STYLE_HINT_FG } });
                text += labelText;
                bytePos += labelLen;
            } else if (text.length + keyOnlyLen <= W) {
                if (gap) { text += gap; bytePos += getByteLength(gap); }
                const keyLen = getByteLength(keyDisplay);
                overlays.push({ start: bytePos, end: bytePos + keyLen, style: { fg: STYLE_KEY_FG, bold: true } });
                text += keyDisplay;
                bytePos += keyLen;
            } else {
                done = true;
            }
        }
    }

    const padded = text.padEnd(W) + "\n";
    return {
        text: padded,
        properties: { type: "toolbar" },
        style: { bg: STYLE_TOOLBAR_BG, extendToLineEnd: true },
        inlineOverlays: overlays,
    };
}

/**
 * Build the (two-row) toolbar with all review-diff shortcuts.
 * Row 1 — navigation; row 2 — actions. Identical regardless of which
 * panel currently has focus (no more files-pane vs diff-pane variants).
 */
function buildToolbar(W: number): TextPropertyEntry[] {
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
        [{ key: "n", label: "next hunk" }, { key: "p", label: "prev hunk" },
         { key: "]", label: "next cmt" }, { key: "[", label: "prev cmt" }],
        inRange
            ? [{ key: "v", label: "select" }, { key: "c", label: "comment" }]
            : [{ key: "s", label: "stage" }, { key: "u", label: "unstage" }, { key: "d", label: "discard" },
               { key: "v", label: "select" }, { key: "c", label: "comment" }],
    ];
    const row2: HintItem[][] = [
        [{ key: "Tab", label: "fold" }, { key: "z a", label: "fold all" }, { key: "z r", label: "unfold all" }],
        inRange
            ? [{ key: "Enter", label: "jump" }, { key: "Alt+o", label: "open file" },
               { key: "e", label: "export" }, { key: "q", label: "close" }]
            : [{ key: "S U D", label: "file-level" }, { key: "Enter", label: "jump" },
               { key: "Alt+o", label: "open file" },
               { key: "e", label: "export" }, { key: "q", label: "close" }],
    ];
    return [buildToolbarRow(W, row1), buildToolbarRow(W, row2)];
}

// --- Buffer Group panel content builders ---

function buildToolbarPanelEntries(): TextPropertyEntry[] {
    // Two-row toolbar: navigation hints on row 1, actions on row 2.
    return buildToolbar(state.viewportWidth);
}

/**
 * Build the unified-diff stream entries. Emits one row per file header
 * followed by all of that file's hunks inline, plus inline comments and
 * a blank separator between files. As a side effect, populates
 * `state.hunkHeaderRows`, `state.diffLineByteOffsets`, and
 * `state.fileHeaderRows` so the rest of the plugin can map cursor rows
 * back to hunks/files.
 */
function buildDiffPanelEntries(): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];

    const hunkHeaderRows: number[] = [];
    const diffLineByteOffsets: number[] = [];
    const fileHeaderRows: Record<string, number> = {};
    const sectionHeaderRows: Record<string, number> = {};
    const hunkRowByHunkId: Record<string, number> = {};
    const diffLineRowByCommentId: Record<string, number> = {};
    const entryPropsByRow: Record<number, Record<string, unknown>> = {};
    // Byte ranges of collapsible bodies, captured in this same single
    // pass so collapse later just registers a host fold (no rebuild).
    // The "body" of an entity is the byte range from the byte after
    // its header's newline up to the byte before the next header that
    // ends it.
    const sectionBodyRange: Record<string, { start: number; end: number }> = {};
    const fileBodyRange: Record<string, { start: number; end: number }> = {};
    const hunkBodyRange: Record<string, { start: number; end: number }> = {};
    let curSection: string | null = null;
    let curFile: string | null = null;
    let curHunk: string | null = null;
    let sectionBodyStart = 0;
    let fileBodyStart = 0;
    let hunkBodyStart = 0;

    let runningByte = 0;
    let row = 0; // 0-indexed counter; row + 1 is the 1-indexed line number
    let lastDiffLineRow = 0; // 1-indexed row of the most recent +/-/context line

    const pushEntry = (entry: TextPropertyEntry) => {
        diffLineByteOffsets.push(runningByte);
        runningByte += getByteLength(entry.text);
        entries.push(entry);
        row++;
    };

    const lines = buildDiffLines(state.viewportWidth);
    for (const line of lines) {
        const props: Record<string, unknown> = { type: line.type };
        if (line.hunkId !== undefined) props.hunkId = line.hunkId;
        if (line.file !== undefined) props.file = line.file;
        if (line.lineType !== undefined) props.lineType = line.lineType;
        if (line.oldLine !== undefined) props.oldLine = line.oldLine;
        if (line.newLine !== undefined) props.newLine = line.newLine;
        if (line.lineContent !== undefined) props.lineContent = line.lineContent;
        if (line.commentId !== undefined) props.commentId = line.commentId;
        if (line.filePath !== undefined) props.filePath = line.filePath;
        if (line.fileKey !== undefined) props.fileKey = line.fileKey;
        if (line.fileIndex !== undefined) props.fileIndex = line.fileIndex;

        const entryStart = runningByte;

        // Header bookkeeping — close any in-progress body for the
        // entities about to be replaced, then open a new body range.
        if (line.type === 'section-header' && line.filePath) {
            if (curHunk) hunkBodyRange[curHunk] = { start: hunkBodyStart, end: entryStart };
            if (curFile) fileBodyRange[curFile] = { start: fileBodyStart, end: entryStart };
            if (curSection) sectionBodyRange[curSection] = { start: sectionBodyStart, end: entryStart };
            curSection = line.filePath;
            curFile = null;
            curHunk = null;
        }
        if (line.type === 'file-header' && line.fileKey) {
            if (curHunk) hunkBodyRange[curHunk] = { start: hunkBodyStart, end: entryStart };
            if (curFile) fileBodyRange[curFile] = { start: fileBodyStart, end: entryStart };
            curFile = line.fileKey;
            curHunk = null;
        }
        if (line.type === 'hunk-header' && line.hunkId) {
            if (curHunk) hunkBodyRange[curHunk] = { start: hunkBodyStart, end: entryStart };
            curHunk = line.hunkId;
        }

        if (line.type === 'hunk-header') {
            hunkHeaderRows.push(row + 1);
            if (line.hunkId) hunkRowByHunkId[line.hunkId] = row + 1;
        }
        if (line.type === 'file-header' && line.fileKey) {
            fileHeaderRows[line.fileKey] = row + 1;
        }
        if (line.type === 'section-header' && line.filePath) {
            sectionHeaderRows[line.filePath] = row + 1;
        }
        if (line.type === 'add' || line.type === 'remove' || line.type === 'context') {
            lastDiffLineRow = row + 1;
        }
        if (line.type === 'comment' && line.commentId) {
            diffLineRowByCommentId[line.commentId] = lastDiffLineRow || (row + 1);
        }

        entryPropsByRow[row + 1] = props;

        pushEntry({
            text: (line.text || "") + "\n",
            style: line.style,
            inlineOverlays: line.inlineOverlays,
            properties: props,
        });

        // After the header is pushed, runningByte points to the first
        // byte of the body that follows.
        if (line.type === 'section-header') sectionBodyStart = runningByte;
        if (line.type === 'file-header') fileBodyStart = runningByte;
        if (line.type === 'hunk-header') hunkBodyStart = runningByte;
    }

    // Close trailing bodies.
    if (curHunk) hunkBodyRange[curHunk] = { start: hunkBodyStart, end: runningByte };
    if (curFile) fileBodyRange[curFile] = { start: fileBodyStart, end: runningByte };
    if (curSection) sectionBodyRange[curSection] = { start: sectionBodyStart, end: runningByte };

    diffLineByteOffsets.push(runningByte);

    state.hunkHeaderRows = hunkHeaderRows;
    state.diffLineByteOffsets = diffLineByteOffsets;
    state.fileHeaderRows = fileHeaderRows;
    state.sectionHeaderRows = sectionHeaderRows;
    state.hunkRowByHunkId = hunkRowByHunkId;
    state.diffLineRowByCommentId = diffLineRowByCommentId;
    state.entryPropsByRow = entryPropsByRow;
    state.sectionBodyRange = sectionBodyRange;
    state.fileBodyRange = fileBodyRange;
    state.hunkBodyRange = hunkBodyRange;
    return entries;
}

/**
 * Build the comments navigation panel. Flat list of comments in the
 * order they appear in the unified diff stream. Each row reads
 *   "path:line  snippet"
 * truncated to fit the panel width. Empty state shows a dim "No comments
 * yet." line. Read-only in this step (interaction lands in Step 5/6).
 */
// Sidebar width fraction (kept in sync with REVIEW_LAYOUT's files panel).
const FILES_PANEL_RATIO = 0.16;

/**
 * Build the file sidebar: one row per changed file, grouped by git
 * category, showing a status glyph, the (left-truncated) path, the
 * +added/-removed counts, and a `*N` badge when the file carries review
 * comments. The row matching the diff viewport's current file is
 * highlighted. Populates `state.filesPanelByRow` so a click resolves back
 * to a file.
 */
function buildFilesPanelEntries(): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];
    state.filesPanelByRow = {};

    const headerLabel = (editor.t("panel.files") || "Files").toUpperCase()
        + (state.fileFilter ? `  /${state.fileFilter}` : "");
    entries.push({
        text: ` ${headerLabel}\n`,
        style: { fg: STYLE_INVERSE_FG, bg: STYLE_INVERSE_BG, bold: true, extendToLineEnd: true },
        properties: { type: "header" },
    });

    if (state.files.length === 0) {
        entries.push({
            text: ` ${editor.t("panel.no_changes") || "No changes."}\n`,
            style: { fg: STYLE_SECTION_HEADER, italic: true },
            properties: { type: "empty" },
        });
        return entries;
    }

    const panelW = Math.max(16, Math.floor(state.viewportWidth * FILES_PANEL_RATIO) - 1);
    // Per-file comment counts drive the `*N` badge.
    const commentCounts: Record<string, number> = {};
    for (const c of state.comments) commentCounts[c.file] = (commentCounts[c.file] || 0) + 1;

    let row1 = 1; // header occupied row 1
    let lastCategory: string | undefined;
    let shownAny = false;
    for (const file of state.files) {
        // Honor the active `/` filter — hide non-matching rows (and any
        // section that ends up empty).
        if (!fileMatchesFilter(file)) continue;
        shownAny = true;
        if (file.category !== lastCategory) {
            lastCategory = file.category;
            let label: string = file.category;
            if (state.mode === 'range' && state.range) label = state.range.label;
            else if (file.category === 'staged') label = editor.t("section.staged") || "Staged";
            else if (file.category === 'unstaged') label = editor.t("section.unstaged") || "Changes";
            else if (file.category === 'untracked') label = editor.t("section.untracked") || "Untracked";
            const count = state.files.filter(f => f.category === file.category && fileMatchesFilter(f)).length;
            const display = state.mode === 'range' ? label : label.toUpperCase();
            row1++;
            entries.push({
                text: ` ${display} (${count})\n`,
                style: { fg: STYLE_SECTION_HEADER, bold: true },
                properties: { type: "files-section" },
            });
        }

        const counts = fileChangeCounts(file);
        const key = fileKey(file);
        const glyph = file.status || ' ';
        const badge = commentCounts[file.path] ? ` *${commentCounts[file.path]}` : '';
        const stats = `  +${counts.added} -${counts.removed}${badge}`;
        // Budget for the name = panel width minus " X " prefix and stats;
        // truncate from the left so the basename stays visible.
        const nameBudget = Math.max(4, panelW - 3 - stats.length);
        let name = file.path;
        if (name.length > nameBudget) name = '…' + name.slice(-(nameBudget - 1));
        row1++;
        const selected = key === state.filesCurrentKey;
        const style: Partial<OverlayOptions> = selected
            ? { bg: STYLE_SELECTED_BG, bold: true, extendToLineEnd: true }
            : {};
        state.filesPanelByRow[row1] = key;
        entries.push({
            text: ` ${glyph} ${name}${stats}\n`,
            style,
            properties: { type: "file", fileKey: key, filePath: file.path },
        });
    }
    if (!shownAny && state.fileFilter) {
        entries.push({
            text: ` ${editor.t("status.filter_no_match") || "No files match"}\n`,
            style: { fg: STYLE_SECTION_HEADER, italic: true },
            properties: { type: "empty" },
        });
    }
    return entries;
}

function buildCommentsPanelEntries(): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];
    state.commentsByRow = {};

    const headerLabel = (editor.t("panel.comments") || "Comments").toUpperCase();
    entries.push({
        text: ` ${headerLabel}\n`,
        style: {
            fg: STYLE_INVERSE_FG,
            bg: STYLE_INVERSE_BG,
            bold: true,
            extendToLineEnd: true,
        },
        properties: { type: "header" },
    });

    if (state.comments.length === 0) {
        entries.push({
            text: ` ${editor.t("panel.no_comments") || "No comments yet."}\n`,
            style: { fg: STYLE_SECTION_HEADER, italic: true },
            properties: { type: "empty" },
        });
        return entries;
    }

    // Order comments by their position in the unified stream. We approximate
    // by sorting by (file index, line number, removed/added preference).
    const fileIndex = (file: string, category: string | undefined): number => {
        for (let i = 0; i < state.files.length; i++) {
            const f = state.files[i];
            if (f.path === file) return i;
        }
        return Number.MAX_SAFE_INTEGER;
    };

    const sortedComments = [...state.comments].sort((a, b) => {
        // Look up via hunk's file
        const hunkA = state.hunks.find(h => h.id === a.hunk_id);
        const hunkB = state.hunks.find(h => h.id === b.hunk_id);
        const fa = fileIndex(a.file, hunkA?.gitStatus);
        const fb = fileIndex(b.file, hunkB?.gitStatus);
        if (fa !== fb) return fa - fb;
        const la = a.new_line ?? a.old_line ?? 0;
        const lb = b.new_line ?? b.old_line ?? 0;
        return la - lb;
    });

    // Each comment renders as a block: a "path:line" location row followed
    // by the note text wrapped over as many rows as needed (no truncation).
    // Every row maps back to the comment id, so clicking or selecting any
    // row of the block jumps to that comment.
    const panelWidth = Math.max(20, Math.floor(state.viewportWidth * 0.25) - 2);
    let rowIdx = 1; // header is row 0 (0-indexed); comments start at row 1
    for (const c of sortedComments) {
        const lineRef = c.new_line ?? c.old_line ?? 0;
        const path = c.file.split('/').pop() || c.file;
        const isCurrent = c.id === state.commentsHighlightId;
        const marker = isCurrent ? '>' : ' ';

        // Location row.
        rowIdx++;
        const locText = `${marker} ${path}:${lineRef}`;
        const locSelected = rowIdx === state.commentsSelectedRow && state.focusPanel === 'comments';
        const locDisplay = locText.length > panelWidth ? locText.slice(0, panelWidth - 1) + '…' : locText;
        state.commentsByRow[rowIdx] = c.id;
        entries.push({
            text: locDisplay + "\n",
            style: locSelected
                ? { bg: STYLE_SELECTED_BG, bold: true, extendToLineEnd: true }
                : { bold: true },
            inlineOverlays: [{ start: 2, end: getByteLength(locText), style: { fg: STYLE_KEY_FG } }],
            properties: { type: "comment-nav", commentId: c.id, file: c.file, line: lineRef },
        });

        // Body rows: the full note text, wrapped and indented.
        const body = c.text.replace(/\s+/g, ' ').trim();
        for (const wl of wrapText(body, panelWidth - 3)) {
            rowIdx++;
            const bodySelected = rowIdx === state.commentsSelectedRow && state.focusPanel === 'comments';
            state.commentsByRow[rowIdx] = c.id;
            entries.push({
                text: `   ${wl}\n`,
                style: bodySelected
                    ? { bg: STYLE_SELECTED_BG, extendToLineEnd: true }
                    : (isCurrent ? undefined : { fg: STYLE_COMMENT }),
                properties: { type: "comment-nav", commentId: c.id, file: c.file, line: lineRef },
            });
        }
    }

    return entries;
}

/**
 * Full refresh — rebuild all three panels. Called on data changes
 * (refreshMagitData, comment add/edit, note edit, resize). NOT called on
 * scroll: scrolling is handled natively by the editor in the panel buffers.
 */
function updateMagitDisplay(): void {
    refreshViewportDimensions();
    if (state.groupId === null) return;
    ensureFocusFile();
    editor.setPanelContent(state.groupId, "toolbar", buildToolbarPanelEntries());
    editor.setPanelContent(state.groupId, "diff", buildDiffPanelEntries());
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
    if (state.panelBuffers["files"] !== undefined) {
        editor.setPanelContent(state.groupId, "files", buildFilesPanelEntries());
    }
    refreshStickyHeader(0);
    applyFolds();
    applyCursorLineOverlay('diff');
}

/**
 * Apply collapse state via the host's folding infrastructure. Folds
 * are designed exactly for "header line stays visible, body lines
 * skipped by the renderer". A fold range covers `[bodyStart, bodyEnd)`
 * — the line containing `bodyStart - 1` (the header) stays visible,
 * everything inside the range gets elided. The host renders its own
 * "..." indicator on the collapsed header line, which is sufficient
 * visual feedback (no need for a triangle swap).
 *
 * Toggling collapse on a 5000-line diff is now O(collapsed_set_size)
 * `addFold` calls. `clearFolds` drops the entire set in one host call
 * so re-applying after a state change is also cheap.
 */
function applyFolds(): void {
    if (state.groupId === null) return;
    const diffId = state.panelBuffers["diff"];
    if (diffId === undefined) return;
    editor.clearFolds(diffId);
    for (const cat of state.collapsedSections) {
        const body = state.sectionBodyRange[cat];
        if (body && body.end > body.start) editor.addFold(diffId, body.start, body.end);
    }
    for (const key of state.collapsedFiles) {
        const body = state.fileBodyRange[key];
        if (body && body.end > body.start) editor.addFold(diffId, body.start, body.end);
    }
    for (const id of state.collapsedHunks) {
        const body = state.hunkBodyRange[id];
        if (body && body.end > body.start) editor.addFold(diffId, body.start, body.end);
    }
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

    const W = state.viewportWidth;
    let text: string;
    let style: Partial<OverlayOptions> = { fg: STYLE_HEADER, bold: true };

    // topVisibleRow is 0-indexed; fileHeaderRows are 1-indexed.
    const top1 = topVisibleRow + 1;
    let bestFile: FileEntry | null = null;
    let bestRow = 0;
    for (const f of state.files) {
        const row = state.fileHeaderRows[fileKey(f)];
        if (row !== undefined && row <= top1 && row > bestRow) {
            bestRow = row;
            bestFile = f;
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

    // Keep the sidebar's highlighted file in sync with the diff's top
    // file. Only repaint when the current file actually changed so
    // scrolling within one file doesn't rebuild the sidebar.
    const curKey = bestFile ? fileKey(bestFile) : null;
    if (curKey !== state.filesCurrentKey) {
        state.filesCurrentKey = curKey;
        if (state.panelBuffers["files"] !== undefined) {
            editor.setPanelContent(state.groupId, "files", buildFilesPanelEntries());
        }
    }
}

/**
 * Helper: jump the diff cursor to the file's first hunk (or its file
 * header if it has no hunks). Auto-expands the file if collapsed.
 */
function jumpToFile(file: FileEntry): void {
    const key = fileKey(file);
    if (state.collapsedFiles.has(key)) {
        state.collapsedFiles.delete(key);
        updateMagitDisplay();
    }
    // Prefer first hunk row; fall back to the file-header row.
    const fileIdx = state.files.indexOf(file);
    if (fileIdx >= 0) {
        // Compute visible hunk index of the first hunk for this file.
        let visibleIdx = 0;
        let foundGlobal = -1;
        for (let i = 0; i < state.hunks.length; i++) {
            const h = state.hunks[i];
            const hKey = fileKeyOf(h.file, h.gitStatus || 'unstaged');
            if (state.collapsedFiles.has(hKey)) continue;
            if (h.file === file.path && h.gitStatus === file.category) {
                foundGlobal = i;
                break;
            }
            visibleIdx++;
        }
        if (foundGlobal >= 0) {
            const row = state.hunkHeaderRows[visibleIdx];
            if (row !== undefined) { jumpDiffCursorToRow(row); return; }
        }
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
        // File header click: toggle the single file.
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

    // Click in the file sidebar: jump to that file and hand focus to the
    // diff so the user can immediately keep navigating.
    if (data.buffer_id === state.panelBuffers["files"]) {
        const key = state.filesPanelByRow[data.buffer_row + 1];
        if (key) {
            const file = state.files.find(f => fileKey(f) === key);
            if (file) {
                if (state.focusOnly && key !== state.filesCurrentKey) {
                    // Focus mode: switch the center to the clicked file.
                    state.filesCurrentKey = key;
                    updateMagitDisplay();
                    jumpDiffCursorToRow(1, { recenter: false });
                } else {
                    jumpToFile(file);
                }
                editor.focusBufferGroupPanel(state.groupId, "diff");
            }
        }
        return;
    }

    // Click in the comments panel: jump to the comment's location and
    // hand focus to the diff so the user can immediately keep navigating.
    if (data.buffer_id === commentsId) {
        const targetRow1 = data.buffer_row + 1;
        const commentId = state.commentsByRow[targetRow1];
        if (commentId) {
            state.commentsSelectedRow = targetRow1;
            jumpToComment(commentId);
            editor.focusBufferGroupPanel(state.groupId, "diff");
            editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
        }
        return;
    }
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
    // Auto-expand whatever's between the cursor and this comment.
    let needRebuild = false;
    if (hunk.gitStatus && state.collapsedSections.has(hunk.gitStatus)) {
        state.collapsedSections.delete(hunk.gitStatus);
        needRebuild = true;
    }
    const file = state.files.find(f => f.path === hunk.file && f.category === hunk.gitStatus);
    if (file) {
        const key = fileKey(file);
        // Focus mode: the comment may live in a file other than the one
        // shown in the center. Switch focus so the anchor row exists.
        if (state.focusOnly && key !== state.filesCurrentKey) {
            state.filesCurrentKey = key;
            needRebuild = true;
        }
        if (state.collapsedFiles.has(key)) {
            state.collapsedFiles.delete(key);
            needRebuild = true;
        }
    }
    if (state.collapsedHunks.has(hunk.id)) {
        state.collapsedHunks.delete(hunk.id);
        needRebuild = true;
    }
    if (needRebuild) updateMagitDisplay();
    // Pin this comment as the highlighted one BEFORE jumping. Any
    // subsequent cursor_moved event that re-derives the highlight
    // will recompute the same id; doing it eagerly avoids a flicker
    // (and works even when the cursor lands on a row whose props
    // don't directly carry a comment id).
    const prevHighlight = state.commentsHighlightId;
    state.commentsHighlightId = commentId;
    if (state.groupId !== null && prevHighlight !== commentId) {
        editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
    }
    // Prefer the diff line the comment is anchored to (line-based);
    // fall back to the hunk header if the lookup hasn't seen the
    // comment yet (race / first render).
    const lineRow = state.diffLineRowByCommentId[commentId];
    if (lineRow !== undefined) { jumpDiffCursorToRow(lineRow); return; }
    const hunkRow = state.hunkRowByHunkId[hunk.id];
    if (hunkRow !== undefined) jumpDiffCursorToRow(hunkRow);
}

function on_review_viewport_changed(data: { split_id: number; buffer_id: number; top_byte: number; top_line: number | null; width: number; height: number }): void {
    if (state.groupId === null) return;
    if (data.buffer_id !== state.panelBuffers["diff"]) return;
    // Prefer top_line when the host provides it. Virtual buffers may not
    // have line metadata, in which case top_line is null — fall back to
    // converting top_byte using our own row-byte index.
    const topRow = data.top_line ?? rowFromByte(data.top_byte);
    state.diffViewportTopRow = topRow;
    refreshStickyHeader(topRow);
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
 * Repaint the synthetic "cursor line" highlight in the diff panel.
 *
 * The diff panel buffer is created with show_cursors=true so the editor
 * moves the cursor natively, but a single-line bg overlay on the cursor row
 * gives a much more visible "you are here" indicator than the bare caret —
 * which matches the magit-style aesthetic and is what the user expects.
 */
function applyCursorLineOverlay(panel: 'diff'): void {
    const bufId = state.panelBuffers[panel];
    if (bufId === undefined) return;
    editor.clearNamespace(bufId, CURSOR_LINE_NS);
    const offsets = state.diffLineByteOffsets;
    if (offsets.length < 2) return;
    const idx = Math.max(0, Math.min(state.diffCursorRow - 1, offsets.length - 2));
    const start = offsets[idx];
    const end = offsets[idx + 1];
    if (end <= start) return;
    editor.addOverlay(bufId, CURSOR_LINE_NS, start, end, {
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

/** Look up the entry's properties for the cursor's current row. Uses
 *  the per-row props map populated during build, which is exact —
 *  unlike `editor.getTextPropertiesAtCursor`, which can return the
 *  previous row's properties when the cursor sits at a row boundary. */
function propsAtCursorRow(): Record<string, unknown> | null {
    return state.entryPropsByRow[state.diffCursorRow] || null;
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
    // Find the comment's row in the panel (header is row 1, comments start at 2).
    const sorted = commentsInPanelOrder();
    const idx = sorted.findIndex(x => x.id === c.id);
    if (idx >= 0) {
        state.commentsSelectedRow = idx + 2;
        editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
    }
}

function review_next_comment() {
    if (state.comments.length === 0) {
        editor.setStatus(editor.t("status.no_comments") || "No comments");
        return;
    }
    const sorted = commentsInPanelOrder();
    // Determine the comment-id currently under the diff cursor (if any).
    const currentRow = state.commentsSelectedRow;
    const currentIdx = currentRow >= 2 ? currentRow - 2 : -1;
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
    const currentRow = state.commentsSelectedRow;
    const currentIdx = currentRow >= 2 ? currentRow - 2 : sorted.length;
    const prevIdx = Math.max(0, currentIdx - 1);
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
    editor.focusBufferGroupPanel(state.groupId, "comments");
    // Ensure the selection highlight shows immediately.
    if (state.commentsSelectedRow < 2 && state.comments.length > 0) {
        state.commentsSelectedRow = 2;
    }
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("review_focus_comments", review_focus_comments);

/**
 * Activate the currently-selected comment in the comments panel:
 * jump the diff cursor to it (auto-expanding the file if collapsed).
 */
function review_open_selected_comment() {
    if (state.commentsSelectedRow < 2) return;
    const commentId = state.commentsByRow[state.commentsSelectedRow];
    if (!commentId) return;
    jumpToComment(commentId);
}
registerHandler("review_open_selected_comment", review_open_selected_comment);

function review_comments_select_next() {
    if (state.groupId === null) return;
    if (state.comments.length === 0) return;
    const total = state.comments.length;
    const currentIdx = Math.max(0, state.commentsSelectedRow - 2);
    const nextIdx = Math.min(total - 1, currentIdx + 1);
    state.commentsSelectedRow = nextIdx + 2;
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("review_comments_select_next", review_comments_select_next);

function review_enter_dispatch() {
    if (state.focusPanel === 'comments') {
        review_open_selected_comment();
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
    const props = propsAtCursorRow();
    let line: number | undefined;
    if (props) {
        const t = props["type"];
        if (t === 'add' || t === 'remove' || t === 'context') {
            const nl = props["newLine"];
            if (typeof nl === 'number') {
                line = nl;
            } else {
                // Pure-removed row: scan forward to the next row carrying a
                // newLine, so we land where the deletion happened. Stop at the
                // end of the stream or when we leave this file's diff body.
                const maxRow = state.diffLineByteOffsets.length - 1;
                for (let r = state.diffCursorRow + 1; r <= maxRow; r++) {
                    const p = state.entryPropsByRow[r];
                    if (!p) continue;
                    if (typeof p["newLine"] === 'number') { line = p["newLine"] as number; break; }
                    if (p["type"] === 'file-header' || p["type"] === 'section-header') break;
                }
            }
        }
    }
    const absPath = state.repoRoot ? editor.pathJoin(state.repoRoot, file.path) : file.path;
    editor.openFile(absPath, line ?? 1, 1);
}
registerHandler("review_open_working_file", review_open_working_file);

function review_comments_select_prev() {
    if (state.groupId === null) return;
    if (state.comments.length === 0) return;
    const currentIdx = Math.max(0, state.commentsSelectedRow - 2);
    const prevIdx = Math.max(0, currentIdx - 1);
    state.commentsSelectedRow = prevIdx + 2;
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("review_comments_select_prev", review_comments_select_prev);

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
    applyCursorLineOverlay('diff');
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
    // Find the row of this hunk's header in the unified stream.
    const hunkIdx = state.hunks.indexOf(hunk);
    let visibleIdx = 0;
    for (let i = 0; i < hunkIdx; i++) {
        const h = state.hunks[i];
        if (state.collapsedFiles.has(fileKeyOf(h.file, h.gitStatus || 'unstaged'))) continue;
        visibleIdx++;
    }
    const headerRow = state.hunkHeaderRows[visibleIdx];
    if (headerRow === undefined) return null;

    const lo = Math.min(sel.startRow, sel.endRow);
    const hi = Math.max(sel.startRow, sel.endRow);
    const startInHunk = lo - headerRow - 1; // -1 because the header row itself is not in hunk.lines
    const endInHunk = hi - headerRow - 1;
    if (startInHunk < 0 || endInHunk >= hunk.lines.length) return null;

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
    editor.setStatus(editor.t(`status.lines_${action}d`) || `Lines ${action}d`);
    await refreshMagitData();
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

function review_nav_up() {
    if (state.focusPanel === 'comments') { review_comments_select_prev(); return; }
    editor.executeAction("move_up");
    if (state.lineSelection) {
        // executeAction has already moved the cursor; sync the selection.
        // Ensure we don't extend out of the hunk.
        const newRow = Math.max(1, state.lineSelection.endRow - 1);
        state.lineSelection.endRow = newRow;
        paintLineSelectionOverlay();
    }
}
registerHandler("review_nav_up", review_nav_up);

function review_nav_down() {
    if (state.focusPanel === 'comments') { review_comments_select_next(); return; }
    editor.executeAction("move_down");
    if (state.lineSelection) {
        state.lineSelection.endRow = state.lineSelection.endRow + 1;
        paintLineSelectionOverlay();
    }
}
registerHandler("review_nav_down", review_nav_down);

function review_page_up() { editor.executeAction("move_page_up"); }
registerHandler("review_page_up", review_page_up);

function review_page_down() { editor.executeAction("move_page_down"); }
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
    editor.writeFile(patchPath, patch);
    // Validate first
    const check = await editor.spawnProcess("git", ["apply", "--check", ...flags, patchPath]);
    if (check.exit_code !== 0) {
        editor.setStatus("Patch failed: " + (check.stderr || "").trim());
        return false;
    }
    const result = await editor.spawnProcess("git", ["apply", ...flags, patchPath]);
    return result.exit_code === 0;
}

/**
 * Merge all text-property records at the cursor of the given panel buffer
 * into a single object. There's typically only one record covering each
 * cursor position; merging keeps callers simple.
 */
function readPropsAtCursor(panel: 'files' | 'diff'): Record<string, unknown> | null {
    const bufId = state.panelBuffers[panel];
    if (bufId === undefined) return null;
    const records = editor.getTextPropertiesAtCursor(bufId);
    if (!records || records.length === 0) return null;
    const merged: Record<string, unknown> = {};
    for (const r of records) Object.assign(merged, r);
    return merged;
}

/**
 * Get the hunk under the cursor in the diff panel, or null.
 *
 * Reads the `hunkId` text property embedded by `buildDiffPanelEntries`. Falls
 * back to the first hunk of the selected file when the cursor is somewhere
 * without a hunkId (e.g. the panel header) so commands like `s` still do
 * something useful.
 */
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
    return state.hunks.find(
        h => h.file === cur.path && h.gitStatus === cur.category
    ) || null;
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
    await editor.spawnProcess("git", ["add", "--", f.path]);
    await refreshMagitData();
}

async function unstageFileEntry(f: FileEntry) {
    rememberPendingHunkAnchor(null);
    await editor.spawnProcess("git", ["reset", "HEAD", "--", f.path]);
    await refreshMagitData();
}

async function stageHunk(hunk: Hunk | null) {
    if (!hunk || !hunk.file) return;
    rememberPendingHunkAnchor(hunk.id);
    if (hunk.gitStatus === 'untracked') {
        await editor.spawnProcess("git", ["add", "--", hunk.file]);
    } else {
        const patch = buildHunkPatch(hunk.file, hunk);
        const ok = await applyHunkPatch(patch, ["--cached"]);
        if (!ok) return;
    }
    editor.setStatus(editor.t("status.hunk_staged") || "Hunk staged");
    await refreshMagitData();
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
    editor.setStatus(editor.t("status.hunk_unstaged") || "Hunk unstaged");
    await refreshMagitData();
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

/** Always-file-level discard (D). Acts on the file the cursor is in. */
function review_discard_file_only() {
    if (state.files.length === 0) return;
    const f = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!f) return;
    pendingDiscardFile = f;
    rememberPendingHunkAnchor(null);
    const action = f.category === 'untracked' ? "Delete" : "Discard changes in";
    editor.startPrompt(`${action} "${f.path}"? This cannot be undone.`, "review-discard-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: `${action} file`, description: "Permanently lose changes", value: "discard" },
        { text: "Cancel", description: "Keep the file as-is", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
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
    pendingDiscardFile = f;
    rememberPendingHunkAnchor(null);
    const action = f.category === 'untracked' ? "Delete" : "Discard changes in";
    editor.startPrompt(`${action} "${f.path}"? This cannot be undone.`, "review-discard-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: `${action} file`, description: "Permanently lose changes", value: "discard" },
        { text: "Cancel", description: "Keep the file as-is", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
}
registerHandler("review_discard_file", review_discard_file);





/**
 * Refresh file list and diffs using the new git status approach, then re-render.
 */
async function refreshMagitData() {
    if (state.mode === 'range' && state.range) {
        const { hunks, files } = await fetchRangeDiff(state.range);
        state.hunks = hunks;
        state.files = files;
        state.emptyState = null;
    } else {
        const status = await getGitStatus();
        state.files = status.files;
        state.emptyState = status.emptyReason;
        state.hunks = await fetchDiffsForFiles(status.files);
    }
    state.diffCursorRow = 1;
    updateMagitDisplay();
    restoreCursorAfterRebuild();
    updateReviewStatus();
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
    const viewport = editor.getViewport();
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
    const gitRootResult = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"]);
    if (gitRootResult.exit_code !== 0) {
        editor.setStatus(editor.t("status.not_git_repo"));
        return;
    }
    const gitRoot = gitRootResult.stdout.trim();
    const absoluteFilePath = editor.pathJoin(gitRoot, h.file);

    // Get old (HEAD) and new (working) file content
    let oldContent: string;
    const gitShow = await editor.spawnProcess("git", ["show", `HEAD:${h.file}`]);
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
        const readResult = await editor.readFile(absoluteFilePath);
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
    const oldLines = oldContent.split('\n');
    const newLines = newContent.split('\n');

    const oldEntries: TextPropertyEntry[] = oldLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

    const newEntries: TextPropertyEntry[] = newLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

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
            newCount: newCount || 1
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

function review_layout_split() {
    if (state.splitView) return;
    state.splitView = true;
    updateMagitDisplay();
    editor.setStatus(editor.t("status.split_view") || "Side-by-side view");
}
registerHandler("review_layout_split", review_layout_split);

function review_layout_stack() {
    if (!state.splitView) {
        editor.setStatus(editor.t("status.unified_view") || "Unified view");
        return;
    }
    state.splitView = false;
    updateMagitDisplay();
    editor.setStatus(editor.t("status.unified_view") || "Unified view");
}
registerHandler("review_layout_stack", review_layout_stack);

async function review_layout_auto() {
    if (state.viewportWidth >= AUTO_SPLIT_MIN_WIDTH) await review_layout_split();
    else review_layout_stack();
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
        " Navigate    n / p      next / prev hunk",
        "             , / .      prev / next file",
        "             ] / [      next / prev comment",
        "             Tab        fold the file under the cursor",
        "             z a / z r  fold all / unfold all",
        " Layout      1 / 2 / 0  split (side-by-side) / stack (unified) / auto",
        " View        a          show / hide inline notes",
        "             /          filter files (empty to clear)",
        "             W          watch: auto-reload when a file is saved",
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
    return state.files.filter(fileMatchesFilter);
}

/** Nearest diff row (add/remove/context) to the cursor, or null if the
 *  current view has no commentable line. Used so `c` off a diff line lands
 *  the user on a real line instead of leaving keystrokes to execute as
 *  commands. */
function nearestDiffRow(): number | null {
    const cur = state.diffCursorRow;
    let best: number | null = null;
    for (const k of Object.keys(state.entryPropsByRow)) {
        const r = Number(k);
        const t = state.entryPropsByRow[r]?.["type"];
        if (t === 'add' || t === 'remove' || t === 'context') {
            if (best === null || Math.abs(r - cur) < Math.abs(best - cur)) best = r;
        }
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
    const next = idx + delta;
    if (next < 0 || next >= vis.length) return;
    state.filesCurrentKey = fileKey(vis[next]);
    updateMagitDisplay();
    jumpDiffCursorToRow(1, { recenter: false });
}
function review_goto_next_file() { review_goto_file(1); }
function review_goto_prev_file() { review_goto_file(-1); }
registerHandler("review_goto_next_file", review_goto_next_file);
registerHandler("review_goto_prev_file", review_goto_prev_file);

// --- File filter (hunk-style `/`) ---
function review_filter_files() {
    const label = editor.t("prompt.filter_files") || "Filter files: ";
    editor.startPromptWithInitial(label, "review-filter", state.fileFilter);
}
registerHandler("review_filter_files", review_filter_files);

editor.on("prompt_confirmed", (args) => {
    if (args.prompt_type !== "review-filter") return true;
    state.fileFilter = (args.input || "").trim();
    ensureFocusFile();
    updateMagitDisplay();
    jumpDiffCursorToRow(1, { recenter: false });
    const vis = visibleFiles().length;
    editor.setStatus(
        state.fileFilter
            ? (editor.t("status.filter_active", { n: String(vis), q: state.fileFilter })
                || `Filter "${state.fileFilter}" — ${vis} file(s)`)
            : (editor.t("status.filter_cleared") || "Filter cleared")
    );
    return true;
});

// --- Watch / auto-reload (hunk-style `--watch`, opt-in via `W`) ---
// When enabled, saving any file in the editor re-runs the diff and rebuilds,
// preserving focus, comments, and folds. Saves are debounced via a
// generation counter (QuickJS has no setTimeout). This keys off the editor's
// own `after_file_save` event rather than a filesystem watch, so it tracks
// edits made in Fresh (the common review-while-editing loop).
let reviewWatchEnabled = false;
let reviewWatchGen = 0;
const WATCH_DEBOUNCE_MS = 200;

function review_toggle_watch() {
    reviewWatchEnabled = !reviewWatchEnabled;
    editor.setStatus(
        reviewWatchEnabled
            ? (editor.t("status.watch_on") || "Watching for changes")
            : (editor.t("status.watch_off") || "Watch off")
    );
}
registerHandler("review_toggle_watch", review_toggle_watch);

editor.on("after_file_save", () => {
    if (!reviewWatchEnabled || state.groupId === null) return true;
    // Range reviews are ref-to-ref; working-tree saves don't change them.
    if (state.mode === 'range') return true;
    const myGen = ++reviewWatchGen;
    void editor.delay(WATCH_DEBOUNCE_MS).then(() => {
        // Superseded by a later save, or the review closed / watch turned
        // off while we waited.
        if (myGen !== reviewWatchGen || !reviewWatchEnabled || state.groupId === null) return;
        void refreshMagitData();
    });
    return true;
});

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
    applyCursorLineOverlay('diff');
    refreshStickyHeader(idx);
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
 * Find the global index in `state.hunks` of the hunk currently visible
 * at the cursor row, scanning the *visible* hunks (i.e. hunks whose
 * file is not collapsed). Returns -1 if no hunk is at or before cursor.
 */
function visibleHunkIndexAtCursor(): number {
    let visibleIdx = -1;
    for (let i = 0; i < state.hunkHeaderRows.length; i++) {
        if (state.hunkHeaderRows[i] <= state.diffCursorRow) visibleIdx = i;
        else break;
    }
    if (visibleIdx < 0) return -1;
    // Map back to the global state.hunks index.
    let visited = 0;
    for (let i = 0; i < state.hunks.length; i++) {
        const h = state.hunks[i];
        if (state.collapsedFiles.has(fileKeyOf(h.file, h.gitStatus || 'unstaged'))) continue;
        if (visited === visibleIdx) return i;
        visited++;
    }
    return -1;
}

function jumpToGlobalHunk(globalIdx: number) {
    if (globalIdx < 0 || globalIdx >= state.hunks.length) return;
    const target = state.hunks[globalIdx];
    const targetFileKey = fileKeyOf(target.file, target.gitStatus || 'unstaged');
    let needRebuild = false;
    // Auto-expand the section, file, AND hunk containing the target so
    // n/p never silently lands on an invisible row.
    if (target.gitStatus && state.collapsedSections.has(target.gitStatus)) {
        state.collapsedSections.delete(target.gitStatus);
        needRebuild = true;
    }
    if (state.collapsedFiles.has(targetFileKey)) {
        state.collapsedFiles.delete(targetFileKey);
        needRebuild = true;
    }
    if (state.collapsedHunks.has(target.id)) {
        state.collapsedHunks.delete(target.id);
        needRebuild = true;
    }
    if (needRebuild) updateMagitDisplay();
    // Look up the target hunk's row directly — much simpler than counting.
    const row = state.hunkRowByHunkId[target.id];
    if (row !== undefined) jumpDiffCursorToRow(row);
}

function review_next_hunk() {
    if (state.groupId === null) return;
    if (state.hunks.length === 0) return;
    const cur = visibleHunkIndexAtCursor();
    // Find next hunk in global order — auto-expanding its file if needed.
    if (cur < 0) {
        jumpToGlobalHunk(0);
        return;
    }
    if (cur + 1 >= state.hunks.length) return;
    jumpToGlobalHunk(cur + 1);
}
registerHandler("review_next_hunk", review_next_hunk);

function review_prev_hunk() {
    if (state.groupId === null) return;
    if (state.hunks.length === 0) return;
    const cur = visibleHunkIndexAtCursor();
    if (cur <= 0) return;
    jumpToGlobalHunk(cur - 1);
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
            if (f.category === 'untracked') {
                await editor.spawnProcess("rm", ["--", f.path]);
            } else {
                await editor.spawnProcess("git", ["checkout", "--", f.path]);
            }
            await refreshMagitData();
            editor.setStatus(`Discarded: ${f.path}`);
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
                editor.setStatus(editor.t("status.hunk_discarded") || "Hunk discarded");
                await refreshMagitData();
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
    await editor.writeFile(filePath, md);
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
    await editor.writeFile(filePath, JSON.stringify(session, null, 2));
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
    state.fileHeaderRows = {};
    state.collapsedFiles = new Set();
    state.collapsedSections = new Set();
    state.collapsedHunks = new Set();
    state.commentsByRow = {};
    state.commentsSelectedRow = 0;
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
        ratio: 0.16,
        first: { type: "scrollable", id: "files" },
        second: {
            type: "split",
            direction: "h",
            ratio: 0.74,
            first: {
                type: "split",
                direction: "v",
                ratio: 0.05,
                first: { type: "fixed", id: "sticky", height: 1 },
                second: { type: "scrollable", id: "diff" },
            },
            second: { type: "scrollable", id: "comments" },
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

    if (state.panelBuffers["diff"] !== undefined) {
        (editor as any).setBufferShowCursors(state.panelBuffers["diff"], true);
    }

    updateMagitDisplay();

    editor.focusBufferGroupPanel(state.groupId!, "diff");

    editor.on("resize", onReviewDiffResize);
    updateReviewStatus();
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
    editor.setStatus(editor.t("status.generating"));

    // Fetch data using the git status approach.
    const status = await getGitStatus();
    state.files = status.files;
    state.emptyState = status.emptyReason;
    state.hunks = await fetchDiffsForFiles(status.files);

    // Persistence setup: worktree mode keyed by repo root.
    state.mode = 'worktree';
    state.range = null;
    state.repoRoot = await detectRepoRoot();
    state.reviewKey = buildReviewKey(state.mode, state.range);

    // Restore persisted comments (if any). We drop orphans so the UI
    // doesn't display comments that no longer point at visible lines.
    const loaded = loadPersistedReview(state.repoRoot, state.reviewKey);
    state.comments = loaded ? pruneOrphanComments(loaded.comments, state.hunks) : [];
    state.note = loaded?.note ?? '';

    resetPerSessionState();
    await openReviewPanels("*Review Diff*");
}
registerHandler("start_review_diff", start_review_diff);

function stop_review_diff() {
    if (state.groupId !== null) {
        editor.closeBufferGroup(state.groupId);
        state.groupId = null;
        state.panelBuffers = {};
    }
    state.reviewBufferId = null;
    reviewWatchEnabled = false;
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
//   - The existing `start_review_branch` commit list (inline, Enter-to-
//     select). Rejected because that view is commit-by-commit and we
//     specifically want a *flattened* diff for batch commenting.
//   - Single prompt with a small suggestion list. Chosen — matches the
//     tone of the existing `start_review_branch` prompt and lets power
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
    const result = await editor.spawnProcess("git", [
        "diff", "--unified=3", `${range.from}..${range.to}`,
    ]);
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
        const exists = await editor.spawnProcess("git", ["rev-parse", "--verify", base]);
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
        const log = await editor.spawnProcess("git", [
            "log", "-n", "5", "--pretty=format:%h %s",
        ]);
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
        editor.setStatus(editor.t("status.cancelled") || "Cancelled");
        return true;
    }
    // Kick off the async bootstrap; the prompt is already dismissed so we
    // can return immediately.
    bootstrapRangeReview(range);
    return true;
});

async function bootstrapRangeReview(range: ReviewRange): Promise<void> {
    editor.setStatus(editor.t("status.generating") || "Generating diff…");
    const { hunks, files } = await fetchRangeDiff(range);
    if (hunks.length === 0) {
        editor.setStatus(
            editor.t("status.review_range_empty", { range: range.label }) ||
                `No changes in ${range.label}`,
        );
        return;
    }
    state.mode = 'range';
    state.range = range;
    state.hunks = hunks;
    state.files = files;
    state.emptyState = null;
    state.repoRoot = await detectRepoRoot();
    state.reviewKey = buildReviewKey(state.mode, state.range);

    // Load persisted comments for this exact range — the diff is static
    // so they always line up.
    const loaded = loadPersistedReview(state.repoRoot, state.reviewKey);
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
    let newPanel: 'diff' | 'comments' | null = null;
    if (data.buffer_id === diffId) newPanel = 'diff';
    else if (data.buffer_id === commentsId) newPanel = 'comments';
    if (newPanel === null || newPanel === state.focusPanel) return;
    state.focusPanel = newPanel;
    // Re-render the comments panel so the selection highlight follows focus.
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("on_review_buffer_activated", on_review_buffer_activated);

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
        state.diffCursorRow = data.line;
        applyCursorLineOverlay('diff');
        // Use the cursor row as a sticky-header anchor too — viewport_changed
        // doesn't always fire reliably for plugin-managed virtual buffers
        // (top_line can be null). Tracking the cursor row gives a snappy
        // "what file am I in" indicator regardless.
        refreshStickyHeader(Math.max(0, data.line - 1));
        updateReviewStatus();
        // Re-render the comments panel only when the highlighted comment
        // actually changes — avoids re-emitting the panel on every
        // cursor tick.
        const newHighlight = currentCommentIdAtCursor();
        if (newHighlight !== prevHighlight) {
            state.commentsHighlightId = newHighlight;
            editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
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
    const relPathResult = await editor.spawnProcess("git", ["-C", fileDir, "ls-files", "--full-name", fileName]);
    let filePath: string;
    if (relPathResult.exit_code === 0 && relPathResult.stdout.trim()) {
        filePath = relPathResult.stdout.trim();
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
        const result = await editor.spawnProcess("git", ["-C", gitRoot, "diff", "--no-index", "--unified=3", "--", "/dev/null", filePath]);
        // git diff --no-index exits with 1 when there are differences, which is expected
        diffOutput = result.stdout || "";
    } else {
        // For tracked files, use normal diff against HEAD
        const result = await editor.spawnProcess("git", ["-C", gitRoot, "diff", "HEAD", "--unified=3", "--", filePath]);
        if (result.exit_code !== 0) {
            editor.setStatus(editor.t("status.failed_git_diff"));
            return;
        }
        diffOutput = result.stdout;
    }

    // Parse hunks from diff output
    const lines = diffOutput.split('\n');
    const fileHunks: Hunk[] = [];
    let currentHunk: Hunk | null = null;

    for (const line of lines) {
        if (line.startsWith('@@')) {
            const match = line.match(/@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@(.*)/);
            if (match) {
                const oldStart = parseInt(match[1]);
                const oldCount = match[2] ? parseInt(match[2]) : 1;
                const newStart = parseInt(match[3]);
                const newCount = match[4] ? parseInt(match[4]) : 1;
                currentHunk = {
                    id: `${filePath}:${newStart}`,
                    file: filePath,
                    range: { start: newStart, end: newStart + newCount - 1 },
                    oldRange: { start: oldStart, end: oldStart + oldCount - 1 },
                    type: isUntracked ? 'add' : 'modify',
                    lines: [],
                    status: 'pending',
                    contextHeader: match[5]?.trim() || "",
                    byteOffset: 0
                };
                fileHunks.push(currentHunk);
            }
        } else if (currentHunk && (line.startsWith('+') || line.startsWith('-') || line.startsWith(' '))) {
            if (!line.startsWith('---') && !line.startsWith('+++')) {
                currentHunk.lines.push(line);
            }
        }
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
    const newContent = await editor.readFile(absolutePath);
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
    const oldLines = oldContent.split('\n');
    const newLines = newContent.split('\n');

    const oldEntries: TextPropertyEntry[] = oldLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

    const newEntries: TextPropertyEntry[] = newLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

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
        newCount: h.range.end - h.range.start + 1
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
// Review PR Branch
//
// A companion view to `start_review_diff` for reviewing the full set of
// commits on a PR branch (rather than just the working-tree changes). It
// opens a buffer group with the commit history on the left (rendered by
// the shared `lib/git_history.ts` helpers the git_log plugin uses) and a
// live-updating `git show` of the selected commit on the right. This reuses
// the same rendering pipeline so both plugins stay visually consistent and
// respect theme keys in one place.
// =============================================================================

interface ReviewBranchState {
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

const branchState: ReviewBranchState = {
    isOpen: false,
    groupId: null,
    logBufferId: null,
    detailBufferId: null,
    commits: [],
    selectedIndex: 0,
    // Empty means "not yet detected"; start_review_branch fills this in
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
    try {
        const r = await editor.spawnProcess("git", [
            "symbolic-ref", "--short", "refs/remotes/origin/HEAD",
        ]);
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
            ]);
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
    const rawHeader = editor.t("panel.review_branch_header", { base: branchState.baseRef });
    const header = (rawHeader && !rawHeader.startsWith("panel.")) ? rawHeader : `Commits (${branchState.baseRef}..HEAD)`;
    const rawFooter = editor.t("panel.review_branch_footer");
    const footer = (rawFooter && !rawFooter.startsWith("panel.")) ? rawFooter : "j/k: navigate · Enter: focus detail · r: refresh · q: close";
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

async function branchRefreshDetail(): Promise<void> {
    if (branchState.groupId === null) return;
    if (branchState.commits.length === 0) {
        const msg = editor.t("status.review_branch_empty") || "No commits in the selected range.";
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
        return;
    }
    const myId = ++branchState.pendingDetailId;
    editor.setPanelContent(
        branchState.groupId,
        "detail",
        buildDetailPlaceholderEntries(
            editor.t("status.loading_commit", { hash: commit.shortHash }) || `Loading ${commit.shortHash}…`,
        ),
    );
    const output = await fetchCommitShow(editor, commit.hash);
    if (myId !== branchState.pendingDetailId) return;
    if (branchState.groupId === null) return;
    branchState.detailCache = { hash: commit.hash, output };
    editor.setPanelContent(
        branchState.groupId,
        "detail",
        buildCommitDetailEntries(commit, output, {}),
    );
}

async function start_review_branch(): Promise<void> {
    if (branchState.isOpen) {
        editor.setStatus(editor.t("status.already_open") || "Review branch already open");
        return;
    }
    // Prompt for the base ref so the user can review any PR, not just
    // one branched off main. The default offered is either what the user
    // picked last time in this session, or the repo's actual default
    // branch (main/master/etc.) on first use.
    const suggested = branchState.baseRef || await detectDefaultBranch();
    const rawPromptText = editor.t("prompt.branch_base", { default: suggested });
    const promptText = (rawPromptText && !rawPromptText.startsWith("prompt."))
        ? rawPromptText
        : `Base ref to compare against (default: ${suggested}):`;
    const input = await editor.prompt(promptText + " ", suggested);
    if (input === null) {
        editor.setStatus(editor.t("status.cancelled") || "Cancelled");
        return;
    }
    const base = input.trim() || suggested;
    branchState.baseRef = base;

    editor.setStatus(editor.t("status.loading") || "Loading commits…");
    branchState.commits = await fetchGitLog(editor, { range: `${base}..HEAD`, maxCommits: 500 });
    if (branchState.commits.length === 0) {
        editor.setStatus(
            editor.t("status.review_branch_empty", { base }) ||
                `No commits in ${base}..HEAD — nothing to review.`,
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
        `*Review Branch ${base}..HEAD*`,
        "review-branch",
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
    editor.on("cursor_moved", on_review_branch_cursor_moved);

    editor.setStatus(
        editor.t("status.review_branch_ready", {
            count: String(branchState.commits.length),
            base,
        }) || `Reviewing ${branchState.commits.length} commits in ${base}..HEAD`,
    );
}
registerHandler("start_review_branch", start_review_branch);

function stop_review_branch(): void {
    if (!branchState.isOpen) return;
    if (branchState.groupId !== null) editor.closeBufferGroup(branchState.groupId);
    editor.off("cursor_moved", on_review_branch_cursor_moved);
    branchState.isOpen = false;
    branchState.groupId = null;
    branchState.logBufferId = null;
    branchState.detailBufferId = null;
    branchState.commits = [];
    branchState.selectedIndex = 0;
    branchState.detailCache = null;
    editor.setStatus(editor.t("status.closed") || "Review branch closed");
}
registerHandler("stop_review_branch", stop_review_branch);

async function review_branch_refresh(): Promise<void> {
    if (!branchState.isOpen) return;
    const base = branchState.baseRef;
    branchState.commits = await fetchGitLog(editor, { range: `${base}..HEAD`, maxCommits: 500 });
    branchState.detailCache = null;
    if (branchState.selectedIndex >= branchState.commits.length) {
        branchState.selectedIndex = Math.max(0, branchState.commits.length - 1);
    }
    branchRenderLog();
    await branchRefreshDetail();
}
registerHandler("review_branch_refresh", review_branch_refresh);

/** Is the detail panel the currently-focused buffer? */
function isReviewBranchDetailFocused(): boolean {
    return (
        branchState.detailBufferId !== null &&
        editor.getActiveBufferId() === branchState.detailBufferId
    );
}

/** The currently-selected commit in the log panel, or null. */
function selectedReviewBranchCommit(): GitCommit | null {
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
function review_branch_enter(): void {
    if (branchState.groupId === null) return;
    if (isReviewBranchDetailFocused()) {
        void review_branch_detail_open_file();
        return;
    }
    editor.focusBufferGroupPanel(branchState.groupId, "detail");
}
registerHandler("review_branch_enter", review_branch_enter);

/**
 * Open the file at the cursor's `(file, line)` text-properties at the
 * currently-selected commit, in a read-only virtual buffer. Mirrors the
 * git-log plugin's `git_log_detail_open_file` so users get the same
 * drill-down from the review-branch detail panel.
 */
async function review_branch_detail_open_file(): Promise<void> {
    if (branchState.detailBufferId === null) return;
    const commit = selectedReviewBranchCommit();
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
    ]);
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
        mode: "review-branch-file-view",
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
    "review_branch_detail_open_file",
    review_branch_detail_open_file,
);

/** Tab: toggle focus between the log and detail panels. */
function review_branch_tab(): void {
    if (branchState.groupId === null) return;
    editor.focusBufferGroupPanel(
        branchState.groupId,
        isReviewBranchDetailFocused() ? "log" : "detail",
    );
}
registerHandler("review_branch_tab", review_branch_tab);

/** q/Escape: focus-back from detail, or close when already on log. */
function review_branch_close_or_back(): void {
    if (branchState.groupId === null) return;
    const active = editor.getActiveBufferId();
    if (branchState.detailBufferId !== null && active === branchState.detailBufferId) {
        editor.focusBufferGroupPanel(branchState.groupId, "log");
        return;
    }
    stop_review_branch();
}
registerHandler("review_branch_close_or_back", review_branch_close_or_back);

function on_review_branch_cursor_moved(data: {
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
registerHandler("on_review_branch_cursor_moved", on_review_branch_cursor_moved);

editor.defineMode(
    "review-branch",
    [
        // vi-style aliases for Up/Down. Everything else (arrows,
        // Page{Up,Down}, Home/End, selection motion, …) is inherited
        // from the Normal keymap via `inheritNormalBindings: true`.
        ["k", "move_up"],
        ["j", "move_down"],
        // Enter: from the log, focus the detail panel; from the detail
        // panel, open the file at the cursor at the selected commit.
        ["Return", "review_branch_enter"],
        // Tab: toggle focus between the log and detail panels.
        ["Tab", "review_branch_tab"],
        ["r", "review_branch_refresh"],
        ["q", "review_branch_close_or_back"],
        ["Escape", "review_branch_close_or_back"],
    ],
    true, // readOnly
    false, // allowTextInput — keeps plain letters from inserting into the RO buffer
    true, // inheritNormalBindings — PageUp/PageDown/arrows/Home/End come from Normal
);

/** Close the file-view virtual buffer opened from the review-branch detail panel. */
function review_branch_file_view_close(): void {
    const id = editor.getActiveBufferId();
    if (id) editor.closeBuffer(id);
}
registerHandler("review_branch_file_view_close", review_branch_file_view_close);

// Mode for the read-only "git show <hash>:<file>" buffer opened from the
// review-branch detail panel. Mirrors git-log's `git-log-file-view`:
// q/Escape close the view, j/k alias Up/Down, and all other Normal
// bindings (arrows, PageUp/Down, Home/End, Ctrl+C copy) are inherited so
// unbound keys don't fall through to edit actions and trip the
// `editing_disabled` status message (see #566).
editor.defineMode(
    "review-branch-file-view",
    [
        ["k", "move_up"],
        ["j", "move_down"],
        ["q", "review_branch_file_view_close"],
        ["Escape", "review_branch_file_view_close"],
    ],
    true, // read-only
    false, // allow_text_input
    true, // inherit Normal-context bindings for unbound keys
);

// Register Modes and Commands
editor.registerCommand("%cmd.review_diff", "%cmd.review_diff_desc", "start_review_diff", null);
editor.registerCommand("%cmd.review_branch", "%cmd.review_branch_desc", "start_review_branch", null);
editor.registerCommand("%cmd.stop_review_branch", "%cmd.stop_review_branch_desc", "stop_review_branch", "review-branch");
editor.registerCommand("%cmd.refresh_review_branch", "%cmd.refresh_review_branch_desc", "review_branch_refresh", "review-branch");
editor.registerCommand("%cmd.stop_review_diff", "%cmd.stop_review_diff_desc", "stop_review_diff", "review-mode");
editor.registerCommand("%cmd.refresh_review_diff", "%cmd.refresh_review_diff_desc", "review_refresh", "review-mode");
editor.registerCommand("%cmd.side_by_side_diff", "%cmd.side_by_side_diff_desc", "side_by_side_diff_current_file", null);

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

editor.defineMode("review-mode", [
    // Native cursor motion in the unified diff stream.
    ["Up", "review_nav_up"], ["Down", "review_nav_down"],
    ["k", "review_nav_up"], ["j", "review_nav_down"],
    ["PageUp", "review_page_up"], ["PageDown", "review_page_down"],
    // Home / End — match the editor's normal-mode defaults so users
    // get the same start-of-line / end-of-line behavior they're used
    // to. Mode bindings replace globals, so we must bind these
    // explicitly even though the actions are built-in.
    ["Home", "move_line_start"], ["End", "move_line_end"],
    // Hunk navigation across the unified stream.
    ["n", "review_next_hunk"], ["p", "review_prev_hunk"],
    // File navigation (hunk-style): focus the prev / next file.
    [",", "review_goto_prev_file"], [".", "review_goto_next_file"],
    // Layout toggle (hunk-style): 1 = split (side-by-side of the file
    // under the cursor), 2 = stack (unified), 0 = auto by terminal width.
    ["1", "review_layout_split"],
    ["2", "review_layout_stack"],
    ["0", "review_layout_auto"],
    // Toggle inline review-note visibility; filter files; watch; help.
    ["a", "review_toggle_agent_notes"],
    ["/", "review_filter_files"],
    ["W", "review_toggle_watch"],
    ["?", "review_help"],
    // Per-file collapse: Tab toggles the file under the cursor;
    // `z a` collapses every file; `z r` reveals (expands) every file.
    ["Tab", "review_toggle_file_collapse"],
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
], true);

editor.debug("Review Diff plugin loaded with review comments support");
