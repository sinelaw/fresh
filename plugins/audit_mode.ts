// Review Diff Plugin
// Provides a unified workflow for reviewing code changes (diffs, conflicts, AI outputs).

/// <reference path="./lib/fresh.d.ts" />
/// <reference path="./lib/types.ts" />
/// <reference path="./lib/virtual-buffer-factory.ts" />

import { VirtualBufferFactory } from "./lib/virtual-buffer-factory.ts";

/**
 * Hunk status for staging
 */
type HunkStatus = 'pending' | 'staged' | 'discarded';

/**
 * Review status for a hunk
 */
type ReviewStatus = 'pending' | 'approved' | 'needs_changes' | 'rejected' | 'question';

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
  status: HunkStatus;
  reviewStatus: ReviewStatus;
  contextHeader: string;
  byteOffset: number; // Position in the virtual buffer
}

/**
 * Review Session State
 */
interface ReviewState {
  hunks: Hunk[];
  hunkStatus: Record<string, HunkStatus>;
  comments: ReviewComment[];
  originalRequest?: string;
  overallFeedback?: string;
  reviewBufferId: number | null;
}

const state: ReviewState = {
  hunks: [],
  hunkStatus: {},
  comments: [],
  reviewBufferId: null,
};

// --- Refresh State ---
let isUpdating = false;

// --- Colors & Styles ---
const STYLE_BORDER: [number, number, number] = [70, 70, 70]; 
const STYLE_HEADER: [number, number, number] = [120, 120, 255]; 
const STYLE_FILE_NAME: [number, number, number] = [220, 220, 100]; 
const STYLE_ADD_BG: [number, number, number] = [40, 100, 40]; // Brighter Green BG
const STYLE_REMOVE_BG: [number, number, number] = [100, 40, 40]; // Brighter Red BG
const STYLE_ADD_TEXT: [number, number, number] = [150, 255, 150]; // Very Bright Green
const STYLE_REMOVE_TEXT: [number, number, number] = [255, 150, 150]; // Very Bright Red
const STYLE_STAGED: [number, number, number] = [100, 100, 100];
const STYLE_DISCARDED: [number, number, number] = [120, 60, 60];
const STYLE_COMMENT: [number, number, number] = [180, 180, 100]; // Yellow for comments
const STYLE_COMMENT_BORDER: [number, number, number] = [100, 100, 60];
const STYLE_APPROVED: [number, number, number] = [100, 200, 100]; // Green checkmark
const STYLE_REJECTED: [number, number, number] = [200, 100, 100]; // Red X
const STYLE_QUESTION: [number, number, number] = [200, 200, 100]; // Yellow ?

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

// --- Diff Logic ---

interface DiffPart {
    text: string;
    type: 'added' | 'removed' | 'unchanged';
}

function diffStrings(oldStr: string, newStr: string): DiffPart[] {
    const n = oldStr.length;
    const m = newStr.length;
    const dp: number[][] = Array.from({ length: n + 1 }, () => new Array(m + 1).fill(0));

    for (let i = 1; i <= n; i++) {
        for (let j = 1; j <= m; j++) {
            if (oldStr[i - 1] === newStr[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }

    const result: DiffPart[] = [];
    let i = n, j = m;
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && oldStr[i - 1] === newStr[j - 1]) {
            result.unshift({ text: oldStr[i - 1], type: 'unchanged' });
            i--; j--;
        } else if (j > 0 && (i === 0 || dp[i][j - 1] >= dp[i - 1][j])) {
            result.unshift({ text: newStr[j - 1], type: 'added' });
            j--;
        } else {
            result.unshift({ text: oldStr[i - 1], type: 'removed' });
            i--;
        }
    }

    const coalesced: DiffPart[] = [];
    for (const part of result) {
        const last = coalesced[coalesced.length - 1];
        if (last && last.type === part.type) {
            last.text += part.text;
        } else {
            coalesced.push(part);
        }
    }
    return coalesced;
}

async function getGitDiff(): Promise<Hunk[]> {
    const result = await editor.spawnProcess("git", ["diff", "HEAD", "--unified=3"]);
    if (result.exit_code !== 0) return [];

    const lines = result.stdout.split('\n');
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
                    id: `${currentFile}:${newStart}`,
                    file: currentFile,
                    range: { start: newStart, end: newStart },
                    oldRange: { start: oldStart, end: oldStart },
                    type: 'modify',
                    lines: [],
                    status: 'pending',
                    reviewStatus: 'pending',
                    contextHeader: match[3]?.trim() || "",
                    byteOffset: 0
                };
                hunks.push(currentHunk);
            }
        } else if (currentHunk && (line.startsWith('+') || line.startsWith('-') || line.startsWith(' '))) {
            if (!line.startsWith('---') && !line.startsWith('+++')) {
                 currentHunk.lines.push(line);
            }
        }
    }
    return hunks;
}

interface HighlightTask {
    range: [number, number];
    fg: [number, number, number];
    bg?: [number, number, number];
    bold?: boolean;
    italic?: boolean;
    extend_to_line_end?: boolean;
}

/**
 * Render the Review Stream buffer content and return highlight tasks
 */
async function renderReviewStream(): Promise<{ entries: TextPropertyEntry[], highlights: HighlightTask[] }> {
  const entries: TextPropertyEntry[] = [];
  const highlights: HighlightTask[] = [];
  let currentFile = "";
  let currentByte = 0;

  // Add help header with keybindings at the TOP
  const helpHeader = "╔══════════════════════════════════════════════════════════════════════════╗\n";
  const helpLen0 = getByteLength(helpHeader);
  entries.push({ text: helpHeader, properties: { type: "help" } });
  highlights.push({ range: [currentByte, currentByte + helpLen0], fg: STYLE_COMMENT_BORDER });
  currentByte += helpLen0;

  const helpLine1 = "║ REVIEW: [c]omment [a]pprove [x]reject [!]changes [?]question [u]ndo       ║\n";
  const helpLen1 = getByteLength(helpLine1);
  entries.push({ text: helpLine1, properties: { type: "help" } });
  highlights.push({ range: [currentByte, currentByte + helpLen1], fg: STYLE_COMMENT });
  currentByte += helpLen1;

  const helpLine2 = "║ STAGE:  [s]tage [d]iscard | NAV: [n]ext [p]rev [Enter]drill [q]uit        ║\n";
  const helpLen2 = getByteLength(helpLine2);
  entries.push({ text: helpLine2, properties: { type: "help" } });
  highlights.push({ range: [currentByte, currentByte + helpLen2], fg: STYLE_COMMENT });
  currentByte += helpLen2;

  const helpLine3 = "║ EXPORT: [E] .review/session.md | [O]verall feedback | [r]efresh           ║\n";
  const helpLen3 = getByteLength(helpLine3);
  entries.push({ text: helpLine3, properties: { type: "help" } });
  highlights.push({ range: [currentByte, currentByte + helpLen3], fg: STYLE_COMMENT });
  currentByte += helpLen3;

  const helpFooter = "╚══════════════════════════════════════════════════════════════════════════╝\n\n";
  const helpLen4 = getByteLength(helpFooter);
  entries.push({ text: helpFooter, properties: { type: "help" } });
  highlights.push({ range: [currentByte, currentByte + helpLen4], fg: STYLE_COMMENT_BORDER });
  currentByte += helpLen4;

  for (let hunkIndex = 0; hunkIndex < state.hunks.length; hunkIndex++) {
    const hunk = state.hunks[hunkIndex];
    if (hunk.file !== currentFile) {
      // Header & Border
      const titlePrefix = "┌─ ";
      const titleLine = `${titlePrefix}${hunk.file} ${"─".repeat(Math.max(0, 60 - hunk.file.length))}\n`;
      const titleLen = getByteLength(titleLine);
      entries.push({ text: titleLine, properties: { type: "banner", file: hunk.file } });
      highlights.push({ range: [currentByte, currentByte + titleLen], fg: STYLE_BORDER });
      const prefixLen = getByteLength(titlePrefix);
      highlights.push({ range: [currentByte + prefixLen, currentByte + prefixLen + getByteLength(hunk.file)], fg: STYLE_FILE_NAME, bold: true });
      currentByte += titleLen;
      currentFile = hunk.file;
    }

    hunk.byteOffset = currentByte;

    // Status icons: staging (left) and review (right)
    const stagingIcon = hunk.status === 'staged' ? '✓' : (hunk.status === 'discarded' ? '✗' : ' ');
    const reviewIcon = hunk.reviewStatus === 'approved' ? '✓' :
                       hunk.reviewStatus === 'rejected' ? '✗' :
                       hunk.reviewStatus === 'needs_changes' ? '!' :
                       hunk.reviewStatus === 'question' ? '?' : ' ';
    const reviewLabel = hunk.reviewStatus !== 'pending' ? ` ← ${hunk.reviewStatus.toUpperCase()}` : '';

    const headerPrefix = "│ ";
    const headerText = `${headerPrefix}${stagingIcon} ${reviewIcon} [ ${hunk.contextHeader} ]${reviewLabel}\n`;
    const headerLen = getByteLength(headerText);

    let hunkColor = STYLE_HEADER;
    if (hunk.status === 'staged') hunkColor = STYLE_STAGED;
    else if (hunk.status === 'discarded') hunkColor = STYLE_DISCARDED;

    let reviewColor = STYLE_HEADER;
    if (hunk.reviewStatus === 'approved') reviewColor = STYLE_APPROVED;
    else if (hunk.reviewStatus === 'rejected') reviewColor = STYLE_REJECTED;
    else if (hunk.reviewStatus === 'needs_changes') reviewColor = STYLE_QUESTION;
    else if (hunk.reviewStatus === 'question') reviewColor = STYLE_QUESTION;

    entries.push({ text: headerText, properties: { type: "header", hunkId: hunk.id, index: hunkIndex } });
    highlights.push({ range: [currentByte, currentByte + headerLen], fg: STYLE_BORDER });
    const headerPrefixLen = getByteLength(headerPrefix);
    // Staging icon
    highlights.push({ range: [currentByte + headerPrefixLen, currentByte + headerPrefixLen + getByteLength(stagingIcon)], fg: hunkColor, bold: true });
    // Review icon
    highlights.push({ range: [currentByte + headerPrefixLen + getByteLength(stagingIcon) + 1, currentByte + headerPrefixLen + getByteLength(stagingIcon) + 1 + getByteLength(reviewIcon)], fg: reviewColor, bold: true });
    // Context header
    const contextStart = currentByte + headerPrefixLen + getByteLength(stagingIcon) + 1 + getByteLength(reviewIcon) + 3;
    highlights.push({ range: [contextStart, currentByte + headerLen - getByteLength(reviewLabel) - 2], fg: hunkColor });
    // Review label
    if (reviewLabel) {
      highlights.push({ range: [currentByte + headerLen - getByteLength(reviewLabel) - 1, currentByte + headerLen - 1], fg: reviewColor, bold: true });
    }
    currentByte += headerLen;

    // Track actual file line numbers as we iterate
    let oldLineNum = hunk.oldRange.start;
    let newLineNum = hunk.range.start;

    for (let i = 0; i < hunk.lines.length; i++) {
        const line = hunk.lines[i];
        const nextLine = hunk.lines[i + 1];
        const marker = line[0];
        const content = line.substring(1);
        const linePrefix = "│   ";
        const lineText = `${linePrefix}${marker} ${content}\n`;
        const lineLen = getByteLength(lineText);
        const prefixLen = getByteLength(linePrefix);

        // Determine line type and which line numbers apply
        const lineType: 'add' | 'remove' | 'context' =
            marker === '+' ? 'add' : marker === '-' ? 'remove' : 'context';
        const curOldLine = lineType !== 'add' ? oldLineNum : undefined;
        const curNewLine = lineType !== 'remove' ? newLineNum : undefined;

        if (line.startsWith('-') && nextLine && nextLine.startsWith('+') && hunk.status === 'pending') {
            const oldContent = line.substring(1);
            const newContent = nextLine.substring(1);
            const diffParts = diffStrings(oldContent, newContent);

            // Removed
            entries.push({ text: lineText, properties: {
                type: "content", hunkId: hunk.id, file: hunk.file,
                lineType: 'remove', oldLine: curOldLine, lineContent: line
            } });
            highlights.push({ range: [currentByte, currentByte + lineLen], fg: STYLE_BORDER });
            highlights.push({ range: [currentByte + prefixLen, currentByte + prefixLen + 1], fg: STYLE_REMOVE_TEXT, bold: true });
            
            let cbOffset = currentByte + prefixLen + 2; 
            diffParts.forEach(p => {
                const pLen = getByteLength(p.text);
                if (p.type === 'removed') {
                    highlights.push({ range: [cbOffset, cbOffset + pLen], fg: STYLE_REMOVE_TEXT, bg: STYLE_REMOVE_BG, bold: true });
                    cbOffset += pLen;
                } else if (p.type === 'unchanged') {
                    highlights.push({ range: [cbOffset, cbOffset + pLen], fg: STYLE_REMOVE_TEXT });
                    cbOffset += pLen;
                }
            });
            currentByte += lineLen;

            // Added (increment old line for the removed line we just processed)
            oldLineNum++;
            const nextLineText = `${linePrefix}+ ${nextLine.substring(1)}\n`;
            const nextLineLen = getByteLength(nextLineText);
            entries.push({ text: nextLineText, properties: {
                type: "content", hunkId: hunk.id, file: hunk.file,
                lineType: 'add', newLine: newLineNum, lineContent: nextLine
            } });
            newLineNum++;
            highlights.push({ range: [currentByte, currentByte + nextLineLen], fg: STYLE_BORDER });
            highlights.push({ range: [currentByte + prefixLen, currentByte + prefixLen + 1], fg: STYLE_ADD_TEXT, bold: true });

            cbOffset = currentByte + prefixLen + 2; 
            diffParts.forEach(p => {
                const pLen = getByteLength(p.text);
                if (p.type === 'added') {
                    highlights.push({ range: [cbOffset, cbOffset + pLen], fg: STYLE_ADD_TEXT, bg: STYLE_ADD_BG, bold: true });
                    cbOffset += pLen;
                } else if (p.type === 'unchanged') {
                    highlights.push({ range: [cbOffset, cbOffset + pLen], fg: STYLE_ADD_TEXT });
                    cbOffset += pLen;
                }
            });
            currentByte += nextLineLen;

            // Render comments for the removed line (curOldLine before increment)
            const removedLineComments = state.comments.filter(c =>
                c.hunk_id === hunk.id && c.line_type === 'remove' && c.old_line === curOldLine
            );
            for (const comment of removedLineComments) {
                const commentPrefix = `│   » [-${comment.old_line}] `;
                const commentLines = comment.text.split('\n');
                for (let ci = 0; ci < commentLines.length; ci++) {
                    const prefix = ci === 0 ? commentPrefix : "│      ";
                    const commentLine = `${prefix}${commentLines[ci]}\n`;
                    const commentLineLen = getByteLength(commentLine);
                    entries.push({ text: commentLine, properties: { type: "comment", commentId: comment.id, hunkId: hunk.id } });
                    highlights.push({ range: [currentByte, currentByte + getByteLength(prefix)], fg: STYLE_COMMENT_BORDER });
                    highlights.push({ range: [currentByte + getByteLength(prefix), currentByte + commentLineLen], fg: STYLE_COMMENT });
                    currentByte += commentLineLen;
                }
            }

            // Render comments for the added line (newLineNum - 1, since we already incremented)
            const addedLineComments = state.comments.filter(c =>
                c.hunk_id === hunk.id && c.line_type === 'add' && c.new_line === (newLineNum - 1)
            );
            for (const comment of addedLineComments) {
                const commentPrefix = `│   » [+${comment.new_line}] `;
                const commentLines = comment.text.split('\n');
                for (let ci = 0; ci < commentLines.length; ci++) {
                    const prefix = ci === 0 ? commentPrefix : "│      ";
                    const commentLine = `${prefix}${commentLines[ci]}\n`;
                    const commentLineLen = getByteLength(commentLine);
                    entries.push({ text: commentLine, properties: { type: "comment", commentId: comment.id, hunkId: hunk.id } });
                    highlights.push({ range: [currentByte, currentByte + getByteLength(prefix)], fg: STYLE_COMMENT_BORDER });
                    highlights.push({ range: [currentByte + getByteLength(prefix), currentByte + commentLineLen], fg: STYLE_COMMENT });
                    currentByte += commentLineLen;
                }
            }

            i++;
        } else {
            entries.push({ text: lineText, properties: {
                type: "content", hunkId: hunk.id, file: hunk.file,
                lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line
            } });
            highlights.push({ range: [currentByte, currentByte + lineLen], fg: STYLE_BORDER });
            if (hunk.status === 'pending') {
                if (line.startsWith('+')) {
                    highlights.push({ range: [currentByte + prefixLen, currentByte + prefixLen + 1], fg: STYLE_ADD_TEXT, bold: true });
                    highlights.push({ range: [currentByte + prefixLen + 2, currentByte + lineLen], fg: STYLE_ADD_TEXT });
                } else if (line.startsWith('-')) {
                    highlights.push({ range: [currentByte + prefixLen, currentByte + prefixLen + 1], fg: STYLE_REMOVE_TEXT, bold: true });
                    highlights.push({ range: [currentByte + prefixLen + 2, currentByte + lineLen], fg: STYLE_REMOVE_TEXT });
                }
            } else {
                highlights.push({ range: [currentByte + prefixLen, currentByte + lineLen], fg: hunkColor });
            }
            currentByte += lineLen;

            // Increment line counters based on line type
            if (lineType === 'remove') oldLineNum++;
            else if (lineType === 'add') newLineNum++;
            else { oldLineNum++; newLineNum++; } // context

            // Render any comments attached to this specific line
            const lineComments = state.comments.filter(c =>
                c.hunk_id === hunk.id && (
                    (lineType === 'remove' && c.old_line === curOldLine) ||
                    (lineType === 'add' && c.new_line === curNewLine) ||
                    (lineType === 'context' && (c.old_line === curOldLine || c.new_line === curNewLine))
                )
            );
            for (const comment of lineComments) {
                const lineRef = comment.line_type === 'add'
                    ? `+${comment.new_line}`
                    : comment.line_type === 'remove'
                    ? `-${comment.old_line}`
                    : `${comment.new_line}`;
                const commentPrefix = `│   » [${lineRef}] `;
                const commentLines = comment.text.split('\n');
                for (let ci = 0; ci < commentLines.length; ci++) {
                    const prefix = ci === 0 ? commentPrefix : "│      ";
                    const commentLine = `${prefix}${commentLines[ci]}\n`;
                    const commentLineLen = getByteLength(commentLine);
                    entries.push({ text: commentLine, properties: { type: "comment", commentId: comment.id, hunkId: hunk.id } });
                    highlights.push({ range: [currentByte, currentByte + getByteLength(prefix)], fg: STYLE_COMMENT_BORDER });
                    highlights.push({ range: [currentByte + getByteLength(prefix), currentByte + commentLineLen], fg: STYLE_COMMENT });
                    currentByte += commentLineLen;
                }
            }
        }
    }

    // Render any comments without specific line info at the end of hunk
    const orphanComments = state.comments.filter(c =>
        c.hunk_id === hunk.id && !c.old_line && !c.new_line
    );
    if (orphanComments.length > 0) {
      const commentBorder = "│   ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄\n";
      const borderLen = getByteLength(commentBorder);
      entries.push({ text: commentBorder, properties: { type: "comment-border" } });
      highlights.push({ range: [currentByte, currentByte + borderLen], fg: STYLE_COMMENT_BORDER });
      currentByte += borderLen;

      for (const comment of orphanComments) {
        const commentPrefix = "│   » ";
        const commentLines = comment.text.split('\n');
        for (let ci = 0; ci < commentLines.length; ci++) {
          const prefix = ci === 0 ? commentPrefix : "│      ";
          const commentLine = `${prefix}${commentLines[ci]}\n`;
          const commentLineLen = getByteLength(commentLine);
          entries.push({ text: commentLine, properties: { type: "comment", commentId: comment.id, hunkId: hunk.id } });
          highlights.push({ range: [currentByte, currentByte + getByteLength(prefix)], fg: STYLE_COMMENT_BORDER });
          highlights.push({ range: [currentByte + getByteLength(prefix), currentByte + commentLineLen], fg: STYLE_COMMENT });
          currentByte += commentLineLen;
        }
      }

      entries.push({ text: commentBorder, properties: { type: "comment-border" } });
      highlights.push({ range: [currentByte, currentByte + borderLen], fg: STYLE_COMMENT_BORDER });
      currentByte += borderLen;
    }

    const isLastOfFile = hunkIndex === state.hunks.length - 1 || state.hunks[hunkIndex + 1].file !== hunk.file;
    if (isLastOfFile) {
        const bottomLine = `└${"─".repeat(64)}\n`;
        const bottomLen = getByteLength(bottomLine);
        entries.push({ text: bottomLine, properties: { type: "border" } });
        highlights.push({ range: [currentByte, currentByte + bottomLen], fg: STYLE_BORDER });
        currentByte += bottomLen;
    }
  }

  if (entries.length === 0) {
    entries.push({ text: "No changes to review.\n", properties: {} });
  } else {
    // Add help footer with keybindings
    const helpSeparator = "\n" + "─".repeat(70) + "\n";
    const helpLen1 = getByteLength(helpSeparator);
    entries.push({ text: helpSeparator, properties: { type: "help" } });
    highlights.push({ range: [currentByte, currentByte + helpLen1], fg: STYLE_BORDER });
    currentByte += helpLen1;

    const helpLine1 = "REVIEW: [c]omment  [a]pprove  [x]reject  [!]needs-changes  [?]question  [u]ndo\n";
    const helpLen2 = getByteLength(helpLine1);
    entries.push({ text: helpLine1, properties: { type: "help" } });
    highlights.push({ range: [currentByte, currentByte + helpLen2], fg: STYLE_COMMENT });
    currentByte += helpLen2;

    const helpLine2 = "STAGE:  [s]tage    [d]iscard  |  NAV: [n]ext [p]rev [Enter]drill-down  [q]uit\n";
    const helpLen3 = getByteLength(helpLine2);
    entries.push({ text: helpLine2, properties: { type: "help" } });
    highlights.push({ range: [currentByte, currentByte + helpLen3], fg: STYLE_COMMENT });
    currentByte += helpLen3;

    const helpLine3 = "EXPORT: [E]xport to .review/session.md  |  [O]verall feedback  [r]efresh\n";
    const helpLen4 = getByteLength(helpLine3);
    entries.push({ text: helpLine3, properties: { type: "help" } });
    highlights.push({ range: [currentByte, currentByte + helpLen4], fg: STYLE_COMMENT });
    currentByte += helpLen4;
  }
  return { entries, highlights };
}

/**
 * Updates the buffer UI (text and highlights) based on current state.hunks
 */
async function updateReviewUI() {
  if (state.reviewBufferId !== null) {
    const { entries, highlights } = await renderReviewStream();
    editor.setVirtualBufferContent(state.reviewBufferId, entries);
    
    editor.clearNamespace(state.reviewBufferId, "review-diff");
    highlights.forEach((h) => {
        const bg = h.bg || [-1, -1, -1];
        // addOverlay signature: bufferId, namespace, start, end, r, g, b, underline, bold, italic, bg_r, bg_g, bg_b
        editor.addOverlay(
            state.reviewBufferId!,
            "review-diff",
            h.range[0],
            h.range[1],
            h.fg[0], h.fg[1], h.fg[2],  // foreground color
            false,                       // underline
            h.bold || false,             // bold
            h.italic || false,           // italic
            bg[0], bg[1], bg[2]          // background color
        );
    });
  }
}

/**
 * Fetches latest diff data and refreshes the UI
 */
async function refreshReviewData() {
    if (isUpdating) return;
    isUpdating = true;
    editor.setStatus("Refreshing review diff...");
    try {
        const newHunks = await getGitDiff();
        newHunks.forEach(h => h.status = state.hunkStatus[h.id] || 'pending');
        state.hunks = newHunks;
        await updateReviewUI();
        editor.setStatus(`Review diff updated. Found ${state.hunks.length} hunks.`);
    } catch (e) {
        editor.debug(`ReviewDiff Error: ${e}`);
    } finally {
        isUpdating = false;
    }
}

// --- Actions ---

globalThis.review_stage_hunk = async () => {
    const props = editor.getTextPropertiesAtCursor(editor.getActiveBufferId());
    if (props.length > 0 && props[0].hunkId) {
        const id = props[0].hunkId as string;
        state.hunkStatus[id] = 'staged';
        const h = state.hunks.find(x => x.id === id);
        if (h) h.status = 'staged';
        await updateReviewUI();
    }
};

globalThis.review_discard_hunk = async () => {
    const props = editor.getTextPropertiesAtCursor(editor.getActiveBufferId());
    if (props.length > 0 && props[0].hunkId) {
        const id = props[0].hunkId as string;
        state.hunkStatus[id] = 'discarded';
        const h = state.hunks.find(x => x.id === id);
        if (h) h.status = 'discarded';
        await updateReviewUI();
    }
};

globalThis.review_undo_action = async () => {
    const props = editor.getTextPropertiesAtCursor(editor.getActiveBufferId());
    if (props.length > 0 && props[0].hunkId) {
        const id = props[0].hunkId as string;
        state.hunkStatus[id] = 'pending';
        const h = state.hunks.find(x => x.id === id);
        if (h) h.status = 'pending';
        await updateReviewUI();
    }
};

globalThis.review_next_hunk = () => {
    const bid = editor.getActiveBufferId();
    const props = editor.getTextPropertiesAtCursor(bid);
    let cur = -1;
    if (props.length > 0 && props[0].index !== undefined) cur = props[0].index as number;
    if (cur + 1 < state.hunks.length) editor.setBufferCursor(bid, state.hunks[cur + 1].byteOffset);
};

globalThis.review_prev_hunk = () => {
    const bid = editor.getActiveBufferId();
    const props = editor.getTextPropertiesAtCursor(bid);
    let cur = state.hunks.length;
    if (props.length > 0 && props[0].index !== undefined) cur = props[0].index as number;
    if (cur - 1 >= 0) editor.setBufferCursor(bid, state.hunks[cur - 1].byteOffset);
};

globalThis.review_refresh = () => { refreshReviewData(); };

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

globalThis.on_viewport_changed = (data: any) => {
    // This handler is now a no-op - scroll sync is handled by the core
    // using the anchor-based ScrollSyncGroup system.
    // Keeping the handler for backward compatibility if core sync fails.
    if (!activeDiffViewState || !activeSideBySideState) return;

    // Skip if core scroll sync is active (we have a scrollSyncGroupId)
    if (activeSideBySideState.scrollSyncGroupId !== null) return;

    const { oldSplitId, newSplitId, oldLineByteOffsets, newLineByteOffsets } = activeSideBySideState;

    if (data.split_id === oldSplitId && newLineByteOffsets.length > 0) {
        // OLD pane scrolled - find which line it's on and sync NEW pane to same line
        const lineNum = findLineForByte(oldLineByteOffsets, data.top_byte);
        const targetByte = newLineByteOffsets[Math.min(lineNum, newLineByteOffsets.length - 1)];
        (editor as any).setSplitScroll(newSplitId, targetByte);
    } else if (data.split_id === newSplitId && oldLineByteOffsets.length > 0) {
        // NEW pane scrolled - find which line it's on and sync OLD pane to same line
        const lineNum = findLineForByte(newLineByteOffsets, data.top_byte);
        const targetByte = oldLineByteOffsets[Math.min(lineNum, oldLineByteOffsets.length - 1)];
        (editor as any).setSplitScroll(oldSplitId, targetByte);
    }
};

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
            fg: [120, 120, 120]  // Gray line numbers
        });

        if (isFiller) {
            // Filler styling - extend to full line width
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: [60, 60, 60],
                bg: [30, 30, 30],
                extend_to_line_end: true
            });
        } else if (line.changeType === 'added' && side === 'new') {
            // Added line (green) - extend to full line width
            highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_ADD_TEXT, bold: true }); // gutter marker
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: STYLE_ADD_TEXT,
                bg: [30, 50, 30],
                extend_to_line_end: true
            });
        } else if (line.changeType === 'removed' && side === 'old') {
            // Removed line (red) - extend to full line width
            highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_REMOVE_TEXT, bold: true }); // gutter marker
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: STYLE_REMOVE_TEXT,
                bg: [50, 30, 30],
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

globalThis.review_drill_down = async () => {
    const bid = editor.getActiveBufferId();
    const props = editor.getTextPropertiesAtCursor(bid);
    if (props.length > 0 && props[0].hunkId) {
        const id = props[0].hunkId as string;
        const h = state.hunks.find(x => x.id === id);
        if (!h) return;

        editor.setStatus("Loading side-by-side diff...");

        // Get all hunks for this file
        const fileHunks = state.hunks.filter(hunk => hunk.file === h.file);

        // Get old (HEAD) and new (working) file content
        const gitShow = await editor.spawnProcess("git", ["show", `HEAD:${h.file}`]);
        if (gitShow.exit_code !== 0) {
            editor.setStatus("Failed to load old file version");
            return;
        }
        const oldContent = gitShow.stdout;

        // Read new file content
        let newContent: string;
        try {
            newContent = await editor.readFile(h.file);
        } catch (e) {
            editor.setStatus("Failed to load new file version");
            return;
        }

        // Close the Review Diff buffer to make room for side-by-side view
        // Store the review buffer ID so we can restore it later
        const reviewBufferId = bid;

        // Compute aligned diff for the FULL file with all hunks
        const alignedLines = computeFullFileAlignedDiff(oldContent, newContent, fileHunks);

        // Generate content for both panes
        const oldPane = generateDiffPaneContent(alignedLines, 'old');
        const newPane = generateDiffPaneContent(alignedLines, 'new');

        // Close any existing side-by-side views
        if (activeSideBySideState) {
            try {
                // Remove scroll sync group first
                if (activeSideBySideState.scrollSyncGroupId !== null) {
                    (editor as any).removeScrollSyncGroup(activeSideBySideState.scrollSyncGroupId);
                }
                editor.closeBuffer(activeSideBySideState.oldBufferId);
                editor.closeBuffer(activeSideBySideState.newBufferId);
            } catch {}
            activeSideBySideState = null;
        }

        // Get the current split ID before closing the Review Diff buffer
        const currentSplitId = (editor as any).getActiveSplitId();

        // Create OLD buffer in the CURRENT split (replaces Review Diff)
        const oldBufferId = await editor.createVirtualBufferInExistingSplit({
            name: `[OLD] ${h.file}`,
            mode: "diff-view",
            read_only: true,
            editing_disabled: true,
            entries: oldPane.entries,
            split_id: currentSplitId,
            show_line_numbers: false,
            line_wrap: false
        });
        const oldSplitId = currentSplitId;

        // Close the Review Diff buffer after showing OLD
        editor.closeBuffer(reviewBufferId);

        // Apply highlights to old pane
        editor.clearNamespace(oldBufferId, "diff-view");
        for (const hl of oldPane.highlights) {
            const bg = hl.bg || [-1, -1, -1];
            editor.addOverlay(
                oldBufferId, "diff-view",
                hl.range[0], hl.range[1],
                hl.fg[0], hl.fg[1], hl.fg[2],
                false, hl.bold || false, false,
                bg[0], bg[1], bg[2],
                hl.extend_to_line_end || false
            );
        }

        // Step 2: Create NEW pane in a vertical split (RIGHT of OLD)
        const newRes = await editor.createVirtualBufferInSplit({
            name: `[NEW] ${h.file}`,
            mode: "diff-view",
            read_only: true,
            editing_disabled: true,
            entries: newPane.entries,
            ratio: 0.5,
            direction: "vertical",
            show_line_numbers: false,
            line_wrap: false
        });
        const newBufferId = newRes.buffer_id;
        const newSplitId = newRes.split_id!;

        // Apply highlights to new pane
        editor.clearNamespace(newBufferId, "diff-view");
        for (const hl of newPane.highlights) {
            const bg = hl.bg || [-1, -1, -1];
            editor.addOverlay(
                newBufferId, "diff-view",
                hl.range[0], hl.range[1],
                hl.fg[0], hl.fg[1], hl.fg[2],
                false, hl.bold || false, false,
                bg[0], bg[1], bg[2],
                hl.extend_to_line_end || false
            );
        }

        // Focus OLD pane (left) - convention is to start on old side
        (editor as any).focusSplit(oldSplitId);

        // Set up core-handled scroll sync using the new anchor-based API
        // This replaces the old viewport_changed hook approach
        let scrollSyncGroupId: number | null = null;
        try {
            // Generate a unique group ID
            scrollSyncGroupId = nextScrollSyncGroupId++;
            (editor as any).createScrollSyncGroup(scrollSyncGroupId, oldSplitId, newSplitId);

            // Compute sync anchors from aligned lines
            // Each aligned line is a sync point - we map line indices to anchors
            // For the new core sync, we use line numbers (not byte offsets)
            const anchors: [number, number][] = [];
            for (let i = 0; i < alignedLines.length; i++) {
                // Add anchors at meaningful boundaries: start of file, and at change boundaries
                const line = alignedLines[i];
                const prevLine = i > 0 ? alignedLines[i - 1] : null;

                // Add anchor at start of file
                if (i === 0) {
                    anchors.push([0, 0]);
                }

                // Add anchor at change boundaries (when change type changes)
                if (prevLine && prevLine.changeType !== line.changeType) {
                    anchors.push([i, i]);
                }
            }

            // Add anchor at end
            if (alignedLines.length > 0) {
                anchors.push([alignedLines.length, alignedLines.length]);
            }

            (editor as any).setScrollSyncAnchors(scrollSyncGroupId, anchors);
        } catch (e) {
            editor.debug(`Failed to create scroll sync group: ${e}`);
            scrollSyncGroupId = null;
        }

        // Store state for synchronized scrolling
        activeSideBySideState = {
            oldSplitId,
            newSplitId,
            oldBufferId,
            newBufferId,
            alignedLines,
            oldLineByteOffsets: oldPane.lineByteOffsets,
            newLineByteOffsets: newPane.lineByteOffsets,
            scrollSyncGroupId
        };
        activeDiffViewState = { lSplit: oldSplitId, rSplit: newSplitId };

        const addedLines = alignedLines.filter(l => l.changeType === 'added').length;
        const removedLines = alignedLines.filter(l => l.changeType === 'removed').length;
        const modifiedLines = alignedLines.filter(l => l.changeType === 'modified').length;
        editor.setStatus(`Side-by-side diff: +${addedLines} -${removedLines} ~${modifiedLines} | 'q' to return`);
    }
};

// Define the diff-view mode with navigation keys
editor.defineMode("diff-view", "special", [
    ["q", "close_buffer"],
    ["j", "move_down"],
    ["k", "move_up"],
    ["g", "move_document_start"],
    ["G", "move_document_end"],
    ["Ctrl+d", "move_page_down"],
    ["Ctrl+u", "move_page_up"],
    ["Down", "move_down"],
    ["Up", "move_up"],
    ["PageDown", "move_page_down"],
    ["PageUp", "move_page_up"],
], true);

// --- Review Comment Actions ---

function getCurrentHunkId(): string | null {
    const bid = editor.getActiveBufferId();
    const props = editor.getTextPropertiesAtCursor(bid);
    if (props.length > 0 && props[0].hunkId) return props[0].hunkId as string;
    return null;
}

interface PendingCommentInfo {
    hunkId: string;
    file: string;
    lineType?: 'add' | 'remove' | 'context';
    oldLine?: number;
    newLine?: number;
    lineContent?: string;
}

function getCurrentLineInfo(): PendingCommentInfo | null {
    const bid = editor.getActiveBufferId();
    const props = editor.getTextPropertiesAtCursor(bid);
    if (props.length > 0 && props[0].hunkId) {
        const hunk = state.hunks.find(h => h.id === props[0].hunkId);
        return {
            hunkId: props[0].hunkId as string,
            file: (props[0].file as string) || hunk?.file || '',
            lineType: props[0].lineType as 'add' | 'remove' | 'context' | undefined,
            oldLine: props[0].oldLine as number | undefined,
            newLine: props[0].newLine as number | undefined,
            lineContent: props[0].lineContent as string | undefined
        };
    }
    return null;
}

// Pending prompt state for event-based prompt handling
let pendingCommentInfo: PendingCommentInfo | null = null;

globalThis.review_add_comment = async () => {
    const info = getCurrentLineInfo();
    if (!info) {
        editor.setStatus("No hunk selected for comment");
        return;
    }
    pendingCommentInfo = info;

    // Show line context in prompt (if on a specific line)
    let lineRef = 'hunk';
    if (info.lineType === 'add' && info.newLine) {
        lineRef = `+${info.newLine}`;
    } else if (info.lineType === 'remove' && info.oldLine) {
        lineRef = `-${info.oldLine}`;
    } else if (info.newLine) {
        lineRef = `L${info.newLine}`;
    } else if (info.oldLine) {
        lineRef = `L${info.oldLine}`;
    }
    editor.startPrompt(`Comment on ${lineRef}: `, "review-comment");
};

// Prompt event handlers
globalThis.on_review_prompt_confirm = (args: { prompt_type: string; input: string }): boolean => {
    if (args.prompt_type !== "review-comment") {
        return true; // Not our prompt
    }
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
        updateReviewUI();
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
        editor.setStatus(`Comment added to ${lineRef}`);
    }
    pendingCommentInfo = null;
    return true;
};

globalThis.on_review_prompt_cancel = (args: { prompt_type: string }): boolean => {
    if (args.prompt_type === "review-comment") {
        pendingCommentInfo = null;
        editor.setStatus("Comment cancelled");
    }
    return true;
};

// Register prompt event handlers
editor.on("prompt_confirmed", "on_review_prompt_confirm");
editor.on("prompt_cancelled", "on_review_prompt_cancel");

globalThis.review_approve_hunk = async () => {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'approved';
        await updateReviewUI();
        editor.setStatus(`Hunk approved`);
    }
};

globalThis.review_reject_hunk = async () => {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'rejected';
        await updateReviewUI();
        editor.setStatus(`Hunk rejected`);
    }
};

globalThis.review_needs_changes = async () => {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'needs_changes';
        await updateReviewUI();
        editor.setStatus(`Hunk marked as needs changes`);
    }
};

globalThis.review_question_hunk = async () => {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'question';
        await updateReviewUI();
        editor.setStatus(`Hunk marked with question`);
    }
};

globalThis.review_clear_status = async () => {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'pending';
        await updateReviewUI();
        editor.setStatus(`Hunk review status cleared`);
    }
};

globalThis.review_set_overall_feedback = async () => {
    const text = await editor.prompt("Overall feedback: ", state.overallFeedback || "");
    if (text !== null) {
        state.overallFeedback = text.trim();
        editor.setStatus(`Overall feedback ${text.trim() ? 'set' : 'cleared'}`);
    }
};

globalThis.review_export_session = async () => {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");

    // Generate markdown content (writeFile creates parent directories)
    let md = `# Code Review Session\n`;
    md += `Date: ${new Date().toISOString()}\n\n`;

    if (state.originalRequest) {
        md += `## Original Request\n${state.originalRequest}\n\n`;
    }

    if (state.overallFeedback) {
        md += `## Overall Feedback\n${state.overallFeedback}\n\n`;
    }

    // Stats
    const approved = state.hunks.filter(h => h.reviewStatus === 'approved').length;
    const rejected = state.hunks.filter(h => h.reviewStatus === 'rejected').length;
    const needsChanges = state.hunks.filter(h => h.reviewStatus === 'needs_changes').length;
    const questions = state.hunks.filter(h => h.reviewStatus === 'question').length;
    md += `## Summary\n`;
    md += `- Total hunks: ${state.hunks.length}\n`;
    md += `- Approved: ${approved}\n`;
    md += `- Rejected: ${rejected}\n`;
    md += `- Needs changes: ${needsChanges}\n`;
    md += `- Questions: ${questions}\n\n`;

    // Group by file
    const fileGroups: Record<string, Hunk[]> = {};
    for (const hunk of state.hunks) {
        if (!fileGroups[hunk.file]) fileGroups[hunk.file] = [];
        fileGroups[hunk.file].push(hunk);
    }

    for (const [file, hunks] of Object.entries(fileGroups)) {
        md += `## File: ${file}\n\n`;
        for (const hunk of hunks) {
            const statusStr = hunk.reviewStatus.toUpperCase();
            md += `### ${hunk.contextHeader || 'Hunk'} (line ${hunk.range.start})\n`;
            md += `**Status**: ${statusStr}\n\n`;

            const hunkComments = state.comments.filter(c => c.hunk_id === hunk.id);
            if (hunkComments.length > 0) {
                md += `**Comments:**\n`;
                for (const c of hunkComments) {
                    // Format line reference
                    let lineRef = '';
                    if (c.line_type === 'add' && c.new_line) {
                        lineRef = `[+${c.new_line}]`;
                    } else if (c.line_type === 'remove' && c.old_line) {
                        lineRef = `[-${c.old_line}]`;
                    } else if (c.new_line) {
                        lineRef = `[L${c.new_line}]`;
                    } else if (c.old_line) {
                        lineRef = `[L${c.old_line}]`;
                    }
                    md += `> 💬 ${lineRef} ${c.text}\n`;
                    if (c.line_content) {
                        md += `> \`${c.line_content.trim()}\`\n`;
                    }
                    md += `\n`;
                }
            }
        }
    }

    // Write file
    const filePath = editor.pathJoin(reviewDir, "session.md");
    await editor.writeFile(filePath, md);
    editor.setStatus(`Review exported to ${filePath}`);
};

globalThis.review_export_json = async () => {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");
    // writeFile creates parent directories

    const session = {
        version: "1.0",
        timestamp: new Date().toISOString(),
        original_request: state.originalRequest || null,
        overall_feedback: state.overallFeedback || null,
        files: {} as Record<string, any>
    };

    for (const hunk of state.hunks) {
        if (!session.files[hunk.file]) session.files[hunk.file] = { hunks: [] };
        const hunkComments = state.comments.filter(c => c.hunk_id === hunk.id);
        session.files[hunk.file].hunks.push({
            context: hunk.contextHeader,
            old_lines: [hunk.oldRange.start, hunk.oldRange.end],
            new_lines: [hunk.range.start, hunk.range.end],
            status: hunk.reviewStatus,
            comments: hunkComments.map(c => ({
                text: c.text,
                line_type: c.line_type || null,
                old_line: c.old_line || null,
                new_line: c.new_line || null,
                line_content: c.line_content || null
            }))
        });
    }

    const filePath = editor.pathJoin(reviewDir, "session.json");
    await editor.writeFile(filePath, JSON.stringify(session, null, 2));
    editor.setStatus(`Review exported to ${filePath}`);
};

globalThis.start_review_diff = async () => {
    editor.setStatus("Generating Review Diff Stream...");
    editor.setContext("review-mode", true);

    // Initial data fetch
    const newHunks = await getGitDiff();
    state.hunks = newHunks;
    state.comments = []; // Reset comments for new session

    const bufferId = await VirtualBufferFactory.create({
        name: "*Review Diff*", mode: "review-mode", read_only: true,
        entries: (await renderReviewStream()).entries, showLineNumbers: false
    });
    state.reviewBufferId = bufferId;
    await updateReviewUI(); // Apply initial highlights

    editor.setStatus(`Review Diff: ${state.hunks.length} hunks | [c]omment [a]pprove [x]reject [!]changes [?]question [E]xport`);
    editor.on("buffer_activated", "on_review_buffer_activated");
    editor.on("buffer_closed", "on_review_buffer_closed");
};

globalThis.stop_review_diff = () => {
    state.reviewBufferId = null;
    editor.setContext("review-mode", false);
    editor.off("buffer_activated", "on_review_buffer_activated");
    editor.off("buffer_closed", "on_review_buffer_closed");
    editor.setStatus("Review Diff Mode stopped.");
};

globalThis.on_review_buffer_activated = (data: any) => {
    if (data.buffer_id === state.reviewBufferId) refreshReviewData();
};

globalThis.on_review_buffer_closed = (data: any) => {
    if (data.buffer_id === state.reviewBufferId) stop_review_diff();
};

// Side-by-side diff for current file (can be called directly without Review Diff mode)
globalThis.side_by_side_diff_current_file = async () => {
    const bid = editor.getActiveBufferId();
    const absolutePath = editor.getBufferPath(bid);

    if (!absolutePath) {
        editor.setStatus("No file open - cannot show diff");
        return;
    }

    editor.setStatus("Loading side-by-side diff...");

    // Get the git root directory to compute relative path
    const gitRootResult = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"]);
    if (gitRootResult.exit_code !== 0) {
        editor.setStatus("Not in a git repository");
        return;
    }
    const gitRoot = gitRootResult.stdout.trim();

    // Compute relative path from git root
    let filePath = absolutePath;
    if (absolutePath.startsWith(gitRoot)) {
        filePath = absolutePath.substring(gitRoot.length + 1); // +1 to skip the leading /
    }

    // Get hunks for this specific file
    const result = await editor.spawnProcess("git", ["diff", "HEAD", "--unified=3", "--", filePath]);
    if (result.exit_code !== 0) {
        editor.setStatus("Failed to get git diff for file");
        return;
    }

    // Parse hunks from diff output
    const lines = result.stdout.split('\n');
    const fileHunks: Hunk[] = [];
    let currentHunk: Hunk | null = null;

    for (const line of lines) {
        if (line.startsWith('@@')) {
            const match = line.match(/@@ -(\d+),?\d* \+(\d+),?\d* @@(.*)/);
            if (match) {
                const oldStart = parseInt(match[1]);
                const newStart = parseInt(match[2]);
                currentHunk = {
                    id: `${filePath}:${newStart}`,
                    file: filePath,
                    range: { start: newStart, end: newStart },
                    oldRange: { start: oldStart, end: oldStart },
                    type: 'modify',
                    lines: [],
                    status: 'pending',
                    reviewStatus: 'pending',
                    contextHeader: match[3]?.trim() || "",
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
        editor.setStatus("No changes in this file");
        return;
    }

    // Get old (HEAD) and new (working) file content
    const gitShow = await editor.spawnProcess("git", ["show", `HEAD:${filePath}`]);
    if (gitShow.exit_code !== 0) {
        editor.setStatus("Failed to load old file version (file may be new)");
        return;
    }
    const oldContent = gitShow.stdout;

    // Read new file content
    let newContent: string;
    try {
        newContent = await editor.readFile(filePath);
    } catch (e) {
        editor.setStatus("Failed to load new file version");
        return;
    }

    // Compute aligned diff for the FULL file
    const alignedLines = computeFullFileAlignedDiff(oldContent, newContent, fileHunks);

    // Generate content for both panes
    const oldPane = generateDiffPaneContent(alignedLines, 'old');
    const newPane = generateDiffPaneContent(alignedLines, 'new');

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

    // Get the current split ID
    const currentSplitId = (editor as any).getActiveSplitId();

    // Create OLD buffer in the CURRENT split
    const oldBufferId = await editor.createVirtualBufferInExistingSplit({
        name: `[OLD] ${filePath}`,
        mode: "diff-view",
        read_only: true,
        editing_disabled: true,
        entries: oldPane.entries,
        split_id: currentSplitId,
        show_line_numbers: false,
        line_wrap: false
    });
    const oldSplitId = currentSplitId;

    // Apply highlights to old pane
    editor.clearNamespace(oldBufferId, "diff-view");
    for (const hl of oldPane.highlights) {
        const bg = hl.bg || [-1, -1, -1];
        editor.addOverlay(
            oldBufferId, "diff-view",
            hl.range[0], hl.range[1],
            hl.fg[0], hl.fg[1], hl.fg[2],
            false, hl.bold || false, false,
            bg[0], bg[1], bg[2],
            hl.extend_to_line_end || false
        );
    }

    // Create NEW pane in a vertical split (RIGHT of OLD)
    const newRes = await editor.createVirtualBufferInSplit({
        name: `[NEW] ${filePath}`,
        mode: "diff-view",
        read_only: true,
        editing_disabled: true,
        entries: newPane.entries,
        ratio: 0.5,
        direction: "vertical",
        show_line_numbers: false,
        line_wrap: false
    });
    const newBufferId = newRes.buffer_id;
    const newSplitId = newRes.split_id!;

    // Apply highlights to new pane
    editor.clearNamespace(newBufferId, "diff-view");
    for (const hl of newPane.highlights) {
        const bg = hl.bg || [-1, -1, -1];
        editor.addOverlay(
            newBufferId, "diff-view",
            hl.range[0], hl.range[1],
            hl.fg[0], hl.fg[1], hl.fg[2],
            false, hl.bold || false, false,
            bg[0], bg[1], bg[2],
            hl.extend_to_line_end || false
        );
    }

    // Focus OLD pane (left)
    (editor as any).focusSplit(oldSplitId);

    // Set up scroll sync
    let scrollSyncGroupId: number | null = null;
    try {
        scrollSyncGroupId = nextScrollSyncGroupId++;
        (editor as any).createScrollSyncGroup(scrollSyncGroupId, oldSplitId, newSplitId);

        const anchors: [number, number][] = [];
        for (let i = 0; i < alignedLines.length; i++) {
            const line = alignedLines[i];
            const prevLine = i > 0 ? alignedLines[i - 1] : null;
            if (i === 0) anchors.push([0, 0]);
            if (prevLine && prevLine.changeType !== line.changeType) {
                anchors.push([i, i]);
            }
        }
        if (alignedLines.length > 0) {
            anchors.push([alignedLines.length, alignedLines.length]);
        }
        (editor as any).setScrollSyncAnchors(scrollSyncGroupId, anchors);
    } catch (e) {
        editor.debug(`Failed to create scroll sync group: ${e}`);
        scrollSyncGroupId = null;
    }

    // Store state
    activeSideBySideState = {
        oldSplitId,
        newSplitId,
        oldBufferId,
        newBufferId,
        alignedLines,
        oldLineByteOffsets: oldPane.lineByteOffsets,
        newLineByteOffsets: newPane.lineByteOffsets,
        scrollSyncGroupId
    };
    activeDiffViewState = { lSplit: oldSplitId, rSplit: newSplitId };

    const addedLines = alignedLines.filter(l => l.changeType === 'added').length;
    const removedLines = alignedLines.filter(l => l.changeType === 'removed').length;
    const modifiedLines = alignedLines.filter(l => l.changeType === 'modified').length;
    editor.setStatus(`Side-by-side diff: +${addedLines} -${removedLines} ~${modifiedLines} | 'q' to return`);
};

// Register Modes and Commands
editor.registerCommand("Review Diff", "Start code review session", "start_review_diff", "global");
editor.registerCommand("Stop Review Diff", "Stop the review session", "stop_review_diff", "review-mode");
editor.registerCommand("Refresh Review Diff", "Refresh the list of changes", "review_refresh", "review-mode");
editor.registerCommand("Side-by-Side Diff", "Show side-by-side diff for current file", "side_by_side_diff_current_file", "global");

// Review Comment Commands
editor.registerCommand("Review: Add Comment", "Add a review comment to the current hunk", "review_add_comment", "review-mode");
editor.registerCommand("Review: Approve Hunk", "Mark hunk as approved", "review_approve_hunk", "review-mode");
editor.registerCommand("Review: Reject Hunk", "Mark hunk as rejected", "review_reject_hunk", "review-mode");
editor.registerCommand("Review: Needs Changes", "Mark hunk as needing changes", "review_needs_changes", "review-mode");
editor.registerCommand("Review: Question", "Mark hunk with a question", "review_question_hunk", "review-mode");
editor.registerCommand("Review: Clear Status", "Clear hunk review status", "review_clear_status", "review-mode");
editor.registerCommand("Review: Overall Feedback", "Set overall review feedback", "review_set_overall_feedback", "review-mode");
editor.registerCommand("Review: Export to Markdown", "Export review to .review/session.md", "review_export_session", "review-mode");
editor.registerCommand("Review: Export to JSON", "Export review to .review/session.json", "review_export_json", "review-mode");

// Handler for when buffers are closed - cleans up scroll sync groups
globalThis.on_buffer_closed = (data: any) => {
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
};

editor.on("buffer_closed", "on_buffer_closed");

editor.defineMode("review-mode", "normal", [
    // Staging actions
    ["s", "review_stage_hunk"], ["d", "review_discard_hunk"],
    // Navigation
    ["n", "review_next_hunk"], ["p", "review_prev_hunk"], ["r", "review_refresh"],
    ["Enter", "review_drill_down"], ["q", "close_buffer"],
    // Review actions
    ["c", "review_add_comment"],
    ["a", "review_approve_hunk"],
    ["x", "review_reject_hunk"],
    ["!", "review_needs_changes"],
    ["?", "review_question_hunk"],
    ["u", "review_clear_status"],
    ["O", "review_set_overall_feedback"],
    // Export
    ["E", "review_export_session"],
], true);

editor.debug("Review Diff plugin loaded with review comments support");