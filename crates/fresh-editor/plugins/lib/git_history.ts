/// <reference path="./fresh.d.ts" />

/**
 * Shared git history rendering helpers used by the git log plugin and the
 * review-diff plugin's branch review mode.
 *
 * All rendering uses theme-keyed colours (`syntax.keyword`, `editor.fg`, etc.)
 * so the panels stay consistent with the editor's current theme. The entry
 * builders produce `TextPropertyEntry[]` lists whose sub-ranges are styled
 * via `inlineOverlays` — no separate imperative overlay pass is required.
 */

// =============================================================================
// Types
// =============================================================================

export interface GitCommit {
  hash: string;
  shortHash: string;
  author: string;
  authorEmail: string;
  date: string;
  relativeDate: string;
  subject: string;
  body: string;
  refs: string;
}

export interface FetchGitLogOptions {
  /** Max commits to fetch (default: 200). */
  maxCommits?: number;
  /** Optional revision range (e.g. "main..HEAD"). Defaults to HEAD. */
  range?: string;
  /** Working directory. Defaults to `editor.getCwd()`. */
  cwd?: string;
}

export interface BuildCommitLogEntriesOptions {
  /** Index of the "selected" row — rendered with the selected-bg highlight. */
  selectedIndex?: number;
  /** Optional header string (e.g. "Commits:"). Default: "Commits:". */
  header?: string;
  /** Footer line (status hint). Omitted when null/undefined. */
  footer?: string | null;
  /** Target width for padding column alignment (default 0 = no padding). */
  width?: number;
  /** "log" property-type prefix for entries (default "log-commit"). */
  propertyType?: string;
}

// =============================================================================
// Theme keys
// =============================================================================

export const GIT_THEME = {
  header: "syntax.keyword" as OverlayColorSpec,
  separator: "ui.split_separator_fg" as OverlayColorSpec,
  hash: "syntax.number" as OverlayColorSpec,
  author: "syntax.function" as OverlayColorSpec,
  date: "syntax.string" as OverlayColorSpec,
  subject: "editor.fg" as OverlayColorSpec,
  subjectMuted: "editor.line_number_fg" as OverlayColorSpec,
  refBranch: "syntax.type" as OverlayColorSpec,
  refRemote: "syntax.function" as OverlayColorSpec,
  refTag: "syntax.number" as OverlayColorSpec,
  refHead: "syntax.keyword" as OverlayColorSpec,
  diffAdd: "editor.diff_add_bg" as OverlayColorSpec,
  diffRemove: "editor.diff_remove_bg" as OverlayColorSpec,
  diffAddFg: "diagnostic.info_fg" as OverlayColorSpec,
  diffRemoveFg: "diagnostic.error_fg" as OverlayColorSpec,
  diffHunk: "syntax.type" as OverlayColorSpec,
  metaLabel: "editor.line_number_fg" as OverlayColorSpec,
  selectionBg: "editor.selection_bg" as OverlayColorSpec,
  sectionBg: "editor.current_line_bg" as OverlayColorSpec,
  footer: "editor.line_number_fg" as OverlayColorSpec,
};

// =============================================================================
// Author initials helper — compact "(AL)" / "(JD)" style label used in the
// aligned log view. Falls back to the raw author when no initials can be
// extracted.
// =============================================================================

export function authorInitials(author: string): string {
  const cleaned = author.replace(/[<>].*/g, "").trim();
  const parts = cleaned.split(/\s+/).filter(p => p.length > 0);
  if (parts.length === 0) return "??";
  if (parts.length === 1) {
    return parts[0].slice(0, 2).toUpperCase();
  }
  const first = parts[0][0] || "?";
  const last = parts[parts.length - 1][0] || "?";
  return (first + last).toUpperCase();
}

// =============================================================================
// Commit fetching
// =============================================================================

export async function fetchGitLog(
  editor: EditorAPI,
  opts: FetchGitLogOptions = {}
): Promise<GitCommit[]> {
  const maxCommits = opts.maxCommits ?? 200;
  const cwd = opts.cwd ?? editor.getCwd();
  const format = "%H%x00%h%x00%an%x00%ae%x00%ai%x00%ar%x00%D%x00%s%x00%b%x1e";
  const args = ["log", `--format=${format}`, `-n${maxCommits}`];
  if (opts.range) args.push(opts.range);

  const result = await editor.spawnProcess("git", args, cwd);
  if (result.exit_code !== 0) return [];

  const commits: GitCommit[] = [];
  const records = result.stdout.split("\x1e");
  for (const record of records) {
    if (!record.trim()) continue;
    const parts = record.split("\x00");
    if (parts.length < 8) continue;
    commits.push({
      hash: parts[0].trim(),
      shortHash: parts[1].trim(),
      author: parts[2].trim(),
      authorEmail: parts[3].trim(),
      date: parts[4].trim(),
      relativeDate: parts[5].trim(),
      refs: parts[6].trim(),
      subject: parts[7].trim(),
      body: parts[8] ? parts[8].trim() : "",
    });
  }
  return commits;
}

/**
 * A single file's diff exceeding this line count is omitted from the
 * rendered `git show` output. Generated files (lockfiles, bundled SVGs,
 * minified JS) can produce megabyte-scale diffs that balloon the detail
 * panel into hundreds of thousands of entries — slow to render and not
 * useful to read. The stat header still lists the file so the user knows
 * it changed; a footer tells them which ones were skipped.
 */
const MAX_DIFF_LINES_PER_FILE = 2000;

/**
 * Rejoin hard-wrapped lines within a paragraph into one logical line. Blank
 * lines remain blank and delimit paragraphs. Used only for commit messages
 * (author-chosen 72-col hard wraps need to be erased so soft-wrap in a
 * narrow panel can re-break at the panel edge).
 */
function reflowParagraphs(text: string): string {
  const out: string[] = [];
  let paragraph = "";
  for (const line of text.split("\n")) {
    if (line === "") {
      if (paragraph !== "") {
        out.push(paragraph);
        paragraph = "";
      }
      out.push("");
      continue;
    }
    paragraph = paragraph === "" ? line : paragraph + " " + line;
  }
  if (paragraph !== "") out.push(paragraph);
  // Drop a single trailing blank that `%B` always adds.
  while (out.length > 0 && out[out.length - 1] === "") out.pop();
  return out.join("\n");
}

export async function fetchCommitShow(
  editor: EditorAPI,
  hash: string,
  cwd?: string
): Promise<string> {
  const workdir = cwd ?? editor.getCwd();

  // 1. numstat — small, lets us spot oversized files before the full diff.
  const numstatResult = await editor.spawnProcess(
    "git",
    ["show", "--numstat", "--format=", hash],
    workdir
  );
  const oversized: string[] = [];
  if (numstatResult.exit_code === 0) {
    for (const line of numstatResult.stdout.split("\n")) {
      if (!line) continue;
      // numstat format: "<added>\t<removed>\t<path>"; "-" for binary files.
      const tab1 = line.indexOf("\t");
      const tab2 = tab1 >= 0 ? line.indexOf("\t", tab1 + 1) : -1;
      if (tab1 < 0 || tab2 < 0) continue;
      const addedStr = line.slice(0, tab1);
      const removedStr = line.slice(tab1 + 1, tab2);
      const path = line.slice(tab2 + 1);
      const added = addedStr === "-" ? 0 : parseInt(addedStr, 10) || 0;
      const removed = removedStr === "-" ? 0 : parseInt(removedStr, 10) || 0;
      if (added + removed > MAX_DIFF_LINES_PER_FILE) {
        oversized.push(path);
      }
    }
  }

  // 2. Metadata — pull the fields we want with a structured format so we
  // can reflow just the message body without having to recognise the
  // header in free-form `git show` output.
  const META_SEP = "\x00";
  const META_FORMAT = [
    "%H",  // full hash
    "%P",  // parent hashes (space-separated)
    "%an", // author name
    "%ae", // author email
    "%aD", // author date, RFC 2822 — matches `git show` default
    "%B",  // raw subject + body
  ].join(META_SEP);
  const metaResult = await editor.spawnProcess(
    "git",
    ["log", "-n1", `--format=${META_FORMAT}`, hash],
    workdir
  );
  if (metaResult.exit_code !== 0) {
    return metaResult.stderr || "(no output)";
  }
  const metaFields = metaResult.stdout.split(META_SEP);
  const fullHash = (metaFields[0] ?? hash).trim();
  const parents = (metaFields[1] ?? "").trim().split(" ").filter((p) => p.length > 0);
  const author = metaFields[2] ?? "";
  const email = metaFields[3] ?? "";
  const date = metaFields[4] ?? "";
  const rawMessage = metaFields[5] ?? "";

  // 3. Stat + patch, no metadata, with oversized paths excluded.
  const showArgs = ["show", "--format=", "--stat", "--patch", hash];
  if (oversized.length > 0) {
    showArgs.push("--", ".");
    for (const p of oversized) showArgs.push(`:(exclude,top)${p}`);
  }
  const showResult = await editor.spawnProcess("git", showArgs, workdir);
  if (showResult.exit_code !== 0) return showResult.stderr || "(no output)";

  // Assemble. The shape matches stock `git show` output so
  // `buildDetailLineEntry` keeps recognising the header.
  let out = `commit ${fullHash}\n`;
  if (parents.length > 1) {
    out += `Merge: ${parents.map((p) => p.slice(0, 7)).join(" ")}\n`;
  }
  out += `Author: ${author} <${email}>\n`;
  out += `Date:   ${date}\n\n`;

  // Reflow only the message. Indent with 4 spaces to match git's own layout.
  const reflowed = reflowParagraphs(rawMessage);
  for (const line of reflowed.split("\n")) {
    out += line === "" ? "\n" : `    ${line}\n`;
  }
  out += "\n";

  // Stat + patch, untouched.
  out += showResult.stdout;

  if (oversized.length > 0) {
    const plural = oversized.length === 1 ? "" : "s";
    out += `\n[${oversized.length} large file${plural} omitted from diff (>${MAX_DIFF_LINES_PER_FILE} lines changed):\n`;
    for (const p of oversized) out += `  ${p}\n`;
    out += `Run \`git show ${hash.slice(0, 12)} -- <path>\` to view.]\n`;
  }

  return out;
}

// =============================================================================
// UTF-8 byte-length helper — the runtime's overlay offsets are in bytes, but
// JS strings are UTF-16. Colocated here so consumers don't have to redefine it.
// =============================================================================

export function byteLength(s: string): number {
  let b = 0;
  for (let i = 0; i < s.length; i++) {
    const code = s.charCodeAt(i);
    if (code <= 0x7f) b += 1;
    else if (code <= 0x7ff) b += 2;
    else if (code >= 0xd800 && code <= 0xdfff) {
      b += 4;
      i++;
    } else b += 3;
  }
  return b;
}

// =============================================================================
// Commit log entry building
// =============================================================================

/**
 * Compute column widths for the aligned commit-log table. Returns widths for
 * (hash, date, initials) columns. Subject and refs fill the remainder.
 */
function commitLogColumnWidths(commits: GitCommit[]): {
  hashW: number;
  dateW: number;
  authorW: number;
} {
  let hashW = 7;
  let dateW = 10;
  let authorW = 2;
  for (const c of commits) {
    if (c.shortHash.length > hashW) hashW = c.shortHash.length;
    if (c.relativeDate.length > dateW) dateW = c.relativeDate.length;
    const ini = authorInitials(c.author);
    if (ini.length > authorW) authorW = ini.length;
  }
  // Clamp so a pathological author/date doesn't swallow the subject column.
  if (hashW > 12) hashW = 12;
  if (dateW > 16) dateW = 16;
  if (authorW > 4) authorW = 4;
  return { hashW, dateW, authorW };
}

/**
 * Classify a git ref decoration tag so it can be coloured appropriately.
 * Matches a single comma-separated entry from `%D` output, e.g.
 * "HEAD -> main", "origin/main", "tag: v1.0".
 */
function refTokenColor(token: string): OverlayColorSpec {
  const t = token.trim();
  if (t.startsWith("tag:")) return GIT_THEME.refTag;
  if (t.startsWith("HEAD")) return GIT_THEME.refHead;
  if (t.includes("/")) return GIT_THEME.refRemote;
  return GIT_THEME.refBranch;
}

/**
 * Build a styled commit-log entry row with aligned columns. All styling uses
 * `inlineOverlays` with theme keys — no imperative overlay pass needed.
 */
function buildCommitRowEntry(
  commit: GitCommit,
  index: number,
  isSelected: boolean,
  widths: { hashW: number; dateW: number; authorW: number },
  propertyType: string
): TextPropertyEntry {
  const shortHash = commit.shortHash.padEnd(widths.hashW);
  const date = commit.relativeDate.padEnd(widths.dateW);
  const ini = authorInitials(commit.author).padEnd(widths.authorW);

  const prefix = " ";
  let byte = byteLength(prefix);
  let text = prefix;
  const overlays: InlineOverlay[] = [];

  // Hash column
  overlays.push({
    start: byte,
    end: byte + byteLength(shortHash),
    style: { fg: GIT_THEME.hash, bold: true },
  });
  text += shortHash;
  byte += byteLength(shortHash);

  // Space
  text += "  ";
  byte += 2;

  // Date column
  overlays.push({
    start: byte,
    end: byte + byteLength(date),
    style: { fg: GIT_THEME.date },
  });
  text += date;
  byte += byteLength(date);

  // Space
  text += "  ";
  byte += 2;

  // Author initials in parentheses
  const authorOpen = "(";
  const authorClose = ")";
  text += authorOpen;
  byte += byteLength(authorOpen);
  overlays.push({
    start: byte,
    end: byte + byteLength(ini),
    style: { fg: GIT_THEME.author, bold: true },
  });
  text += ini;
  byte += byteLength(ini);
  text += authorClose;
  byte += byteLength(authorClose);

  // Space
  text += " ";
  byte += 1;

  // Subject
  overlays.push({
    start: byte,
    end: byte + byteLength(commit.subject),
    style: { fg: GIT_THEME.subject },
  });
  text += commit.subject;
  byte += byteLength(commit.subject);

  // Refs (if any) — tokenise and colour each separately. %D returns a
  // comma-separated list like "HEAD -> main, origin/main, tag: v1".
  if (commit.refs) {
    text += "  ";
    byte += 2;
    const tokens = commit.refs.split(",").map(t => t.trim()).filter(t => t.length > 0);
    for (let i = 0; i < tokens.length; i++) {
      if (i > 0) {
        text += " ";
        byte += 1;
      }
      // "HEAD -> main" renders as two logical tokens inside one entry;
      // treat the whole token as one coloured chunk for simplicity.
      const t = tokens[i];
      const bracket = `[${t}]`;
      overlays.push({
        start: byte,
        end: byte + byteLength(bracket),
        style: { fg: refTokenColor(t), bold: true },
      });
      text += bracket;
      byte += byteLength(bracket);
    }
  }

  const finalText = text + "\n";

  const style: Partial<OverlayOptions> = isSelected
    ? { bg: GIT_THEME.selectionBg, extendToLineEnd: true, bold: true }
    : {};

  return {
    text: finalText,
    properties: {
      type: propertyType,
      index,
      hash: commit.hash,
      shortHash: commit.shortHash,
      author: commit.author,
      date: commit.relativeDate,
      subject: commit.subject,
      refs: commit.refs,
    },
    style,
    inlineOverlays: overlays,
  };
}

export function buildCommitLogEntries(
  commits: GitCommit[],
  opts: BuildCommitLogEntriesOptions = {}
): TextPropertyEntry[] {
  const header = opts.header ?? "Commits:";
  const footer = opts.footer;
  const selectedIndex = opts.selectedIndex ?? -1;
  const propertyType = opts.propertyType ?? "log-commit";

  const entries: TextPropertyEntry[] = [];

  entries.push({
    text: header + "\n",
    properties: { type: "log-header" },
    style: { fg: GIT_THEME.header, bold: true, underline: true },
  });

  if (commits.length === 0) {
    entries.push({
      text: "  (no commits)\n",
      properties: { type: "log-empty" },
      style: { fg: GIT_THEME.metaLabel, italic: true },
    });
  } else {
    const widths = commitLogColumnWidths(commits);
    for (let i = 0; i < commits.length; i++) {
      entries.push(
        buildCommitRowEntry(commits[i], i, i === selectedIndex, widths, propertyType)
      );
    }
  }

  if (footer) {
    entries.push({
      text: "\n",
      properties: { type: "log-blank" },
    });
    entries.push({
      text: footer + "\n",
      properties: { type: "log-footer" },
      style: { fg: GIT_THEME.footer, italic: true },
    });
  }

  return entries;
}

// =============================================================================
// Commit detail (git show) entry building
// =============================================================================

interface DetailBuildContext {
  currentFile: string | null;
  currentNewLine: number;
}

/**
 * Style a single line from `git show --stat --patch` output as a styled
 * TextPropertyEntry with inlineOverlays. Tracks file/line context for click
 * navigation.
 */
function buildDetailLineEntry(
  line: string,
  ctx: DetailBuildContext
): TextPropertyEntry {
  const props: Record<string, unknown> = { type: "detail-line" };
  const overlays: InlineOverlay[] = [];
  let lineStyle: Partial<OverlayOptions> = {};

  // "diff --git a/... b/..."
  const diffHeader = line.match(/^diff --git a\/(.+) b\/(.+)$/);
  if (diffHeader) {
    ctx.currentFile = diffHeader[2];
    ctx.currentNewLine = 0;
    props.type = "detail-diff-header";
    props.file = ctx.currentFile;
    lineStyle = { fg: GIT_THEME.header, bold: true };
  } else if (line.startsWith("+++ b/")) {
    ctx.currentFile = line.slice(6);
    props.type = "detail-diff-header";
    props.file = ctx.currentFile;
    lineStyle = { fg: GIT_THEME.header, bold: true };
  } else if (line.startsWith("+++ ") || line.startsWith("--- ") || line.startsWith("index ")) {
    props.type = "detail-diff-header";
    lineStyle = { fg: GIT_THEME.subjectMuted };
  } else if (line.startsWith("@@")) {
    const hunkMatch = line.match(/@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@/);
    if (hunkMatch) ctx.currentNewLine = parseInt(hunkMatch[1], 10);
    props.type = "detail-hunk-header";
    props.file = ctx.currentFile;
    props.line = ctx.currentNewLine;
    lineStyle = { fg: GIT_THEME.diffHunk, bold: true, extendToLineEnd: true };
  } else if (line.startsWith("+") && !line.startsWith("+++")) {
    props.type = "detail-add";
    props.file = ctx.currentFile;
    props.line = ctx.currentNewLine;
    ctx.currentNewLine++;
    lineStyle = { fg: GIT_THEME.diffAddFg, bg: GIT_THEME.diffAdd, extendToLineEnd: true };
  } else if (line.startsWith("-") && !line.startsWith("---")) {
    props.type = "detail-remove";
    props.file = ctx.currentFile;
    lineStyle = { fg: GIT_THEME.diffRemoveFg, bg: GIT_THEME.diffRemove, extendToLineEnd: true };
  } else if (line.startsWith(" ") && ctx.currentFile && ctx.currentNewLine > 0) {
    props.type = "detail-context";
    props.file = ctx.currentFile;
    props.line = ctx.currentNewLine;
    ctx.currentNewLine++;
  } else if (line.startsWith("commit ")) {
    props.type = "detail-commit-line";
    const hashMatch = line.match(/^commit ([a-f0-9]+)/);
    if (hashMatch) {
      props.hash = hashMatch[1];
      // Colour just "commit" and the hash chunk separately.
      const commitWord = "commit ";
      overlays.push({
        start: 0,
        end: byteLength(commitWord),
        style: { fg: GIT_THEME.metaLabel, bold: true },
      });
      overlays.push({
        start: byteLength(commitWord),
        end: byteLength(commitWord) + byteLength(hashMatch[1]),
        style: { fg: GIT_THEME.hash, bold: true },
      });
    }
  } else if (/^(Author|Date|Commit|Merge|AuthorDate|CommitDate):/.test(line)) {
    const colonIdx = line.indexOf(":");
    props.type = "detail-meta";
    overlays.push({
      start: 0,
      end: byteLength(line.slice(0, colonIdx + 1)),
      style: { fg: GIT_THEME.metaLabel, bold: true },
    });
    const fieldKey = line.slice(0, colonIdx).toLowerCase();
    if (fieldKey === "author") {
      overlays.push({
        start: byteLength(line.slice(0, colonIdx + 1)),
        end: byteLength(line),
        style: { fg: GIT_THEME.author },
      });
    } else if (fieldKey.includes("date")) {
      overlays.push({
        start: byteLength(line.slice(0, colonIdx + 1)),
        end: byteLength(line),
        style: { fg: GIT_THEME.date },
      });
    }
  }

  return {
    text: line + "\n",
    properties: props,
    style: lineStyle,
    inlineOverlays: overlays,
  };
}

/**
 * Build the entries for a commit detail view — a colourful replay of
 * `git show --stat --patch`. The commit message body is already reflowed
 * by `fetchCommitShow`; stat lines and diff lines pass through unchanged.
 */
export function buildCommitDetailEntries(
  commit: GitCommit | null,
  showOutput: string,
  opts: { footer?: string | null } = {}
): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  if (commit) {
    entries.push({
      text: `${commit.shortHash}  ${commit.subject}\n`,
      properties: { type: "detail-title", hash: commit.hash },
      style: { fg: GIT_THEME.header, bold: true, underline: true },
    });
  }

  const ctx: DetailBuildContext = { currentFile: null, currentNewLine: 0 };
  for (const line of showOutput.split("\n")) {
    entries.push(buildDetailLineEntry(line, ctx));
  }

  const footer = opts.footer;
  if (footer) {
    entries.push({
      text: "\n",
      properties: { type: "detail-blank" },
    });
    entries.push({
      text: footer + "\n",
      properties: { type: "detail-footer" },
      style: { fg: GIT_THEME.footer, italic: true },
    });
  }

  return entries;
}

// =============================================================================
// Placeholder entries shown in the detail panel while no commit has been
// loaded yet (e.g. during initial render or when the log is empty).
// =============================================================================

export function buildDetailPlaceholderEntries(message: string): TextPropertyEntry[] {
  return [
    {
      text: "\n",
      properties: { type: "detail-blank" },
    },
    {
      text: "  " + message + "\n",
      properties: { type: "detail-placeholder" },
      style: { fg: GIT_THEME.metaLabel, italic: true },
    },
  ];
}
