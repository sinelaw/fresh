/// <reference path="./lib/fresh.d.ts" />

/**
 * Live Grep Plugin
 *
 * Project-wide search rendered as a centred floating overlay
 * (issue #1796). Search results stream in as the user types; arrow
 * keys navigate; Enter opens at the match location.
 *
 * Search backend is pluggable. The plugin ships three built-in
 * providers (ripgrep → git grep → grep) selected by priority on
 * each invocation; users and other plugins can register additional
 * providers via the exported plugin API:
 *
 *     const liveGrep = editor.getPluginApi("live-grep");
 *     liveGrep?.registerProvider({
 *       name: "fff",
 *       priority: 100,                     // higher wins
 *       isAvailable: async () => {
 *         const r = await editor.spawnProcess("fff", ["--version"], editor.getCwd());
 *         return r.exit_code === 0;
 *       },
 *       search: async (query, { cwd, maxResults }) => {
 *         const r = await editor.spawnProcess("fff", [query], cwd);
 *         return parseFFFOutput(r.stdout);
 *       },
 *     });
 *
 * The provider whose `isAvailable()` returns true with the highest
 * priority is selected on each Live Grep invocation; the result is
 * cached for the duration of the prompt.
 */

import { Finder, parseGrepOutput } from "./lib/finder.ts";
import { col, raw, row, spacer, styledRow, toggle, wrappingRow } from "./lib/widgets.ts";

const editor = getEditor();

/** The data sources Universal Search can look in. `files` is the
 *  classic project-file grep; the others are opt-in scopes layered on
 *  top. Each enabled scope contributes tagged matches to one merged
 *  result list. See `docs/internal/global-search-ux.md`. */
type ScopeId = "files" | "ignored" | "buffers" | "terminals" | "diagnostics";

// One Live Grep match. Mirrors the JSON shape ripgrep emits with
// `--line-number --column --no-heading`; built-in non-rg providers
// (git grep, grep) normalise to this shape via parseGrepOutput.
// `source` tags which scope produced the match so the result row can
// show a badge and (later) pick a scope-appropriate open action.
// Undefined means the classic file source (`files`).
interface GrepMatch {
  file: string;
  line: number;
  column: number;
  content: string;
  source?: ScopeId;
}

/** Options passed to a provider's `search` callback. */
export interface SearchOpts {
  /** Working directory the search should run in (the editor's cwd). */
  cwd: string;
  /** Caller's preferred result cap. Providers may return fewer.
   *  Returning more is allowed; the Finder caps at its own
   *  `maxResults`. */
  maxResults: number;
  /** When true, the "Ignored & hidden" scope is on: providers should
   *  also search `.gitignore`d / hidden files. Built-in `rg` and
   *  `git-grep` honour this; other built-ins (ag/ack/grep) currently
   *  ignore it and always search their default set. */
  includeIgnored?: boolean;
  /** When true, match whole words only (rg `-w`, git-grep/grep `-w`).
   *  Providers should add the appropriate flag. */
  wholeWord?: boolean;
  /** When true (the default), the query is a regular expression; when
   *  false, it's a literal/fixed string the provider must escape (rg
   *  `-F`, git-grep/grep `-F`). Custom providers should honour this so
   *  the query is interpreted consistently with the toolbar toggle. */
  regex?: boolean;
}

/** A registered Live Grep backend. */
export interface LiveGrepProvider {
  /** Stable id, surfaced in status messages. Two providers with the
   *  same name are both kept; only the higher-priority one is ever
   *  selected unless it becomes unavailable. */
  name: string;
  /** Higher priority wins. Built-ins use 0/-1/-2; user-registered
   *  providers default to 0 if omitted. */
  priority?: number;
  /** Cheap probe — typically `editor.spawnProcess("foo", [], cwd)`
   *  and check `exit_code`. May be sync or async. Failures (thrown
   *  errors) are treated as "not available". */
  isAvailable: () => boolean | Promise<boolean>;
  /** Run the search. Return an array of matches; an empty array
   *  means "no matches" (not "provider broken"). Errors thrown
   *  here surface as a status message and bypass the next
   *  provider — the registry doesn't fall back automatically once
   *  a provider is selected. */
  search: (query: string, opts: SearchOpts) => Promise<GrepMatch[]>;
}

/** Public surface exposed via `editor.getPluginApi("live-grep")`. */
export type LiveGrepApi = {
  /** Add a provider. Returns an unregister function. */
  registerProvider(provider: LiveGrepProvider): () => void;
  /** Remove every provider whose name matches. Returns true if at
   *  least one was removed. */
  unregisterProvider(name: string): boolean;
  /** Inspect the current provider list, sorted by priority desc.
   *  Useful for status / debugging / settings UIs. */
  listProviders(): { name: string; priority: number }[];
  /** Forget the cached "selected provider" — the next search runs a
   *  fresh `isAvailable()` probe. Call from init.ts after late
   *  registrations or after the user installs a new binary. */
  resetSelection(): void;
};

declare global {
  interface FreshPluginRegistry {
    "live-grep": LiveGrepApi;
  }
}

// Cap on the number of matches a single search returns. Higher than
// the previous 100 to actually fit a typical refactor's worth of
// hits in one snapshot, but bounded so a runaway query doesn't
// stream the entire codebase into the overlay.
const MAX_RESULTS = 1000;

// ── Scopes (Universal Search) ─────────────────────────────────────
//
// Live Grep is growing into a one-stop search: the user toggles which
// data sources to look in from the overlay toolbar. `files` is the
// classic project grep; `ignored`, `buffers`, `diagnostics` layer on
// top. Toggles are wired through prompt-context keybindings (Alt+…)
// that resolve to the plugin handlers registered below — no core
// Action is required (the host dispatches unknown action names as
// plugin actions). See `docs/internal/global-search-ux.md`.

interface ScopeDef {
  id: ScopeId;
  /** i18n key for the toolbar label. */
  labelKey: string;
  /** Plugin action / handler name a keybinding resolves to. */
  action: string;
  /** Short badge shown on a result row from this scope (omitted for
   *  `files`, whose rows are the unprefixed default). */
  badge?: string;
}

const SCOPES: ScopeDef[] = [
  { id: "files", labelKey: "scope.files", action: "live_grep_toggle_files" },
  { id: "ignored", labelKey: "scope.ignored", action: "live_grep_toggle_ignored", badge: "ign" },
  { id: "buffers", labelKey: "scope.buffers", action: "live_grep_toggle_buffers", badge: "buf" },
  { id: "terminals", labelKey: "scope.terminals", action: "live_grep_toggle_terminals", badge: "term" },
  { id: "diagnostics", labelKey: "scope.diagnostics", action: "live_grep_toggle_diagnostics", badge: "diag" },
];

// Default scope set: same as the classic Live Grep, *minus* ignored
// files (off — they were noisy) and *plus* unsaved open buffers and
// terminal scrollback (on). `files` on, `ignored` off, `buffers` on,
// `terminals` on, `diagnostics` off.
const scopeEnabled: Record<ScopeId, boolean> = {
  files: true,
  ignored: false,
  buffers: true,
  terminals: true,
  diagnostics: false,
};

// True only while our floating overlay is open. The scope-toggle
// keybindings live in the shared `prompt` context, so they can fire
// inside *any* prompt; the handlers no-op unless our overlay owns the
// screen.
let overlayActive = false;

// The most recent query, so Resume can re-open the *same* flow with it
// pre-filled (rather than a bespoke cached-results overlay).
let lastQuery = "";

// ── Search modes ──────────────────────────────────────────────────
//
// Separate from *where* we search (scopes): these control *how* the
// query is interpreted, and are threaded to every provider (and the
// JS-side scopes) so each can escape/format it correctly. `regex` is
// on by default (matches the historical rg/git-grep behaviour);
// `wholeWord` is off.
type ModeId = "word" | "regex";

interface ModeDef {
  id: ModeId;
  /** Stable widget key for the toolbar toggle. */
  key: string;
  /** i18n key for the toggle label. */
  labelKey: string;
}

const MODES: ModeDef[] = [
  { id: "word", key: "mode_word", labelKey: "mode.word" },
  { id: "regex", key: "mode_regex", labelKey: "mode.regex" },
];

const searchModes: Record<ModeId, boolean> = {
  word: false,
  regex: true,
};

/** A compiled matcher for the JS-side scopes (buffers, diagnostics).
 *  Returns the 1-based column of the first match on a line, or -1. */
type LineMatcher = (line: string) => number;

/** Build a line matcher honouring the current `searchModes`. Smart-case:
 *  case-insensitive unless the query has an uppercase letter. An invalid
 *  regex matches nothing (the provider scopes surface the rg/grep error;
 *  the JS scopes just contribute no rows). */
function buildLineMatcher(query: string): LineMatcher {
  const smartCaseInsensitive = query === query.toLowerCase();
  if (searchModes.regex) {
    const flags = smartCaseInsensitive ? "i" : "";
    const pattern = searchModes.word ? `\\b(?:${query})\\b` : query;
    let re: RegExp;
    try {
      re = new RegExp(pattern, flags);
    } catch {
      return () => -1;
    }
    return (line) => {
      const m = re.exec(line);
      return m ? m.index + 1 : -1;
    };
  }
  // Literal (fixed-string) matching.
  const needle = smartCaseInsensitive ? query.toLowerCase() : query;
  const isWord = (ch: string) => /[A-Za-z0-9_]/.test(ch);
  return (line) => {
    const hay = smartCaseInsensitive ? line.toLowerCase() : line;
    let from = 0;
    for (;;) {
      const idx = hay.indexOf(needle, from);
      if (idx < 0) return -1;
      if (!searchModes.word) return idx + 1;
      const before = idx > 0 ? hay[idx - 1] : "";
      const after = idx + needle.length < hay.length ? hay[idx + needle.length] : "";
      if (!isWord(before) && !isWord(after)) return idx + 1;
      from = idx + 1;
    }
  };
}

// ── Registry ──────────────────────────────────────────────────────

const providers: LiveGrepProvider[] = [];
let cachedSelected: LiveGrepProvider | null | undefined = undefined;
// Set by `search` after each query so the toolbar can show
// "1000+ matches" when a result set was clipped at MAX_RESULTS.
// Reset to false on every new query (before the provider call).
let lastSearchTruncated = false;

function sortByPriority(): void {
  providers.sort((a, b) => (b.priority ?? 0) - (a.priority ?? 0));
}

function registerProvider(provider: LiveGrepProvider): () => void {
  if (typeof provider !== "object" || provider === null) {
    throw new Error("live-grep.registerProvider: provider must be an object");
  }
  if (typeof provider.name !== "string" || provider.name.length === 0) {
    throw new Error("live-grep.registerProvider: name must be a non-empty string");
  }
  if (typeof provider.isAvailable !== "function") {
    throw new Error("live-grep.registerProvider: isAvailable must be a function");
  }
  if (typeof provider.search !== "function") {
    throw new Error("live-grep.registerProvider: search must be a function");
  }
  providers.push(provider);
  sortByPriority();
  cachedSelected = undefined; // re-probe on next invocation
  return () => {
    const i = providers.indexOf(provider);
    if (i >= 0) {
      providers.splice(i, 1);
      cachedSelected = undefined;
    }
  };
}

function unregisterProvider(name: string): boolean {
  let removed = false;
  for (let i = providers.length - 1; i >= 0; i--) {
    if (providers[i].name === name) {
      providers.splice(i, 1);
      removed = true;
    }
  }
  if (removed) cachedSelected = undefined;
  return removed;
}

// Build the scope toolbar as real `Toggle` widgets (themed + clickable),
// each keyed to the plugin action it fires on click — the host maps a click
// straight to that action, the same one the Alt+… binding triggers. The
// per-control accelerator (`⌥L` etc.) is rendered right after its toggle in
// the keybinding-hint colour, so the affordance sits at the control rather
// than in a footer list.
function buildToolbarSpec(): WidgetSpec {
  // Two stacked rows: the search *sources* ("Search in: …") and the search
  // *modes* ("Match: …"). Each toggle is a nested non-wrapping row — an
  // atomic group of `toggle + accelerator` — so the wrapping parent never
  // splits a label from its `Alt+…` hint across lines.
  const prefix = (text: string): WidgetSpec =>
    raw([styledRow([{ text, style: { fg: "ui.popup_border_fg" } }])]);

  const sources: WidgetSpec[] = [spacer(1), prefix(editor.t("label.search_in"))];
  SCOPES.forEach((s) => {
    sources.push(spacer(2));
    const parts: WidgetSpec[] = [
      toggle(scopeEnabled[s.id], editor.t(s.labelKey), { key: s.id }),
    ];
    const accel = editor.getKeybindingLabel(s.action, "prompt");
    if (accel) {
      parts.push(raw([styledRow([{ text: ` ${accel}`, style: { fg: "ui.help_key_fg" } }])]));
    }
    sources.push(row(...parts));
  });

  const modes: WidgetSpec[] = [spacer(1), prefix(editor.t("label.match"))];
  MODES.forEach((m) => {
    modes.push(spacer(2));
    modes.push(toggle(searchModes[m.id], editor.t(m.labelKey), { key: m.key }));
  });

  return col(wrappingRow(...sources), wrappingRow(...modes));
}

// Footer: the active provider, the truncation indicator, and the
// generic/action hints that have no on-screen control to attach to
// (provider-cycle, save-matches). Per-control accelerators live on the
// toggles themselves (see buildToolbarSpec).
function buildFooterSegments(provider: LiveGrepProvider | null): StyledText[] {
  const sepStyle = { fg: "ui.popup_border_fg" };
  const hintStyle = { fg: "ui.help_key_fg" };
  const segs: StyledText[] = [];
  const push = (parts: StyledText[]) => {
    if (segs.length > 0) segs.push({ text: " · ", style: sepStyle });
    segs.push(...parts);
  };
  // Only surface the grep provider when a file-backed scope is on — it's
  // irrelevant when searching only buffers/terminals/diagnostics.
  if (provider && (scopeEnabled.files || scopeEnabled.ignored)) {
    push([{ text: "Provider: " }, { text: provider.name, style: { bold: true } }]);
  }
  if (lastSearchTruncated) {
    push([{ text: `${MAX_RESULTS}+ matches` }]);
  }
  const pushHint = (key: string | null, label: string) => {
    if (!key) return;
    push([{ text: key, style: hintStyle }, { text: ` ${label}` }]);
  };
  pushHint(
    editor.getKeybindingLabel("cycle_live_grep_provider", "prompt"),
    "switch grep provider"
  );
  pushHint(
    editor.getKeybindingLabel("live_grep_export_quickfix", "prompt"),
    "save matches"
  );
  if (segs.length > 0) segs.unshift({ text: " " });
  return segs;
}

// Refresh the overlay chrome: the scope toolbar (header band) and the footer
// hints. Name kept as `updateOverlayTitle` for its many call sites; it no
// longer sets a styled-text title — the widget toolbar replaces it.
function updateOverlayTitle(provider: LiveGrepProvider | null): void {
  editor.setPromptToolbar(buildToolbarSpec());
  editor.setPromptFooter(buildFooterSegments(provider));
}

async function selectProvider(): Promise<LiveGrepProvider | null> {
  if (cachedSelected !== undefined) {
    updateOverlayTitle(cachedSelected);
    return cachedSelected;
  }
  for (const p of providers) {
    try {
      const ok = await Promise.resolve(p.isAvailable());
      if (ok) {
        cachedSelected = p;
        editor.debug(`[live-grep] selected provider: ${p.name}`);
        updateOverlayTitle(p);
        return p;
      }
    } catch (e) {
      editor.debug(`[live-grep] ${p.name}.isAvailable threw: ${e}`);
    }
  }
  cachedSelected = null;
  updateOverlayTitle(null);
  return null;
}

// ── Built-in providers ──────────────────────────────────────────

registerProvider({
  name: "rg",
  priority: -1,
  isAvailable: async () => {
    try {
      const r = await editor.spawnProcess("rg", ["--version"], editor.getCwd());
      return r.exit_code === 0;
    } catch {
      return false;
    }
  },
  search: async (query, { cwd, maxResults, includeIgnored, wholeWord, regex }) => {
    const args = [
      "--line-number",
      "--column",
      "--no-heading",
      "--color=never",
      "--smart-case",
      `--max-count=${maxResults}`,
      // Always skip the VCS metadata dir — even with the Ignored scope
      // on, `.git` internals are never what the user is looking for.
      "-g", "!.git",
    ];
    if (regex === false) args.push("--fixed-strings");
    if (wholeWord) args.push("--word-regexp");
    if (includeIgnored) {
      // Search ignored *and* hidden files (dotfiles). `.git` stays
      // excluded via the glob above.
      args.push("--no-ignore", "--hidden");
    } else {
      // Default: respect ignore files, plus prune the usual heavy
      // build/vendor dirs and lockfiles that bury real hits.
      args.push("-g", "!node_modules", "-g", "!target", "-g", "!*.lock");
    }
    args.push("--", query);
    const r = await editor.spawnProcess("rg", args, cwd);
    if (r.exit_code === 0) {
      return parseGrepOutput(r.stdout, maxResults, (msg) => editor.debug(msg)) as GrepMatch[];
    }
    throw new Error(`rg exited with code ${r.exit_code}: ${r.stderr}`);
  },
});

registerProvider({
  name: "ag",
  priority: -2,
  isAvailable: async () => {
    try {
      const r = await editor.spawnProcess("ag", ["--version"], editor.getCwd());
      return r.exit_code === 0;
    } catch {
      return false;
    }
  },
  search: async (query, { cwd, maxResults, wholeWord, regex }) => {
    const args = [
      "--column",
      "--numbers",
      "--nogroup",
      "--nocolor",
      "--smart-case",
      "--ignore", ".git",
      "--ignore", "node_modules",
      "--ignore", "target",
      "--ignore", "*.lock",
    ];
    if (regex === false) args.push("--literal");
    if (wholeWord) args.push("--word-regexp");
    args.push("--", query);
    const r = await editor.spawnProcess("ag", args, cwd);
    if (r.exit_code === 0 || r.exit_code === 1) {
      return parseGrepOutput(r.stdout, maxResults, (msg) => editor.debug(msg)) as GrepMatch[];
    }
    throw new Error(`ag exited with code ${r.exit_code}: ${r.stderr}`);
  },
});

registerProvider({
  name: "git-grep",
  // Top priority. git grep is the default *when available* — i.e.
  // when the working directory is inside a git repo with `git`
  // installed. `isAvailable` checks both, and outside a repo the
  // registry falls through to ripgrep / ag / ack / grep in order.
  priority: 0,
  isAvailable: async () => {
    try {
      // git grep needs both `git` on PATH and to be inside a repo.
      const cwd = editor.getCwd();
      const ver = await editor.spawnProcess("git", ["--version"], cwd);
      if (ver.exit_code !== 0) return false;
      const inRepo = await editor.spawnProcess(
        "git",
        ["rev-parse", "--is-inside-work-tree"],
        cwd
      );
      return inRepo.exit_code === 0;
    } catch {
      return false;
    }
  },
  search: async (query, { cwd, maxResults, includeIgnored, wholeWord, regex }) => {
    const args = ["grep", "-n", "--column", "-I"];
    // Default git-grep is basic regex; use extended when regex is on, or
    // fixed-strings when it's off so the query is matched literally.
    args.push(regex === false ? "-F" : "-E");
    if (wholeWord) args.push("-w");
    if (includeIgnored) {
      // Widen beyond tracked files: include untracked, and stop
      // honouring the standard ignore files so `.gitignore`d content
      // is searched too.
      args.push("--untracked", "--no-exclude-standard");
    }
    args.push("-e", query);
    const r = await editor.spawnProcess("git", args, cwd);
    // git grep exits 1 when no matches — treat as empty, not error.
    if (r.exit_code === 0 || r.exit_code === 1) {
      return parseGrepOutput(r.stdout, maxResults, (msg) => editor.debug(msg)) as GrepMatch[];
    }
    throw new Error(`git grep exited with code ${r.exit_code}: ${r.stderr}`);
  },
});

registerProvider({
  name: "ack",
  priority: -3,
  // Note: ack/grep are kept at lower priority than ripgrep/ag/
  // git-grep because they're slower on large trees; the cycler
  // skips them automatically when a faster backend is available.
  isAvailable: async () => {
    try {
      const r = await editor.spawnProcess("ack", ["--version"], editor.getCwd());
      return r.exit_code === 0;
    } catch {
      return false;
    }
  },
  search: async (query, { cwd, maxResults, wholeWord, regex }) => {
    const args = ["--nocolor", "--column", "--smart-case"];
    if (regex === false) args.push("--literal");
    if (wholeWord) args.push("--word-regexp");
    args.push("--", query);
    const r = await editor.spawnProcess("ack", args, cwd);
    if (r.exit_code === 0 || r.exit_code === 1) {
      return parseGrepOutput(r.stdout, maxResults, (msg) => editor.debug(msg)) as GrepMatch[];
    }
    throw new Error(`ack exited with code ${r.exit_code}: ${r.stderr}`);
  },
});

// Note: `fff` is *not* shipped as a built-in. There's no canonical
// "fff" grep tool with a known argument shape — the most popular
// binary named `fff` is the bash file-manager
// (https://github.com/dylanaraps/fff), which is interactive and
// doesn't accept a search pattern as an argument. Wiring a guess
// here would silently return zero results for that flavour. Users
// who have their own `fff` (or any other custom tool) should
// register it from init.ts where the exact CLI is known. The
// starter init.ts template documents the pattern.

registerProvider({
  name: "grep",
  priority: -4,
  isAvailable: async () => {
    try {
      const r = await editor.spawnProcess("grep", ["--version"], editor.getCwd());
      return r.exit_code === 0;
    } catch {
      return false;
    }
  },
  search: async (query, { cwd, maxResults, wholeWord, regex }) => {
    const args = [
      "-rn",
      "-I",
      "--exclude-dir=.git",
      "--exclude-dir=node_modules",
      "--exclude-dir=target",
    ];
    args.push(regex === false ? "-F" : "-E");
    if (wholeWord) args.push("-w");
    args.push("--", query, ".");
    const r = await editor.spawnProcess("grep", args, cwd);
    if (r.exit_code === 0 || r.exit_code === 1) {
      // grep emits `path:line:content` (no column). parseGrepOutput's
      // 3-field fallback handles the missing column (defaults to 1).
      return parseGrepOutput(r.stdout, maxResults, (msg) => editor.debug(msg)) as GrepMatch[];
    }
    throw new Error(`grep exited with code ${r.exit_code}: ${r.stderr}`);
  },
});

// ── Wiring ──────────────────────────────────────────────────────

function badgeFor(source: ScopeId | undefined): string {
  if (!source || source === "files") return "";
  const def = SCOPES.find((s) => s.id === source);
  return def?.badge ? `[${def.badge}] ` : "";
}

const finder = new Finder<GrepMatch>(editor, {
  id: "live-grep",
  format: (match) => ({
    label: `${badgeFor(match.source)}${match.file}:${match.line}`,
    description:
      match.content.length > 60
        ? match.content.substring(0, 57).trim() + "..."
        : match.content.trim(),
    location: {
      file: match.file,
      line: match.line,
      column: match.column,
    },
  }),
  onClose: () => {
    overlayActive = false;
  },
  // Override the Finder's default "open file + status: Opened X"
  // so we can surface the resume shortcut here. The shortcut is
  // hidden inside the overlay (it can't apply while the overlay
  // is open), but it's exactly what the user needs to know once
  // they've jumped to a result and want to keep browsing.
  onSelect: (_item, entry) => {
    if (entry.location) {
      editor.openFile(
        entry.location.file,
        entry.location.line,
        entry.location.column
      );
    }
    const resumeKey = editor.getKeybindingLabel("resume_live_grep", "normal");
    if (resumeKey) {
      editor.setStatus(`${resumeKey} to resume search`);
    }
  },
  preview: false,
  maxResults: MAX_RESULTS,
});

/**
 * Switch to the next *available* registered provider, in priority
 * order, wrapping at the end. Unavailable providers (those whose
 * `isAvailable()` returns false right now) are skipped — pressing
 * the cycle key never lands on a backend that can't actually run.
 *
 * Side effects: updates `cachedSelected` so the next search uses
 * the new provider, fires a status message naming the new
 * provider, and re-runs the current query (via the prompt-changed
 * hook the Finder is already listening for).
 */
async function cycleProvider(): Promise<void> {
  if (providers.length === 0) {
    editor.setStatus("Live Grep: no providers registered");
    return;
  }
  // Find the position to start scanning from. If a provider is
  // currently cached, start *after* it so we genuinely move on; if
  // not, start from the top of the list.
  const currentIdx =
    cachedSelected != null ? providers.indexOf(cachedSelected) : -1;
  // Walk the full list once (mod len), skipping any provider whose
  // probe says unavailable. If we wrap back to where we started
  // without finding a different available provider, surface a
  // status message and leave the selection alone.
  for (let step = 1; step <= providers.length; step++) {
    const idx = (currentIdx + step + providers.length) % providers.length;
    const candidate = providers[idx];
    if (candidate === cachedSelected) {
      // Looped past the start without finding anything else
      // available; only the current one is usable.
      editor.setStatus(
        `Live Grep: no other available providers (still on ${candidate.name})`
      );
      return;
    }
    let ok = false;
    try {
      ok = await Promise.resolve(candidate.isAvailable());
    } catch (e) {
      editor.debug(`[live-grep] ${candidate.name}.isAvailable threw: ${e}`);
    }
    if (!ok) continue;
    cachedSelected = candidate;
    // Reflect the new provider in the overlay's title bar
    // immediately — the status row gets clobbered by the search
    // result count, but the title stays put.
    updateOverlayTitle(candidate);
    // Re-run the current query through the new provider so the
    // result list updates without the user having to type a
    // throwaway character. `refresh()` itself sets status to
    // "Found N matches" — we want the user to see the *cycle*
    // result, so re-set the status afterwards.
    await finder.refresh();
    editor.setStatus(`Live Grep: switched to ${candidate.name}`);
    return;
  }
  editor.setStatus("Live Grep: no available providers");
}
registerHandler("live_grep_cycle_provider", cycleProvider);
// `registerHandler` only sets a globalThis function — to make the
// editor's `execute_action` path find it across the plugin-context
// boundary the action also has to live in the registered-actions
// table. `registerCommand` is the public-facing mechanism that
// inserts that entry. Doubles as a palette-discoverable command.
editor.registerCommand(
  "%cmd.live_grep_cycle_provider",
  "%cmd.live_grep_cycle_provider_desc",
  "live_grep_cycle_provider",
  null
);

// Don't pull whole multi-MB buffers across the FFI boundary to grep
// them line-by-line in JS — cap at a sane size and skip the rest.
const MAX_BUFFER_SCAN_BYTES = 2_000_000;

// Strip ANSI escape sequences so terminal scrollback (stored with
// colour codes in the backing file) shows as plain text in results.
function stripAnsi(s: string): string {
  return s
    // CSI … final byte (colours, cursor moves, etc.)
    .replace(/\x1b\[[0-9;?]*[ -/]*[@-~]/g, "")
    // OSC … terminated by BEL or ST
    .replace(/\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)/g, "")
    // Remaining two-byte escapes
    .replace(/\x1b[@-Z\\-_]/g, "");
}

/** Search terminal scrollback. Terminal backing files live under the
 *  current working directory's terminal subdir
 *  (`getTerminalDir()` → `<data_dir>/terminals/<encoded-cwd>/`) — open
 *  terminals stream their scrollback there live, and closed terminals
 *  are retained there too (renamed `*-closed-*.txt`). Scoping to that
 *  subdir keeps the search to *this* project / worktree. We grep it
 *  with rg (falling back to grep), then strip ANSI for display.
 *  Opening a hit opens the backing file at the matched line. */
async function searchTerminals(query: string, limit: number): Promise<GrepMatch[]> {
  if (limit <= 0) return [];
  const dir = editor.getTerminalDir();
  const cwd = editor.getCwd();
  let raw: GrepMatch[] = [];
  try {
    const rgArgs = [
      "--line-number", "--column", "--no-heading", "--color=never",
      "--smart-case", "--text", `--max-count=${limit}`,
      // Only the rendered `.txt` backing files — not the raw `.log`
      // replay logs, which would double every hit.
      "-g", "*.txt",
    ];
    if (searchModes.regex === false) rgArgs.push("--fixed-strings");
    if (searchModes.word) rgArgs.push("--word-regexp");
    rgArgs.push("--", query, dir);
    const r = await editor.spawnProcess("rg", rgArgs, cwd);
    if (r.exit_code === 0) {
      raw = parseGrepOutput(r.stdout, limit, (m) => editor.debug(m)) as GrepMatch[];
    } else if (r.exit_code !== 1) {
      // rg missing or path error → fall back to grep (-a: treat the
      // ANSI-laden logs as text rather than skipping them as binary).
      const gArgs = ["-rn", "-a", "--include=*.txt"];
      gArgs.push(searchModes.regex === false ? "-F" : "-E");
      if (searchModes.word) gArgs.push("-w");
      gArgs.push("--", query, dir);
      const g = await editor.spawnProcess("grep", gArgs, cwd);
      if (g.exit_code === 0) {
        raw = parseGrepOutput(g.stdout, limit, (m) => editor.debug(m)) as GrepMatch[];
      }
    }
  } catch (e) {
    editor.debug(`[live_grep:terminals] ${e}`);
  }
  return raw.slice(0, limit).map((m) => ({
    ...m,
    source: "terminals" as const,
    content: stripAnsi(m.content),
  }));
}

/** Search the text of currently-open, modified file buffers.
 *  Scoped to *modified* buffers on purpose: unmodified buffers are
 *  already covered by the on-disk file scan, so this surfaces exactly
 *  the unsaved edits a disk grep would miss, without double-reporting. */
async function searchOpenBuffers(query: string, limit: number): Promise<GrepMatch[]> {
  if (limit <= 0) return [];
  const out: GrepMatch[] = [];
  const matchCol = buildLineMatcher(query);
  for (const b of editor.listBuffers()) {
    if (out.length >= limit) break;
    if (b.is_virtual || !b.path || !b.modified) continue;
    if (b.length > MAX_BUFFER_SCAN_BYTES) continue;
    let text: string;
    try {
      text = await editor.getBufferText(b.id, 0, b.length);
    } catch {
      continue;
    }
    const lines = text.split("\n");
    for (let i = 0; i < lines.length && out.length < limit; i++) {
      const col = matchCol(lines[i]);
      if (col > 0) {
        out.push({ file: b.path, line: i + 1, column: col, content: lines[i], source: "buffers" });
      }
    }
  }
  return out;
}

function severityLabel(sev: number | null | undefined): string {
  switch (sev) {
    case 1: return "error";
    case 2: return "warning";
    case 3: return "info";
    case 4: return "hint";
    default: return "diagnostic";
  }
}

/** Search active LSP diagnostics by message text. Matches jump to the
 *  diagnostic's range like any other location. */
function searchDiagnostics(query: string, limit: number): GrepMatch[] {
  if (limit <= 0) return [];
  const out: GrepMatch[] = [];
  const matchCol = buildLineMatcher(query);
  for (const d of editor.getAllDiagnostics()) {
    if (out.length >= limit) break;
    if (matchCol(d.message) <= 0) continue;
    const file = d.uri.startsWith("file://") ? decodeURIComponent(d.uri.slice("file://".length)) : d.uri;
    out.push({
      file,
      line: (d.range?.start?.line ?? 0) + 1,
      column: (d.range?.start?.character ?? 0) + 1,
      content: `${severityLabel(d.severity)}: ${d.message}`,
      source: "diagnostics",
    });
  }
  return out;
}

// Run the project-file grep for the enabled file-backed scopes
// (`files` / `ignored`). Returns null when no provider is available so
// the caller can decide whether that's fatal (no other scope on) or
// merely a skipped source.
async function searchFiles(query: string): Promise<GrepMatch[] | null> {
  const provider = await selectProvider();
  if (!provider) return null;
  try {
    const results = await provider.search(query, {
      cwd: editor.getCwd(),
      maxResults: MAX_RESULTS,
      includeIgnored: scopeEnabled.ignored,
      wholeWord: searchModes.word,
      regex: searchModes.regex,
    });
    return results.map((m) => ({ ...m, source: "files" as const }));
  } catch (e) {
    editor.error(`[live_grep:${provider.name}] ${e}`);
    throw new Error(`${provider.name}: ${e instanceof Error ? e.message : String(e)}`);
  }
}

// Fan the query out across every enabled scope and merge into one
// capped, tagged result list. Order is files → buffers → diagnostics
// so the most common hits lead.
async function search(query: string): Promise<GrepMatch[]> {
  lastQuery = query;
  const wasTruncated = lastSearchTruncated;
  const results: GrepMatch[] = [];
  const remaining = () => MAX_RESULTS - results.length;

  if (scopeEnabled.files || scopeEnabled.ignored) {
    const fileMatches = await searchFiles(query);
    if (fileMatches === null) {
      // No grep backend. Only fatal if there's nothing else to search.
      if (!scopeEnabled.buffers && !scopeEnabled.diagnostics) {
        throw new Error(
          "no search backend available — install ripgrep, or register a provider via init.ts (`editor.getPluginApi(\"live-grep\")?.registerProvider(...)`)."
        );
      }
    } else {
      for (const m of fileMatches) {
        if (results.length >= MAX_RESULTS) break;
        results.push(m);
      }
    }
  }

  if (scopeEnabled.buffers && remaining() > 0) {
    results.push(...await searchOpenBuffers(query, remaining()));
  }

  if (scopeEnabled.terminals && remaining() > 0) {
    results.push(...await searchTerminals(query, remaining()));
  }

  if (scopeEnabled.diagnostics && remaining() > 0) {
    results.push(...searchDiagnostics(query, remaining()));
  }

  lastSearchTruncated = results.length >= MAX_RESULTS;
  // Refresh the toolbar whenever the truncation indicator changes so
  // it appears (or disappears) alongside the new results.
  if (lastSearchTruncated !== wasTruncated) {
    updateOverlayTitle(cachedSelected ?? null);
  }
  return results;
}

// Scope toggling is host-owned: the host flips the toggle's checked state
// (on click, Space on the focused toggle, or the Alt+… shortcuts below) and
// emits a `widget_event`; we react here by syncing the scope set and
// re-running the search. We never re-send the toolbar spec on a toggle — the
// host already updated the checkbox visual.
editor.on("widget_event", (args) => {
  if (!overlayActive || args.event_type !== "toggle") return;
  const payload = args.payload as { checked?: boolean } | undefined;
  const scope = SCOPES.find((s) => s.id === args.widget_key);
  const mode = MODES.find((m) => m.key === args.widget_key);
  let label: string;
  let on: boolean;
  if (scope) {
    on = payload?.checked ?? !scopeEnabled[scope.id];
    scopeEnabled[scope.id] = on;
    label = editor.t(scope.labelKey);
  } else if (mode) {
    on = payload?.checked ?? !searchModes[mode.id];
    searchModes[mode.id] = on;
    label = editor.t(mode.labelKey);
  } else {
    return;
  }
  // The footer's provider line depends on the file scopes; refresh it.
  editor.setPromptFooter(buildFooterSegments(cachedSelected ?? null));
  void finder.refresh();
  editor.setStatus(`Search: ${label} ${on ? "on" : "off"}`);
});

// The per-scope Alt+… shortcuts (and palette entries) just route through the
// host toggle path, so click / Space / shortcut all converge on the same
// widget_event above. The action's keybinding label is what the toolbar
// shows as each toggle's inline accelerator.
for (const s of SCOPES) {
  registerHandler(s.action, () => {
    editor.toggleOverlayToolbarWidget(s.id);
  });
  editor.registerCommand(`%cmd.${s.action}`, `%cmd.${s.action}_desc`, s.action, null);
}

// Shared open flow for both fresh start and resume. `initialQuery`
// pre-fills the input (Resume passes the last query) — Resume is just the
// same flow with prepopulated data, no bespoke overlay.
function openLiveGrep(initialQuery: string): void {
  overlayActive = true;
  finder.prompt({
    title: editor.t("prompt.live_grep"),
    source: {
      mode: "search",
      search,
      debounceMs: 150,
      minQueryLength: 2,
    },
    floatingOverlay: true,
    ...(initialQuery ? { initialQuery } : {}),
  });
  // Pre-populate the overlay's frame title with the cached
  // provider name (if any) before the user types — avoids the
  // brief "Live Grep" → "Live Grep · rg" flash when the
  // first search resolves selectProvider().
  if (cachedSelected) {
    updateOverlayTitle(cachedSelected);
  } else {
    // Kick off provider probing in the background so the title
    // updates as soon as the first available probe resolves,
    // rather than waiting for the first keystroke.
    void selectProvider();
  }
}

function start_live_grep(): void {
  openLiveGrep("");
}
registerHandler("start_live_grep", start_live_grep);

// Resume: identical flow, just seeded with the last query so the user
// picks up where they left off — same overlay, same toolbar, same scopes.
function resume_live_grep(): void {
  openLiveGrep(lastQuery);
}
registerHandler("resume_live_grep", resume_live_grep);
// Register the action→plugin-context mapping so the core `resume_live_grep`
// action (Alt+R / the built-in "Resume Live Grep" palette command) resolves
// to this handler. A never-activated custom context keeps it out of the
// palette so it doesn't duplicate the core entry.
editor.registerCommand(
  "Live Grep: Resume (internal)",
  "",
  "resume_live_grep",
  "live-grep-internal"
);

editor.registerCommand(
  "%cmd.live_grep",
  "%cmd.live_grep_desc",
  "start_live_grep",
  null
);

editor.exportPluginApi("live-grep", {
  registerProvider,
  unregisterProvider,
  listProviders(): { name: string; priority: number }[] {
    return providers.map((p) => ({
      name: p.name,
      priority: p.priority ?? 0,
    }));
  },
  resetSelection(): void {
    cachedSelected = undefined;
  },
} satisfies LiveGrepApi);

editor.debug("Live Grep plugin loaded (provider registry)");
