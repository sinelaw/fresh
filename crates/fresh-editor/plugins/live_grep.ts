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

const editor = getEditor();

// One Live Grep match. Mirrors the JSON shape ripgrep emits with
// `--line-number --column --no-heading`; built-in non-rg providers
// (git grep, grep) normalise to this shape via parseGrepOutput.
interface GrepMatch {
  file: string;
  line: number;
  column: number;
  content: string;
}

/** Options passed to a provider's `search` callback. */
export interface SearchOpts {
  /** Working directory the search should run in (the editor's cwd). */
  cwd: string;
  /** Caller's preferred result cap. Providers may return fewer.
   *  Returning more is allowed; the Finder caps at its own
   *  `maxResults`. */
  maxResults: number;
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

// ── Registry ──────────────────────────────────────────────────────

const providers: LiveGrepProvider[] = [];
let cachedSelected: LiveGrepProvider | null | undefined = undefined;

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

function updateOverlayTitle(provider: LiveGrepProvider | null): void {
  // Reflect the active provider in the floating overlay's frame
  // header so the user always knows which backend is producing
  // the results, even after the search-result status overwrites
  // any one-shot "switched to" message. Append the actual bound
  // shortcuts (whatever the user remapped to) as hints — pulled
  // from the editor's keybinding registry, not hardcoded, so they
  // always match the user's actual config.
  const hints: string[] = [];
  const cycleKey = editor.getKeybindingLabel(
    "cycle_live_grep_provider",
    "prompt"
  );
  if (cycleKey) hints.push(`${cycleKey} cycle`);
  const exportKey = editor.getKeybindingLabel(
    "live_grep_export_quickfix",
    "prompt"
  );
  if (exportKey) hints.push(`${exportKey} → Quickfix`);
  const resumeKey = editor.getKeybindingLabel("resume_live_grep", "normal");
  if (resumeKey) hints.push(`${resumeKey} resume`);
  const hintSuffix = hints.length > 0 ? ` · ${hints.join(" · ")}` : "";
  const label = provider
    ? `Live Grep · ${provider.name}${hintSuffix}`
    : `Live Grep${hintSuffix}`;
  editor.setPromptTitle(label);
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
  name: "ripgrep",
  priority: -1,
  isAvailable: async () => {
    try {
      const r = await editor.spawnProcess("rg", ["--version"], editor.getCwd());
      return r.exit_code === 0;
    } catch {
      return false;
    }
  },
  search: async (query, { cwd, maxResults }) => {
    const r = await editor.spawnProcess(
      "rg",
      [
        "--line-number",
        "--column",
        "--no-heading",
        "--color=never",
        "--smart-case",
        `--max-count=${maxResults}`,
        "-g", "!.git",
        "-g", "!node_modules",
        "-g", "!target",
        "-g", "!*.lock",
        "--",
        query,
      ],
      cwd
    );
    if (r.exit_code === 0) {
      return parseGrepOutput(r.stdout, maxResults) as GrepMatch[];
    }
    return [];
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
  search: async (query, { cwd, maxResults }) => {
    const r = await editor.spawnProcess(
      "ag",
      [
        "--column",
        "--numbers",
        "--nogroup",
        "--nocolor",
        "--smart-case",
        "--ignore", ".git",
        "--ignore", "node_modules",
        "--ignore", "target",
        "--ignore", "*.lock",
        "--",
        query,
      ],
      cwd
    );
    if (r.exit_code === 0 || r.exit_code === 1) {
      return parseGrepOutput(r.stdout, maxResults) as GrepMatch[];
    }
    return [];
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
  search: async (query, { cwd, maxResults }) => {
    const r = await editor.spawnProcess(
      "git",
      ["grep", "-n", "--column", "-I", "-e", query],
      cwd
    );
    // git grep exits 1 when no matches — treat as empty, not error.
    if (r.exit_code === 0 || r.exit_code === 1) {
      return parseGrepOutput(r.stdout, maxResults) as GrepMatch[];
    }
    return [];
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
  search: async (query, { cwd, maxResults }) => {
    const r = await editor.spawnProcess(
      "ack",
      [
        "--nocolor",
        "--column",
        "--smart-case",
        "--",
        query,
      ],
      cwd
    );
    if (r.exit_code === 0 || r.exit_code === 1) {
      return parseGrepOutput(r.stdout, maxResults) as GrepMatch[];
    }
    return [];
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
  search: async (query, { cwd, maxResults }) => {
    const r = await editor.spawnProcess(
      "grep",
      [
        "-rn",
        "--column",
        "-I",
        "--exclude-dir=.git",
        "--exclude-dir=node_modules",
        "--exclude-dir=target",
        "--",
        query,
        ".",
      ],
      cwd
    );
    if (r.exit_code === 0 || r.exit_code === 1) {
      // POSIX grep doesn't emit `path:line:col:content` natively
      // even with `--column`; on most BSD/GNU greps the format is
      // still `path:line:content`. parseGrepOutput tolerates the
      // missing column.
      return parseGrepOutput(r.stdout, maxResults) as GrepMatch[];
    }
    return [];
  },
});

// ── Wiring ──────────────────────────────────────────────────────

const finder = new Finder<GrepMatch>(editor, {
  id: "live-grep",
  format: (match) => ({
    label: `${match.file}:${match.line}`,
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
  preview: false,
  maxResults: 100,
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

async function search(query: string): Promise<GrepMatch[]> {
  const provider = await selectProvider();
  if (!provider) {
    editor.setStatus(
      "Live Grep: no search backend available — install ripgrep, or register a provider via init.ts (`editor.getPluginApi(\"live-grep\")?.registerProvider(...)`)."
    );
    return [];
  }
  try {
    return await provider.search(query, {
      cwd: editor.getCwd(),
      maxResults: 100,
    });
  } catch (e) {
    editor.setStatus(`Live Grep (${provider.name}) failed: ${e}`);
    return [];
  }
}

function start_live_grep(): void {
  finder.prompt({
    title: editor.t("prompt.live_grep"),
    source: {
      mode: "search",
      search,
      debounceMs: 150,
      minQueryLength: 2,
    },
    floatingOverlay: true,
  });
  // Pre-populate the overlay's frame title with the cached
  // provider name (if any) before the user types — avoids the
  // brief "Live Grep" → "Live Grep · ripgrep" flash when the
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
registerHandler("start_live_grep", start_live_grep);

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
