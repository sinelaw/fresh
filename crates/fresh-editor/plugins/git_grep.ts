/// <reference path="./lib/fresh.d.ts" />

/**
 * Git Grep Plugin
 *
 * Provides interactive git grep functionality with live search results
 * and preview panel. Uses the Finder abstraction for unified search UX.
 */

import { Finder, parseGrepOutput } from "./lib/finder.ts";
import { git, resolveGitRepo, toAbsInRepo } from "./lib/git_repo.ts";
import { raw, row, spacer, styledRow, toggle } from "./lib/widgets.ts";

const editor = getEditor();

// ── Case sensitivity ──────────────────────────────────────────────
//
// `git grep` matches case-sensitively by default. Fresh's search
// surfaces do not (issue #3212): a search for `todo` finds `TODO`
// unless you say otherwise, so this passes `-i` until the Case toggle
// on the prompt toolbar is checked.
//
// Module-level, so the choice outlives one prompt and holds for the
// rest of the session. The `editor.search.case_sensitive` config
// preset decides only where the session starts.
//
// The key is namespaced: widget events are delivered to every plugin
// listening, and a bare "case" is what the Search & Replace panel's own
// toggle is keyed, so an unqualified key would have each plugin acting
// on the other's clicks.
const CASE_WIDGET_KEY = "git_grep_case";

// True from the moment our prompt opens until it is dismissed. A
// *confirmed* selection doesn't run the Finder's `onClose`, so this can
// stay set after the prompt is gone — which is harmless, because the
// namespaced key is what actually keeps both paths inert elsewhere: no
// other toolbar carries `git_grep_case`, so a widget event never names
// it and `toggleOverlayToolbarWidget` finds nothing to flip.
let promptActive = false;

function configuredCaseSensitive(): boolean {
  const cfg = editor.getConfig() as
    | { editor?: { search?: { case_sensitive?: unknown } } }
    | null;
  return cfg?.editor?.search?.case_sensitive === true;
}

let caseSensitive = configuredCaseSensitive();

/** The prompt's one-row toolbar: a single Case toggle, with its
 *  keybinding hint inline when the action is bound. */
function setToolbar(): void {
  const parts = [
    spacer(1),
    toggle(caseSensitive, editor.t("mode.case"), { key: CASE_WIDGET_KEY }),
  ];
  const accel = editor.getKeybindingLabel("git_grep_toggle_case", "prompt");
  if (accel) {
    parts.push(
      raw([styledRow([{ text: ` ${accel}`, style: { fg: "ui.help_key_fg" } }])]),
    );
  }
  editor.setPromptToolbar(row(...parts));
}

// One git-grep hit. `file` is repo-relative (what git prints and what the
// user sees); `abs` is the absolute path we actually open, because in a
// monorepo the repo root differs from the workspace cwd, so a repo-relative
// path wouldn't resolve.
interface GrepMatch {
  file: string;
  abs: string;
  line: number;
  column: number;
  content: string;
}

// Create the finder instance
const finder = new Finder<GrepMatch>(editor, {
  id: "git-grep",
  format: (match) => ({
    label: `${match.file}:${match.line}`,
    description:
      match.content.length > 60
        ? match.content.substring(0, 57).trim() + "..."
        : match.content.trim(),
    location: {
      file: match.abs,
      line: match.line,
      column: match.column,
    },
  }),
  // The floating overlay draws its own preview band, so the Finder's
  // split preview must be off — `lib/finder.ts` names this case exactly
  // ("or that draw their own preview"), and Live Grep does the same. With
  // both on, arrowing through results opens each hit as a preview tab in
  // the split *behind* the overlay, churning that split's tab bar and
  // loading every file twice (two LSP didOpen/didClose per result).
  preview: false,
  maxResults: 100,
  onClose: () => {
    promptActive = false;
  },
});

// Search function using git grep. Resolves the repo from the active buffer
// (so it works from a sub-project buffer even when the workspace root isn't a
// repo) and runs git inside it via the shared gateway.
async function searchWithGitGrep(query: string): Promise<GrepMatch[]> {
  const repo = await resolveGitRepo(editor);
  if (!repo) {
    editor.setStatus(editor.t("status.not_in_git"));
    return [];
  }

  const args = ["grep", "-n", "--column", "-I"];
  if (!caseSensitive) args.push("-i");
  args.push("--", query);
  const result = await git(editor, repo, args);

  // git grep's exit codes: 0 = matches found, 1 = no matches (a normal,
  // successful search — NOT a failure), >=2 = a real error (bad pattern,
  // broken repo, …). Treating exit 1 as an error made every fruitless search
  // log an ERROR and raise the status-bar warning badge, so an ordinary
  // "nothing matched" looked like a plugin crash (issue #2591). Mirror
  // live_grep's git-grep provider: accept 0 and 1, error only on a real
  // failure.
  if (result.exit_code === 0 || result.exit_code === 1) {
    const matches = parseGrepOutput(
      result.stdout,
      100,
      (msg) => editor.debug(msg),
    ) as GrepMatch[];
    // `git grep` prints repo-relative paths; join each onto the repo root so
    // selecting a result opens the right file regardless of the workspace cwd.
    return matches.map((m) => ({ ...m, abs: toAbsInRepo(editor, repo, m.file) }));
  }
  // A negative exit code means the search was superseded/killed as the user
  // kept typing (the Finder cancels the in-flight git) — stay quiet for that.
  if (result.exit_code > 1) {
    editor.error(`[git_grep] process exited with code ${result.exit_code}: ${result.stderr}`);
    editor.setStatus(`git grep failed (exit ${result.exit_code})`);
  }
  return [];
}

// Global function to start git grep
function start_git_grep() : void {
  finder.prompt({
    title: editor.t("prompt.grep"),
    source: {
      mode: "search",
      search: searchWithGitGrep,
      debounceMs: 150,
      minQueryLength: 1,
    },
    // A toolbar is only painted on a floating overlay — the host draws it
    // as part of the overlay card, and a bottom-row prompt has nowhere to
    // put one (`overlay_card_description` declines a non-overlay prompt).
    // Universal Search is already an overlay for the same reason, and this
    // prompt shows a preview pane, which wants the room regardless.
    floatingOverlay: true,
  });
  promptActive = true;
  setToolbar();
}
registerHandler("start_git_grep", start_git_grep);

// The toggle: click it, Space on it, or run the action. All three
// converge here — the action just drives the same widget the pointer
// does, so there is only one path that can flip the flag.
editor.on("widget_event", (args) => {
  if (!promptActive) return;
  if (args.event_type !== "toggle" || args.widget_key !== CASE_WIDGET_KEY) return;
  const checked = (args.payload as { checked?: boolean } | undefined)?.checked;
  caseSensitive = checked ?? !caseSensitive;
  // The host already flipped the toggle's visual; rebuilding keeps the
  // spec and the flag in step (and the accelerator hint with them).
  setToolbar();
  void finder.refresh();
  editor.setStatus(
    `${editor.t("mode.case")}: ${caseSensitive ? "on" : "off"}`,
  );
});

registerHandler("git_grep_toggle_case", () => {
  if (!promptActive) return;
  editor.toggleOverlayToolbarWidget(CASE_WIDGET_KEY);
});

// Register commands
editor.registerCommand("%cmd.grep", "%cmd.grep_desc", "start_git_grep", null);
editor.registerCommand(
  "%cmd.git_grep_toggle_case",
  "%cmd.git_grep_toggle_case_desc",
  "git_grep_toggle_case",
  null,
);

// Log that plugin loaded successfully
editor.debug("Git Grep plugin loaded (using Finder abstraction)");
