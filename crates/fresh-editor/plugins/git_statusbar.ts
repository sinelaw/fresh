/// <reference path="./lib/fresh.d.ts" />

import { gitCwdCandidate } from "./lib/git_repo.ts";

const editor = getEditor();

const GIT_BRANCH = "branch";

let lastDetectedBranch = editor.t("status.detecting_branch");
let inFlight: Promise<string> | null = null;

// HEAD-file watcher. Branch changes correspond exactly to mutations of
// the `HEAD` file inside the relevant git dir (resolved by
// `git rev-parse --git-path HEAD` so worktrees / submodules / `--git-dir`
// setups work). When HEAD changes we re-spawn `git rev-parse --abbrev-ref`
// and push the new value; otherwise we never spawn git on the hot path.
let headWatchHandle: number | null = null;
let watchedCwd: string | null = null;
let watchedHeadPath: string | null = null;
let ensureWatchInFlight: Promise<void> | null = null;


async function discoverHeadPath(cwd: string): Promise<string | null> {
  const result = await editor.spawnProcess(
    "git",
    ["rev-parse", "--git-path", "HEAD"],
    cwd,
  );
  if (result.exit_code !== 0) return null;
  const headPath = result.stdout.trim();
  if (!headPath) return null;
  // `--git-path` returns a path relative to cwd unless the git dir is
  // outside (e.g. worktree). Make absolute so notify gets a stable target.
  return headPath.startsWith("/") ? headPath : `${cwd}/${headPath}`;
}

async function ensureHeadWatch(): Promise<void> {
  const cwd = gitCwdCandidate(editor);
  if (watchedCwd === cwd && headWatchHandle !== null) return;
  if (ensureWatchInFlight) return ensureWatchInFlight;

  ensureWatchInFlight = (async () => {
    try {
      if (headWatchHandle !== null) {
        editor.unwatchPath(headWatchHandle);
        headWatchHandle = null;
        watchedHeadPath = null;
      }
      watchedCwd = cwd;
      const headPath = await discoverHeadPath(cwd);
      if (!headPath) return;
      try {
        headWatchHandle = await editor.watchPath(headPath, false);
        watchedHeadPath = headPath;
      } catch (_e) {
        // Watch registration failed (path missing, kernel limit). Fall
        // back to event-driven refresh — getCurrentGitBranch is still
        // gated by inFlight + the per-event invocation pattern below.
      }
    } finally {
      ensureWatchInFlight = null;
    }
  })();
  return ensureWatchInFlight;
}

async function getCurrentGitBranch(): Promise<string> {
  if (inFlight) return inFlight;
  inFlight = (async () => {
    try {
      const cwd = gitCwdCandidate(editor);
      const result = await editor.spawnProcess(
        "git",
        ["rev-parse", "--abbrev-ref", "HEAD"],
        cwd,
      );
      if (result.exit_code === 0) {
        const branch = result.stdout.trim();
        lastDetectedBranch = branch || "HEAD";
      } else {
        lastDetectedBranch = editor.t("status.not_in_git");
      }
      return lastDetectedBranch;
    } finally {
      inFlight = null;
    }
  })();
  return inFlight;
}

async function refreshForActiveBuffer(): Promise<void> {
  // Lazy: pick up cwd changes (Orchestrator window switch, etc.) the next
  // time anything triggers us.
  ensureHeadWatch();
  const bufferId = editor.getActiveBufferId();
  if (bufferId === 0) return;
  const branch = await getCurrentGitBranch();
  editor.setStatusBarValue(bufferId, GIT_BRANCH, branch);
}

editor.registerStatusBarElement(GIT_BRANCH, editor.t("status.git_branch"));

// Refresh the branch label when:
// - The user switches to a different buffer (the per-buffer value may not
//   be set yet for that buffer).
// - A file is freshly opened (same reason).
// - A file is saved (best-effort UX: the user may have committed via an
//   external terminal between events; the watchPath below catches actual
//   HEAD mutations).
// - The editor regains focus from another window — covers the case of
//   running `git checkout` in an external terminal while fresh was unfocused.
//
// Notably *not* in this list (compared to the legacy version): render_start,
// cursor_moved, after_insert, after_delete, buffer_deactivated, buffer_closed.
// None of them can change the current branch, and render_start was being
// fired ~300/s — see #2009 for the feedback-loop investigation.
[
  "buffer_activated",
  "after_file_open",
  "after_file_save",
  "focus_gained",
].forEach((event) => {
  editor.on(event, async () => {
    await refreshForActiveBuffer();
  });
});

// path_changed → HEAD file mutated → branch may have changed.
editor.on("path_changed", async (args) => {
  if (args.handle !== headWatchHandle) return;
  await refreshForActiveBuffer();
});

// Kick off the first detection at load time so the status bar populates
// before any user event fires.
refreshForActiveBuffer();
