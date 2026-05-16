/// <reference path="./lib/fresh.d.ts" />

const editor = getEditor();

const GIT_BRANCH = "branch";

let lastDetectedTimestamp = 0;
let lastDetectedBranch = editor.t("status.detecting_branch");

let inFlight: Promise<string> | null = null;

async function getCurrentGitBranch(): Promise<string> {
  const now = Date.now();

  if (now - lastDetectedTimestamp < 5000) {
    return lastDetectedBranch;
  }
  // Coalesce concurrent callers onto a single spawn. Without this, every
  // event handler that fires during the in-flight `git rev-parse` re-enters
  // here, sees the still-stale timestamp, and spawns its own copy.
  if (inFlight) {
    return inFlight;
  }

  inFlight = (async () => {
    try {
      const cwd = editor.getCwd();
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

      lastDetectedTimestamp = Date.now();
      return lastDetectedBranch;
    } finally {
      inFlight = null;
    }
  })();

  return inFlight;
}

editor.registerStatusBarElement(GIT_BRANCH, editor.t("status.git_branch"));

[
  "buffer_activated",
  "buffer_deactivated",
  "buffer_closed",
  "after_file_open",
  "after_file_save",
  "after_insert",
  "after_delete",
  "cursor_moved",
  "render_start",
].forEach((event) => {
  editor.on(event, async () => {
    const bufferId = editor.getActiveBufferId();
    if (bufferId === 0) {
      return;
    }
    editor.setStatusBarValue(bufferId, GIT_BRANCH, await getCurrentGitBranch());
  });
});
