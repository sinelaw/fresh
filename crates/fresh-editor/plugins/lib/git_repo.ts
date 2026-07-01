/// <reference path="./fresh.d.ts" />

/**
 * A resolved git repository. The only way to obtain one is `resolveGitRepo`,
 * which guarantees `root` is a real toplevel path. Pass this to `git()`
 * instead of a raw cwd string so a git command can never run in an
 * unresolved / non-repo directory (which silently fails as "not a git repo").
 */
export type GitRepo = { readonly root: string };

/**
 * The single place cwd-resolution logic lives: the active buffer's directory
 * (so monorepo sub-projects resolve to their own repo), falling back to the
 * editor cwd. Returns a candidate directory to *probe* — not a confirmed repo.
 */
export function gitCwdCandidate(editor: EditorAPI): string {
  const bufferId = editor.getActiveBufferId();
  if (bufferId) {
    const bufPath = editor.getBufferPath(bufferId);
    if (bufPath) {
      const dir = editor.pathDirname(bufPath);
      if (dir) return dir;
    }
  }
  return editor.getCwd();
}

/**
 * Resolve the git repository for the current context, or `null` when not
 * inside one. Because this returns `GitRepo | null`, callers are forced to
 * handle the "not a repo" case explicitly instead of falling through to a
 * bare cwd.
 */
export async function resolveGitRepo(editor: EditorAPI): Promise<GitRepo | null> {
  try {
    const cwd = gitCwdCandidate(editor);
    const r = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"], cwd);
    if (r.exit_code !== 0) return null;
    const root = r.stdout.trim();
    return root ? { root } : null;
  } catch {
    return null;
  }
}

/** Run a git command inside a resolved repository. */
export function git(
  editor: EditorAPI,
  repo: GitRepo,
  args: string[],
): ProcessHandle<SpawnResult> {
  return editor.spawnProcess("git", args, repo.root);
}
