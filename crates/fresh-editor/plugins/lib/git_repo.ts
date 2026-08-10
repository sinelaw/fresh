/// <reference path="./fresh.d.ts" />

/**
 * The single source of truth for "which git repository does this operation
 * belong to, and how do I run git in it".
 *
 * Every git-touching plugin that is keyed to the file/buffer the user is
 * working with MUST resolve through this module instead of computing a cwd of
 * its own. That is what makes it impossible to accidentally run git in the
 * workspace root — which, in a monorepo whose root is not itself a repo,
 * silently fails as "fatal: not a git repository". The opaque `GitRepo`
 * (obtainable only by resolving) forces callers to handle the "not a repo"
 * case explicitly rather than falling through to a bare cwd.
 */

/** A resolved git repository. `root` is a real `rev-parse --show-toplevel`. */
export type GitRepo = { readonly root: string };

/**
 * Number of subdirectory levels scanned below a non-repo workspace root when
 * discovering nested sub-repos (level 1 = direct children of the root).
 *
 * This is the monorepo depth *contract*: the Rust index watcher
 * (`app/git_index.rs::resolve_git_indexes_blocking`) scans the SAME levels for
 * a different purpose (watching each repo's index for change events). The two
 * serve different runtimes and cannot share code, so this constant is the
 * canonical value both sides must agree on — keep them in sync.
 */
export const MONOREPO_MAX_DEPTH = 3;

/**
 * Candidate directory to *probe* for the active buffer's repo: the active
 * buffer's own directory (so a monorepo sub-project resolves to its own repo),
 * falling back to the editor cwd. This is only a candidate — pass it through
 * `resolveGitRepo`, which confirms it actually is a repo.
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

/** Resolve the git repo containing `dir`, or null if `dir` is not inside one. */
async function repoAt(editor: EditorAPI, dir: string): Promise<GitRepo | null> {
  try {
    const r = await editor.spawnProcess(
      "git",
      ["rev-parse", "--show-toplevel"],
      dir,
    );
    if (r.exit_code !== 0) return null;
    const root = r.stdout.trim();
    return root ? { root } : null;
  } catch {
    return null;
  }
}

/**
 * Resolve the repo for the current context — the active buffer's directory,
 * falling back to the editor cwd — or `null` when not inside a repo. Use this
 * for buffer-scoped operations (grep, find-file, status bar).
 */
export function resolveGitRepo(editor: EditorAPI): Promise<GitRepo | null> {
  return repoAt(editor, gitCwdCandidate(editor));
}

/**
 * Resolve the repo that contains a specific file, or `null`. Use this for
 * per-file operations (blame, single-file log, merge-conflict) so the repo is
 * the file's own sub-project regardless of which buffer happens to be active.
 */
export function resolveGitRepoForPath(
  editor: EditorAPI,
  filePath: string,
): Promise<GitRepo | null> {
  const dir = editor.pathDirname(filePath) || editor.getCwd();
  return repoAt(editor, dir);
}

/**
 * Run a git command inside a resolved repository. This is the only sanctioned
 * way to spawn git for repo-scoped work: because it takes a `GitRepo` (not a
 * raw string) the command can never run in an unresolved / non-repo directory.
 * `stdoutTo` streams stdout to a file, matching `editor.spawnProcess`.
 */
export function git(
  editor: EditorAPI,
  repo: GitRepo,
  args: string[],
  stdoutTo?: string,
): ProcessHandle<SpawnResult> {
  return editor.spawnProcess("git", args, repo.root, stdoutTo);
}

/**
 * Absolute path for a repo-relative path (e.g. a line of `git ls-files` or
 * `git grep` output). In a monorepo the workspace root differs from the repo
 * root, so a repo-relative path must be joined onto the repo root to open.
 */
export function toAbsInRepo(
  editor: EditorAPI,
  repo: GitRepo,
  relPath: string,
): string {
  return editor.pathJoin(repo.root, relPath);
}

/**
 * Repo-relative form of an absolute path inside `repo` — for object refs like
 * `git show :0:<path>` / `<rev>:<path>` that must be rooted at the repo. Falls
 * back to the input unchanged if it isn't under the repo root.
 */
export function repoRelativePath(repo: GitRepo, absPath: string): string {
  if (absPath === repo.root) return "";
  const prefix = repo.root.endsWith("/") ? repo.root : repo.root + "/";
  return absPath.startsWith(prefix) ? absPath.slice(prefix.length) : absPath;
}

/**
 * Recursively discover directories containing a `.git` entry, `maxDepth`
 * levels below `dir` (level 1 = direct children). Stops descending into a
 * directory once its `.git` is found (a repo's internals are git's own
 * concern — submodules are managed by git, not rediscovered here). Skips
 * hidden directories and `node_modules`.
 *
 * This is the single TypeScript implementation of monorepo sub-repo discovery
 * (only the file-explorer decorations need it — every other feature resolves a
 * single repo from the active buffer/file). A parallel BFS for *index
 * watching* lives in Rust (`app/git_index.rs`); both scan levels
 * 1..=MONOREPO_MAX_DEPTH.
 */
export function discoverSubRepos(
  editor: EditorAPI,
  dir: string,
  maxDepth: number = MONOREPO_MAX_DEPTH,
): string[] {
  if (maxDepth <= 0) return [];
  const repos: string[] = [];
  const entries = editor.readDir(editor.authorityPath(dir));
  for (const entry of entries) {
    if (
      entry.name.startsWith(".") ||
      entry.name === "node_modules" ||
      !entry.is_dir
    ) {
      continue;
    }
    const subDir = editor.pathJoin(dir, entry.name);
    if (editor.fileExists(editor.authorityPath(editor.pathJoin(subDir, ".git")))) {
      repos.push(subDir);
    } else {
      repos.push(...discoverSubRepos(editor, subDir, maxDepth - 1));
    }
  }
  return repos;
}
