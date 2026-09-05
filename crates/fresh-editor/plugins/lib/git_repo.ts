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
 * User settings that reshape git's patch output, pinned back to the documented
 * default. Written as top-level `-c` overrides rather than per-sub-command
 * flags: `git stash show` mangles `--src-prefix=`/`--dst-prefix=`, and `-c`
 * behaves the same for every sub-command (`diff`, `show`, `stash show`, ...).
 * Plumbing (`diff-files -p` and friends) is no escape hatch: `core.quotePath`
 * and `diff.suppressBlankEmpty` are honoured there too.
 */
const DIFF_FORMAT_CONFIG = [
  // The `a/` / `b/` path prefixes the parsers match on.
  "-c", "diff.noprefix=false",
  "-c", "diff.mnemonicPrefix=false",
  "-c", "diff.srcPrefix=a/",
  "-c", "diff.dstPrefix=b/",
  // Non-ASCII paths otherwise come out quoted and octal-escaped in every
  // header (`diff --git "a/caf\303\251"`), and in `--numstat`/`--stat` rows.
  "-c", "core.quotepath=false",
  // Empty context lines otherwise lose their leading space, so a parser
  // classifying rows by their first byte drops them.
  "-c", "diff.suppressBlankEmpty=false",
  // Paths stay repo-relative, and files outside git's cwd are not omitted.
  "-c", "diff.relative=false",
  // A changed submodule is one `Subproject commit` hunk, not a nested diff
  // whose `diff --git` paths are relative to the submodule.
  "-c", "diff.submodule=short",
];

/**
 * Build the `git` argv for a patch-producing sub-command whose stdout a plugin
 * parses (`diff --git` / `+++ b/` / `@@` headers, `+`/`-`/` ` rows). Every
 * knob neutralised here is one a user may legitimately have set, and any one
 * alone breaks the parse: `diff.external` swaps in a tool's own format,
 * textconv diffs converted text (so hunk line numbers stop addressing the
 * real file), `color.diff=always` wraps every line in escapes, and the
 * config knobs above reshape the headers and rows.
 */
export function diffArgs(subcommand: string[], ...rest: string[]): string[] {
  return [
    // Top-level, so it has to precede the sub-command. `git diff` refreshes
    // the index and writes it back under `.git/index.lock` just as
    // `git status` does, and the review watch runs both on a timer (#3126)
    // — pinning the flag here rather than at one call site keeps the panel
    // from racing the user's own `git` for that lock whichever sub-command
    // the tick reaches for.
    "--no-optional-locks",
    ...DIFF_FORMAT_CONFIG,
    ...subcommand,
    "--no-ext-diff",
    "--no-textconv",
    "--no-color",
    ...rest,
  ];
}

/**
 * Route an already-assembled git argv through `diffArgs`, splitting it at its
 * first option so the leading sub-command words (`diff`, `stash show`, ...)
 * stay in front. Lets callers that build the argv dynamically inherit the
 * format pinning instead of each having to repeat the flag list.
 */
export function withDiffArgs(command: string[]): string[] {
  const firstOption = command.findIndex((a) => a.startsWith("-"));
  const cut = firstOption === -1 ? command.length : firstOption;
  return diffArgs(command.slice(0, cut), ...command.slice(cut));
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
