/// <reference path="./fresh.d.ts" />

/**
 * Where the Orchestrator keeps a project's state.
 *
 * Everything a project owns under the data dir lives in one directory named
 * after the project's path: its archive manifest, its `.archived` graveyard,
 * and the worktrees cut for its workspaces. The name comes from
 * `editor.pathStorageKey()` — `<last component>-<16 hex digits>` — so the
 * editor and the plugins agree on a name for one project, and there is no
 * second implementation of the scheme here to drift from the editor's.
 *
 * **Why not spell the path out.** This directory used to be named by folding
 * the whole path into one component: separators became `_` and nothing else
 * was touched. Three things were wrong with that, and the key fixes all of
 * them at once rather than one character class at a time.
 *
 * A Windows root kept its drive colon (`C:_code_myrepo`), and
 * `editor.pathJoin` reads a component whose second character is `:` as
 * drive-absolute and discards everything joined before it — so the data dir
 * fell off the front, `git worktree add` resolved the leftover against the
 * current directory, inside the project, and the absolute-path check on the
 * other side refused the session (#3811). The key holds no separator, colon
 * or anything else a file cannot be called on any platform.
 *
 * `/` folded to the empty string, and `.` and `..` passed straight through
 * — all names `pathJoin` rewrites, which put a project's state on top of
 * `<data dir>/orchestrator/` itself, the level `scanArchiveManifests` lists
 * for per-project directories. (`editor.getCwd()` answers a bare `.` when
 * the editor cannot read its own state, so this was reachable.) The key is
 * always a non-empty component with no leading `.`.
 *
 * And `_` stood for a run of anything replaced, so `/home/u/a_b`,
 * `/home/u/a/b` and `/home/u/a:b` shared one directory, where each saw the
 * others' worktree names as taken by `freeWorktreeName`. The key's hash runs
 * over the whole path, so the readable head is free to be lossy and distinct
 * projects stay distinct (sinelaw/fresh#1971).
 *
 * **What it costs.** `myrepo-9c1f44a2bb08e37d` says less in a directory
 * listing than `home_user_code_myrepo` did, and nothing turns it back into a
 * path. Neither mattered: the editor took the same trade for its own
 * per-path directories in #3859, and an archive entry has always carried its
 * own `repo_root` precisely because this name was never decodable.
 */

/** The directory `project`'s state lives in. */
export function projectStateDir(editor: EditorAPI, project: string): string {
  return editor.pathJoin(
    editor.getDataDir(),
    "orchestrator",
    editor.pathStorageKey(project),
  );
}

/**
 * The same directory as a release before the storage key named it, newest
 * spelling first.
 *
 * Only the manifest is looked up here, and only to read: a worktree is
 * found through git, which records it in the repository by absolute path,
 * and an archive entry names its own directory absolutely, so nothing has
 * to move to the new name — see `loadArchiveManifest`. Append a spelling
 * rather than replacing one: a user coming from any release should still
 * find what it wrote.
 */
export function legacyProjectStateDirs(editor: EditorAPI, project: string): string[] {
  return LEGACY_KEYS.map((key) =>
    editor.pathJoin(editor.getDataDir(), "orchestrator", key(project))
  );
}

const LEGACY_KEYS: ((project: string) => string)[] = [legacySeparatorSlug];

/**
 * The path with its separators folded to `_` and a leading run dropped.
 *
 * Kept only to find what it wrote. A path with no component left folded to
 * the empty string, which `pathJoin` skips, so such a project's manifest sat
 * directly in `<data dir>/orchestrator/` — this returns that too, rather
 * than pretending it was never written.
 */
export function legacySeparatorSlug(project: string): string {
  return project.replace(/^[\\/]+/, "").replace(/[\\/]+/g, "_");
}
