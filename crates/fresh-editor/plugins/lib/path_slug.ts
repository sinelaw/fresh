/// <reference path="./fresh.d.ts" />

/**
 * Where the Orchestrator keeps a project's state: its archive manifest, its
 * `.archived` graveyard, and the worktrees cut for its workspaces.
 *
 * The directory is named by `editor.pathStorageKey()`, so the scheme is not
 * spelled out a second time here and cannot drift from the one the editor
 * names its own per-path directories with. That the key is a single component
 * holding nothing a filename cannot hold is what keeps `editor.pathJoin` from
 * reading it as a prefix and dropping the data dir joined before it — see
 * #3811, and the commit that moved to the key.
 *
 * The older name is kept only to find what it wrote; nothing is moved to the
 * new one, because nothing has to be (`loadArchiveManifest`).
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
 * The same directory as a release before the storage key named it: the path
 * with its separators folded to `_` and a leading run dropped.
 *
 * A path with no component left folded to the empty string, which `pathJoin`
 * skips, so such a project's state sat directly in `<data dir>/orchestrator/`
 * — this returns that too, rather than pretending it was never written.
 */
export function legacyProjectStateDir(editor: EditorAPI, project: string): string {
  const folded = project.replace(/^[\\/]+/, "").replace(/[\\/]+/g, "_");
  return editor.pathJoin(editor.getDataDir(), "orchestrator", folded);
}
