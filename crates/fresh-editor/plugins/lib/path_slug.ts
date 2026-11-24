/// <reference path="./fresh.d.ts" />

/**
 * Turning a project path and a workspace name into directories the
 * Orchestrator can keep state in on any platform.
 *
 * Each project's state — its archive manifest, its `.archived` graveyard, and
 * the worktrees cut for its workspaces — lives in one directory named by
 * `editor.pathStorageKey()`, so the scheme is not spelled out a second time
 * here and cannot drift from the one the editor names its own per-path
 * directories with. That the key is a single component holding nothing a
 * filename cannot hold is what keeps `editor.pathJoin` from reading it as a
 * prefix and dropping the data dir joined before it — see #3811.
 *
 * The older name is kept only to find what it wrote; nothing is moved to the
 * new one, because nothing has to be (`loadArchiveManifest`).
 */

/**
 * One spelling of a project path, so that two spellings of one project do not
 * get two state directories.
 *
 * The storage key takes `\\?\` prefixes and letter case as written, and on
 * Windows both reach it: `editor.getCwd()` hands plugins the extended-length
 * prefix of a canonicalised path verbatim, while git's `--show-toplevel`
 * answers `C:/…`, so the form's preview and the create could otherwise
 * disagree about which directory a worktree goes in. Separators and trailing
 * ones need no help — the key already treats `/` and `\` alike and drops
 * empty components.
 *
 * Only the drive letter is case-folded. The rest of the path is left as
 * written, because a POSIX filesystem tells `/srv/Proj` and `/srv/proj`
 * apart and they are two projects there.
 */
export function oneProjectSpelling(project: string): string {
  // `\\?\UNC\server\share` is the extended spelling of `\\server\share`;
  // `\\?\C:\…` and the `\\.\` device form strip to the plain path.
  const unc = /^[\\/]{2}[?.][\\/]UNC[\\/]/i.exec(project);
  const plain = unc
    ? `\\\\${project.slice(unc[0].length)}`
    : project.replace(/^[\\/]{2}[?.][\\/]/, "");
  return plain.replace(/^[A-Za-z]:/, (drive) => drive.toUpperCase());
}

/** The directory `project`'s state lives in. */
export function projectStateDir(editor: EditorAPI, project: string): string {
  return editor.pathJoin(
    editor.getDataDir(),
    "orchestrator",
    editor.pathStorageKey(oneProjectSpelling(project)),
  );
}

/**
 * The same directory as a release before the storage key named it: the path
 * with its separators folded to `_` and a leading run dropped.
 *
 * Deliberately not normalised the way `projectStateDir` normalises — this has
 * to reproduce what that release wrote, not improve on it. A path with no
 * component left folded to the empty string, which `pathJoin` skips, so such
 * a project's state sat directly in `<data dir>/orchestrator/`; this returns
 * that too, rather than pretending it was never written.
 */
export function legacyProjectStateDir(editor: EditorAPI, project: string): string {
  const folded = project.replace(/^[\\/]+/, "").replace(/[\\/]+/g, "_");
  return editor.pathJoin(editor.getDataDir(), "orchestrator", folded);
}

/** Names Windows reserves whatever extension follows them. */
const RESERVED = /^(con|prn|aux|nul|com[1-9]|lpt[1-9])(\..*)?$/i;

/**
 * `raw` as one directory name — which is also a git branch name, since a
 * workspace's worktree directory and the branch cut for it share it.
 *
 * A run of anything outside `[A-Za-z0-9._-]` becomes one `-`, and leading and
 * trailing `.` and `-` go: both are what Windows forbids or silently strips
 * at the ends of a name, and the same set git refuses in a ref. A name
 * Windows reserves for a device takes a suffix, because `NUL` names the null
 * device whatever directory you ask for it in. Empty in, empty out — the
 * caller decides what a nameless workspace is called.
 */
export function safeNameComponent(raw: string): string {
  const name = raw
    .replace(/[^A-Za-z0-9._-]+/g, "-")
    .replace(/^[.-]+/, "")
    .replace(/[.-]+$/, "");
  return RESERVED.test(name) ? `${name}-ws` : name;
}
