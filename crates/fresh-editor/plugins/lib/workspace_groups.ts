/**
 * Universal Search's "All workspaces": which workspaces are searched, in
 * what order, and how their matches are listed — one group per workspace
 * under a header row. Pure, so it is unit-tested without an editor
 * (`tests/workspace_groups.test.ts`); `live_grep.ts` feeds it the editor's
 * window list and the Orchestrator dock's order.
 */

/** A workspace (an editor window) whose matches are listed as one group. */
export interface Workspace {
  /** Its window id (`listWindows()`). */
  id: number;
  /** Its name as the dock shows it. */
  name: string;
  /** Its root: the cwd its files are searched in. */
  root: string;
  /** The workspace the search was started from. */
  current: boolean;
  /** On a remote host (SSH, a container, a pod), not this computer. */
  remote: boolean;
}

/** The part of a `WindowInfo` this needs. */
export interface WindowLike {
  id: number;
  label: string;
  root: string;
  /** Set for a workspace on a remote host: whether it is connected. */
  remote?: { connected: boolean } | null;
}

/** One dock row: a workspace's window and its dock name. */
export interface DockEntry {
  windowId: number;
  name: string;
}

/**
 * The workspaces to search: the current one first, then the others in dock
 * order — those the dock does not list (no Orchestrator, or a window it has
 * not seen) after it, by window id. Each is searched on its own machine, so
 * a remote one is searched on its host; one that is not connected (restored
 * from disk, or its link dropped) has no host to search on yet and is left
 * out. Each is named as the dock names it, else by its window label.
 */
export function orderWorkspaces(
  current: { id: number; root: string },
  windows: readonly WindowLike[],
  dock: readonly DockEntry[],
): Workspace[] {
  const rank = (id: number): number => {
    const i = dock.findIndex((d) => d.windowId === id);
    return i >= 0 ? i : dock.length + id;
  };
  const name = (w: { id: number; label: string }): string =>
    dock.find((d) => d.windowId === w.id)?.name || w.label;
  const here = windows.find((w) => w.id === current.id);
  const others = windows
    .filter((w) => w.id !== current.id && !(w.remote && !w.remote.connected))
    .sort((a, b) => rank(a.id) - rank(b.id))
    .map((w) => ({ id: w.id, name: name(w), root: w.root, current: false, remote: !!w.remote }));
  return [
    {
      id: current.id,
      name: name({ id: current.id, label: here?.label ?? "" }),
      root: current.root,
      current: true,
      remote: !!here?.remote,
    },
    ...others,
  ];
}

/**
 * Each workspace's matches under a header row (`heading(workspace, count)`),
 * in the order given, with the matches tagged with their workspace. A
 * workspace with no matches gets no header.
 */
export function groupByWorkspace<T>(
  groups: readonly { workspace: Workspace; matches: readonly T[] }[],
  heading: (workspace: Workspace, count: number) => T,
  tag: (match: T, workspace: Workspace) => T,
): T[] {
  return groups.flatMap(({ workspace, matches }) =>
    matches.length === 0 ? [] : [heading(workspace, matches.length), ...matches.map((m) => tag(m, workspace))]
  );
}

/**
 * The name a terminal backing-file hit is listed under: the open terminal
 * that writes `backingFile`, by its tab label; else the file's own name
 * (a closed terminal's retained file, or one in a workspace not opened yet).
 */
export function terminalRowName(
  backingFile: string,
  screens: readonly { file: string; name: string }[],
): string {
  const base = (path: string) => path.slice(Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\")) + 1);
  const own = base(backingFile);
  return screens.find((s) => s.file !== "" && s.name && base(s.file) === own)?.name ?? own;
}
