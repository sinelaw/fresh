// Kept apart from the plugin so it can be tested without an editor.

/**
 * Encode a working directory the way Claude Code names its project bucket:
 * every non-alphanumeric character becomes a dash, runs not collapsed.
 * The transform is not reversible (`~/a-b` and `~/a/b` encode the same).
 */
export function encodeProjectDir(path: string): string {
  const normalised = path.replace(/\\/g, "/");
  // A lone root keeps its slash; anything else loses a trailing one.
  const trimmed =
    normalised === "/" || /^[A-Za-z]:\/$/.test(normalised)
      ? normalised
      : normalised.replace(/\/+$/, "");
  return trimmed.replace(/[^A-Za-z0-9]/g, "-");
}

