// Paths as the server gives them: absolute and "/"-separated, a
// workspace's files under its root.

// The last part of `path`: a file's or folder's name.
/** function baseName(path: String) => String */
export function baseName(path) { return path.slice(path.lastIndexOf("/") + 1); }

// The folder `path` is in ("/" for what is at the top).
/** function parentOf(path: String) => String */
export function parentOf(path) {
  const i = path.lastIndexOf("/");
  return i <= 0 ? "/" : path.slice(0, i);
}

// `path` from `root` (a list of at most one: none when it is not under it).
/** function under(root: String, path: String) => String[] */
export function under(root, path) { return path.startsWith(`${root}/`) ? [path.slice(root.length + 1)] : []; }

// `path` as people read it: from `root` when it is under it, else whole.
/** function shownFrom(root: String, path: String) => String */
export function shownFrom(root, path) {
  for (const r of under(root, path)) return r;
  return path;
}
