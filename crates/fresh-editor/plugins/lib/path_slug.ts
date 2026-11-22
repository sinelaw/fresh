/**
 * Turning a project path into one directory name.
 *
 * The Orchestrator keeps its per-project state under
 * `<data dir>/orchestrator/<slug>/`, so the slug has to be a single path
 * component: separators become underscores, and any leading separator is
 * dropped so the result is never anchored to the filesystem root.
 *
 * `:` is replaced too. A surviving drive colon (`C:_code_myrepo`, which is
 * what `C:\code\myrepo` slugged to) turns the component into a
 * drive-relative path, and `pathJoin` treats a component carrying a prefix
 * like an absolute one: it drops everything joined before it. The data dir
 * disappeared from the result, `git worktree add` resolved the leftover
 * against the current directory — inside the project — and the absolute-path
 * check on the other side refused the session.
 */

export function slugify(p: string): string {
  return p.replace(/^[\\/:]+/, "").replace(/[\\/:]+/g, "_");
}
