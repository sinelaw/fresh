/** Project-path slugs (`lib/path_slug.ts`). */
import { slugify } from "../lib/path_slug.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

/** What `editor.pathJoin` needs of a component for
 *  `<data dir>/orchestrator/<slug>/<name>` to stay under the data dir: a
 *  non-empty name with no separator, no drive prefix it would read as
 *  absolute, and not one of the `.` / `..` it rewrites. Nothing Windows
 *  forbids in a component either, since the same slug names a directory
 *  there. */
const isOneComponent = (s: string): boolean =>
  s.length > 0 && !/[\\/:*?"<>|\u0000-\u001F]/.test(s) && s !== "." && s !== "..";

// ── separators ────────────────────────────────────────────────────

eq(slugify("/home/user/proj"), "home_user_proj", "a POSIX path loses its leading separator");
eq(slugify("C:\\code\\myrepo"), "C_code_myrepo", "a windows path loses its drive colon");
eq(slugify("\\\\server\\share\\proj"), "server_share_proj", "a UNC share is not anchored");
eq(slugify("/home//user/proj"), "home_user_proj", "a run of separators collapses to one underscore");
eq(slugify("proj"), "proj", "a bare name is already a component");

// ── two spellings of one root agree ───────────────────────────────

// `editor.getCwd()` hands plugins a canonicalised Windows path with its
// `\\?\` prefix still on, and `?` is not a character a Windows file can be
// named with. Dropping the leading run also makes the prefixed spelling
// agree with the plain one, rather than giving the project a second state
// directory.
eq(slugify("\\\\?\\C:\\code\\myrepo"), "C_code_myrepo", "a `\\\\?\\` prefix slugs as the path it prefixes");
eq(slugify("/home/user/proj/"), "home_user_proj", "a trailing separator is not a component of its own");

// ── a path with no component left ─────────────────────────────────

// `pathJoin` drops a `.` and pops the component before a `..`, and an empty
// slug contributes nothing, so all three put the project's state on top of
// `<data dir>/orchestrator/` — the level `scanArchiveManifests` lists for
// per-project directories. `getCwd()` answers a bare `.` when the editor
// cannot read its own state.
eq(slugify("/"), "_root", "the filesystem root still names a directory");
eq(slugify("."), "_root", "`.` is not a component `pathJoin` keeps");
eq(slugify(".."), "_root", "`..` would pop the directory above it");

// ── the result is always one component ────────────────────────────

for (
  const p of [
    "/home/user/proj",
    "C:\\code\\myrepo",
    "C:/code/myrepo",
    "\\\\server\\share",
    "\\\\?\\C:\\code\\myrepo",
    "/",
    ".",
    "..",
    "/srv/we*rd?name",
    "/srv/a<b>c|d",
  ]
) {
  eq(isOneComponent(slugify(p)), true, `${JSON.stringify(p)} slugs to one path component`);
}

// A drive colon left in the slug is the bug: `pathJoin` clears whatever it
// was joined onto, so `<data dir>/orchestrator/<slug>/<name>` came out as the
// drive-relative `C:_code_myrepo/<name>` and the worktree landed inside the
// project.
eq(/^[A-Za-z]:/.test(slugify("C:\\code\\myrepo")), false, "the slug carries no drive prefix");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
