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

/** Whether the slug is usable as a single path component: no separator, and
 *  no drive prefix that `pathJoin` would resolve as an absolute path. */
const isOneComponent = (s: string): boolean => s.length > 0 && !/[\\/:]/.test(s);

// ── separators ────────────────────────────────────────────────────

eq(slugify("/home/user/proj"), "home_user_proj", "a POSIX path loses its leading separator");
eq(slugify("C:\\code\\myrepo"), "C_code_myrepo", "a windows path loses its drive colon");
eq(slugify("\\\\server\\share\\proj"), "server_share_proj", "a UNC share is not anchored");
eq(slugify("/home//user/proj"), "home_user_proj", "a run of separators collapses to one underscore");
eq(slugify("proj"), "proj", "a bare name is already a component");

// ── the result is always one component ────────────────────────────

for (const p of ["/home/user/proj", "C:\\code\\myrepo", "C:/code/myrepo", "\\\\server\\share"]) {
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
