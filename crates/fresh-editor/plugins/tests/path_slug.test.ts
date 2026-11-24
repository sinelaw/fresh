/** Where a project's state lives (`lib/path_slug.ts`). */
import { legacyProjectStateDir, projectStateDir } from "../lib/path_slug.ts";

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

// Only the calls these two make. The storage key is the editor's — the same
// code that names the editor's own per-path directories, which is why it is
// asked for rather than spelled out again — and its own tests pin it, so the
// stub just makes it visible here.
const ed = {
  getDataDir: () => "/data",
  pathStorageKey: (p: string) => `KEY(${p})`,
  // Empty segments are skipped, as the host's `pathJoin` skips them.
  pathJoin: (...parts: string[]) => parts.filter((p) => p !== "").join("/"),
} as unknown as Parameters<typeof projectStateDir>[0];

eq(
  projectStateDir(ed, "/home/u/proj"),
  "/data/orchestrator/KEY(/home/u/proj)",
  "a project's state is the data dir, `orchestrator`, and the path's storage key",
);

// The older name, kept only to find what it wrote — so these are a record of
// what a previous release produced, warts and all, not of what a good name
// looks like. The colon is the one #3811 was about; it survived here, and on
// POSIX it worked.
const legacy = (p: string) => legacyProjectStateDir(ed, p);
const under = (name: string) => `/data/orchestrator/${name}`;

eq(legacy("/home/user/proj"), under("home_user_proj"), "separators became underscores");
eq(legacy("/home//user/proj"), under("home_user_proj"), "a run of them became one");
eq(legacy("C:\\code\\myrepo"), under("C:_code_myrepo"), "a windows root kept its drive colon");
eq(legacy("\\\\server\\share\\proj"), under("server_share_proj"), "a UNC share lost its lead");
eq(legacy("/home/u/a:b"), under("home_u_a:b"), "a POSIX colon survived — and worked");
eq(legacy("/home/user/proj/"), under("home_user_proj_"), "a trailing separator made its own");

// `/`, `.` and `..` are names `pathJoin` rewrites, so such a project's state
// sat in the shared directory itself. Looking there is what finds it.
eq(legacy("/"), "/data/orchestrator", "the filesystem root named no directory of its own");
eq(legacy("."), under("."), "`.` passed straight through");
eq(legacy(".."), under(".."), "`..` did too");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
