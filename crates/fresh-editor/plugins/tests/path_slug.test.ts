/** Where a project's state lives (`lib/path_slug.ts`). */
import {
  legacyProjectStateDirs,
  legacySeparatorSlug,
  projectStateDir,
} from "../lib/path_slug.ts";

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

// A project's state directory is named by `editor.pathStorageKey()` now, which
// the editor computes and whose own tests pin it. What is left here is the
// spelling a release before it used, kept only so a project's archive is still
// found where that release wrote it. So these cases are a record of what was
// written, warts and all — not of what a good name looks like.

eq(legacySeparatorSlug("/home/user/proj"), "home_user_proj", "separators became underscores");
eq(legacySeparatorSlug("/home//user/proj"), "home_user_proj", "a run of them became one");
eq(legacySeparatorSlug("C:\\code\\myrepo"), "C:_code_myrepo", "a windows root kept its drive colon");
eq(legacySeparatorSlug("\\\\server\\share\\proj"), "server_share_proj", "a UNC share lost its lead");
eq(legacySeparatorSlug("proj"), "proj", "a bare name was already a component");
eq(legacySeparatorSlug("/home/u/a:b"), "home_u_a:b", "a POSIX colon survived — and worked");
eq(legacySeparatorSlug("/home/user/proj/"), "home_user_proj_", "a trailing separator made its own");

// These are the names that were not a single component at all. `pathJoin`
// skips an empty segment and a `.`, and pops the one before a `..`, so such a
// project's state sat in `<data dir>/orchestrator/` itself — which is why
// `legacyProjectStateDirs` still looks there rather than pretending nothing
// was ever written for them.
eq(legacySeparatorSlug("/"), "", "the filesystem root named no directory");
eq(legacySeparatorSlug("."), ".", "`.` passed straight through");
eq(legacySeparatorSlug(".."), "..", "`..` did too");

// ── where a project's state lives ─────────────────────────────────

// Only the three calls these two make. The key itself is the editor's — it
// computes it with the same code that names the editor's own per-path
// directories, which is the point of asking for it rather than spelling one
// out here — so the stub just makes it visible.
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
eq(
  legacyProjectStateDirs(ed, "/home/u/proj"),
  ["/data/orchestrator/home_u_proj"],
  "the legacy directory is the same two levels, under the old spelling",
);
// `pathJoin` skips the empty segment, so this really was the shared level —
// looking there is what finds a manifest written for a filesystem root.
eq(
  legacyProjectStateDirs(ed, "/"),
  ["/data/orchestrator"],
  "a root's legacy state sat in the shared directory itself",
);

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
