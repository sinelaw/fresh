/**
 * Turning a project path into one directory name.
 *
 * The Orchestrator keeps each project's state under
 * `<data dir>/orchestrator/<slug>/`: the archive manifest, the `.archived`
 * graveyard, and the worktrees a new workspace is cut into. So the slug has
 * to be a single directory name — and one the platform can actually hold,
 * because `scanArchiveManifests` finds a project's state by listing the
 * level above it.
 *
 * **What is replaced.** Both path separators, the drive colon, and the rest
 * of what Windows forbids in a path component (`* ? " < > |` and the C0
 * controls). The separators are what made the slug one component in the
 * first place. The colon is #3811: `editor.pathJoin` reads a component
 * whose second character is `:` as drive-absolute and discards everything
 * joined before it, so the Windows slug `C:_code_myrepo` took the data dir
 * with it — `git worktree add` resolved the leftover against the current
 * directory, inside the project, and the absolute-path check on the other
 * side refused the session. The rest arrive through the same door:
 * `editor.getCwd()` hands plugins the `\\?\` prefix of a canonicalised
 * Windows path verbatim, and no Windows file can be called
 * `?_C_code_myrepo`.
 *
 * **Runs, not characters.** A run of replaced characters stands for one
 * underscore, and a leading or trailing run for none, so `\\?\C:\code\myrepo`
 * and `C:\code\myrepo` slug alike, as do `~/proj` and `~/proj/`. Two
 * spellings of one root should not get two state directories; this is the
 * same normalisation `path_encode` applies on the Rust side.
 *
 * **A name with nothing left in it.** `/` has no component at all, and `.`
 * and `..` are ones `pathJoin` does not keep: `.` is dropped and `..` pops
 * the component before it, so the state landed a level up — on top of where
 * `scanArchiveManifests` looks for per-project directories. (`getCwd()`
 * returns a bare `.` when the editor cannot read its own state, so this is
 * reachable rather than theoretical.) Those collapse to `_root` instead,
 * which is not a name an ordinary project root slugs to.
 *
 * **Changing the rule strands what the old one wrote.** A project whose
 * slug changed gets a fresh state directory and does not find its
 * `archived.json` or `.archived` graveyard again; its worktrees are
 * unaffected, because git records those in the repository by absolute path
 * and the Open dialog discovers them through git rather than through the
 * slug. Nothing migrates the old directory: renaming it would break the
 * `git worktree` registrations of everything already cut inside it, so
 * moving one is `git worktree move` per worktree, not a rename. Weigh that
 * before widening the class again — and note that `unarchiveOne` drops a
 * restored entry through `loadArchiveManifest(repoRoot)`, i.e. the *new*
 * slug's manifest, so an entry left in an old one keeps being listed by
 * `scanArchiveManifests` after it has been restored.
 *
 * **Slugging is one way, and not injective.** `_` stands for a run of
 * anything replaced, so `/home/u/a_b`, `/home/u/a/b` and `/home/u/a:b` share
 * a slug and therefore a state directory, where each would see the others'
 * worktree names as taken. Nothing here closes that; the Rust side moved to
 * `path_encode::storage_key_for_path` (`<tail>-<fnv64>`) for exactly this
 * reason, and sinelaw/fresh#1971 leaves the plugin slug as a follow-up.
 *
 * **Read back by Rust, in one place.** `migrate_legacy_windows`
 * (`app/orchestrator_persistence.rs`) lists `<data dir>/orchestrator/` and
 * runs `workspace::decode_filename_to_path` (`_` → `/`) over the names it
 * finds, so this spelling and that one share a directory level. It only
 * reads a directory holding a legacy `windows.json`, which no plugin ever
 * wrote and which the Rust side named with
 * `path_encode::encode_path_for_filename` — an encoding that
 * percent-escapes every character replaced here, and `_` itself. So no name
 * whose slug this module changed can carry one, and that decode is no more
 * lossy than it was. Keep it that way: a character folded to `_` here is a
 * character the decode hands back as a separator.
 */

/** Separators, the drive colon, and the rest of what Windows forbids in a
 *  path component. */
const REPLACED = /[\\/:*?"<>|\u0000-\u001F]+/;

/** The directory name a project path's state lives under. */
export function slugify(p: string): string {
  // Splitting on the runs drops the leading and trailing ones with the empty
  // strings they leave, so the class is written once and the three cases are
  // one rule.
  const slug = p.split(REPLACED).filter((part) => part !== "").join("_");
  return /[^.]/.test(slug) ? slug : "_root";
}
