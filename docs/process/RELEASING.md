# Releasing: how to update CHANGELOG.md

> Process doc, not architecture. Written for whoever (human or agent) prepares
> the next version's `CHANGELOG.md` section before a release.

## 1. Determine the versions

```
git fetch --unshallow            # if the clone is shallow
LAST_TAG=$(git describe --tags --abbrev=0)   # e.g. v0.4.10
```

If `git describe` fails because the tag isn't an ancestor of HEAD, fall back
to `git tag --sort=-v:refname | head` for the highest semver tag and use
`git log LAST_TAG..HEAD` / `git diff LAST_TAG..HEAD` as plain set-diffs — they
work correctly even without common ancestry.

`NEW_VERSION` is the next patch/minor bump of `LAST_TAG` (0.4.10 → 0.4.11). If
`CHANGELOG.md` already has a `## NEW_VERSION` section, you're updating it, not
adding a duplicate.

## 2. Audit CHANGELOG.md before writing anything

**Do this every time, even if you audited recently.** The single most common
failure mode is not tampering in the malicious sense — it's PRs whose author
(often another agent) writes a changelog bullet and inserts it right under
the first `### Features`/`### Bug Fixes` header in the file, without checking
whether that section has already been tagged and released. This has happened
repeatedly, across unrelated PRs, even shortly after a previous audit fixed
it — because master kept moving between audits. **Treat this as routine
maintenance, not a one-time cleanup.**

```
git diff LAST_TAG..HEAD -- CHANGELOG.md
```

Any change to a section *other than* the very top — i.e. any edit inside an
already-tagged `## X.Y.Z` block — is a defect, no matter how well-written the
prose is or how real the underlying fix is. Symptoms to look for:

- New `* **Bold lead** - ...` lines inserted into `## LAST_TAG`'s own
  `### Features`/`### Bug Fixes`, usually right after the section header or
  wedged between two existing bullets.
- The same pattern one section further back (`## LAST_TAG_MINUS_1`), if a
  later merge based its insertion point on an even older view of the file.
- No section content ever *removed* by this — it's purely stray insertions,
  which is precisely why it's easy to miss in review (the diff looks
  additive and harmless).

For every stray bullet, decide whether it's real or fabricated:

```
git log LAST_TAG..HEAD --oneline --grep "#NNNN"
git grep -in "#NNNN" -- '*.rs' '*.ts'
```

Code presence plus a test named after the issue number counts as real
evidence even if no single commit mentions the number by itself (a squashed
commit can bundle several fixes). If you find zero evidence anywhere in the
repo, drop the bullet outright.

Then:

1. Restore every already-tagged section (`LAST_TAG`'s and all older ones) to
   **byte-identical** match with `git show LAST_TAG:CHANGELOG.md`:
   ```
   git show LAST_TAG:CHANGELOG.md > CHANGELOG.md
   ```
   (then re-add the new `## NEW_VERSION` section on top — see below).
2. Verify with a diff after editing:
   ```
   diff <(git show LAST_TAG:CHANGELOG.md) <(sed -n '/^## LAST_TAG$/,$p' CHANGELOG.md)
   ```
   It must come back clean (the only acceptable noise is the leading
   `# Release Notes` header lost by the `sed` slice).
3. Move every stray-but-real entry into the new version's section instead of
   discarding it — it describes a fix that genuinely landed after the tag, so
   it belongs in the next release, not the one already shipped.

## 3. Gather source material — do all of these

1. **Full commit messages:** `git log LAST_TAG..HEAD --no-merges --pretty=full`.
   Read bodies, not just subjects — they carry the user-visible symptom and
   the issue number a squashed subject line drops.
2. **The full diff:** `git diff LAST_TAG..HEAD --stat`, then the diff itself by
   path. Confirms what actually changed and catches user-facing behavior the
   commit message undersells.
3. **GitHub issues/PRs**, cross-referencing every `#NNNN` in a commit message
   against the issue/PR to get its title, reporter, and author login. This is
   also how you catch a number that refers to a *PR*, not an issue — PRs are
   almost always authored by the repo owner, so citing one without checking
   can wrongly imply the owner reported their own bug.

## 4. Decide what's actually in this release

A fix counts as "in this release" only if the commit that fixes it is in
`LAST_TAG..HEAD` (or, per the audit above, present in the tree with no
evidence it existed at `LAST_TAG`). An issue's *closed* date is irrelevant —
inclusion is decided by where the fixing commit lands. An issue closed after
this release ships, by a commit that predates the tag, belongs to a release
that already happened.

**Stealth features:** omit anything for a feature area that's under
embargo/stealth (currently: the web UI, and the native GUI build) — don't
even reference it indirectly. Everything else genuinely shipped goes in.

## 5. Write the new section

Add `## NEW_VERSION` at the top, below `# Release Notes`, matching the
existing sections' shape:

- Same boilerplate opening (the "follow me on X" line, the Settings UI tip).
- `### Features`, `### Bug Fixes`, `### Internals` (omit `Internals` if empty).
- **Every bullet is a short bold lead + at most one clause of detail.** The
  bold text alone should read as the complete summary — `**LSP requests that
  time out now say so**` not a generic label followed by a paragraph. Cut
  adjectives, cut restated context, cut framework-comparison asides unless
  they're the clearest way to say what changed.
- Issue/PR numbers in parentheses at the end: `(#2602)`, `(#2536, #2539)`.
- **Attribution:** `reported by @user`, `requested by @user`, `by @user` for
  a contributed PR — for every issue/PR that has one, **except** `sinelaw`
  and `claude`/bot accounts, which are never credited (most issues in this
  repo are filed by the owner; check the reporter before adding a credit, not
  after).
- Group commits into one bullet per user-facing change, not one per commit.
  A multi-commit PR (fix + three review-round follow-ups) is one bullet.
- Skip internal-only changes (refactors, module moves, test-only changes)
  unless there's a genuine user-facing benefit — describe the benefit, not
  the mechanism. Roll unavoidable internal work into one `### Internals`
  bullet.
- No mention of the Web UI, ever, in a public-facing section.

## 6. Verify before finishing

- `git diff LAST_TAG..HEAD -- CHANGELOG.md` shows changes **only** inside the
  new version's section — run this exact command, not just a visual scan.
- Every `#NNNN` cited corresponds to a fix actually in this release (§4).
- No issue fixed since the tag is missing (except deliberate stealth
  omissions).
- No attribution to `sinelaw` or `claude`/bots anywhere in the new section.
- No mention of the Web UI anywhere in the new section.
- Every bullet leads with a bold, self-contained summary and is as short as
  it can be while staying accurate.
- Valid, consistent Markdown.

## 7. The PR, and staying in sync with a moving master

This repo merges fast. Master can advance — and re-introduce the §2 defect —
while your PR is open. Treat every rebase or merge-conflict resolution as a
reason to redo the audit, not just a text conflict to resolve:

- If `git merge`/rebase reports a conflict inside `CHANGELOG.md`, don't just
  pick a side line-by-line. Check what actually landed on master since your
  branch's base (`git log <old-base>..origin/master --oneline -- CHANGELOG.md`)
  — if it added stray bullets to an already-tagged section again (likely),
  resolve by dropping them from that section and folding the real ones into
  your new version's section, exactly as in §2.
- Prefer rebasing your release-prep branch onto the latest `origin/master`
  before merging, so the diff you're reviewing is against current reality.
  Since this is your own branch, a `git reset --hard origin/master` followed
  by re-applying the CHANGELOG edit as one clean commit (rather than layering
  merge commits) keeps history simple; force-push it with
  `--force-with-lease`.
- Re-run the full §6 verification after any such rebase — don't assume a fix
  that was clean before is still clean after master moved.

## Quick reference

```
# find LAST_TAG
git describe --tags --abbrev=0

# does the tagged section still match its tag?
diff <(git show LAST_TAG:CHANGELOG.md) <(sed -n '/^## LAST_TAG$/,$p' CHANGELOG.md)

# what commits/issues landed since the tag?
git log LAST_TAG..HEAD --no-merges --pretty=full
git log LAST_TAG..HEAD --oneline --grep "#NNNN"

# after any rebase, re-check nothing outside the new section changed
git diff origin/master -- CHANGELOG.md | grep -E '^-[^-]'   # must be empty
```
