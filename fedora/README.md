# Fedora packaging & official review for `fresh-editor`

This directory contains everything needed to submit Fresh to Fedora as a
**bundled Rust application** and shepherd it through the official package
review. None of this requires the existing `cargo-generate-rpm` CI path — that
produces a "binary dropped into an RPM", which Fedora does **not** accept.
Fedora builds from source on its own infrastructure (Koji), driven by the
`.spec` here.

| File | Purpose |
|------|---------|
| `fresh-editor.spec` | Fedora-style spec: builds the `fresh` TUI binary from source, bundles vendored crates. |
| `gen-vendor.sh` | Produces the `*-vendor.tar.zst` dependency tarball (Source1). |
| `gen-bundled-license.sh` | Computes the bundled-dependency `License:` expression. |
| `docker-build.sh` | Builds the RPM from source in a Fedora **container** — for smoke-testing the spec on a non-Fedora host. |

All commands below assume a **Fedora host** (or toolbox/mock chroot). This repo
was developed on Arch, which has none of `dnf`/`mock`/`rpmbuild`/`rust2rpm`, so
the build/lint steps cannot be run there directly.

> **Quick local smoke test (any host with Docker):** `./fedora/docker-build.sh`
> stages the source + vendored deps and runs `rpmbuild` inside `fedora:41`,
> dropping the resulting RPMs in `fedora/out/`. This is a convenience check of
> the spec, **not** a substitute for the clean-chroot `mock` build and
> `fedora-review` run that the official review requires (steps 4–5 below).

---

## 0. Prerequisites (one-time, on Fedora)

```bash
sudo dnf install \
  fedora-packager fedora-review rpmdevtools mock \
  cargo cargo-license cargo-vendor-filterer rust2rpm \
  copr-cli
rpmdev-setuptree                 # creates ~/rpmbuild/{SOURCES,SPECS,...}
sudo usermod -a -G mock "$USER"  # log out/in afterwards
```

You also need a [FAS account](https://accounts.fedoraproject.org/) for the
review and a Bugzilla account (same login).

---

## 1. Fix the upstream license metadata first  ⚠️ blocker

`Cargo.toml` declares `license = "GPL-2.0"`, but `LICENSE` is GPLv2 **with the
"or (at your option) any later version" clause**. The correct SPDX id is
`GPL-2.0-or-later`. Fix it upstream before the review (reviewers will flag the
deprecated/incorrect tag):

```toml
# Cargo.toml  [workspace.package]
license = "GPL-2.0-or-later"
```

The spec already uses `GPL-2.0-or-later` for `SourceLicense`.

## 2. Generate the vendor tarball + license expression

```bash
./fedora/gen-vendor.sh             # -> fresh-editor-0.4.0-vendor.tar.zst
./fedora/gen-bundled-license.sh    # prints the unique SPDX licenses
```

Take the output of the second command, build the SPDX `AND` expression, and
replace the `# FIXME` `License:` line in `fresh-editor.spec`. Validate every
identifier against the
[Fedora allowed-licenses list](https://docs.fedoraproject.org/en-US/legal/allowed-licenses/).

## 3. Stage sources and build the SRPM

```bash
VERSION=0.4.0
# Source0: upstream tarball as GitHub serves it (expands to fresh-0.4.0/)
spectool -g -C ~/rpmbuild/SOURCES fedora/fresh-editor.spec   # fetches Source0
cp fresh-editor-${VERSION}-vendor.tar.zst ~/rpmbuild/SOURCES/
cp fedora/fresh-editor.spec ~/rpmbuild/SPECS/

rpmbuild -bs ~/rpmbuild/SPECS/fresh-editor.spec   # build SRPM
```

## 4. Build in mock (clean chroot — this is what Koji does)

```bash
mock -r fedora-rawhide-x86_64 ~/rpmbuild/SRPMS/fresh-editor-${VERSION}-*.src.rpm
```

Fix any build failures (most likely: a missing `BuildRequires`, or a crate
needing a system `-devel` lib surfaced by bindgen). Re-vendor / rebuild as
needed.

## 5. Self-review with fedora-review

```bash
fedora-review -n fresh-editor      # against the SRPM + spec in cwd
```

Resolve every `[!]` item. Common ones for bundled Rust apps: the bundled
`License:` expression, `Provides: bundled(crate(...))` (rust2rpm can emit
these — consider generating the spec skeleton with
`rust2rpm --vendor fresh-editor` and merging), and `%license`/`%doc` coverage.

## 6. Test in COPR (optional but recommended — gives a real public build)

```bash
copr-cli create fresh-editor --chroot fedora-rawhide-x86_64 --chroot fedora-41-x86_64
copr-cli build fresh-editor ~/rpmbuild/SRPMS/fresh-editor-${VERSION}-*.src.rpm
```

This also gives users `dnf copr enable <you>/fresh-editor` immediately, before
official inclusion lands.

## 7. File the Package Review request (Bugzilla)

There is **no existing review bug** — confirmed by searching Red Hat Bugzilla
(`component=Package Review`, summary `fresh-editor` → 0 matches). Create one:

- **Product:** Fedora
- **Component:** Package Review
- **Summary:** `Review Request: fresh-editor - Lightweight, fast terminal text editor with LSP and TypeScript plugins`
- **Description:** include the **raw URLs** to the uploaded `.spec` and `.src.rpm`
  (COPR result dir works), plus:

  ```
  Spec URL: https://.../fresh-editor.spec
  SRPM URL: https://.../fresh-editor-0.4.0-1.fc<rel>.src.rpm
  Description: <the %description text>
  Fedora Account System Username: <your FAS>

  Koji scratch build: <link from `koji build --scratch rawhide <srpm>`>
  ```

If this is your first Fedora package, you need a sponsor: also block the
tracker bug **FE-NEEDSPONSOR** (bug 177841) and read
<https://docs.fedoraproject.org/en-US/package-maintainers/Joining_the_Package_Maintainers/>.

## 8. After approval

`fedpkg request-repo` / `request-branch`, import, build in Koji, submit a
Bodhi update. Full flow:
<https://docs.fedoraproject.org/en-US/package-maintainers/New_Package_Process_for_New_Contributors/>.

---

## Notes / known sharp edges

- **Bundled crypto:** the `http` default feature pulls `ureq + rustls + ring`.
  Bundling `ring` is acceptable for an application, but expect a reviewer
  question. If it becomes a blocker, building with `--no-default-features
  --features plugins,runtime,embed-plugins,tree-sitter` (dropping `http`)
  removes the entire TLS stack — adjust the `%cargo_build` line accordingly.
- **`target/rpm/release/` path:** the install step assumes the cargo-rpm-macros
  target dir. If `%cargo_build` on your Fedora release uses a different layout,
  change the `install` source path (or switch to `%cargo_install`).
- **rust2rpm shortcut:** `rust2rpm --vendor fresh-editor` can generate a spec
  skeleton (with `Provides: bundled(crate(...))` and a license stub) you can
  diff against `fresh-editor.spec` here to catch anything missed.
