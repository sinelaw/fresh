#!/usr/bin/env bash
# Update the Salsa packaging repo (sinelaw/fresh-editor) to a new upstream
# Fresh release, check it on Salsa CI, and upload it to mentors.debian.net.
#
# Usage: scripts/debian-salsa-update.sh [--repo DIR] COMMAND [options]
#
#   --repo DIR        the Salsa checkout, on debian/latest (default:
#                     $SALSA_REPO). Tarballs and build-area/ go next to it.
#
#   prepare VERSION   Import upstream VERSION (uscan + gbp import-orig +
#                     pristine-tar), sync debian/ with the release's
#                     packaging/debian, set the changelog to VERSION-1,
#                     commit, then build the source package and run lintian
#                     locally. Nothing is pushed.
#       --watch FILE         use FILE as debian/watch instead of the release's
#       --drop-patch NAME    drop debian/patches/NAME (repeatable)
#       --revision N         Debian revision (default 1)
#   check             Build the source package and run lintian locally.
#   push              Sign any unsigned upstream/* tag and push debian/latest,
#                     upstream/latest, pristine-tar and the upstream tag.
#   wait-ci           Wait for the Salsa pipeline of debian/latest's HEAD and
#                     print every job; exits non-zero unless it succeeded.
#   upload            Build the source package (pristine-tar orig, -sa), run
#                     lintian, sign with the mentors key, dput to mentors,
#                     then tag debian/<version> (signed) and push the tag.
#                     Re-uploading a version moves its tag to debian/latest;
#                     the old tag is kept locally as debian/<version>_<date>.
#       --no-upload          sign and tag, but upload and push nothing; run
#                            `publish` later (say, once wait-ci passes)
#   publish           Upload the .changes that `upload --no-upload` signed and
#                     push its tag. Needs no key, so the key can be unlocked
#                     once, while CI is still running.
#   all VERSION       prepare, push, wait-ci and upload in one go.
#
# Each release's packaging/debian is the source of truth; Salsa keeps its own
# debian/salsa-ci.yml and debian/gbp.conf, and debian/patches. A patch that
# the new upstream already contains (it reverse-applies) is dropped
# automatically; a patch that no longer applies stops `prepare` so you can
# refresh it or pass --drop-patch.
#
# Debian tools (gbp, uscan, pristine-tar, lintian, debsign, dput) run in a
# Debian unstable docker image (fresh-deb-tools), built on first use. Signing
# goes through the host's gpg-agent; when the key is locked, the script says
# how to unlock it. The key is `keyid` from the checkout's .git/gbp.conf, and
# commits are authored as debian/control's Maintainer.
#
# For example, for the next release:
#
#   scripts/debian-salsa-update.sh --repo ../debian-salsa/fresh-editor all 0.5.9

set -euo pipefail

REPO="${SALSA_REPO:-}"
if [ "${1:-}" = --repo ]; then REPO="${2:-}"; shift 2 || true; fi
case "${1:-}" in -h|--help|"") sed -n '2,/^$/s/^# \{0,1\}//p' "$0"; exit 0 ;; esac
[ -n "$REPO" ] || { echo "error: pass --repo DIR (the Salsa checkout) or set SALSA_REPO" >&2; exit 2; }
REPO="$(cd "$REPO" && pwd)"
[ -f "$REPO/debian/control" ] && [ -d "$REPO/.git" ] \
  || { echo "error: $REPO is not a checkout of the Salsa repo" >&2; exit 2; }
# The tarballs and build-area/ live next to the checkout, as gbp expects.
TOP="$(dirname "$REPO")"
PROJECT="sinelaw%2Ffresh-editor"
SALSA_API="https://salsa.debian.org/api/v4/projects/${PROJECT}"
IMAGE=fresh-deb-tools
KEEP=(salsa-ci.yml gbp.conf)
MAINTAINER="$(sed -n 's/^Maintainer: *//p' "$REPO/debian/control")"
NAME="${MAINTAINER% <*}"
EMAIL="${MAINTAINER##*<}"; EMAIL="${EMAIL%>}"

die() { echo "error: $*" >&2; exit 1; }
say() { echo "==> $*"; }

ensure_image() {
  docker image inspect "$IMAGE" >/dev/null 2>&1 && return
  say "building docker image $IMAGE"
  docker build -t "$IMAGE" - <<'EOF'
FROM debian:unstable
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    git git-buildpackage pristine-tar devscripts dput lintian dpkg-dev debhelper gnupg \
    ca-certificates curl xz-utils libwww-perl liblwp-protocol-https-perl dh-cargo python3-debian \
    && rm -rf /var/lib/apt/lists/*
EOF
}

# Run a command in the Debian image, in the repo, as the host user. The repo's
# parent directory is /w. For signing, the container gets a private GnuPG home
# with a copy of the host's public keyring and the host gpg-agent's socket, so
# it signs with the host's (unlocked) key and never writes to ~/.gnupg.
deb() {
  ensure_image
  local gpg_args=() setup=":"
  local sock; sock="$(gpgconf --list-dirs agent-socket 2>/dev/null || true)"
  if [ -S "$sock" ]; then
    gpg_args=(-v "${GNUPGHOME:-$HOME/.gnupg}:/gnupg-host:ro" -v "$sock:/run/host-gpg-agent")
    setup='mkdir -m700 /tmp/.gnupg && cp /gnupg-host/pubring.kbx /gnupg-host/trustdb.gpg /tmp/.gnupg/ && ln -s /run/host-gpg-agent /tmp/.gnupg/S.gpg-agent && echo no-autostart > /tmp/.gnupg/gpg.conf'
  fi
  docker run --rm -i --user "$(id -u):$(id -g)" -e HOME=/tmp \
    -e GIT_AUTHOR_NAME="$NAME" -e GIT_AUTHOR_EMAIL="$EMAIL" \
    -e GIT_COMMITTER_NAME="$NAME" -e GIT_COMMITTER_EMAIL="$EMAIL" \
    -e DEBFULLNAME="$NAME" -e DEBEMAIL="$EMAIL" \
    "${gpg_args[@]}" \
    -v "$TOP:/w" -w "/w/$(basename "$REPO")" "$IMAGE" \
    sh -c "$setup"' && exec "$@"' sh "$@"
}

g() { git -C "$REPO" -c commit.gpgsign=false -c tag.gpgsign=false -c gpg.format=openpgp "$@"; }

keyid() {
  g config -f "$REPO/.git/gbp.conf" DEFAULT.keyid 2>/dev/null \
    || die "no keyid in $REPO/.git/gbp.conf"
}

key_unlocked() {
  echo test | gpg --batch --pinentry-mode error -u "$(keyid)" --clearsign >/dev/null 2>&1
}

need_key() {
  key_unlocked && return
  cat >&2 <<EOF
The signing key $(keyid) is locked. Unlock it (gpg-agent caches it for a
while), then run this command again:

  echo test | gpg --clearsign -u $(keyid) >/dev/null
EOF
  exit 3
}

version() { deb dpkg-parsechangelog -S Version; }

# Set the date on debian/changelog's first trailer line.
changelog_date() { sed -i "0,/^ -- /s/^\( -- .*>\)  .*/\1  $1/" "$REPO/debian/changelog"; }

require_clean_debian_latest() {
  [ "$(g rev-parse --abbrev-ref HEAD)" = debian/latest ] || die "not on debian/latest"
  [ -z "$(g status --porcelain)" ] || die "working tree of $REPO is not clean"
}

build_source() {
  local v; v="$(version)"
  say "building source package ${v} (pristine-tar orig, -sa)"
  rm -rf "${TOP}/build-area/fresh-editor-${v%-*}"
  deb gbp buildpackage --git-pristine-tar --git-export-dir=../build-area \
    --git-builder='dpkg-buildpackage -S -sa -d -nc -us -uc' >/dev/null
  say "lintian"
  deb lintian --info --display-info --pedantic --fail-on error,warning \
    "../build-area/fresh-editor_${v}_source.changes"
}

cmd_prepare() {
  local version="" watch="" revision=1 drops=()
  while [ $# -gt 0 ]; do
    case "$1" in
      --watch) watch="$(realpath "$2")"; shift 2 ;;
      --drop-patch) drops+=("$2"); shift 2 ;;
      --revision) revision="$2"; shift 2 ;;
      -*) die "unknown option $1" ;;
      *) version="$1"; shift ;;
    esac
  done
  [ -n "$version" ] || die "prepare needs VERSION"
  require_clean_debian_latest
  g fetch --quiet origin
  for b in debian/latest upstream/latest pristine-tar; do
    [ "$(g rev-parse "$b")" = "$(g rev-parse "origin/$b")" ] \
      || say "note: $b differs from origin/$b"
  done

  # 1. Import the release with uscan (the watch file Salsa CI will test).
  if g rev-parse --verify --quiet "refs/tags/upstream/${version}" >/dev/null; then
    say "upstream/${version} already imported"
  else
    local tmp; tmp="$(mktemp -d -p "$TOP" .uscan.XXXXXX)"
    mkdir -p "$tmp/pkg/debian"
    g show debian/latest:debian/changelog > "$tmp/pkg/debian/changelog"
    if [ -n "$watch" ]; then cp "$watch" "$tmp/pkg/debian/watch"
    else g show "debian/latest:debian/watch" > "$tmp/pkg/debian/watch"; fi
    say "uscan: downloading ${version}"
    (REPO="$tmp/pkg" TOP="$tmp" deb uscan --download-version "$version" --destdir .. >/dev/null)
    local orig="${tmp}/fresh-editor_${version}.orig.tar.gz"
    [ -e "$orig" ] || die "uscan did not produce $(basename "$orig")"
    cp -L "$orig" "${TOP}/fresh-editor-${version}.tar.gz"
    ln -sf "fresh-editor-${version}.tar.gz" "${TOP}/fresh-editor_${version}.orig.tar.gz"
    rm -rf "$tmp"
    local sign=--no-sign-tags
    key_unlocked && sign=--sign-tags
    say "gbp import-orig ${version} ($sign)"
    deb gbp import-orig --no-interactive --pristine-tar "$sign" \
      --upstream-version="$version" "../fresh-editor_${version}.orig.tar.gz"
  fi

  # 2. Patches: decide before touching the tree. Drop the requested ones and
  # the ones upstream already has; stop on one that no longer applies.
  local kept=() dropped=() p
  if [ -f "$REPO/debian/patches/series" ]; then
    local src; src="$(mktemp -d -p "$TOP" .src.XXXXXX)"
    g archive "upstream/${version}" | tar -x -C "$src"
    while read -r p _; do
      case "$p" in ''|'#'*) continue ;; esac
      if printf '%s\n' "${drops[@]}" | grep -qxF "$p"; then
        say "dropping patch $p (--drop-patch)"; dropped+=("$p")
      elif patch -d "$src" -p1 -R --dry-run -s < "$REPO/debian/patches/$p" >/dev/null 2>&1; then
        say "dropping patch $p (already upstream)"; dropped+=("$p")
      elif patch -d "$src" -p1 -s < "$REPO/debian/patches/$p" >/dev/null 2>&1; then
        kept+=("$p")
      else
        rm -rf "$src"
        die "debian/patches/$p no longer applies to ${version}: refresh it, or rerun with --drop-patch $p"
      fi
    done < "$REPO/debian/patches/series"
    rm -rf "$src"
  fi

  # 3. Sync debian/ with the release's packaging/debian.
  say "syncing debian/ with packaging/debian from upstream/${version}"
  local pkg; pkg="$(mktemp -d -p "$TOP" .pkg.XXXXXX)"
  g archive "upstream/${version}" packaging/debian | tar -x -C "$pkg"
  [ -f "$pkg/packaging/debian/changelog" ] || die "upstream/${version} has no packaging/debian"
  local f k keep
  for f in "${KEEP[@]}"; do rm -f "$pkg/packaging/debian/$f"; done
  for f in $(g ls-files debian); do
    case "$f" in
      debian/patches/*) ;;
      *) keep=0; for k in "${KEEP[@]}"; do [ "$f" = "debian/$k" ] && keep=1; done
         [ "$keep" = 1 ] || g rm -q "$f" ;;
    esac
  done
  cp -a "$pkg/packaging/debian/." "$REPO/debian/"
  rm -rf "$pkg"
  [ -n "$watch" ] && cp "$watch" "$REPO/debian/watch"
  for p in "${dropped[@]}"; do g rm -q "debian/patches/$p"; done
  if [ -f "$REPO/debian/patches/series" ]; then
    if [ ${#kept[@]} -eq 0 ]; then
      say "no patches left; removing debian/patches"
      g rm -rq debian/patches
    else
      printf '%s\n' "${kept[@]}" > "$REPO/debian/patches/series"
    fi
  fi

  # 4. Changelog: the release's entry, at VERSION-REVISION, dated when debian/
  # changes. The trailer keeps debian/latest's date while nothing else differs,
  # so rerunning prepare commits nothing; otherwise it is stamped now, as
  # `dch -r` would, rather than carrying whatever date packaging/debian had.
  sed -i "1s/^fresh-editor ([^)]*)/fresh-editor (${version}-${revision})/" "$REPO/debian/changelog"
  [ "$(version)" = "${version}-${revision}" ] || die "changelog version not set"
  local dated; dated="$(g show HEAD:debian/changelog | sed -n 's/^ -- .*>  //p' | head -1)"
  [ -n "$dated" ] && changelog_date "$dated"
  g add -A debian
  if ! g diff --cached --quiet; then
    changelog_date "$(date -R)"
    g add debian/changelog
  fi
  if g diff --cached --quiet; then
    say "debian/ already up to date"
  else
    g commit -q -m "Update to upstream ${version}"
    say "committed: $(g log --oneline -1)"
  fi
  g diff --stat "@{u}" -- debian || true

  build_source
  say "prepared ${version}-${revision}; next: $0 push"
}

cmd_push() {
  require_clean_debian_latest
  need_key
  local t
  for t in $(g tag -l 'upstream/*'); do
    g cat-file -p "$t" | grep -q 'BEGIN PGP SIGNATURE' && continue
    g ls-remote --exit-code --tags origin "refs/tags/$t" >/dev/null 2>&1 && continue
    say "signing tag $t"
    g tag -f -s -u "$(keyid)" -m "$(g tag -l --format='%(contents:subject)' "$t")" \
      "$t" "$t^{commit}" >/dev/null
  done
  local tags=()
  for t in $(g tag -l 'upstream/*'); do
    g ls-remote --exit-code --tags origin "refs/tags/$t" >/dev/null 2>&1 || tags+=("refs/tags/$t")
  done
  say "pushing debian/latest upstream/latest pristine-tar ${tags[*]}"
  g push origin debian/latest upstream/latest pristine-tar "${tags[@]}"
}

cmd_wait_ci() {
  local sha; sha="$(g rev-parse debian/latest)"
  say "waiting for the Salsa pipeline of ${sha:0:9}"
  local id="" status="" deadline=$(( $(date +%s) + 3 * 3600 ))
  while :; do
    [ "$(date +%s)" -lt "$deadline" ] || die "no finished pipeline after 3 hours"
    id="$(curl -fsS "${SALSA_API}/pipelines?sha=${sha}&per_page=1" | jq -r '.[0].id // empty')"
    if [ -n "$id" ]; then
      status="$(curl -fsS "${SALSA_API}/pipelines/${id}" | jq -r .status)"
      case "$status" in success|failed|canceled|skipped) break ;; esac
    fi
    sleep 60
  done
  curl -fsS "${SALSA_API}/pipelines/${id}/jobs?per_page=100" \
    | jq -r '.[] | "\(.status)\t\(.name)\t\(.web_url)"' | sort -k2 | column -t -s $'\t'
  echo "pipeline https://salsa.debian.org/sinelaw/fresh-editor/-/pipelines/${id}: ${status}"
  [ "$status" = success ]
}

release_tag() { local v; v="$(version)"; echo "debian/${v//\~/_}"; }

# Tag debian/latest as debian/<version>, signed. When the tag is already on
# an older commit (a re-upload of the same version), keep the old tag object
# as debian/<version>_<its date> and move the tag.
tag_release() {
  local tag; tag="$(release_tag)"
  local head; head="$(g rev-parse debian/latest)"
  if g rev-parse --verify --quiet "refs/tags/$tag" >/dev/null; then
    [ "$(g rev-parse "$tag^{commit}")" = "$head" ] && return
    local when; when="$(g for-each-ref --format='%(taggerdate:unix)' "refs/tags/$tag")"
    [ -n "$when" ] || when="$(g log -1 --format=%ct "$tag")"
    say "re-upload: moving $tag to ${head:0:9}, keeping the old one locally as ${tag}_${when}"
    g update-ref "refs/tags/${tag}_${when}" "$(g rev-parse "refs/tags/$tag")"
    g tag -d "$tag" >/dev/null
  fi
  g tag -s -u "$(keyid)" -m "fresh-editor Debian release $(version)" "$tag" debian/latest
}

# Push debian/<version>, replacing Salsa's copy when a re-upload moved it.
push_release_tag() {
  local tag; tag="$(release_tag)"
  local remote; remote="$(g ls-remote --tags origin "refs/tags/$tag" | cut -f1)"
  if [ -n "$remote" ] && [ "$remote" != "$(g rev-parse "refs/tags/$tag")" ]; then
    say "replacing $tag on Salsa (re-upload)"
    g push --force-with-lease="refs/tags/$tag:$remote" origin "refs/tags/$tag"
  else
    g push origin "refs/tags/$tag"
  fi
}

dput_mentors() {
  say "uploading ${1##*/} to mentors"
  deb sh -c 'cat > /tmp/.dput.cf <<EOF
[mentors]
fqdn = mentors.debian.net
incoming = /upload
method = https
allow_unsigned_uploads = 0
progress_indicator = 0
allowed_distributions = .*
EOF
exec dput -f mentors "$1"' sh "$1"
}

cmd_upload() {
  local upload=1
  [ "${1:-}" = --no-upload ] && upload=0
  require_clean_debian_latest
  need_key
  local v; v="$(version)"
  build_source
  local changes="../build-area/fresh-editor_${v}_source.changes"
  say "signing ${changes##*/} with $(keyid)"
  deb debsign -k"$(keyid)" "$changes"
  tag_release
  if [ "$upload" = 0 ]; then
    say "signed and tagged $(release_tag); after CI passes: $0 --repo $REPO publish"
    return
  fi
  dput_mentors "$changes"
  push_release_tag
  say "done: https://mentors.debian.net/package/fresh-editor/"
}

cmd_publish() {
  require_clean_debian_latest
  local v; v="$(version)"
  local changes="../build-area/fresh-editor_${v}_source.changes"
  [ -f "${TOP}/build-area/${changes##*/}" ] || die "no ${changes##*/}; run upload --no-upload first"
  grep -q 'BEGIN PGP SIGNATURE' "${TOP}/build-area/${changes##*/}" || die "${changes##*/} is not signed"
  # The tag was made with the signature, so it pins what was signed.
  local tag; tag="$(release_tag)"
  g rev-parse --verify --quiet "refs/tags/$tag" >/dev/null || die "no tag $tag; run upload --no-upload first"
  [ "$(g rev-parse "$tag^{commit}")" = "$(g rev-parse debian/latest)" ] \
    || die "debian/latest moved since ${changes##*/} was signed; run upload again"
  dput_mentors "$changes"
  push_release_tag
  say "done: https://mentors.debian.net/package/fresh-editor/"
}

cmd="${1:-}"; [ $# -gt 0 ] && shift
case "$cmd" in
  prepare) cmd_prepare "$@" ;;
  check) build_source ;;
  push) cmd_push ;;
  wait-ci) cmd_wait_ci ;;
  upload) cmd_upload "$@" ;;
  publish) cmd_publish ;;
  all) cmd_prepare "$@" && cmd_push && cmd_wait_ci && cmd_upload ;;
  *) die "unknown command $cmd (see --help)" ;;
esac
