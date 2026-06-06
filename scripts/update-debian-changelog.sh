#!/usr/bin/env bash
# Rewrite debian/changelog so the top entry matches the workspace version
# in Cargo.toml. Run in CI (and locally) before `dpkg-buildpackage`.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

VERSION="$(awk -F'"' '
  /^\[workspace\.package\]/ { in_wp = 1; next }
  /^\[/                     { in_wp = 0 }
  in_wp && /^version[[:space:]]*=/ { print $2; exit }
' "${REPO_ROOT}/Cargo.toml")"

if [ -z "${VERSION}" ]; then
  echo "could not extract version from Cargo.toml" >&2
  exit 1
fi

DATE="$(date -u -R)"
MAINTAINER="${DEBFULLNAME:-Noam Lewis} <${DEBEMAIL:-i9uqoqj7k@mozmail.com}>"

cat > "${REPO_ROOT}/debian/changelog" <<EOF
fresh-editor (${VERSION}-1) unstable; urgency=medium

  * Release ${VERSION}. See upstream CHANGELOG.md for details.

 -- ${MAINTAINER}  ${DATE}
EOF

echo "wrote ${REPO_ROOT}/debian/changelog at ${VERSION}-1"
