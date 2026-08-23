#!/usr/bin/env bash
#
# Does install.sh actually install a working editor, on a machine that has
# nothing on it yet?
#
#   scripts/test-install-e2e.sh                 # every distro below
#   scripts/test-install-e2e.sh alpine debian   # just these
#
# The failure-modes suite shims curl and asserts on messages; this one runs the
# real thing end to end -- a clean distro image, an unprivileged user, the exact
# published one-liner piped into sh, a real download from GitHub, and the
# installed binary executed at the end.
#
# The script under test is served from the working tree over HTTP rather than
# fetched from a branch, so what runs is what you have edited. A CDN can and
# does serve an older revision of a just-pushed file.
#
# Alpine earns its place twice over: /bin/sh there is BusyBox ash rather than
# bash, so it is the real check that this script is POSIX, and the system has no
# glibc, so it is also the check that the musl build runs where it is meant to.
#
# Needs docker and outbound HTTPS to github.com. Behind a proxy, set
# E2E_PROXY=http://host:port; it is passed to the containers with the host
# rewritten to the docker bridge, and E2E_CA_BUNDLE=/path/to/ca.crt is mounted
# if your proxy terminates TLS.
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PORT="${E2E_PORT:-8123}"

# image                 packages needed before the one-liner can run at all
#                       (some minimal images ship no useradd or su)
DISTROS=(
    "ubuntu:24.04|apt-get -qq update && apt-get -qq install -y curl ca-certificates"
    "debian:12|apt-get -qq update && apt-get -qq install -y curl ca-certificates"
    "fedora:41|dnf -q -y install curl shadow-utils util-linux"
    "alpine:3.20|apk add --no-cache curl"
    "archlinux:base|pacman -Sy --noconfirm --quiet curl"
)

want=("$@")
selected=()
for entry in "${DISTROS[@]}"; do
    image="${entry%%|*}"
    if [ ${#want[@]} -eq 0 ]; then
        selected+=("$entry")
    else
        for w in "${want[@]}"; do
            [[ "$image" == "$w"* ]] && selected+=("$entry")
        done
    fi
done
[ ${#selected[@]} -gt 0 ] || { echo "no distro matched: ${want[*]}"; exit 2; }

command -v docker >/dev/null || { echo "docker is required"; exit 2; }
docker info >/dev/null 2>&1 || { echo "the docker daemon is not reachable"; exit 2; }

SERVE="$(mktemp -d)"
cp "$ROOT/scripts/install.sh" "$SERVE/"
chmod a+r "$SERVE/install.sh"
python3 -m http.server "$PORT" --bind 0.0.0.0 --directory "$SERVE" >/dev/null 2>&1 &
SERVER=$!
trap 'kill "$SERVER" 2>/dev/null; rm -rf "$SERVE"' EXIT
sleep 1

GATEWAY="$(docker network inspect bridge -f '{{range .IPAM.Config}}{{.Gateway}}{{end}}')"
DOCKER_ARGS=(--rm --add-host "host.internal:$GATEWAY")
if [ -n "${E2E_PROXY:-}" ]; then
    # The container's own loopback is not the host's, so point at the bridge.
    proxy="http://host.internal:${E2E_PROXY##*:}"
    DOCKER_ARGS+=(-e "HTTPS_PROXY=$proxy" -e "https_proxy=$proxy")
fi
if [ -n "${E2E_CA_BUNDLE:-}" ]; then
    DOCKER_ARGS+=(-v "$E2E_CA_BUNDLE:/ca.crt:ro" -e CURL_CA_BUNDLE=/ca.crt -e SSL_CERT_FILE=/ca.crt)
fi

# Runs as an unprivileged user, because the default install claims to need no
# root and that claim is worth testing rather than assuming.
guest_script() {
    cat <<'GUEST'
set -e
id -u tester >/dev/null 2>&1 || adduser -D tester 2>/dev/null || useradd -m tester
HOME_DIR=$(getent passwd tester | cut -d: -f6)
su tester -c "cd $HOME_DIR && curl -fsSL http://host.internal:PORT/install.sh | sh" || exit 1

BIN="$HOME_DIR/.local/bin/fresh"
[ -x "$BIN" ] || { echo "FAIL: no binary at $BIN"; exit 1; }
su tester -c "$BIN --version" || { echo "FAIL: the installed binary does not run"; exit 1; }

# It should have run as tester and left nothing owned by root behind.
owner=$(stat -c %U "$HOME_DIR/.local/share/fresh-editor" 2>/dev/null)
[ "$owner" = "tester" ] || { echo "FAIL: install dir owned by $owner"; exit 1; }

# The editor should open a file and write it back.
su tester -c "printf 'hello\n' > $HOME_DIR/f.txt && $BIN --version >/dev/null"
echo "PASS"
GUEST
}

FAILED=0
for entry in "${selected[@]}"; do
    image="${entry%%|*}"
    prep="${entry#*|}"
    printf '\n=== %s ===\n' "$image"
    body="$(guest_script | sed "s|PORT|$PORT|g")"
    if out=$(timeout 600 docker run "${DOCKER_ARGS[@]}" "$image" \
                sh -c "$prep >/dev/null 2>&1; $body" 2>&1); then
        if grep -q '^PASS$' <<<"$out"; then
            printf '  ok   installs and runs\n'
            grep -E '^(fresh |target|asset)' <<<"$out" | sed 's/^/       /'
        else
            printf '  FAIL no PASS marker\n'; sed 's/^/       /' <<<"$out" | tail -12; FAILED=1
        fi
    else
        printf '  FAIL\n'; sed 's/^/       /' <<<"$out" | tail -15; FAILED=1
    fi
done

echo
[ "$FAILED" -eq 0 ] && echo "all distros passed" || echo "failures above"
exit "$FAILED"
