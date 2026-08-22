#!/usr/bin/env bash
#
# Does install.sh fail honestly when GitHub does not answer?
#
#   scripts/test-install-failure-modes.sh
#
# The bug this pins down: helpers in install.sh return a URL by echoing it and
# callers capture that with $(...). When a diagnostic went to stdout, a failed
# lookup returned its own error message *as the URL*, the emptiness check that
# was supposed to catch it passed, and curl was handed a string of ANSI escapes
# to fetch -- reporting `bad range in URL position 3`, which names neither the
# real cause (an exhausted API rate limit) nor anything the user can act on.
#
# An exhausted rate limit is the ordinary case, not an exotic one: the
# unauthenticated GitHub API allows 60 requests per hour per source IP, so any
# shared egress -- a NAT, a CI runner, an office network -- reaches it.
#
# Hermetic: curl is shimmed and PATH is reduced to that shim plus the utilities
# install.sh needs, so no network is touched and the fallback methods (nix,
# cargo, npm) are absent rather than slow.
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INSTALL_SH="$ROOT/scripts/install.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

FAILED=0
pass() { printf '  ok   %s\n' "$1"; }
fail() { printf '  FAIL %s\n' "$1"; FAILED=1; }

# PATH holding only what install.sh legitimately uses. Anything absent from this
# list is absent from the test, which is what keeps the fallback methods from
# compiling anything.
BINDIR="$WORK/bin"
mkdir -p "$BINDIR"
for tool in sh mktemp chmod tar awk grep cut tr head sed ln mkdir rm cp mv uname basename dirname sha256sum cat sort find touch id timeout; do
    src="$(command -v "$tool" 2>/dev/null)" && ln -sf "$src" "$BINDIR/$tool"
done

# $1 = api http status the shim reports, $2 = whether asset downloads succeed
make_curl_shim() {
    cat > "$BINDIR/curl" <<EOF
#!/bin/sh
# Faithful to the two behaviors install.sh depends on: without -f, curl exits 0
# on an HTTP error and reports the status through -w; with -f it exits 22.
for a in "\$@"; do
  case "\$a" in
    *api.github.com*)
      case " \$* " in
        *-f*) echo "curl: (22) The requested URL returned error: $1" >&2; exit 22 ;;
      esac
      case " \$* " in *" -w "*) printf '%s' "$1" ;; esac
      exit 0 ;;
    https://github.com/*/releases/*)
      if [ "$2" != "ok" ]; then
        echo "curl: (22) The requested URL returned error: 404" >&2
        exit 22
      fi
      exit 0 ;;
  esac
done
# Every curl call install.sh makes targets one of the shapes above, so reaching
# here means the URL was built from something that is not a URL -- which is
# exactly the failure this script exists to catch. Real curl rejects such an
# argument too, just with a message ("bad range in URL") that describes its own
# parser rather than the cause.
echo "SHIM-UNEXPECTED-URL: \$*" >&2
exit 3
EOF
    chmod +x "$BINDIR/curl"
}

run_install() {
    # A fresh HOME per run so nothing leaks between cases.
    local home="$WORK/home.$1"
    mkdir -p "$home"
    env -i PATH="$BINDIR" HOME="$home" SHELL=/bin/sh \
        timeout 60 sh "$INSTALL_SH" "${@:2}" 2>&1
}

# Most assertions below are negative -- that some bad string is *absent* -- and
# those pass for free if the script never ran. Every case is gated on this
# first, so a broken harness reads as a failure rather than a clean sweep.
assert_ran() {
    # $1 = label, $2 = output
    if grep -q 'Installing\|Looking for\|\[INFO\]' <<<"$2"; then
        pass "$1: install.sh ran"
    else
        fail "$1: install.sh did not run (harness broken); output: ${2:0:120}"
    fi
}

echo "install.sh failure modes"

# --- The reported failure: API rate limited. ---
make_curl_shim 403 fail
OUT="$(run_install ratelimited --method=tarball)"
assert_ran "rate-limited" "$OUT"

if grep -q 'bad range in URL' <<<"$OUT"; then
    fail "reports curl's URL-parsing error instead of the real cause"
else
    pass "no 'bad range in URL' from a mangled URL"
fi

if grep -q 'SHIM-UNEXPECTED-URL' <<<"$OUT"; then
    fail "handed curl something that is not a release URL"
else
    pass "never asks curl to fetch a non-URL"
fi

if grep -qE 'Downloading .*(ERROR|\[0;31m)' <<<"$OUT"; then
    fail "a diagnostic was captured and used as a URL"
else
    pass "diagnostics do not leak into captured values"
fi

if grep -q 'rate limit' <<<"$OUT"; then
    pass "names the rate limit as the cause"
else
    fail "does not explain that the API rate limit is the cause"
fi

if grep -q 'GITHUB_TOKEN' <<<"$OUT"; then
    pass "points at GITHUB_TOKEN as the way out"
else
    fail "offers no remedy for the rate limit"
fi

# --- The API is rate limited but the asset download works. ---
# The universal build's name carries no version, so /releases/latest/download
# reaches it without the API at all; this is the default `curl | sh` path and it
# should not be affected by the limit.
make_curl_shim 403 ok
OUT="$(run_install redirect --method=tarball)"
assert_ran "redirect" "$OUT"

if grep -q 'rate limit' <<<"$OUT"; then
    fail "consulted the rate-limited API when the redirect would do"
else
    pass "default install does not consult the API"
fi

# --- A hard network failure. ---
make_curl_shim 000 fail
OUT="$(run_install offline --method=tarball)"
assert_ran "offline" "$OUT"

if grep -q 'SHIM-UNEXPECTED-URL' <<<"$OUT"; then
    fail "offline path still handed curl a bad URL"
else
    pass "offline path fails without a bad URL"
fi

if grep -qi 'could not reach\|network' <<<"$OUT"; then
    pass "offline path says the network is the problem"
else
    fail "offline path does not mention the network"
fi

# --- Helpers whose value is captured must not fail merely by having nothing
# --- to say. Under `set -e`, `x=$(f)` takes f's status, so a helper that
# --- returns non-zero on an ordinary empty result aborts the whole installer.
probe="$WORK/probe.sh"
{
    echo 'set -e'
    sed -n '/^api_auth_header()/,/^}/p' "$INSTALL_SH"
    echo '_auth=$(api_auth_header)'
    echo 'echo "SURVIVED:${_auth}"'
} > "$probe"

OUT="$(env -u GITHUB_TOKEN -u GH_TOKEN sh "$probe" 2>&1)"
if [[ "$OUT" == SURVIVED:* ]]; then
    pass "api_auth_header succeeds when no token is set"
else
    fail "api_auth_header aborts the installer when no token is set"
fi

OUT="$(env GITHUB_TOKEN=probe-token sh "$probe" 2>&1)"
if [[ "$OUT" == *"Bearer probe-token"* ]]; then
    pass "api_auth_header uses GITHUB_TOKEN when present"
else
    fail "api_auth_header ignores GITHUB_TOKEN"
fi

echo
if [ "$FAILED" -eq 0 ]; then
    echo "all checks passed"
else
    echo "failures above"
fi
exit "$FAILED"
