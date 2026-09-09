#!/usr/bin/env bash
#
# Does install.sh stay honest, and stay anonymous, when GitHub does not answer?
#
#   scripts/test-install-failure-modes.sh
#
# Two properties are pinned here.
#
# 1. Anonymous access is enough. The installer must never call the GitHub API:
#    unauthenticated, that API allows 60 requests per hour per source IP, so from
#    any shared egress -- a NAT, a CI runner, an office network -- it is often
#    already spent and answers 403. Installing an editor should not depend on a
#    budget the user does not control, nor require a token. The public download
#    endpoints need no credential, so those are all install.sh may use.
#
# 2. A failed lookup is never mistaken for a successful one. Helpers here return
#    a URL by echoing it and callers capture that with $(...). When diagnostics
#    went to stdout, a failed lookup returned its own error text *as the URL*,
#    the emptiness check meant to catch that passed, and curl was handed a string
#    of ANSI escapes to fetch -- reporting `bad range in URL position 3`, which
#    names curl's own parser rather than anything the user can act on.
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

# $1 = status the download endpoints report ("ok" serves them)
make_curl_shim() {
    cat > "$BINDIR/curl" <<EOF
#!/bin/sh
# Records every request so the assertions can inspect what was asked for, then
# answers as configured. Faithful to the two curl behaviors install.sh relies
# on: without -f curl exits 0 on an HTTP error and reports the status through
# -w, and with -f it exits 22.
echo "\$*" >> "$WORK/requests.log"

for a in "\$@"; do
  case "\$a" in
    *api.github.com*)
      echo "SHIM-API-CALL: \$a" >&2
      exit 3 ;;
    */releases/latest)
      # The version redirect. -w '%{url_effective}' reports where it landed.
      if [ "$1" = "ok" ]; then
        case " \$* " in *" -w "*) printf 'https://github.com/o/r/releases/tag/v9.9.9' ;; esac
        exit 0
      fi
      case " \$* " in *" -w "*) printf '' ;; esac
      exit 22 ;;
    */releases/latest/download/*)
      if [ "$1" = "ok" ]; then
        case " \$* " in *" -w "*) printf '200' ;; esac
        exit 0
      fi
      case " \$* " in *" -w "*) printf '%s' "$1" ;; esac
      # Faithful to the distinction install.sh depends on: with -f curl fails the
      # call on an HTTP error, without -f it succeeds and leaves the status to be
      # read from -w. Getting this wrong here silently turns every status into a
      # transport failure and the diagnosis assertions below stop testing anything.
      case " \$* " in
        *" -f"*|*"-fsSL"*|*"-fSL"*) exit 22 ;;
      esac
      if [ "$1" = "000" ]; then exit 7; fi
      exit 0 ;;
  esac
done
# Every curl call install.sh makes targets one of the shapes above, so reaching
# here means the URL was built from something that is not a URL -- exactly the
# failure this script exists to catch. Real curl rejects such an argument too,
# just with a message ("bad range in URL") describing its own parser.
echo "SHIM-UNEXPECTED-URL: \$*" >&2
exit 3
EOF
    chmod +x "$BINDIR/curl"
    : > "$WORK/requests.log"
}

run_install() {
    local home="$WORK/home.$1"
    mkdir -p "$home"
    # A token is deliberately present in the environment: if install.sh ever
    # grows an authenticated path again, these runs would quietly take it and
    # the anonymity assertions would stop meaning anything.
    env -i PATH="$BINDIR" HOME="$home" SHELL=/bin/sh \
        GITHUB_TOKEN=must-not-be-used GH_TOKEN=must-not-be-used \
        timeout 60 sh "$INSTALL_SH" "${@:2}" 2>&1
}

# Most assertions below are negative -- that some bad string is *absent* -- and
# those pass for free if the script never ran. Every case is gated on this
# first, so a broken harness reads as a failure rather than a clean sweep.
assert_ran() {
    if grep -q 'Installing\|Looking for\|\[INFO\]' <<<"$2"; then
        pass "$1: install.sh ran"
    else
        fail "$1: install.sh did not run (harness broken); output: ${2:0:120}"
    fi
}

assert_anonymous() {
    # $1 = label, $2 = output
    if grep -q 'SHIM-API-CALL' <<<"$2"; then
        fail "$1: called the GitHub API"
    else
        pass "$1: no GitHub API call"
    fi
    if grep -qi 'authorization\|bearer\|must-not-be-used' "$WORK/requests.log"; then
        fail "$1: sent credentials"
    else
        pass "$1: sent no credentials"
    fi
}

assert_no_bad_url() {
    if grep -q 'SHIM-UNEXPECTED-URL\|bad range in URL' <<<"$2"; then
        fail "$1: handed curl something that is not a URL"
    else
        pass "$1: never asks curl to fetch a non-URL"
    fi
    if grep -qE 'Downloading .*(ERROR|\[0;31m)' <<<"$2"; then
        fail "$1: a diagnostic was captured and used as a URL"
    else
        pass "$1: diagnostics do not leak into captured values"
    fi
}

echo "install.sh failure modes"

# --- Anonymous access is enough, for every method. ---
for method in tarball deb rpm appimage; do
    make_curl_shim ok
    OUT="$(run_install "anon.$method" --method="$method")"
    assert_anonymous "$method" "$OUT"
done

# --- Downloads refused. GitHub rate limits anonymous downloads too, so a 403
# --- here is not evidence the asset is missing and must not be reported as if
# --- it were. ---
make_curl_shim 403
OUT="$(run_install ratelimited --method=tarball)"
assert_ran "download 403" "$OUT"
assert_no_bad_url "download 403" "$OUT"
if grep -qi 'rate limiting downloads' <<<"$OUT"; then
    pass "a refused download is reported as rate limiting, not as a missing asset"
else
    fail "a refused download is not diagnosed as rate limiting"
fi

# --- The asset genuinely is not there. ---
make_curl_shim 404
OUT="$(run_install missing --method=tarball)"
assert_no_bad_url "download 404" "$OUT"
if grep -qi 'not published' <<<"$OUT"; then
    pass "a 404 is reported as a missing asset"
else
    fail "a 404 is not distinguished from other failures"
fi

# --- No network at all. ---
make_curl_shim 000
OUT="$(run_install offline --method=tarball)"
assert_ran "offline" "$OUT"
assert_no_bad_url "offline" "$OUT"
if grep -qi 'could not reach' <<<"$OUT"; then
    pass "offline path says the network is the problem"
else
    fail "offline path does not mention the network"
fi

echo
if [ "$FAILED" -eq 0 ]; then
    echo "all checks passed"
else
    echo "failures above"
fi
exit "$FAILED"
