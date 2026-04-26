# Source me to put the fake `devcontainer` and `docker` CLIs first on PATH.
#
#   source scripts/fake-devcontainer/activate.sh
#   ./target/debug/fresh /path/to/workspace-with-.devcontainer
#
# Or print env-export lines (for `eval` in CI):
#
#   eval "$(scripts/fake-devcontainer/activate.sh --print-env)"

_fake_dc_self="${BASH_SOURCE[0]:-${(%):-%N}}"
if [ -z "${_fake_dc_self:-}" ]; then
  echo "fake-devcontainer activate.sh: must be sourced from bash or zsh" >&2
  return 1 2>/dev/null || exit 1
fi

_fake_dc_dir="$(cd "$(dirname "$_fake_dc_self")" && pwd)"
_fake_dc_bin="$_fake_dc_dir/bin"

if [ "${1:-}" = "--print-env" ]; then
  printf 'export PATH="%s:%s"\n' "$_fake_dc_bin" "$PATH"
  if [ -z "${FAKE_DEVCONTAINER_STATE:-}" ]; then
    printf 'export FAKE_DEVCONTAINER_STATE="%s"\n' "${XDG_CACHE_HOME:-$HOME/.cache}/fake-devcontainer"
  fi
  unset _fake_dc_self _fake_dc_dir _fake_dc_bin
  return 0 2>/dev/null || exit 0
fi

case ":$PATH:" in
  *":$_fake_dc_bin:"*) ;;  # already on PATH
  *) export PATH="$_fake_dc_bin:$PATH" ;;
esac

if [ -z "${FAKE_DEVCONTAINER_STATE:-}" ]; then
  export FAKE_DEVCONTAINER_STATE="${XDG_CACHE_HOME:-$HOME/.cache}/fake-devcontainer"
fi

echo "fake-devcontainer: PATH-shimmed devcontainer/docker (state: $FAKE_DEVCONTAINER_STATE)" >&2

unset _fake_dc_self _fake_dc_dir _fake_dc_bin
