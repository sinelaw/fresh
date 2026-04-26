# Shared state helpers for the fake devcontainer/docker CLIs.
# Sourced by bin/devcontainer and bin/docker. Pure POSIX-ish bash.

fake_state_dir() {
  if [ -n "${FAKE_DEVCONTAINER_STATE:-}" ]; then
    printf '%s\n' "$FAKE_DEVCONTAINER_STATE"
    return
  fi
  cache_root="${XDG_CACHE_HOME:-$HOME/.cache}"
  printf '%s\n' "$cache_root/fake-devcontainer"
}

fake_state_init() {
  state="$(fake_state_dir)"
  mkdir -p "$state/containers"
  printf '%s\n' "$state"
}

# Deterministic-ish 12-hex id derived from workspace + epoch ms + random.
# Uses sha256sum if present, else falls back to /dev/urandom hex.
fake_alloc_id() {
  workspace="$1"
  if command -v sha256sum >/dev/null 2>&1; then
    seed="$workspace|$(date +%s%N 2>/dev/null || date +%s)|$RANDOM"
    printf '%s' "$seed" | sha256sum | cut -c1-12
  elif command -v shasum >/dev/null 2>&1; then
    seed="$workspace|$(date +%s)|$RANDOM"
    printf '%s' "$seed" | shasum -a 256 | cut -c1-12
  else
    od -An -N6 -tx1 /dev/urandom | tr -d ' \n'
  fi
}

# Find the most recently created container id for a workspace, or empty.
fake_find_container_for_workspace() {
  workspace="$1"
  state="$(fake_state_dir)"
  [ -d "$state/containers" ] || return 0
  best_ts=0
  best_id=""
  for dir in "$state"/containers/*/; do
    [ -d "$dir" ] || continue
    [ -f "$dir/workspace" ] || continue
    [ "$(cat "$dir/workspace")" = "$workspace" ] || continue
    [ "$(cat "$dir/status" 2>/dev/null)" = "running" ] || continue
    ts=$(cat "$dir/created_at" 2>/dev/null || echo 0)
    if [ "$ts" -gt "$best_ts" ]; then
      best_ts=$ts
      best_id="$(basename "$dir")"
    fi
  done
  [ -n "$best_id" ] && printf '%s\n' "$best_id"
}

# Sleep for milliseconds. Honours sub-second sleeps where supported.
fake_sleep_ms() {
  ms="$1"
  [ "$ms" -le 0 ] 2>/dev/null && return 0
  # GNU sleep / BSD sleep accept fractional seconds.
  secs=$(awk -v m="$ms" 'BEGIN { printf "%.3f", m/1000 }')
  sleep "$secs" 2>/dev/null || sleep 1
}
