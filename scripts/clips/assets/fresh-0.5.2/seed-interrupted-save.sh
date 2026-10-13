#!/usr/bin/env bash
# Leave behind what a save that died partway leaves behind, so the next start
# offers the Interrupted Save dialog. Nothing is interrupted for real: the
# copy and its record are written straight into the recovery directory, in
# the shape fresh's own sweep looks for (see tests/e2e/interrupted_save_recovery.rs).
set -euo pipefail
dest="$(pwd)/docs/moorings.md"
dir="${XDG_DATA_HOME:-$HOME/.local/share}/fresh/recovery"
mkdir -p "$dir"
pid=2000000000                      # never a live process
tmp="$dir/.inplace-moorings.md-$pid-1.tmp"
sed 's/Berths 1-6/Berths 1-8/' "$dest" > "$tmp"   # must differ, or it is swept
hash="$(printf '%s' "$dest" | sha256sum | cut -c1-16)"
cat > "$dir/$hash.inplace.json" <<EOF
{"dest_path":"$dest","temp_path":"$tmp","uid":$(id -u),"gid":$(id -g),
 "mode":420,"started_at":1700000000,"pid":$pid}
EOF
