#!/usr/bin/env bash
#
# QuickLSP Benchmark Runner
#
# Usage: ./scripts/bench_repos.sh [--skip-clone]

set -euo pipefail

REPOS_DIR="/tmp/quicklsp-bench-repos"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WORKSPACE_DIR="$(cd "$SCRIPT_DIR/../../.." && pwd)"

declare -A REPOS=(
    ["ripgrep"]="https://github.com/BurntSushi/ripgrep.git"
    ["bat"]="https://github.com/sharkdp/bat.git"
    ["flask"]="https://github.com/pallets/flask.git"
    ["httpie"]="https://github.com/httpie/cli.git"
    ["express"]="https://github.com/expressjs/express.git"
    ["preact"]="https://github.com/preactjs/preact.git"
    ["esbuild"]="https://github.com/evanw/esbuild.git"
    ["jq"]="https://github.com/jqlang/jq.git"
)

clone_repos() {
    echo "── Cloning repositories to $REPOS_DIR ──"
    mkdir -p "$REPOS_DIR"
    for name in "${!REPOS[@]}"; do
        local url="${REPOS[$name]}"
        local dest="$REPOS_DIR/$name"
        if [ -d "$dest" ]; then
            echo "  [skip] $name"
        else
            echo "  [clone] $name ← $url"
            git clone --depth 1 --single-branch "$url" "$dest" 2>/dev/null || {
                echo "  [WARN] Failed to clone $name"
            }
        fi
    done
    echo
}

build_bench() {
    echo "── Building benchmark (release) ──"
    cd "$WORKSPACE_DIR"
    cargo build -p quicklsp --example repo_benchmark --release 2>&1 | tail -3
    echo
}

run_bench() {
    echo "── Running benchmark ──"
    cd "$WORKSPACE_DIR"
    QUICKLSP_REPOS_DIR="$REPOS_DIR" \
        cargo run -p quicklsp --example repo_benchmark --release 2>&1
}

if [[ "${1:-}" != "--skip-clone" ]]; then
    clone_repos
fi

build_bench
run_bench
