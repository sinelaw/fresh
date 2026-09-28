#!/usr/bin/env bash
# Prepare the work dir (default: target/ad) for recording the Fresh ad:
# demo repo, isolated XDG config, fake agent shims, a 2 GB log, fonts, logo.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
W="${AD_WORK:-$REPO/target/ad}"
mkdir -p "$W"/{bin,casts,frames,fonts,xdg/config/fresh,xdg/config-orch/fresh}

# Editor config: Tokyo Night, no welcome tab; the orchestrator variant keeps the dock.
cat > "$W/xdg/config/fresh/config.json" <<'JSON'
{
  "theme": "builtin://tokyo-night",
  "check_for_updates": false,
  "plugins": {
    "welcome_screen": { "enabled": false },
    "dashboard": { "enabled": false },
    "vi_mode": { "enabled": false },
    "orchestrator": { "enabled": true, "settings": { "autoOpenDock": false } }
  }
}
JSON
python3 - "$W" <<'PY'
import json, sys
w = sys.argv[1]
c = json.load(open(f"{w}/xdg/config/fresh/config.json"))
c["plugins"]["orchestrator"] = {"enabled": True}
json.dump(c, open(f"{w}/xdg/config-orch/fresh/config.json", "w"), indent=1)
PY

# `fresh` on PATH for the shell clips, and fake `claude`/`codex` agents (staged output,
# see tests/fixtures/coding_agent.py). COLUMNS pins the agent to the pane width: in a
# narrow Orchestrator layout the agent's pty reports more columns than the pane shows,
# and its cursor-up redraw leaves stale spinner lines behind.
printf '#!/bin/sh\nexec "%s" --no-upgrade-check --no-restore "$@"\n' "${FRESH:-$REPO/target/debug/fresh}" > "$W/bin/fresh"
cp "$REPO/crates/fresh-editor/tests/fixtures/coding_agent.py" "$W/bin/"
for a in claude codex; do
  printf '#!/bin/sh\nCOLUMNS=58 exec python3 "%s/bin/coding_agent.py" --as %s "$@"\n' "$W" "$a" > "$W/bin/$a"
done
chmod +x "$W"/bin/*

# Demo project: no project manifest, so the workspace opens Trusted.
D="$W/demo"
if [[ ! -d "$D/.git" ]]; then
  mkdir -p "$D/src"
  cat > "$D/src/main.rs" <<'RS'
mod cache;
mod server;

use server::Server;

fn main() {
    let port = 8080;
    let server = Server::new(port);

    // TODO: read port from env
    println!("listening on {port}");
    server.run();
}
RS
  cat > "$D/src/server.rs" <<'RS'
use crate::cache::Cache;

pub struct Server {
    port: u16,
    cache: Cache,
}

impl Server {
    pub fn new(port: u16) -> Self {
        let cache = Cache::with_capacity(1024);
        Self { port, cache }
    }

    pub fn run(&self) {
        let user = self.lookup("alice");
        let user_id = user.id;
        let user_name = user.name;
        log(user_id, user_name);
        // TODO: graceful shutdown
    }

    fn lookup(&self, name: &str) -> User {
        self.cache.get(name).unwrap_or_default()
    }
}
RS
  cat > "$D/src/cache.rs" <<'RS'
use std::collections::HashMap;

#[derive(Default)]
pub struct Cache {
    items: HashMap<String, Entry>,
    hits: u64,
}

impl Cache {
    pub fn with_capacity(n: usize) -> Self {
        let items = HashMap::with_capacity(n);
        Self { items, hits: 0 }
    }

    // TODO: evict least recently used
    pub fn get(&self, key: &str) -> Option<&Entry> {
        self.items.get(key)
    }
}
RS
  printf '# tiny-server\n\nA tiny HTTP server with an in-memory cache.\n\n## TODO\n\n- [ ] TLS support\n- [x] Caching\n' > "$D/README.md"
  printf '# Notes\n\n- TODO: benchmark the cache\n' > "$D/notes.md"
  echo "huge.log" > "$D/.gitignore"
  git_() { GIT_CONFIG_GLOBAL=/dev/null git -C "$D" -c user.email=demo@local -c user.name=Demo -c commit.gpgsign=false "$@"; }
  git_ -c init.defaultBranch=main init -q
  git_ add . && git_ commit -qm init
fi

# A 2 GB log for the huge-file shot.
if [[ ! -f "$D/huge.log" ]]; then
  python3 - "$D/huge.log" <<'PY'
import random, sys
random.seed(1)
lv = ['INFO ', 'INFO ', 'INFO ', 'DEBUG', 'WARN ', 'ERROR']
paths = ['/api/users', '/api/orders', '/health', '/api/cart', '/login', '/api/search?q=fresh']
block = ''.join(
    f"2026-09-28T{(i//3600)%24:02d}:{(i//60)%60:02d}:{i%60:02d}.{i%1000:03d}Z {random.choice(lv)} "
    f"req={i:08x} {random.choice(['GET ', 'POST'])} {random.choice(paths)} {random.randint(200, 504)} {random.randint(1, 900)}ms\n"
    for i in range(200000)).encode()
with open(sys.argv[1], 'wb') as f:
    n = 0
    while n < 2 * 1024 ** 3:
        f.write(block); n += len(block)
PY
fi

# Fonts and logo for the compositor.
for w in Regular Bold ExtraBold; do
  [[ -f "$W/fonts/JBM-$w.woff2" ]] || curl -fsSL -o "$W/fonts/JBM-$w.woff2" \
    "https://cdn.jsdelivr.net/gh/JetBrains/JetBrainsMono@master/fonts/webfonts/JetBrainsMono-$w.woff2"
done
[[ -f "$W/fonts/Inter-900.woff2" ]] || curl -fsSL -o "$W/fonts/Inter-900.woff2" \
  "https://cdn.jsdelivr.net/npm/@fontsource-variable/inter/files/inter-latin-wght-normal.woff2"
for f in instrument-serif-latin-400-normal instrument-serif-latin-400-italic; do
  [[ -f "$W/fonts/$f.woff2" ]] || curl -fsSL -o "$W/fonts/$f.woff2" \
    "https://cdn.jsdelivr.net/npm/@fontsource/instrument-serif/files/$f.woff2"
done
python3 -c "from PIL import Image; im=Image.open('$REPO/docs/logo.png'); im.thumbnail((600,600)); im.save('$W/logo.png')"
echo "work dir ready: $W"
