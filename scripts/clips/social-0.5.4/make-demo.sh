#!/bin/sh
# Build the small project the clip films: a markdown note, two Rust files with
# three commits of history, and one uncommitted change for the diff views.
# No Cargo.toml or package.json, so the workspace opens Trusted.
set -eu
dir=${1:?usage: make-demo.sh DIR}
rm -rf "$dir"; mkdir -p "$dir"; cd "$dir"
git init -q
g() { git -c user.email=dev@fresh -c user.name=dev "$@"; }

cat > notes.md <<'MD'
# Release notes ✨

Fresh **0.5.4** keeps launches *simple*,
and startup stays fast.

- `fresh FILE` opens just that file
- workspaces load on first use
- the dock starts closed

| Mode        | Shows        |
|-------------|--------------|
| Source      | raw text 📝  |
| Rendered    | layout 🎨    |
| Dual 双     | both 🔀      |

```rust
fn main() {
    println!("fast 🚀");
}
```

> Lightweight by default.

MD
cat > main.rs <<'RS'
use std::time::Instant;

/// Open a file and paint the first frame.
fn main() {
    let start = Instant::now();
    let path = std::env::args().nth(1).unwrap_or_default();
    let text = std::fs::read_to_string(&path).unwrap_or_default();

    for (n, line) in text.lines().enumerate() {
        println!("{:>4} {}", n + 1, line);
    }

    eprintln!("first frame in {:?}", start.elapsed());
}
RS
printf 'naïve café — 東京 ☕\n' > café.txt
g add -A; g commit -qm init
printf 'pub fn greet(name: &str) -> String {\n    format!("hello, {name}")\n}\n' > lib.rs
g add lib.rs; g commit -qm "Add greet()"
sed -i 's/hello, /hi there, /' lib.rs; g commit -qam "Friendlier greeting"
# the uncommitted change Live Diff and Review Diff show
sed -i 's|    let start = Instant::now();|    let start = Instant::now();\n    let quiet = std::env::var("QUIET").is_ok();|' main.rs
echo .bash_history > .git/info/exclude
