#!/usr/bin/env node
'use strict';

// Stage what electron-builder packages but the repository does not keep under
// desktop/: the `fresh` binary (bin/), the platform icons (build/, where
// electron-builder looks for them) and the window icon (assets/).
//
//   node scripts/stage.js [path/to/fresh]
//
// Without an argument, the newest of the workspace's release and debug builds
// is used (in CARGO_TARGET_DIR, when set). The binary must be built with the
// `web` feature — the release builds are.

const fs = require('node:fs');
const path = require('node:path');

const { workspaceBinaries } = require('../src/backend');

const desktop = path.resolve(__dirname, '..');
const repo = path.resolve(desktop, '..');

function newestBuild() {
  const built = workspaceBinaries().slice(1).filter((c) => fs.existsSync(c));
  built.sort((a, b) => fs.statSync(b).mtimeMs - fs.statSync(a).mtimeMs);
  return built[0] || workspaceBinaries()[1];
}

const binary = path.resolve(process.argv[2] || newestBuild());
if (!fs.existsSync(binary)) {
  console.error(`no fresh binary at ${binary}`);
  console.error('build one with: cargo build --release --features web');
  process.exit(1);
}

function copy(from, to) {
  fs.mkdirSync(path.dirname(to), { recursive: true });
  fs.copyFileSync(from, to);
  console.log(`staged ${path.relative(desktop, to)}  ←  ${path.relative(repo, from) || from}`);
}

fs.rmSync(path.join(desktop, 'bin'), { recursive: true, force: true });
copy(binary, path.join(desktop, 'bin', path.basename(binary).endsWith('.exe') ? 'fresh.exe' : 'fresh'));
if (!binary.endsWith('.exe')) fs.chmodSync(path.join(desktop, 'bin', 'fresh'), 0o755);

// The bundled editor is updated by installing a newer Fresh Desktop, never in
// place. Its install receipt (crates/fresh-update/src/receipt.rs, read from
// next to the binary) says so: no `fresh --cmd update`, no self-replacement —
// and, unlike an environment variable, it reaches no other `fresh` the
// editor's terminals run.
fs.writeFileSync(path.join(desktop, 'bin', 'install-receipt.toml'), [
  '# Fresh Desktop bundles this editor; update by installing a newer Fresh Desktop.',
  'schema = 1',
  'channel = "unknown"',
  'managed = true',
  'self_update = false',
  '',
].join('\n'));
console.log('staged bin/install-receipt.toml');

const icons = path.join(repo, 'docs', 'icons');
copy(path.join(icons, 'macos', 'AppIcon.icns'), path.join(desktop, 'build', 'icon.icns'));
copy(path.join(icons, 'windows', 'app.ico'), path.join(desktop, 'build', 'icon.ico'));
copy(path.join(icons, 'linux', 'fresh_512.png'), path.join(desktop, 'build', 'icon.png'));
copy(path.join(icons, 'linux', 'hicolor', '256x256', 'apps', 'fresh.png'), path.join(desktop, 'assets', 'icon.png'));
