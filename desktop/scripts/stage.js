#!/usr/bin/env node
'use strict';

// Stage what electron-builder packages but the repository does not keep under
// desktop/: the `fresh` binary (bin/), the platform icons (build/, where
// electron-builder looks for them) and the window icon (assets/).
//
//   node scripts/stage.js [path/to/fresh]
//
// Without an argument, the workspace's target/release build is used. The
// binary must be built with the `web` feature — the release builds are.

const fs = require('node:fs');
const path = require('node:path');

const desktop = path.resolve(__dirname, '..');
const repo = path.resolve(desktop, '..');
const exe = process.platform === 'win32' ? 'fresh.exe' : 'fresh';

const binary = path.resolve(process.argv[2] || path.join(repo, 'target', 'release', exe));
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

const icons = path.join(repo, 'docs', 'icons');
copy(path.join(icons, 'macos', 'AppIcon.icns'), path.join(desktop, 'build', 'icon.icns'));
copy(path.join(icons, 'windows', 'app.ico'), path.join(desktop, 'build', 'icon.ico'));
copy(path.join(icons, 'linux', 'fresh_512.png'), path.join(desktop, 'build', 'icon.png'));
copy(path.join(icons, 'linux', 'hicolor', '256x256', 'apps', 'fresh.png'), path.join(desktop, 'assets', 'icon.png'));
