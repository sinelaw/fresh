#!/usr/bin/env node
'use strict';

// Build Fresh Desktop from source into this platform's package, in one step:
//
//   node desktop/scripts/package.js          (from the repository root)
//
//   Linux    → desktop/dist/fresh-desktop-<version>-linux-<arch>.AppImage
//   macOS    → desktop/dist/fresh-desktop-<version>-macos-<arch>.dmg
//   Windows  → desktop/dist/fresh-desktop-<version>-windows-<arch>-setup.exe
//
// It builds the editor (`cargo build --release --features web`), installs the
// app's npm dependencies when they are missing, stages the editor and icons
// (scripts/stage.js) and runs electron-builder for this OS and CPU. Needs Rust
// and Node.js; nothing is signed with a developer certificate (macOS is
// ad-hoc signed, as the release builds are).
//
// Options:
//   --binary <path>   package this `fresh` instead of building one (it must
//                     have the `web` feature); FRESH_BIN does the same
//   --version <x.y.z> the version to stamp (default: the workspace's)
//
// The release workflow (.github/workflows/desktop-builds.yml) does the same
// steps, but packages a universal macOS dmg from two lipo'd builds; this
// script packages the machine's own architecture.

const { spawnSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');

const desktop = path.resolve(__dirname, '..');
const repo = path.resolve(desktop, '..');
const windows = process.platform === 'win32';

function option(name) {
  const i = process.argv.indexOf(name);
  if (i < 0) return null;
  const value = process.argv[i + 1];
  if (!value || value.startsWith('--')) fail(`${name} needs a value`);
  return value;
}

function fail(msg) {
  console.error(`package: ${msg}`);
  process.exit(1);
}

function run(cmd, args, opts = {}) {
  console.log(`\n$ ${[cmd, ...args].join(' ')}`);
  // npm is a .cmd script on Windows, which only a shell runs.
  const r = spawnSync(cmd, args, { stdio: 'inherit', shell: windows && cmd === 'npm', ...opts });
  if (r.error) fail(`could not run ${cmd}: ${r.error.message}`);
  if (r.status !== 0) fail(`${cmd} exited with ${r.status}`);
}

// ── What to build for ──────────────────────────────────────────────────────
// electron-builder's names for the platform and CPU, and the names the
// release artifacts use for them.
const platform = { linux: 'linux', darwin: 'mac', win32: 'win' }[process.platform];
if (!platform) fail(`no Fresh Desktop package for ${process.platform}`);
const arch = { x64: 'x64', arm64: 'arm64' }[process.arch];
if (!arch) fail(`no Fresh Desktop package for ${process.arch} CPUs`);
const target = { linux: 'AppImage', mac: 'dmg', win: 'nsis' }[platform];
const archName = platform === 'win' ? arch : { x64: 'x86_64', arm64: 'aarch64' }[arch];
const artifactName = {
  linux: `fresh-desktop-\${version}-linux-${archName}.\${ext}`,
  mac: `fresh-desktop-\${version}-macos-${arch === 'arm64' ? 'arm64' : 'x86_64'}.\${ext}`,
  win: `fresh-desktop-\${version}-windows-${archName}-setup.\${ext}`,
}[platform];

function workspaceVersion() {
  const toml = fs.readFileSync(path.join(repo, 'Cargo.toml'), 'utf8');
  const m = /\[workspace\.package\][^[]*?^version\s*=\s*"([^"]+)"/m.exec(toml);
  if (!m) fail('could not read the workspace version from Cargo.toml');
  return m[1];
}
const version = option('--version') || workspaceVersion();

// ── 1. The editor ──────────────────────────────────────────────────────────
let binary = option('--binary') || process.env.FRESH_BIN;
if (!binary) {
  run('cargo', ['build', '--release', '--package', 'fresh-editor', '--features', 'web'], { cwd: repo });
  const targetDir = process.env.CARGO_TARGET_DIR
    ? path.resolve(repo, process.env.CARGO_TARGET_DIR)
    : path.join(repo, 'target');
  binary = path.join(targetDir, 'release', windows ? 'fresh.exe' : 'fresh');
}
binary = path.resolve(binary);
if (!fs.existsSync(binary)) fail(`no fresh binary at ${binary}`);

// ── 2. Electron and electron-builder ───────────────────────────────────────
// `npm ci` only when there is no install yet: it starts from scratch, and
// Electron is a large download.
if (!fs.existsSync(path.join(desktop, 'node_modules', '.package-lock.json'))) {
  run('npm', ['ci', '--no-audit', '--no-fund'], { cwd: desktop });
}

// ── 3. Stage the editor and icons ──────────────────────────────────────────
run(process.execPath, [path.join(__dirname, 'stage.js'), binary], { cwd: desktop });

// ── 4. Package ─────────────────────────────────────────────────────────────
const builder = require.resolve('electron-builder/cli.js', { paths: [desktop] });
run(process.execPath, [
  builder, `--${platform}`, target, `--${arch}`, '--publish', 'never',
  `-c.extraMetadata.version=${version}`,
  `-c.${platform}.artifactName=${artifactName}`,
], {
  cwd: desktop,
  env: {
    // No developer certificate: macOS is ad-hoc signed (electron-builder.yml),
    // Windows is unsigned. Set CSC_* yourself to sign with your own.
    CSC_IDENTITY_AUTO_DISCOVERY: 'false',
    ...process.env,
  },
});

const ext = { linux: 'AppImage', mac: 'dmg', win: 'exe' }[platform];
const built = path.join(desktop, 'dist', artifactName.replace('${version}', version).replace('${ext}', ext));
if (!fs.existsSync(built)) fail(`electron-builder finished, but ${built} is not there`);
console.log(`\nBuilt ${built}`);
