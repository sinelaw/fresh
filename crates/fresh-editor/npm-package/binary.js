const os = require('os');

function getBinaryInfo() {
  const platform = os.platform();
  const arch = os.arch();

  const targets = {
    'darwin-x64': { target: 'x86_64-apple-darwin', ext: 'tar.xz' },
    'darwin-arm64': { target: 'aarch64-apple-darwin', ext: 'tar.xz' },
    'linux-x64': { target: 'x86_64-unknown-linux-gnu', ext: 'tar.xz' },
    'linux-arm64': { target: 'aarch64-unknown-linux-gnu', ext: 'tar.xz' },
    'win32-x64': { target: 'x86_64-pc-windows-msvc', ext: 'zip' }
  };

  const key = `${platform}-${arch}`;
  const info = targets[key];

  if (!info) {
    throw new Error(`Unsupported platform: ${platform}-${arch}`);
  }

  return {
    ...info,
    binaryName: platform === 'win32' ? 'fresh.exe' : 'fresh'
  };
}

module.exports = { getBinaryInfo };
