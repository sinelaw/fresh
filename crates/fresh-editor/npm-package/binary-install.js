const https = require('https');
const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const { getBinaryInfo } = require('./binary');

const VERSION = require('./package.json').version;
const REPO = 'sinelaw/fresh';

function download(url, dest) {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(dest);
    file.on('error', (err) => {
      file.close();
      reject(err);
    });
    https.get(url, (response) => {
      if (response.statusCode === 302 || response.statusCode === 301) {
        file.close(() => {
          download(response.headers.location, dest).then(resolve).catch(reject);
        });
        return;
      }
      if (response.statusCode !== 200) {
        file.close();
        reject(new Error(`Failed to download: ${response.statusCode}`));
        return;
      }
      response.pipe(file);
      file.on('finish', () => { file.close(); resolve(); });
    }).on('error', (err) => {
      file.close();
      reject(err);
    });
  });
}

async function install() {
  const info = getBinaryInfo();
  const archiveName = `fresh-editor-${info.target}.${info.ext}`;
  const url = `https://github.com/${REPO}/releases/download/v${VERSION}/${archiveName}`;
  const archivePath = path.join(__dirname, archiveName);
  const binDir = path.join(__dirname, 'bin');
  const binaryPath = path.join(binDir, info.binaryName);

  console.log(`Downloading ${url}...`);
  await download(url, archivePath);

  // Verify download succeeded
  const stats = fs.statSync(archivePath);
  if (stats.size === 0) {
    throw new Error(`Downloaded file is empty: ${archivePath}`);
  }
  console.log(`Downloaded ${stats.size} bytes`);

  fs.mkdirSync(binDir, { recursive: true });

  if (info.ext === 'tar.xz') {
    execSync(`tar -xJf "${archivePath}" -C "${binDir}" --strip-components=1`, { stdio: 'inherit' });
  } else if (info.ext === 'zip') {
    if (process.platform === 'win32') {
      execSync(`powershell -Command "Expand-Archive -Path '${archivePath}' -DestinationPath '${binDir}' -Force"`, { stdio: 'inherit' });
      // Move files from nested directory
      const nested = path.join(binDir, `fresh-editor-${info.target}`);
      if (fs.existsSync(nested)) {
        fs.readdirSync(nested).forEach(f => {
          fs.renameSync(path.join(nested, f), path.join(binDir, f));
        });
        fs.rmdirSync(nested);
      }
    } else {
      execSync(`unzip -o "${archivePath}" -d "${binDir}"`, { stdio: 'inherit' });
    }
  }

  fs.unlinkSync(archivePath);

  // Verify binary exists
  if (!fs.existsSync(binaryPath)) {
    throw new Error(`Installation failed: binary not found at ${binaryPath}`);
  }

  console.log('fresh-editor installed successfully!');
}

module.exports = { install };
