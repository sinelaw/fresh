const https = require('https');
const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const { getBinaryInfo } = require('./binary');

const VERSION = require('./package.json').version;
const REPO = 'sinelaw/fresh';

// Retry helper for Windows file locking issues
async function withRetry(fn, retries = 5) {
    for (let i = 0; i < retries; i++) {
        try {
            return await fn();
        } catch (e) {
            if (i === retries - 1) throw e;
            await new Promise(r => setTimeout(r, 100 * (i + 1)));
        }
    }
}

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
                    // Clean up partial file before redirect
                    fs.unlink(dest, () => {
                        download(response.headers.location, dest).then(resolve).catch(reject);
                    });
                });
                return;
            }
            if (response.statusCode !== 200) {
                file.close();
                fs.unlink(dest, () => {
                    reject(new Error(`Failed to download: ${response.statusCode}`));
                });
                return;
            }
            response.pipe(file);
            file.on('finish', () => {
                file.close((err) => {
                    if (err) reject(err);
                    else if (process.platform === 'win32') {
                        // Brief delay on Windows to ensure file handle is released
                        setTimeout(resolve, 50);
                    } else {
                        resolve();
                    }
                });
            });
        }).on('error', (err) => {
            file.close();
            fs.unlink(dest, () => reject(err));
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

    // Extract with retry logic to handle Windows file locking
    try {
        await withRetry(async () => {
            if (info.ext === 'tar.xz') {
                execSync(`tar -xJf "${archivePath}" -C "${binDir}" --strip-components=1`, { stdio: 'inherit' });
            } else if (info.ext === 'zip') {
                if (process.platform === 'win32') {
                    // Use absolute paths for PowerShell with -LiteralPath
                    const absArchive = path.resolve(archivePath);
                    const absBin = path.resolve(binDir);

                    execSync(
                        `powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "& {Expand-Archive -LiteralPath '${absArchive}' -DestinationPath '${absBin}' -Force}"`,
                        { stdio: 'inherit', windowsHide: true }
                    );

                    // Move files from nested directory if present
                    const nested = path.join(binDir, `fresh-editor-${info.target}`);
                    if (fs.existsSync(nested)) {
                        const files = fs.readdirSync(nested);
                        for (const f of files) {
                            const srcPath = path.join(nested, f);
                            const destPath = path.join(binDir, f);
                            if (fs.existsSync(destPath)) {
                                fs.rmSync(destPath, { recursive: true, force: true });
                            }
                            fs.renameSync(srcPath, destPath);
                        }
                        fs.rmSync(nested, { recursive: true, force: true });
                    }
                } else {
                    execSync(`unzip -o "${archivePath}" -d "${binDir}"`, { stdio: 'inherit' });
                    // Handle nested directory for non-Windows
                    const nested = path.join(binDir, `fresh-editor-${info.target}`);
                    if (fs.existsSync(nested)) {
                        const files = fs.readdirSync(nested);
                        for (const f of files) {
                            const srcPath = path.join(nested, f);
                            const destPath = path.join(binDir, f);
                            if (fs.existsSync(destPath)) {
                                fs.rmSync(destPath, { recursive: true, force: true });
                            }
                            fs.renameSync(srcPath, destPath);
                        }
                        fs.rmdirSync(nested);
                    }
                }
            }
        });
    } catch (error) {
        error.message = `Extraction failed after retries: ${error.message}. Archive: ${archivePath}`;
        throw error;
    }

    // Cleanup archive file
    try {
        if (fs.existsSync(archivePath)) {
            fs.unlinkSync(archivePath);
        }
    } catch (e) {
        console.warn('Could not delete archive file:', e.message);
    }

    // Verify binary exists
    if (!fs.existsSync(binaryPath)) {
        throw new Error(`Installation failed: binary not found at ${binaryPath}. Please check the release assets.`);
    }

    // Provenance receipt (sidecar next to the binary). Overrides the generic
    // `tarball` receipt that ships inside the archive so the editor defers
    // updates to npm rather than self-swapping.
    try {
        const receipt = [
            'schema = 1',
            'channel = "npm"',
            `version = "${VERSION}"`,
            'package_name = "fresh-editor"',
            'managed = true',
            'self_update = false',
            '',
            '[hints]',
            'npm_pkg = "@fresh-editor/fresh-editor"',
            '',
        ].join('\n');
        fs.writeFileSync(path.join(binDir, 'install-receipt.toml'), receipt);
    } catch (e) {
        console.warn('Could not write install receipt:', e.message);
    }

    console.log('fresh-editor installed successfully!');
}

module.exports = { install };
