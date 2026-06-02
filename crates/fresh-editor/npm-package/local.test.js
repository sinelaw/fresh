// This script tests the npm package locally.
// You can optionally pass a version as a command-line argument.
//
// Examples:
//   node local.test.js 1.2.3
//     -> Tests the npm package for version 1.2.3
//
//   node local.test.js
//     -> Tests the latest version from the GitHub release
//
// You can also pass the `--keep` flag to preserve the temporary test directory:
//
//   node local.test.js 1.2.3 --keep
//
// When `--keep` is provided, the temporary test directory will NOT be deleted
// Any other value (or no value) is treated as false, and the directory will be
// cleaned up automatically.


const { execSync } = require("child_process");
const fs = require("fs");
const path = require("path");
const https = require("https");

function run(command, options = {}) {
    console.log("> " + command);
    execSync(command, { stdio: "inherit", ...options });
}

// Fetch latest GitHub release version
function getLatestReleaseVersion() {
    return new Promise((resolve, reject) => {
        const options = {
            hostname: 'api.github.com',
            path: '/repos/sinelaw/fresh/releases/latest',
            headers: { 'User-Agent': 'npm-package-test' }
        };

        https.get(options, (res) => {
            let data = '';
            res.on('data', chunk => data += chunk);
            res.on('end', () => {
                try {
                    const release = JSON.parse(data);
                    const version = release.tag_name.replace(/^v/, '');
                    resolve(version);
                } catch (err) {
                    reject(err);
                }
            });
        }).on('error', reject);
    });
}

console.log("Testing Fresh npm package locally...\n");

const argVersion = process.argv[2];
const keepTempDir = process.argv[3];



if (argVersion) {
    console.log("Using provided version:", argVersion, "\n");
    runTest(argVersion);
} else {
    // Get latest release version from GitHub
    console.log("No version specified, fetching latest release from GitHub...");
    getLatestReleaseVersion().then(VERSION => {
        console.log("Using latest release version:", VERSION, "\n");
        runTest(VERSION);
    }).catch(err => {
        console.error("[ERROR] Failed to fetch latest release:", err.message);
        console.error("You can specify a version manually: node local.test.js <version>");
        process.exit(1);
    });
}

function runTest(VERSION) {

    // Generate package.json from template
    console.log("Creating package.json from template...");
    const template = fs.readFileSync("package.json.template", "utf8");
    const pkgJson = template.replace(/VERSION_PLACEHOLDER/g, VERSION);
    fs.writeFileSync("package.json", pkgJson);

    // Copy supporting files
    console.log("Copying supporting files...");
    ["README.md", "LICENSE", "CHANGELOG.md"].forEach(file => {
        const src = path.join("..", file);
        if (fs.existsSync(src)) fs.copyFileSync(src, file);
    });

    console.log("[OK] Files prepared\n");

    // Remove old tarballs
    fs.readdirSync(".").filter(f => f.endsWith(".tgz")).forEach(f => fs.unlinkSync(f));

    // Create npm tarball
    console.log("Creating npm package tarball...");
    run("npm pack");

    const tarball = fs.readdirSync(".").find(f => f.endsWith(".tgz"));
    if (!tarball) throw new Error("npm pack did not produce a .tgz file");
    console.log("[OK] Created:", tarball, "\n");

    // Install into temporary directory
    console.log("Testing installation in a temporary directory...");
    const tempDir = "test-package";
    if (fs.existsSync(tempDir)) {
        fs.rmSync(tempDir, { recursive: true, force: true });
    }
    fs.mkdirSync(tempDir);

    // Copy all package files to test directory for npm link support
    ["binary.js", "binary-install.js", "install.js", "run-fresh.js"].forEach(file => {
        if (fs.existsSync(file)) fs.copyFileSync(file, path.join(tempDir, file));
    });
    ["README.md", "LICENSE", "CHANGELOG.md"].forEach(file => {
        if (fs.existsSync(file)) fs.copyFileSync(file, path.join(tempDir, file));
    });
    
    const tempTemplate = fs.readFileSync("package.json.template", "utf8");
    const tempPkgJson = tempTemplate
        .replace(/VERSION_PLACEHOLDER/g, VERSION)
        .replace(/"@fresh-editor\/fresh-editor"/, '"fresh-test-package"');
    fs.writeFileSync(path.join(tempDir, "package.json"), tempPkgJson);

    try {
        run(`npm install "${path.join(process.cwd(), tarball)}"`, { cwd: tempDir });
        console.log("\n[OK] Installation completed\n");
    } catch (err) {
        console.log("\n[ERROR] Installation failed:");
        console.log(err.message);

        // Cleanup and exit with error
        console.log("\nCleaning up...");
        ["package.json", "README.md", "LICENSE", "CHANGELOG.md"].forEach(f => {
            if (fs.existsSync(f)) fs.unlinkSync(f);
        });
        if (fs.existsSync(tarball)) fs.unlinkSync(tarball);
        if (fs.existsSync(tempDir)) fs.rmSync(tempDir, { recursive: true, force: true });
        console.log("[ERROR] Test failed!");
        process.exit(1);
    }

    // Verify binary
    console.log("Verifying fresh binary...");
    const freshBinary =
        process.platform === "win32"
            ? path.join(tempDir, "node_modules", ".bin", "fresh.cmd")
            : path.join(tempDir, "node_modules", ".bin", "fresh");

    if (!fs.existsSync(freshBinary)) {
        throw new Error(`fresh binary not found at: ${freshBinary}`);
    }

    run(`"${freshBinary}" --version`);
    console.log("[OK] Binary executed successfully\n");

    // Cleanup repo files
    console.log("Cleaning up generated files in repo...");
    ["package.json", "README.md", "LICENSE", "CHANGELOG.md"].forEach(file => {
        if (fs.existsSync(file)) fs.unlinkSync(file);
    });
    if (fs.existsSync(tarball)) fs.unlinkSync(tarball);

    // Cleanup temp folder
    if (keepTempDir !== '--keep' && fs.existsSync(tempDir)) {
        fs.rmSync(tempDir, { recursive: true, force: true });
    }

    console.log("\n[SUCCESS] All tests passed!");
}

