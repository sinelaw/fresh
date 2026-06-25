import { spawn } from "node:child_process";
import { platform } from "node:os";

import { startProxyServer } from "./fresh-ai-proxy.mjs";

const IS_WINDOWS = platform() === "win32";

function runCommand(command, args, options = {}) {
  const actualCommand = IS_WINDOWS && command === "npx"
    ? "cmd.exe"
    : command;
  const actualArgs = IS_WINDOWS && command === "npx"
    ? ["/c", "npx", ...args]
    : args;

  return new Promise((resolve, reject) => {
    const child = spawn(actualCommand, actualArgs, {
      cwd: options.cwd,
      shell: false,
      stdio: ["ignore", "pipe", "pipe"],
      env: options.env ?? process.env,
    });

    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk) => { stdout += chunk.toString(); });
    child.stderr.on("data", (chunk) => { stderr += chunk.toString(); });
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) {
        resolve({ stdout, stderr, code });
      } else {
        reject(new Error(`${actualCommand} ${actualArgs.join(" ")} failed with code ${code}\n${stdout}\n${stderr}`));
      }
    });
  });
}

async function main() {
  console.log("1. Proxy unit checks");
  await runCommand("node", ["scripts/fresh-ai-proxy.test.mjs"]);
  console.log("PASS proxy unit checks");

  console.log("2. Fresh AI plugin TypeScript check");
  await runCommand("npx", [
    "-p",
    "typescript",
    "tsc",
    "--noEmit",
    "--strict",
    "--target",
    "esnext",
    "--moduleResolution",
    "node",
    "--lib",
    "esnext,dom",
    "--skipLibCheck",
    "--allowImportingTsExtensions",
    "--ignoreConfig",
    "--ignoreDeprecations",
    "6.0",
    "crates/fresh-editor/plugins/azure_ai_chat.ts",
  ]);
  console.log("PASS plugin TypeScript check");

  console.log("3. Fresh runtime plugin bundling check");
  await runCommand("npx", [
    "@fresh-editor/fresh-editor",
    "--check-plugin",
    "crates/fresh-editor/plugins/azure_ai_chat.ts",
  ]);
  console.log("PASS runtime plugin bundling check");

  console.log("4. Fresh runtime behavior check");
  await runCommand("node", ["scripts/fresh-ai-plugin-behavior.test.mjs"]);
  console.log("PASS runtime behavior check");

  console.log("5. Live proxy health check");
  const env = {
    ...process.env,
    AZURE_OPENAI_ENDPOINT: "https://example.openai.azure.com",
    AZURE_OPENAI_DEPLOYMENT: "gpt-4o",
    AZURE_OPENAI_API_KEY: "secret",
    AZURE_OPENAI_API_VERSION: "2024-10-21",
    FRESH_AI_PROXY_PORT: "8799",
  };
  const server = await startProxyServer({ env, logger: { log() {}, error() {} } });
  try {
    const response = await fetch("http://127.0.0.1:8799/health");
    const body = await response.json();
    if (response.status !== 200 || body.deployment !== "gpt-4o") {
      throw new Error(`Unexpected proxy health response: ${response.status} ${JSON.stringify(body)}`);
    }
  } finally {
    await new Promise((resolve, reject) => server.close((err) => err ? reject(err) : resolve()));
  }
  console.log("PASS live proxy health check");

  console.log("Fresh AI verification passed.");
}

main().catch((error) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
