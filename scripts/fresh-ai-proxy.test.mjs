import assert from "node:assert/strict";
import { once } from "node:events";

import {
  buildAzureChatCompletionsUrl,
  buildAzureHeaders,
  createProxyServer,
  isDirectCliExecution,
  loadProxyConfig,
  loadProxyEnv,
  parseEnvFile,
  toAzureChatRequest,
} from "./fresh-ai-proxy.mjs";

async function run(name, fn) {
  try {
    await fn();
    console.log(`PASS ${name}`);
  } catch (error) {
    console.error(`FAIL ${name}`);
    throw error;
  }
}

await run("parseEnvFile reads Azure-style key value pairs", () => {
  assert.deepEqual(
    parseEnvFile(`\uFEFF# comment\nAZURE_OPENAI_ENDPOINT=https://example.openai.azure.com\nexport AZURE_OPENAI_DEPLOYMENT=gpt-4o\nAZURE_OPENAI_API_KEY=\"secret\"\nFRESH_AI_PROXY_PORT='8788'\n`),
    {
      AZURE_OPENAI_ENDPOINT: "https://example.openai.azure.com",
      AZURE_OPENAI_DEPLOYMENT: "gpt-4o",
      AZURE_OPENAI_API_KEY: "secret",
      FRESH_AI_PROXY_PORT: "8788",
    },
  );
});

await run("loadProxyEnv loads .env.fresh-ai values and lets process env override them", () => {
  const seenPaths = [];
  const env = loadProxyEnv({
    cwd: "C:\\repo",
    env: {
      AZURE_OPENAI_API_KEY: "override",
    },
    fileExists(filePath) {
      seenPaths.push(filePath);
      return filePath.endsWith(".env.fresh-ai");
    },
    readFile() {
      return `AZURE_OPENAI_ENDPOINT=https://example.openai.azure.com\nAZURE_OPENAI_DEPLOYMENT=gpt-4o\nAZURE_OPENAI_API_KEY=secret\n`;
    },
  });

  assert.equal(seenPaths.some((filePath) => filePath.endsWith(".env.fresh-ai")), true);
  assert.equal(env.AZURE_OPENAI_ENDPOINT, "https://example.openai.azure.com");
  assert.equal(env.AZURE_OPENAI_DEPLOYMENT, "gpt-4o");
  assert.equal(env.AZURE_OPENAI_API_KEY, "override");
});

await run("isDirectCliExecution recognizes Windows file entry paths", () => {
  assert.equal(
    isDirectCliExecution(
      "file:///C:/Users/ishea/fresh/scripts/fresh-ai-proxy.mjs",
      "C:\\Users\\ishea\\fresh\\scripts\\fresh-ai-proxy.mjs",
    ),
    true,
  );
  assert.equal(
    isDirectCliExecution(
      "file:///C:/Users/ishea/fresh/scripts/fresh-ai-proxy.mjs",
      "C:\\Users\\ishea\\fresh\\scripts\\other.mjs",
    ),
    false,
  );
});

await run("loadProxyConfig requires endpoint deployment and auth", () => {
  assert.throws(() => loadProxyConfig({}), /Missing AZURE_OPENAI_ENDPOINT/);
  assert.throws(
    () => loadProxyConfig({ AZURE_OPENAI_ENDPOINT: "https://example.openai.azure.com" }),
    /Missing AZURE_OPENAI_DEPLOYMENT/,
  );
  assert.throws(
    () => loadProxyConfig({
      AZURE_OPENAI_ENDPOINT: "https://example.openai.azure.com",
      AZURE_OPENAI_DEPLOYMENT: "gpt-4o",
    }),
    /Missing AZURE_OPENAI_API_KEY or AZURE_OPENAI_BEARER_TOKEN/,
  );
  assert.throws(
    () => loadProxyConfig({
      AZURE_OPENAI_ENDPOINT: "https://example.openai.azure.com",
      AZURE_OPENAI_DEPLOYMENT: "https://example.services.ai.azure.com/api/projects/demo",
      AZURE_OPENAI_API_KEY: "secret",
    }),
    /must be a deployment name, not a URL/,
  );
});

await run("buildAzureChatCompletionsUrl uses deployment path and api-version", () => {
  const url = buildAzureChatCompletionsUrl({
    endpoint: "https://example.openai.azure.com",
    deployment: "deepseek-v4-pro",
    apiVersion: "2024-10-21",
  });

  assert.equal(
    url,
    "https://example.openai.azure.com/openai/deployments/deepseek-v4-pro/chat/completions?api-version=2024-10-21",
  );
});

await run("buildAzureHeaders prefers api-key and otherwise uses bearer auth", () => {
  assert.deepEqual(
    buildAzureHeaders({ apiKey: "abc", bearerToken: "", endpoint: "x", deployment: "y", apiVersion: "z" }),
    {
      "content-type": "application/json",
      "api-key": "abc",
    },
  );

  assert.deepEqual(
    buildAzureHeaders({ apiKey: "", bearerToken: "token", endpoint: "x", deployment: "y", apiVersion: "z" }),
    {
      "content-type": "application/json",
      authorization: "Bearer token",
    },
  );
});

await run("toAzureChatRequest strips model while preserving messages", () => {
  const request = toAzureChatRequest({
    model: "azure-proxy",
    messages: [{ role: "user", content: "hello" }],
    temperature: 0.2,
  });

  assert.deepEqual(request, {
    messages: [{ role: "user", content: "hello" }],
    temperature: 0.2,
  });
});

await run("proxy server forwards OpenAI-style chat requests to Azure format", async () => {
  let forwarded = null;
  const server = createProxyServer({
    env: {
      AZURE_OPENAI_ENDPOINT: "https://example.openai.azure.com",
      AZURE_OPENAI_DEPLOYMENT: "gpt-4o",
      AZURE_OPENAI_API_KEY: "secret",
      AZURE_OPENAI_API_VERSION: "2024-10-21",
    },
    fetchImpl: async (url, init) => {
      forwarded = { url, init };
      return new Response(JSON.stringify({ choices: [{ message: { content: "ok" } }] }), {
        status: 200,
        headers: { "content-type": "application/json" },
      });
    },
    logger: { error() {}, log() {} },
  });

  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const address = server.address();
  const baseUrl = `http://127.0.0.1:${address.port}`;

  const response = await fetch(`${baseUrl}/v1/chat/completions`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      model: "azure-proxy",
      messages: [{ role: "user", content: "hello" }],
      temperature: 0.2,
    }),
  });

  assert.equal(response.status, 200);
  assert.deepEqual(await response.json(), { choices: [{ message: { content: "ok" } }] });
  assert.equal(
    forwarded.url,
    "https://example.openai.azure.com/openai/deployments/gpt-4o/chat/completions?api-version=2024-10-21",
  );
  assert.equal(forwarded.init.method, "POST");
  assert.equal(forwarded.init.headers["api-key"], "secret");
  assert.deepEqual(JSON.parse(forwarded.init.body), {
    messages: [{ role: "user", content: "hello" }],
    temperature: 0.2,
  });

  await new Promise((resolve, reject) => server.close((error) => error ? reject(error) : resolve()));
});

console.log("All proxy checks passed.");
