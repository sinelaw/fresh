import { createServer } from "node:http";
import { existsSync, readFileSync } from "node:fs";
import path from "node:path";
import { Readable } from "node:stream";
import { pathToFileURL } from "node:url";

const DEFAULT_PORT = 8787;
const DEFAULT_API_VERSION = "2024-10-21";
const DEFAULT_ENV_FILES = [".env.fresh-ai", ".env"];

function trim(value) {
  return typeof value === "string" ? value.trim() : "";
}

export function parseEnvFile(content) {
  const parsed = {};

  for (const rawLine of content.replace(/^\uFEFF/, "").split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith("#")) {
      continue;
    }

    const normalized = line.startsWith("export ") ? line.slice(7).trim() : line;
    const separator = normalized.indexOf("=");
    if (separator <= 0) {
      continue;
    }

    const key = normalized.slice(0, separator).trim();
    let value = normalized.slice(separator + 1).trim();
    if (!key) {
      continue;
    }

    const quote = value[0];
    if ((quote === '"' || quote === "'") && value.endsWith(quote)) {
      value = value.slice(1, -1);
    }

    parsed[key] = value;
  }

  return parsed;
}

function resolveEnvFilePath(filePath, cwd) {
  return path.isAbsolute(filePath) ? filePath : path.join(cwd, filePath);
}

export function loadProxyEnv({ env = process.env, cwd = process.cwd(), fileExists = existsSync, readFile = readFileSync } = {}) {
  const explicitEnvFile = trim(env.FRESH_AI_PROXY_ENV_FILE);
  const candidates = explicitEnvFile
    ? [resolveEnvFilePath(explicitEnvFile, cwd)]
    : DEFAULT_ENV_FILES.map((fileName) => path.join(cwd, fileName));

  for (const candidate of candidates) {
    if (!fileExists(candidate)) {
      continue;
    }

    const fileValues = parseEnvFile(readFile(candidate, "utf8"));
    return {
      ...fileValues,
      ...env,
    };
  }

  return { ...env };
}

export function loadProxyConfig(env = process.env) {
  const endpoint = trim(
    env.AZURE_OPENAI_ENDPOINT ?? env.AZURE_AI_FOUNDRY_ENDPOINT,
  );
  const deployment = trim(
    env.AZURE_OPENAI_DEPLOYMENT ?? env.AZURE_AI_FOUNDRY_DEPLOYMENT,
  );
  const apiVersion = trim(env.AZURE_OPENAI_API_VERSION) || DEFAULT_API_VERSION;
  const apiKey = trim(env.AZURE_OPENAI_API_KEY);
  const bearerToken = trim(env.AZURE_OPENAI_BEARER_TOKEN);
  const port = Number.parseInt(trim(env.FRESH_AI_PROXY_PORT), 10) || DEFAULT_PORT;

  if (!endpoint) {
    throw new Error("Missing AZURE_OPENAI_ENDPOINT (or AZURE_AI_FOUNDRY_ENDPOINT)");
  }
  if (!deployment) {
    throw new Error("Missing AZURE_OPENAI_DEPLOYMENT (or AZURE_AI_FOUNDRY_DEPLOYMENT)");
  }
  if (/^https?:\/\//i.test(deployment)) {
    throw new Error("AZURE_OPENAI_DEPLOYMENT must be a deployment name, not a URL");
  }
  if (!apiKey && !bearerToken) {
    throw new Error("Missing AZURE_OPENAI_API_KEY or AZURE_OPENAI_BEARER_TOKEN");
  }

  return { endpoint, deployment, apiVersion, apiKey, bearerToken, port };
}

export function buildAzureChatCompletionsUrl(config) {
  const url = new URL(
    `/openai/deployments/${encodeURIComponent(config.deployment)}/chat/completions`,
    config.endpoint.endsWith("/") ? config.endpoint : `${config.endpoint}/`,
  );
  url.searchParams.set("api-version", config.apiVersion);
  return url.toString();
}

export function buildAzureHeaders(config, incomingHeaders = {}) {
  const headers = {
    "content-type": "application/json",
  };

  const userAgent = incomingHeaders["user-agent"] ?? incomingHeaders["User-Agent"];
  if (userAgent) {
    headers["user-agent"] = userAgent;
  }

  if (config.apiKey) {
    headers["api-key"] = config.apiKey;
  } else {
    headers.authorization = `Bearer ${config.bearerToken}`;
  }

  return headers;
}

export function toAzureChatRequest(body) {
  if (!body || typeof body !== "object") {
    throw new Error("Request body must be a JSON object");
  }
  if (!Array.isArray(body.messages) || body.messages.length === 0) {
    throw new Error("Request body must include a non-empty messages array");
  }

  const forwarded = { ...body };
  delete forwarded.model;
  return forwarded;
}

async function readJsonBody(request) {
  const chunks = [];
  for await (const chunk of request) {
    chunks.push(chunk);
  }

  const raw = Buffer.concat(chunks).toString("utf8");
  if (!raw) {
    throw new Error("Request body is empty");
  }

  try {
    return JSON.parse(raw);
  } catch {
    throw new Error("Request body must be valid JSON");
  }
}

function sendJson(response, statusCode, payload) {
  response.writeHead(statusCode, { "content-type": "application/json" });
  response.end(JSON.stringify(payload, null, 2));
}

export async function forwardChatCompletions({ requestBody, env = process.env, fetchImpl = fetch, incomingHeaders = {} }) {
  const config = loadProxyConfig(env);
  const azureBody = toAzureChatRequest(requestBody);
  const azureResponse = await fetchImpl(buildAzureChatCompletionsUrl(config), {
    method: "POST",
    headers: buildAzureHeaders(config, incomingHeaders),
    body: JSON.stringify(azureBody),
  });

  return { azureResponse, config };
}

export function createProxyServer({ env = process.env, fetchImpl = fetch, logger = console } = {}) {
  return createServer(async (request, response) => {
    try {
      if (request.method === "GET" && request.url === "/health") {
        const config = loadProxyConfig(env);
        sendJson(response, 200, {
          ok: true,
          endpoint: config.endpoint,
          deployment: config.deployment,
          apiVersion: config.apiVersion,
        });
        return;
      }

      if (request.method === "POST" && request.url === "/v1/chat/completions") {
        const requestBody = await readJsonBody(request);
        const { azureResponse } = await forwardChatCompletions({
          requestBody,
          env,
          fetchImpl,
          incomingHeaders: request.headers,
        });

        const contentType = azureResponse.headers.get("content-type") || "application/json";
        response.writeHead(azureResponse.status, { "content-type": contentType });

        if (azureResponse.body) {
          await Readable.fromWeb(azureResponse.body).pipe(response);
        } else {
          response.end();
        }
        return;
      }

      sendJson(response, 404, {
        error: {
          message: "Not found",
          type: "invalid_request_error",
        },
      });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      logger.error?.(`[fresh-ai-proxy] ${message}`);
      const statusCode = message.startsWith("Missing ") || message.includes("must") ? 400 : 500;
      sendJson(response, statusCode, {
        error: {
          message,
          type: "proxy_error",
        },
      });
    }
  });
}

export function isDirectCliExecution(importMetaUrl, argv1 = process.argv[1]) {
  if (!argv1) {
    return false;
  }

  return importMetaUrl === pathToFileURL(path.resolve(argv1)).href;
}

export async function startProxyServer({ env = process.env, fetchImpl = fetch, logger = console } = {}) {
  const config = loadProxyConfig(env);
  const server = createProxyServer({ env, fetchImpl, logger });

  await new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(config.port, "127.0.0.1", resolve);
  });

  logger.log?.(`[fresh-ai-proxy] listening on http://127.0.0.1:${config.port}`);
  logger.log?.(`[fresh-ai-proxy] forwarding to ${buildAzureChatCompletionsUrl(config)}`);
  return server;
}

if (isDirectCliExecution(import.meta.url)) {
  const env = loadProxyEnv();
  startProxyServer({ env }).catch((error) => {
    const message = error instanceof Error ? error.message : String(error);
    console.error(`[fresh-ai-proxy] failed to start: ${message}`);
    process.exitCode = 1;
  });
}
