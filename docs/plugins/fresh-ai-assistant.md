# Fresh AI Assistant

Fresh AI Assistant is a Fresh plugin plus a local OpenAI-compatible proxy that lets Fresh send standard chat-completion requests to Azure OpenAI or Azure AI Foundry.

## Architecture

```text
Fresh Editor
  -> Fresh AI plugin
  -> http://localhost:8787/v1/chat/completions
  -> local proxy
  -> Azure OpenAI / Azure AI Foundry
```

The plugin never calls Azure directly. It talks to the local proxy using an OpenAI-style request. The proxy rewrites that request into Azure's deployment-based `/openai/deployments/{deployment}/chat/completions?api-version=...` shape.

## Files

- Plugin: `crates/fresh-editor/plugins/azure_ai_chat.ts`
- Proxy: `scripts/fresh-ai-proxy.mjs`
- Proxy checks: `scripts/fresh-ai-proxy.test.mjs`
- Local env template: `.env.fresh-ai.example`
- Local env file: `.env.fresh-ai` (gitignored)

## Azure Environment Variables

You can either export variables in your shell or fill in `.env.fresh-ai` at the repo root. The proxy now auto-loads `.env.fresh-ai` first, then `.env`, unless `FRESH_AI_PROXY_ENV_FILE` points somewhere else.

Recommended local file:

```dotenv
AZURE_OPENAI_ENDPOINT=https://<resource>.openai.azure.com
AZURE_OPENAI_DEPLOYMENT=<deployment-name>
AZURE_OPENAI_API_KEY=<api-key>
AZURE_OPENAI_API_VERSION=2024-10-21
```

Bearer-token auth is also supported:

```dotenv
AZURE_OPENAI_BEARER_TOKEN=<entra-token>
```

Optional:

```dotenv
FRESH_AI_PROXY_PORT=8787
```

Shell-based setup still works:

```powershell
$env:AZURE_OPENAI_ENDPOINT = "https://<resource>.openai.azure.com"
$env:AZURE_OPENAI_DEPLOYMENT = "<deployment-name>"
$env:AZURE_OPENAI_API_KEY = "<api-key>"
$env:AZURE_OPENAI_API_VERSION = "2024-10-21"
```

## Start The Proxy

From the repo root:

```bash
npm run fresh-ai-proxy
```

Health check:

```bash
curl http://127.0.0.1:8787/health
```

## Fresh Plugin Settings

The plugin registers these settings:

- `baseUrl` default: `http://localhost:8787/v1`
- `model` default: `azure-proxy`
- `apiKey` optional proxy auth token
- `systemPrompt`
- `proxyScriptPath` default: `scripts/fresh-ai-proxy.mjs`
- `temperature`
- `maxTokens`

For the local Azure proxy flow, leave `baseUrl` pointed at localhost.

## Fresh Commands

The plugin exposes these command-palette entries:

- `Fresh AI: Open Chat`
- `Fresh AI: Start Local Proxy`
- `Fresh AI: Stop Local Proxy`
- `Fresh AI: Explain Selection`
- `Fresh AI: Refactor Selection`
- `Fresh AI: Generate Tests for Selection`
- `Fresh AI: Fix Selection or Error`
- `Fresh AI: Ask About Current File`
- `Fresh AI: Replace Selection`

## First-Version Workflow

1. Fill in `.env.fresh-ai` or set the Azure variables in the shell Fresh inherits from.
2. Start Fresh in this repo.
3. Run `Fresh AI: Start Local Proxy` or start it externally with `npm run fresh-ai-proxy`.
4. Open a source file.
5. Select code for selection-based commands.
6. Run one of the Fresh AI commands from the command palette.
7. Review the result in either:
   - a new AI output buffer, or
   - the current buffer for `Replace Selection`.

## End-to-End Verification Checklist

Use this checklist when validating the full workflow in a real Fresh session:

1. `npm run test:fresh-ai-proxy` passes.
2. `.env.fresh-ai` contains a valid endpoint, deployment, and either an API key or bearer token.
3. `curl http://127.0.0.1:8787/health` returns `200` with endpoint, deployment, and apiVersion.
4. Fresh starts with `azure_ai_chat.ts` loaded.
5. `Fresh AI: Open Chat` opens the panel.
6. `Fresh AI: Explain Selection` produces a new output buffer from selected code.
7. `Fresh AI: Generate Tests for Selection` produces test code in a new output buffer.
8. `Fresh AI: Ask About Current File` answers a prompt about the active file.
9. `Fresh AI: Replace Selection` rewrites the selected code in-place.
10. Requests succeed against real Azure credentials through the local proxy rather than a direct Azure call from the plugin.

## Current Verification In This Repo

These checks are executable in the current repo state:

```bash
npm run test:fresh-ai-proxy
npm run test:fresh-ai
```

`npm run test:fresh-ai` covers:

- proxy translation checks
- env-file loading for the proxy
- targeted TypeScript validation for `azure_ai_chat.ts`
- Fresh runtime bundling through `fresh --check-plugin`
- runtime behavior checks for key plugin flows
- live proxy `/health` verification

The repo-wide plugin tsconfig still reports unrelated pre-existing errors in other plugin files, so the targeted plugin check is the meaningful signal for the Fresh AI plugin itself.
