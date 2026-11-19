// init.ts for the landing page's Orchestrator scene (kitty_orchestrator.py).
//
// Builds the dock the loop walks through: six workspaces, four filed into two
// folders, each showing something different — an agent at work, a Review Diff,
// an agent waiting on a permission prompt, another agent at work, the main
// checkout with the file explorer beside a Rust file, and API docs in
// markdown compose mode. The agents are setup.sh's fakes (staged output).
// Writes $AD_SCENE_READY once every workspace is filled.

const CACHE_RS = `use std::collections::HashMap;

#[derive(Default)]
pub struct Cache {
    items: HashMap<String, Entry>,
    order: Vec<String>,
    capacity: usize,
    hits: u64,
}

impl Cache {
    pub fn with_capacity(n: usize) -> Self {
        let items = HashMap::with_capacity(n);
        Self { items, order: Vec::with_capacity(n), capacity: n, hits: 0 }
    }

    pub fn get(&mut self, key: &str) -> Option<&Entry> {
        if self.items.contains_key(key) {
            self.hits += 1;
            self.touch(key);
        }
        self.items.get(key)
    }

    /// Evict the least recently used entry once the cache is full.
    pub fn insert(&mut self, key: String, entry: Entry) {
        if self.items.len() >= self.capacity {
            let oldest = self.order.remove(0);
            self.items.remove(&oldest);
        }
        self.order.push(key.clone());
        self.items.insert(key, entry);
    }

    fn touch(&mut self, key: &str) {
        self.order.retain(|k| k != key);
        self.order.push(key.to_string());
    }
}
`;

const API_MD = `# tiny-server API

Every endpoint speaks JSON and answers within the cache's deadline. Requests
that miss the cache fall through to the store and warm it on the way back.

## Authentication

Send a bearer token in the \`Authorization\` header. Tokens expire after **15
minutes**; refresh them with \`POST /auth/refresh\` before they lapse.

## Endpoints

### \`GET /users/{id}\`

Returns one user. Cached for 60 seconds per id, so a rename can take a minute
to show up everywhere.

### \`POST /orders\`

Creates an order and returns it with status \`pending\`. Rate limited to
**20 requests a minute** per token; past that the server answers \`429\` with a
\`Retry-After\` header.

> Retries are safe: send the same \`Idempotency-Key\` and the first order wins.

## Errors

- \`401\`: the token is missing, malformed or expired
- \`404\`: no such resource
- \`429\`: slow down, and see \`Retry-After\`
`;

editor.on("plugins_loaded", async () => {
  const orch = editor.getPluginApi("orchestrator");
  if (!orch) return;
  const main = orch.listWorkspaces().find((w) => w.active);
  if (!main) return;
  orch.renameWorkspace(main.workspaceId, "acme-api");
  const perf = orch.createFolder("Performance");
  const sec = orch.createFolder("Security");

  const make = async (name: string, agent: string, folder: string | null) => {
    const ws = await orch.newWorkspace({ name, agent, newBranch: name, teach: false });
    if (folder) orch.moveWorkspace(ws.workspaceId, folder);
    return ws;
  };
  await make("rate-limiter", "claude", perf);
  const pool = await make("db-pool-tuning", "claude", perf);
  await make("auth-bypass", "opencode --ask", sec);
  await make("token-expiry", "opencode", sec);
  const docs = await make("api-docs", "codex", null);

  // db-pool-tuning: its change under review
  // (replaceFile: writeFile only makes new files.) Review Diff finds its repo
  // from the active buffer, so the worktree's own file is opened first.
  editor.replaceFile(pool.root + "/src/cache.rs", CACHE_RS);
  await orch.focusWorkspace(pool.workspaceId);
  await editor.delay(800);
  editor.openFile(pool.root + "/src/cache.rs");
  await editor.delay(500);
  await editor.runCommand("Review Diff");
  await editor.delay(1500);

  // api-docs: the docs it is writing, in compose mode
  editor.writeFile(docs.root + "/API.md", API_MD);
  await orch.focusWorkspace(docs.workspaceId);
  await editor.delay(800);
  editor.openFile(docs.root + "/API.md");
  await editor.delay(800);
  await editor.runCommand("Markdown: Toggle Compose/Preview");
  await editor.delay(800);

  // acme-api, the main checkout: a Rust file beside the file explorer
  await orch.focusWorkspace(main.workspaceId);
  await editor.delay(800);
  editor.openFile(editor.getCwd() + "/src/server.rs");
  await editor.delay(500);
  await editor.runCommand("Toggle File Explorer");
  await editor.delay(1500);
  editor.setStatus("scene ready");
  const marker = editor.getEnv("AD_SCENE_READY");
  if (marker) editor.writeFile(marker, "ready\n");
});
