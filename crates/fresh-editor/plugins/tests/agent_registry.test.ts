/** The agent registry: which command resolves to which agent, and which commands already name a session. */
import { AGENT_REGISTRY, agentEntryForBase, namedSession } from "../lib/agent_registry.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

eq(AGENT_REGISTRY.map((e) => e.id), ["claude", "codex", "opencode", "aider"], "registry order");
for (const e of AGENT_REGISTRY) eq(agentEntryForBase(e.id)?.id, e.id, `${e.id} matches its own id`);
eq(agentEntryForBase("bash"), null, "unknown command matches nothing");

const claude = agentEntryForBase("claude")!.spec.provision!;
const named = (argv: string[]) => namedSession(argv, claude);
const ID = "07297126-25ab-45a6-9fd2-5eb5e460298b";

eq(named(["claude"]), null, "bare command names no session");
eq(named(["claude", "--model", "opus"]), null, "unrelated flags name no session");
eq(named(["claude", "--resume", ID]), { id: ID }, "--resume <uuid>");
eq(named(["claude", `--resume=${ID}`]), { id: ID }, "--resume=<uuid>");
eq(named(["claude", "-r", ID]), { id: ID }, "-r <uuid>");
eq(named(["claude", "--session-id", ID]), { id: ID }, "--session-id <uuid>");
eq(named(["claude", "--resume"]), {}, "bare --resume (picker) names no id");
eq(named(["claude", "--resume", "fix the build"]), {}, "--resume <search term> names no id");
eq(named(["claude", "-c"]), {}, "-c");
eq(named(["claude", "--continue"]), {}, "--continue");

console.log(failures === 0 ? "\nAll agent registry tests passed." : `\n${failures} failed.`);
if (failures > 0) process.exit(1);
