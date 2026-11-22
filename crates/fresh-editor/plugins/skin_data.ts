/// <reference path="./lib/fresh.d.ts" />

/**
 * Data for web UI skins. Publishes a fixed set of named values every two
 * seconds with `editor.setSkinValue`; a skin shows them in its furniture with
 * `data-bind="skin_data:<name>"` (text) or `data-bind-var="--v:skin_data:<name>"`
 * (a CSS custom property, unitless for numbers). Skins never bring code of
 * their own: this plugin is what they read. The values (see
 * docs/features/web-ui-skins.md, "Live values"):
 *
 *   cpu, mem         percent busy / in use, 0–100
 *   memText          "used / total G"
 *   load             1, 5 and 15-minute load averages
 *   uptime           "3h 12m"
 *   errors, warnings diagnostics counts across open files
 *   buffers          open files
 *   time             "HH:MM"
 *
 * TODO(before merge): the system figures (cpu, mem, memText, load, uptime)
 * read /proc and so only exist on Linux. They must work on macOS and Windows
 * too — likely a host-side source (e.g. the sysinfo crate) exposed to plugins
 * — before this ships; elsewhere those values are simply absent today and a
 * skin shows its placeholder.
 *
 * Publishing an unchanged value costs no redraw, so the tick is cheap even in
 * the terminal, where nothing reads these.
 */
const editor = getEditor();

let lastCpu: { busy: number; total: number } | null = null;

function readProc(name: string): string | null {
  try {
    return editor.readFile(`/proc/${name}`);
  } catch {
    return null;
  }
}

function cpuPercent(): string | null {
  const stat = readProc("stat");
  if (!stat) return null;
  const line = stat.split("\n").find((l) => l.startsWith("cpu "));
  if (!line) return null;
  const f = line.trim().split(/\s+/).slice(1).map(Number);
  const idle = (f[3] ?? 0) + (f[4] ?? 0);
  const total = f.reduce((a, b) => a + b, 0);
  const busy = total - idle;
  const prev = lastCpu;
  lastCpu = { busy, total };
  if (!prev || total <= prev.total) return null;
  return String(Math.round(((busy - prev.busy) / (total - prev.total)) * 100));
}

function memory(): { pct: string; text: string } | null {
  const info = readProc("meminfo");
  if (!info) return null;
  const kb = (key: string): number => {
    const m = new RegExp(`^${key}:\\s+(\\d+)`, "m").exec(info);
    return m ? Number(m[1]) : 0;
  };
  const total = kb("MemTotal");
  if (!total) return null;
  const used = total - kb("MemAvailable");
  const gib = (k: number): string => (k / 1048576).toFixed(1);
  return { pct: String(Math.round((used / total) * 100)), text: `${gib(used)} / ${gib(total)} G` };
}

function uptime(): string | null {
  const up = readProc("uptime");
  if (!up) return null;
  const s = Math.floor(Number(up.split(" ")[0]));
  const d = Math.floor(s / 86400);
  const h = Math.floor((s % 86400) / 3600);
  const m = Math.floor((s % 3600) / 60);
  return d ? `${d}d ${h}h` : h ? `${h}h ${m}m` : `${m}m`;
}

function publish(name: string, value: string | null): void {
  if (value !== null) editor.setSkinValue(name, value);
}

function tick(): void {
  publish("cpu", cpuPercent());
  const mem = memory();
  publish("mem", mem ? mem.pct : null);
  publish("memText", mem ? mem.text : null);
  const load = readProc("loadavg");
  publish("load", load ? load.split(" ").slice(0, 3).join("  ") : null);
  publish("uptime", uptime());

  const diags = editor.getAllDiagnostics();
  publish("errors", String(diags.filter((d) => d.severity === 1).length));
  publish("warnings", String(diags.filter((d) => d.severity === 2).length));
  publish("buffers", String(editor.listBuffers().filter((b) => !b.is_virtual).length));
  const now = new Date();
  publish("time", `${String(now.getHours()).padStart(2, "0")}:${String(now.getMinutes()).padStart(2, "0")}`);
}

registerHandler("skinDataTick", tick);
editor.setInterval(2000, "skinDataTick");
tick();
