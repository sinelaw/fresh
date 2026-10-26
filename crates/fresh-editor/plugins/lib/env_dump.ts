/**
 * Capturing a shell's environment as data, shared by the plugins that probe
 * a container's or pod's login env (devcontainer userEnvProbe, k8s).
 */

/** The line `ENV_DUMP` prints before the env records. Whatever the login
 *  shell's rc files wrote to stdout comes before it and is dropped, so their
 *  output is never read as, or merged into, a variable. */
const ENV_DUMP_MARKER = "__FRESH_ENV_DUMP_5d0b7a__";

/** Shell command that prints the marker line, then the environment.
 *  `env -0` ends each `KEY=VALUE` record with NUL instead of a newline, so a
 *  multi-line value (an exported Bash function) is not cut short and its
 *  continuation lines are not read as variables of their own (#3586). An
 *  `env` without `-0` falls back to one record per line. */
export const ENV_DUMP = `printf '\\n%s\\n' ${ENV_DUMP_MARKER}; env -0 2>/dev/null || env`;

/** Parse an `ENV_DUMP` into `[key, value]` pairs: everything up to the
 *  marker line is dropped (output with no marker is read whole), then the
 *  records are NUL-separated when present, else lines. */
export function parseEnvDump(stdout: string): Array<[string, string]> {
  let dump = stdout;
  const at = dump.indexOf(ENV_DUMP_MARKER);
  if (at >= 0) {
    dump = dump.slice(at + ENV_DUMP_MARKER.length);
    if (dump.startsWith("\n")) dump = dump.slice(1);
  }
  const records = dump.includes("\0") ? dump.split("\0") : dump.split("\n");
  const out: Array<[string, string]> = [];
  for (const record of records) {
    const eq = record.indexOf("=");
    if (eq > 0) out.push([record.slice(0, eq), record.slice(eq + 1)]);
  }
  return out;
}
