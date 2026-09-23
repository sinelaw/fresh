/**
 * tmux format-output parsing, kept free of editor calls so tests can import it.
 * tmux escapes its format output: a control byte becomes `\nnn` octal and a
 * backslash is doubled, so fields cannot be recovered with a substring split.
 */

/** The field separator byte; tmux prints it as `\037`. */
const SEP_BYTE = 0x1f;
const SEP = "\x1f";

export const SESSION_FORMAT =
  `#{session_id}${SEP}#{session_name}${SEP}#{session_created}${SEP}` +
  `#{session_attached}${SEP}#{session_path}`;
export const PANE_FORMAT =
  `#{session_id}${SEP}#{window_index}${SEP}#{window_name}${SEP}#{pane_id}${SEP}` +
  `#{pane_pid}${SEP}#{pane_current_path}${SEP}#{pane_current_command}${SEP}#{pane_title}`;

/** Foreground commands worth reporting as a probable agent. */
export const AGENT_COMMANDS = new Set([
  "claude",
  "codex",
  "gemini",
  "goose",
  "aider",
  "cursor-agent",
  "copilot",
  "qwen",
  "droid",
  "opencode",
  "amp",
]);

function decodeOctal(text: string): number | null {
  if (text.length !== 3) return null;
  let value = 0;
  for (const ch of text) {
    if (ch < "0" || ch > "7") return null;
    value = value * 8 + (ch.charCodeAt(0) - 48);
  }
  return value <= 0xff ? value : null;
}

/** Split one line of tmux format output into fields, undoing the escaping.
 *  Escapes are consumed atomically; an escape tmux would never produce passes through. */
export function splitEscapedFields(line: string): string[] {
  const fields: string[] = [];
  let current = "";
  let i = 0;

  while (i < line.length) {
    if (line[i] !== "\\") {
      current += line[i];
      i += 1;
      continue;
    }
    const next = line[i + 1];
    if (next === "\\") {
      current += "\\";
      i += 2;
      continue;
    }
    if (next !== undefined && next >= "0" && next <= "9") {
      const value = decodeOctal(line.slice(i + 1, i + 4));
      if (value !== null) {
        if (value === SEP_BYTE) {
          fields.push(current);
          current = "";
        } else {
          current += String.fromCharCode(value);
        }
        i += 4;
        continue;
      }
    }
    current += line[i];
    i += 1;
  }
  fields.push(current);
  return fields;
}

