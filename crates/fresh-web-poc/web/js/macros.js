// Keyboard macros, as the editor's: recorded into registers 0–9 (Record
// Macro… asks which), stopped with F5, played back with F4 (the last one)
// or Play Macro… (asks which); List Macros shows them. A macro is the
// commands run (by key, menu or palette) and the text typed or pasted into
// the editor while recording, played back in order. Commands that manage
// macros, and the palette's own opening (the command it runs is recorded),
// are left out; what is typed into a prompt or a panel is not recorded.
// Registers last as long as the page.

import { asCommand } from "./commands.js";

const REGISTERS = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

// A macro's step: a command run (with its argument, if any), or text
// typed (a run of keys, one step) or pasted.
/** type Step = {kind: "command", command: String, arg: String | Undefined} | {kind: "text", text: String, typed: Boolean} */
// A macro being recorded: its register and its steps so far.
/** type Recording = {register: String, steps: Step[]} */

/** function commandStep(command: String, arg: String | Undefined) => Step */
function commandStep(command, arg) { return { kind: "command", command, arg }; }
/** function textStep(text: String, typed: Boolean) => Step */
function textStep(text, typed) { return { kind: "text", text, typed }; }

// `steps` and the text `text` (typed, or pasted): typing after typing
// goes on the same step.
/** function withText(steps: Step[], text: String, typed: Boolean) => Step[] */
function withText(steps, text, typed) {
  const last = steps.at(-1);
  if (typed && last !== undefined && last.kind === "text" && last.typed) return [...steps.slice(0, -1), textStep(last.text + text, true)];
  return [...steps, textStep(text, typed)];
}

// A step as List Macros shows it (`titleOf` a command's title).
/** function describeStep(s: Step, titleOf: (String) => String) => String */
function describeStep(s, titleOf) {
  if (s.kind === "text") return `  ${s.typed ? "type" : "paste"} ${JSON.stringify(s.text)}`;
  const arg = s.arg;
  return `  ${titleOf(s.command)}${arg !== undefined ? ` ${JSON.stringify(arg)}` : ""}`;
}

/** function plural(n: Number, word: String) => String */
function plural(n, word) { return `${n} ${word}${n === 1 ? "" : "s"}`; }

// Empty state, typed.
/** function noMacros() => Map<String, Step[]> */
function noMacros() { return new Map(); }
/** function noRecording() => Recording[] */
function noRecording() { return []; }
/** function noRegister() => String[] */
function noRegister() { return []; }

export class Macros {
  constructor(app) {
    this.app = app;
    this.registers = noMacros(); // register -> its steps
    this.recording = noRecording(); // the macro being recorded: a list of at most one
    this.playing = false;
    this.last = noRegister(); // the register last recorded or played: at most one
    this.depth = 0; // commands running inside a command are its own doing
  }

  // A step as it happens; recorded while recording (and not playing).
  command(id, arg) {
    if (this.playing || this.depth > 0 || id.startsWith("macro.") || id.startsWith("palette.")) return;
    this.recording = this.recording.map((r) => ({ register: r.register, steps: [...r.steps, commandStep(id, arg)] }));
  }
  text(text, typed) {
    if (this.playing) return;
    this.recording = this.recording.map((r) => ({ register: r.register, steps: withText(r.steps, text, typed) }));
  }

  start(register) {
    this.stop();
    this.recording = [{ register, steps: [] }];
    this.app().setStatus(`Recording macro ${register}… (${this.app().keyLabel("macro.stop") || "Stop Recording Macro"} stops)`);
    this.app().updateStatus();
  }
  stop() {
    const done = this.recording;
    if (!done.length) { this.app().setStatus("Not recording a macro"); return; }
    this.recording = [];
    for (const r of done) {
      this.registers.set(r.register, r.steps);
      this.last = [r.register];
      this.app().setStatus(`Macro ${r.register}: ${plural(r.steps.length, "step")} recorded (${this.app().keyLabel("macro.playLast") || "Play Last Macro"} plays it)`);
    }
  }

  async play(register) {
    if (!this.registers.has(register)) { this.app().setStatus(`No macro in register ${register}`); return; }
    const steps = this.registers.get(register) ?? [];
    if (this.playing) return;
    if (this.recording.some((r) => r.register === register)) { this.app().setStatus(`Macro ${register} is being recorded`); return; }
    this.playing = true;
    this.last = [register];
    try {
      for (const s of steps) {
        const pane = this.app().activePane;
        const v = pane.view;
        if (s.kind === "command") await this.app().run(s.command, s.arg);
        else if (v !== undefined && !v.buf.readOnly) {
          if (s.typed) this.app().editing.type(pane, s.text, true);
          else this.app().paste(s.text);
        }
      }
    } finally {
      this.playing = false;
    }
    this.app().setStatus(`Played macro ${register} (${plural(steps.length, "step")})`);
  }

  // Ask for a register: resolves with it, or null.
  pick(title) {
    return this.app().choose(title, REGISTERS.map((r) => {
      const steps = this.registers.get(r);
      return { label: r, detail: steps ? plural(steps.length, "step") : "empty", value: r };
    }));
  }

  // Every register's steps, as text.
  describe() {
    const titleOf = (id) => { const c = this.app().commands.get(id); return c ? c.title : id; };
    const out = [];
    for (const r of REGISTERS) {
      const steps = this.registers.get(r);
      if (!steps) continue;
      out.push(`Macro ${r}${this.last.includes(r) ? " (last)" : ""}:`);
      for (const s of steps) out.push(describeStep(s, titleOf));
      out.push("");
    }
    return out.length ? out.join("\n") : "No macros recorded. Record one with Record Macro… in the command palette.\n";
  }

  register() {
    const cmd = (id, title, run) => { this.app().commands.set(id, { title, run: asCommand(run), source: "core", hidden: false }); };
    cmd("macro.record", "Macro: Record Macro…", async () => { const r = await this.pick("Record macro into register"); if (r) this.start(r); });
    cmd("macro.stop", "Macro: Stop Recording Macro", () => this.stop());
    cmd("macro.play", "Macro: Play Macro…", async () => { const r = await this.pick("Play macro"); if (r) await this.play(r); });
    cmd("macro.playLast", "Macro: Play Last Macro", async () => {
      if (!this.last.length) { this.app().setStatus("No macro recorded yet"); return; }
      for (const r of this.last) await this.play(r);
    });
    cmd("macro.list", "Macro: List Macros", () => this.app().createVirtual({
      name: "*Macros*", entries: [{ text: this.describe(), props: {}, style: null }],
      mode: null, readOnly: true, language: null, keep: false, place: "tab", lineNumbers: true,
    }));
  }
}
