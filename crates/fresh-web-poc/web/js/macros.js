// Keyboard macros, as the editor's: recorded into registers 0–9 (Record
// Macro… asks which), stopped with F5, played back with F4 (the last one)
// or Play Macro… (asks which); List Macros shows them. A macro is the
// commands run (by key, menu or palette) and the text typed or pasted into
// the editor while recording, played back in order. Commands that manage
// macros, and the palette's own opening (the command it runs is recorded),
// are left out; what is typed into a prompt or a panel is not recorded.
// Registers last as long as the page.

import { some, none, listOf } from "./maybe.js";
import { plural } from "./words.js";

const REGISTERS = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

// A macro's step: a command run (with its argument, if any), or text
// typed (a run of keys, one step) or pasted.
/** type Step =
 *   | {kind: "command", command: String, arg: String | Undefined}
 *   | {kind: "text", text: String, typed: Boolean}
 */
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

// A step's parts: the command it runs (null: text), or the text typed
// (`typed`) or pasted.
/** function stepParts(s: Step) => {
 *   command: String | Null,
 *   arg: String | Undefined,
 *   text: String,
 *   typed: Boolean,
 * } */
function stepParts(s) {
  return s.kind === "command" ? { command: s.command, arg: s.arg, text: "", typed: false } : { command: null, arg: undefined, text: s.text, typed: s.typed };
}

// Every register's steps, as text (`last`: the register last recorded or
// played).
/** function describeMacros(
 *   registers: Map<String, Step[]>,
 *   last: String[],
 *   titleOf: (String) => String,
 * ) => String */
function describeMacros(registers, last, titleOf) {
  const out = REGISTERS.flatMap((r) => {
    const steps = registers.get(r);
    return steps === undefined ? [] : [`Macro ${r}${last.includes(r) ? " (last)" : ""}:`, ...steps.map((s) => describeStep(s, titleOf)), ""];
  });
  return out.length ? out.join("\n") : "No macros recorded. Record one with Record Macro… in the command palette.\n";
}

// A command macros offer: its id, title, and what it does.
/** type MacroCommand = {id: String, title: String, run: () => Promise<Undefined>} */
/** function macroCommand(
 *   id: String,
 *   title: String,
 *   run: () => Promise<Undefined>,
 * ) => MacroCommand */
function macroCommand(id, title, run) { return { id, title, run }; }

// Empty state, typed.
/** function noMacros() => Map<String, Step[]> */
function noMacros() { return new Map(); }
/** function noRecording() => Recording[] */
function noRecording() { return []; }
/** function noRegister() => String[] */
function noRegister() { return []; }

// A command's keys, or (it has none) what to call it.
/** function keysOr(keys: String, name: String) => String */
function keysOr(keys, name) { return keys !== "" ? keys : name; }

export class Macros {
  // `editor`: what macros ask of the editor (js/main.js).
  constructor(editor) {
    const port /*: MacrosEditor */ = editor;
    this.editor = port;
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
    this.editor.say(`Recording macro ${register}… (${keysOr(this.editor.keyLabel("macro.stop"), "Stop Recording Macro")} stops)`);
    this.editor.updateStatus();
  }
  stop() {
    const done = this.recording;
    if (done.length === 0) { this.editor.say("Not recording a macro"); return; }
    this.recording = [];
    for (const r of done) {
      this.registers.set(r.register, r.steps);
      this.last = [r.register];
      this.editor.say(`Macro ${r.register}: ${plural(r.steps.length, "step")} recorded (${keysOr(this.editor.keyLabel("macro.playLast"), "Play Last Macro")} plays it)`);
    }
  }

  async play(register) {
    if (!this.registers.has(register)) { this.editor.say(`No macro in register ${register}`); return; }
    const steps = this.registers.get(register) ?? [];
    if (this.playing) return;
    if (this.recording.some((r) => r.register === register)) { this.editor.say(`Macro ${register} is being recorded`); return; }
    this.playing = true;
    this.last = [register];
    try {
      for (const s of steps) {
        const a = stepParts(s);
        const command = a.command;
        if (command !== null) await this.editor.run(command, a.arg);
        else this.editor.insert(a.text, a.typed);
      }
    } finally {
      this.playing = false;
    }
    this.editor.say(`Played macro ${register} (${plural(steps.length, "step")})`);
  }

  // Ask for a register: resolves with it, or null.
  pick(title) {
    return new Promise((resolve) => this.editor.pick({
      title,
      value: "",
      items: REGISTERS.map((r) => ({ label: r, detail: this.registers.has(r) ? plural((this.registers.get(r) ?? []).length, "step") : "empty", value: r })),
      onAccept: (it) => { resolve(some(it.value)); },
      onChange: (_it) => {},
      onCancel: () => { resolve(none()); },
    }));
  }

  // Every register's steps, as text.
  describe() {
    return describeMacros(this.registers, this.last, (id) => this.editor.titleOf(id));
  }

  // Its commands, for the editor to register.
  commands() {
    return [
      macroCommand("macro.record", "Macro: Record Macro…", async () => { for (const r of listOf(await this.pick("Record macro into register"))) this.start(r); }),
      macroCommand("macro.stop", "Macro: Stop Recording Macro", async () => { this.stop(); }),
      macroCommand("macro.play", "Macro: Play Macro…", async () => { for (const r of listOf(await this.pick("Play macro"))) await this.play(r); }),
      macroCommand("macro.playLast", "Macro: Play Last Macro", async () => {
        if (this.last.length === 0) { this.editor.say("No macro recorded yet"); return; }
        for (const r of this.last) await this.play(r);
      }),
      macroCommand("macro.list", "Macro: List Macros", async () => { this.editor.showText("*Macros*", this.describe()); }),
    ];
  }
}
