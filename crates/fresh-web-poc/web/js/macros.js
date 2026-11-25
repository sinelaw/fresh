// Keyboard macros, as the editor's: recorded into registers 0–9 (Record
// Macro… asks which), stopped with F5, played back with F4 (the last one)
// or Play Macro… (asks which); List Macros shows them. A macro is the
// commands run (by key, menu or palette) and the text typed or pasted into
// the editor while recording, played back in order. Commands that manage
// macros, and the palette's own opening (the command it runs is recorded),
// are left out; what is typed into a prompt or a panel is not recorded.
// Registers last as long as the page.

const REGISTERS = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

export class Macros {
  constructor(app) {
    this.app = app;
    // register -> its steps, [{command, arg, text, typed}]: a command run
    // (with its argument, or undefined; text "") or text typed or pasted
    // (command "").
    this.registers = new Map();
    this.recording = null; // {register, steps}
    this.playing = false;
    this.last = null; // the register last recorded or played
    this.depth = 0; // commands running inside a command are its own doing
  }

  // A step as it happens; recorded while recording (and not playing).
  command(id, arg) {
    if (!this.recording || this.playing || this.depth > 0 || id.startsWith("macro.") || id.startsWith("palette.")) return;
    this.recording.steps.push({ command: id, arg, text: "", typed: false });
  }
  text(text, typed) {
    if (!this.recording || this.playing) return;
    const steps = this.recording.steps;
    const last = steps[steps.length - 1];
    // A run of typed characters is one step.
    if (typed && last && last.typed) last.text += text;
    else steps.push({ command: "", arg: undefined, text, typed: !!typed });
  }

  start(register) {
    if (this.recording) this.stop();
    this.recording = { register, steps: [] };
    this.app().setStatus(`Recording macro ${register}… (${this.app().keyLabel("macro.stop") || "Stop Recording Macro"} stops)`);
    this.app().updateStatus();
  }
  stop() {
    const r = this.recording;
    if (!r) { this.app().setStatus("Not recording a macro"); return; }
    this.recording = null;
    this.registers.set(r.register, r.steps);
    this.last = r.register;
    this.app().setStatus(`Macro ${r.register}: ${r.steps.length} step${r.steps.length === 1 ? "" : "s"} recorded (${this.app().keyLabel("macro.playLast") || "Play Last Macro"} plays it)`);
  }

  async play(register) {
    const steps = this.registers.get(register);
    if (!steps) { this.app().setStatus(`No macro in register ${register}`); return; }
    if (this.playing) return;
    if (this.recording && this.recording.register === register) { this.app().setStatus(`Macro ${register} is being recorded`); return; }
    this.playing = true;
    this.last = register;
    try {
      for (const s of steps) {
        const pane = this.app().activePane;
        if (s.command) await this.app().run(s.command, s.arg);
        else if (pane.view && !pane.view.buf.readOnly) {
          if (s.typed) this.app().editing.type(pane, s.text, true);
          else this.app().paste(s.text);
        }
      }
    } finally {
      this.playing = false;
    }
    this.app().setStatus(`Played macro ${register} (${steps.length} step${steps.length === 1 ? "" : "s"})`);
  }

  // Ask for a register: resolves with it, or null.
  pick(title) {
    return this.app().choose(title, REGISTERS.map((r) => ({
      label: r,
      detail: this.registers.has(r) ? `${this.registers.get(r).length} steps` : "empty",
      value: r,
    })));
  }

  // Every register's steps, as text.
  describe() {
    const out = [];
    for (const r of REGISTERS) {
      const steps = this.registers.get(r);
      if (!steps) continue;
      out.push(`Macro ${r}${r === this.last ? " (last)" : ""}:`);
      for (const s of steps) {
        if (s.command) out.push(`  ${this.app().commands.get(s.command)?.title || s.command}${s.arg !== undefined ? ` ${JSON.stringify(s.arg)}` : ""}`);
        else out.push(`  ${s.typed ? "type" : "paste"} ${JSON.stringify(s.text)}`);
      }
      out.push("");
    }
    return out.length ? out.join("\n") : "No macros recorded. Record one with Record Macro… in the command palette.\n";
  }

  register() {
    const cmd = (id, title, run) => this.app().commands.set(id, { title, run, source: "core", hidden: false });
    cmd("macro.record", "Macro: Record Macro…", async () => { const r = await this.pick("Record macro into register"); if (r) this.start(r); });
    cmd("macro.stop", "Macro: Stop Recording Macro", () => this.stop());
    cmd("macro.play", "Macro: Play Macro…", async () => { const r = await this.pick("Play macro"); if (r) await this.play(r); });
    cmd("macro.playLast", "Macro: Play Last Macro", () => (this.last ? this.play(this.last) : this.app().setStatus("No macro recorded yet")));
    cmd("macro.list", "Macro: List Macros", () => this.app().createVirtual({
      name: "*Macros*", entries: [{ text: this.describe(), props: {}, style: null }],
      mode: null, readOnly: true, language: null, keep: false, place: "tab", lineNumbers: true,
    }));
  }
}
