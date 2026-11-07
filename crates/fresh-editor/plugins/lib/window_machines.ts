/// <reference path="./fresh.d.ts" />

/**
 * One machine per editor window: `editor.openMachine({ kind: "window" })`,
 * opened on first use and kept until the window closes. A command `run` on it
 * executes where that window's files are — on its SSH host or in its
 * container for a remote window — whichever window is in front.
 *
 * The owner wires `close` to the `window_closed` hook:
 *
 *     const machines = new WindowMachines(editor);
 *     editor.on("window_closed", ({ id }) => machines.close(id));
 */
export class WindowMachines {
  private readonly machines = new Map<number, Promise<FreshMachine>>();

  constructor(private readonly editor: EditorAPI) {}

  /** Window `windowId`'s machine. Rejects for a window that is gone; a failed
   *  open is not kept, so the next call asks again. */
  get(windowId: number): Promise<FreshMachine> {
    let machine = this.machines.get(windowId);
    if (!machine) {
      machine = this.editor.openMachine({ kind: "window", window: windowId });
      machine.catch(() => this.machines.delete(windowId));
      this.machines.set(windowId, machine);
    }
    return machine;
  }

  /** Close window `windowId`'s machine, if one was opened. */
  close(windowId: number): void {
    const machine = this.machines.get(windowId);
    this.machines.delete(windowId);
    void machine?.then((m) => m.close(), () => {});
  }
}
