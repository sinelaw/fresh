/// <reference path="./fresh.d.ts" />

/**
 * Shared composite controls: the **path picker** and the **Machine picker**.
 *
 * Each was built by hand in every dialog that had one — the path picker three
 * times (a project's folder, a clone's folder, an ssh identity file), each
 * with its own copy of the browser's state, rows, list handling, focus moves
 * and field write-back, and slightly different gaps and rules. One copy lives
 * here, so the pieces look and behave the same wherever a dialog puts them.
 *
 * - {@link PathPicker}: a text field, `Browse…` beside it, and — while it is
 *   open — a browser of one machine's folders under them. `⏎` on a row goes
 *   up, goes in, or picks (a git folder; where a plain folder will do, the
 *   folder shown; with `files`, a file). A pick fills the field and gives it
 *   focus. `Browse…` pressed again closes the browser.
 * - {@link machinePicker}: the Machine dropdown with `+ Add machine…` beside
 *   it.
 *
 * The dialog keeps the field's value (it is the dialog's data) and says where
 * the browser starts; the picker owns everything else.
 */

import {
  button,
  col,
  dropdown,
  label,
  labeledSection,
  list,
  row,
  spacer,
  text,
  type WidgetEvt,
  type WidgetSpec,
} from "./widgets.ts";

/** One entry of a listed folder. */
export interface BrowseEntry {
  name: string;
  /** The folder holds a `.git`. */
  git: boolean;
  /** A file, not a folder (listed only when picking a file). */
  file?: boolean;
}

/** What the browser reads a machine through, and the words it shows. */
export interface BrowseSource {
  /** The entries of `dir` on `machineKey`; files too when `files`. */
  list(machineKey: string, dir: string, files: boolean): Promise<{ entries: BrowseEntry[]; error: string }>;
  /** The folder above `dir`. */
  parentDir(dir: string): string;
  /** The browser's title: the machine and the folder shown. */
  title(machineKey: string, dir: string): string;
  /** Translate one of the picker's strings (`repo.browse`, `repo.loading`,
   *  `repo.use_this_folder`, `repo.browse_hint*`). */
  t(key: string): string;
}

/** Where a browser is and what it shows. */
export interface FolderBrowser {
  machineKey: string;
  dir: string;
  entries: BrowseEntry[];
  loading: boolean;
  error: string;
  index: number;
  /** A plain folder will do, so the folder shown can itself be picked. */
  picksAny: boolean;
  /** Files are listed too, and picking one is the answer (dotfiles shown). */
  files: boolean;
}

/** The panel calls the picker makes: focus and the field's value. */
export interface PickerPanel {
  setFocusKey(widgetKey: string): boolean;
  setValue(widgetKey: string, value: string, cursorByte?: number): boolean;
}

export interface PathPickerOptions {
  source: BrowseSource;
  /** The text field's key. */
  key: string;
  /** `Browse…`'s key. */
  browseKey: string;
  /** The browser list's key. */
  listKey: string;
  /** The field's label; a function when it changes with the dialog. */
  label: string | (() => string);
  labelWidth: number;
  /** The field's width in columns; omitted, it fills what the row leaves
   *  beside `Browse…` (the host sizes it from the width it is laid out at). */
  fieldWidth?: number;
  /** The field's value and caret, as the dialog holds them. */
  value(): { value: string; cursor: number };
  /** Where `Browse…` starts and what it may pick, read each time it opens.
   *  `typed` is the field's value, trimmed. */
  start(typed: string): { machineKey: string; dir: string; picksAny?: boolean; files?: boolean };
  /** A path was picked: store it as the field's value. The picker then
   *  redraws, shows it in the field and focuses the field. */
  onPick(path: string): void;
  render(): void;
  panel(): PickerPanel | null;
}

const NOTE_STYLE = { fg: "ui.menu_disabled_fg", italic: true } as const;

/** Blank columns between the field and `Browse…`. */
const GAP = 2;

/** Rows a browser shows at a time. */
const BROWSER_ROWS = 8;

/** A text field for a path, `Browse…` beside it, and its folder browser. */
export class PathPicker {
  /** The open browser, or null. */
  browser: FolderBrowser | null = null;

  constructor(private readonly o: PathPickerOptions) {}

  /** The field row, and the browser under it while it is open. `fieldLabel`
   *  overrides the options' for this draw. */
  rows(fieldLabel?: string): WidgetSpec[] {
    const { value, cursor } = this.o.value();
    const field = text({
      value,
      cursorByte: cursor,
      label: fieldLabel ?? (typeof this.o.label === "function" ? this.o.label() : this.o.label),
      labelWidth: this.o.labelWidth,
      ...(this.o.fieldWidth === undefined ? { fullWidth: true } : { fieldWidth: this.o.fieldWidth }),
      key: this.o.key,
    });
    const out = [row(field, spacer(GAP), button(this.o.source.t("repo.browse"), { key: this.o.browseKey }))];
    if (this.browser) out.push(this.browserRow(this.browser));
    return out;
  }

  /** Close the browser without redrawing (the dialog is resetting). */
  reset(): void {
    this.browser = null;
  }

  /** Close the browser and give `Browse…` the focus back. False when there
   *  was nothing open. */
  close(): boolean {
    if (!this.browser) return false;
    this.browser = null;
    this.o.render();
    this.o.panel()?.setFocusKey(this.o.browseKey);
    return true;
  }

  /** `Browse…`: open the browser, or close it when it is open. */
  toggle(): void {
    if (this.browser) {
      this.browser = null;
      this.o.render();
      return;
    }
    const s = this.o.start(this.o.value().value.trim());
    const b: FolderBrowser = {
      machineKey: s.machineKey,
      dir: "",
      entries: [],
      loading: true,
      error: "",
      index: 0,
      picksAny: !!s.picksAny,
      files: !!s.files,
    };
    this.browser = b;
    void this.go(b, s.dir).then(() => {
      if (this.browser === b) this.o.panel()?.setFocusKey(this.o.listKey);
    });
  }

  /** Up a folder (Backspace in the list). False when the list is not what
   *  has focus. */
  up(focusKey: string): boolean {
    const b = this.browser;
    if (!b || focusKey !== this.o.listKey) return false;
    void this.go(b, this.o.source.parentDir(b.dir));
    return true;
  }

  /** The picker's own events: `Browse…` and the browser's rows. True when
   *  the event was one of them. */
  handle(e: WidgetEvt): boolean {
    if (e.event_type === "activate" && e.widget_key === this.o.browseKey) {
      this.toggle();
      return true;
    }
    if (e.event_type !== "select" && e.event_type !== "activate") return false;
    const payload = (e.payload ?? {}) as Record<string, unknown>;
    // A key names the list; a row click names the row, with the list's key
    // in the payload.
    if (e.widget_key !== this.o.listKey && payload.list_key !== this.o.listKey) return false;
    const b = this.browser;
    const idx = typeof payload.index === "number" ? payload.index : -1;
    if (!b || idx < 0) return true;
    b.index = idx;
    if (e.event_type === "activate") {
      const picked = this.activate(b, idx);
      if (picked) {
        this.browser = null;
        this.o.onPick(picked);
        this.o.render();
        const { value, cursor } = this.o.value();
        this.o.panel()?.setValue(this.o.key, value, cursor);
        this.o.panel()?.setFocusKey(this.o.key);
      }
    }
    return true;
  }

  private items(b: FolderBrowser): { text: string; dir: string | null; up?: boolean; file?: string }[] {
    const child = (name: string) => (b.dir === "/" ? `/${name}` : `${b.dir.replace(/\/+$/, "")}/${name}`);
    return [
      { text: "..", dir: this.o.source.parentDir(b.dir), up: true },
      ...(b.picksAny ? [{ text: `✓ ${this.o.source.t("repo.use_this_folder")}`, dir: null }] : []),
      ...b.entries.map((e) =>
        e.file
          ? { text: e.name, dir: null, file: child(e.name) }
          : { text: `${`${e.name}/`.padEnd(40)} ${e.git ? "git" : ""}`, dir: child(e.name) }
      ),
    ];
  }

  private browserRow(b: FolderBrowser): WidgetSpec {
    const t = (k: string) => this.o.source.t(k);
    // The list stays mounted while a folder loads or fails to list: taking
    // it out of the tree would drop the keyboard focus it holds onto
    // whatever control comes first. What is happening goes in the line under
    // it instead. Its `..` row stays too, the way back from a folder that
    // would not list.
    const items = b.loading || b.error ? this.items(b).slice(0, 1) : this.items(b);
    const body = list({
      items: items.map((i) => ({ text: i.text })),
      selectedIndex: Math.max(0, Math.min(b.index, items.length - 1)),
      visibleRows: BROWSER_ROWS,
      key: this.o.listKey,
    });
    const hint = b.loading
      ? label(t("repo.loading"), { style: NOTE_STYLE })
      : b.error
      ? label(`✗ ${b.error}`, { style: { fg: "diagnostic.error_fg" }, wrap: true })
      : label(t(b.files ? "repo.browse_hint_file" : b.picksAny ? "repo.browse_hint_any" : "repo.browse_hint"), {
        style: NOTE_STYLE,
      });
    return row(
      spacer(2),
      labeledSection({
        label: this.o.source.title(b.machineKey, b.dir),
        child: col(body, hint),
      }),
    );
  }

  private async go(b: FolderBrowser, dir: string): Promise<void> {
    b.dir = dir;
    b.entries = [];
    b.loading = true;
    b.error = "";
    b.index = 0;
    this.o.render();
    const r = await this.o.source.list(b.machineKey, dir, b.files);
    if (b.dir !== dir || this.browser !== b) return;
    b.entries = r.entries;
    b.error = r.error;
    b.loading = false;
    this.o.render();
  }

  // `⏎` on a row: go up, go in, or pick — a git folder, a file, or (when a
  // plain folder will do) the folder shown. Answers the pick, or null.
  private activate(b: FolderBrowser, index: number): string | null {
    if (b.loading) return null;
    const item = this.items(b)[index];
    if (!item) return null;
    if (item.up) {
      void this.go(b, item.dir!);
      return null;
    }
    if (item.file) return item.file;
    if (item.dir === null) return b.dir;
    const e = b.entries.find((x) => item.dir!.endsWith(`/${x.name}`));
    if (e?.git) return item.dir;
    void this.go(b, item.dir);
    return null;
  }
}

/** The Machine picker: the dropdown of machines, `+ Add machine…` beside it. */
export function machinePicker(o: {
  options: string[];
  selectedIndex: number;
  label: string;
  labelWidth: number;
  /** The dropdown's key. */
  key: string;
  /** `+ Add machine…`'s key. */
  addKey: string;
  /** `+ Add machine…`'s text. */
  addLabel: string;
}): WidgetSpec {
  return row(
    dropdown(o.options, {
      selectedIndex: o.selectedIndex,
      label: o.label,
      labelWidth: o.labelWidth,
      key: o.key,
    }),
    spacer(GAP + 1),
    button(o.addLabel, { key: o.addKey }),
  );
}
