/// <reference path="./lib/fresh.d.ts" />
import {
  button,
  col,
  flexSpacer,
  labeledSection,
  raw,
  row,
  spacer,
  type StyledSegment,
  styledRow,
  text,
  textInput,
  textInputChar,
  tree,
  type WidgetSpec,
  WidgetPanel,
  key as widgetKey,
} from "./lib/widgets.ts";

const editor = getEditor();

// ═════════════════════════════════════════════════════════════════════
//   WELCOME SCREEN
//
//   The startup surface: a scrollable buffer that onboards three
//   audiences on one page without overwhelming the simplest. It has one
//   automatic behaviour, and one setting to govern it: when Fresh
//   launches, open as a tab behind whatever is already there. It never
//   takes the foreground on its own, never closes another buffer, and
//   never reopens because something else went away — closing it is the
//   reader's, and `Welcome` in the palette brings it back.
//   Design note: docs/internal/welcome-screen-design.md.
//
//   Structure is a ladder. The first viewport is a zero-anxiety zone
//   (wordmark, one line, three numbered doors, four verbs, one
//   reassurance) and mentions no LSP, git, worktree or agent.
//   Scrolling descends through three bannered levels ordered by
//   sophistication; the Contents section in the sidebar, and the three
//   door cards, jump straight to one.
//
//   Everything here is built from the existing plugin surface — a
//   virtual buffer with a mounted widget panel (the same pairing
//   `search_replace.ts` uses), the widget library's own controls, and
//   the orchestrator's exported API. No new host mechanism.
//
//   The demos are real rather than illustrated: the finder runs over
//   `git ls-files`, the theme buttons call `applyTheme`, the git card
//   reads `git status`, and the workspace rows come from
//   `getPluginApi("orchestrator").listWorkspaces()`.
// ═════════════════════════════════════════════════════════════════════

const C = {
  art: "syntax.function",
  title: "syntax.keyword",
  accent: "syntax.function",
  /** All prose. The page is a document, so its default ink is the
   *  editor's default ink — not `syntax.comment`, which is green on the
   *  stock dark theme, nor `syntax.string`, which is dark red on light. */
  body: "editor.fg",
  /** Literal quotable things only: paths, commands, branch names. */
  value: "syntax.string",
  /** Genuinely recessive in every shipped theme, unlike `syntax.comment`. */
  muted: "editor.line_number_fg",
  /** Bullets, markers, separators — structure, not content. */
  gutter: "editor.line_number_fg",
  key: "ui.help_key_fg",
  /** `ui.popup_border_fg` is built for a floating edge over dimmed
   *  content, so it is deliberately loud; across ten full-width frames
   *  it made the chrome the brightest ink on the page. This is the
   *  editor's own "rule between regions" colour. */
  frame: "ui.split_separator_fg",
  /** The brightest ink the theme has for "under the pointer". */
  hoverFg: "ui.menu_hover_fg",
  /** The block behind a chip. `ui.popup_bg` is the obvious name for it
   *  and the wrong colour: on the stock dark theme it is `editor.bg`
   *  exactly, so the chips had no block at all. This is the theme's own
   *  "a subtle band lies over the editor background" colour, which is
   *  the thing a chip actually needs, and it lifts in either polarity. */
  chipBg: "editor.current_line_bg",
  /** Decorative markers — `▸`, `▼`, the finder's caret. An accent, not
   *  gutter grey: they are the page's punctuation, and the eye uses
   *  them to find the next actionable thing.
   *
   *  Not `ui.file_status_added_fg`, which several shipped themes leave
   *  unset — it fell back to a grey indistinguishable from the rule. */
  mark: "syntax.type",
  ok: "ui.file_status_added_fg",
  err: "diagnostic.error_fg",
};

/** Per-porcelain-code colour, matching the file explorer and git gutter.
 *  Themes that leave these unset fall back to the `diagnostic.*` family,
 *  so they stay theme-derived rather than a hardcoded grey. */
/** A word that can be clicked is marked the way a link is marked, and
 *  it is marked *always* — an underline that only appears under the
 *  pointer teaches nothing, because you have to already be pointing at
 *  the thing to learn that you could.
 *
 *  It follows that the label a link carries must be exactly its text:
 *  an underline runs the width of the button's cells, so a marker
 *  baked into the label (`▸ `, `● `) or padding from `fullWidth` is
 *  underlined too. Markers sit outside the button now, and nothing
 *  underlined is full-width. */
const LINK = { underline: true };

/** Under the pointer, a link lifts rather than lighting up: brightest
 *  ink, bold, and the underline it already had. A background band reads
 *  as a selection — a state the thing is in — where a glow reads as the
 *  pointer being on it, which is what is true. */
const HOVER_LINK = { fg: C.hoverFg, bold: true, underline: true };

/** The same lift, for things that are not links: the framed buttons,
 *  whose brackets already say what they are, and the door cards, which
 *  are cards. Underlining either would be marking what is already
 *  marked. */
const GLOW = { fg: C.hoverFg, bold: true };

/** A row of a list, under the pointer. The band is the affordance here
 *  rather than the lift a link gets: the row IS the target, so the
 *  highlight has to say where the target's edges are — which a glow on
 *  the words alone cannot. `chipBg` is the theme's own "a subtle band
 *  lies over the editor background", so it reads in either polarity. */
const HOVER_ROW = { fg: C.hoverFg, bold: true, bg: C.chipBg };

/** The marked row at rest. The `▸` says which row it is; this says it
 *  loudly enough to find without the pointer on it. */
const SELECTED_ROW = { fg: C.mark, bold: true };

function statusFg(xy: string): string {
  const c = xy.trim();
  if (c === "??") return "ui.file_status_untracked_fg";
  if (c.includes("U") || c === "AA" || c === "DD") return "ui.file_status_conflicted_fg";
  if (c.includes("R")) return "ui.file_status_renamed_fg";
  if (c.includes("D")) return "ui.file_status_deleted_fg";
  if (c.includes("A")) return "ui.file_status_added_fg";
  return "ui.file_status_modified_fg";
}

// ── Model ────────────────────────────────────────────────────────────

type FinderItem = { path: string };

type Workspace = {
  name: string;
  branch: string;
  agentState: string;
  kind: string;
  active: boolean;
  windowId: number;
};

let bufferId: number | null = null;
let panel: WidgetPanel | null = null;
let opening = false;
const folded = new Set<string>();

let finderQuery = "";
let finderCursor = 0;
let repoFiles: string[] | null = null;
let repoFilesLoading = false;
let finderHits: FinderItem[] = [];

/** How many finder results the card shows at once. The list scrolls
 *  within this window rather than growing, so the card keeps its height
 *  whatever the query matches. */
const FINDER_ROWS = 6;
let finderIndex = 0;

let themeNames: string[] = [];
/** The theme the editor is actually on, so the swatch row can mark it.
 *  Read from config at open and updated when a swatch applies a new
 *  one; nothing about *colour* depends on it, because every colour on
 *  this page is a key the host resolves at paint time. */
let activeTheme = "";

/** Config stores a *registry key* (`builtin://dark`); the swatch row and
 *  `applyTheme` both speak the bare name the registry object is keyed by
 *  (`dark`). Comparing the two directly meant the `●` marker was missing
 *  on every launch and only appeared once you clicked a swatch — which
 *  reassigns `activeTheme` from the bare side. Normalise on the way in so
 *  both sides of `name === activeTheme` are the same kind of string. */
function bareThemeName(name: string): string {
  const sep = name.indexOf("://");
  return sep === -1 ? name : name.slice(sep + 3);
}

function readActiveTheme(): void {
  const cfg = editor.getConfig() as { theme?: string } | null;
  const name = cfg?.theme;
  if (typeof name === "string" && name) activeTheme = bareThemeName(name);
}

let gitDirty: string[] = [];
let gitBranch = "";
let gitProbed = false;

let workspaces: Workspace[] = [];

/** Best-effort proxy for "which widget has focus", mirrored from
 *  `widget_event`. The welcome buffer is a document first, so Up/Down
 *  scroll it — except while the finder owns focus, where they walk its
 *  results. The widget runtime does not report focus directly; this is
 *  the same proxy `search_replace.ts` keeps for its history walk. */
let lastFocusedWidget = "";

function finderFocused(): boolean {
  return lastFocusedWidget === "finderField";
}

/** Buffer line of each level banner, resolved after each render by
 *  searching the painted buffer text for the banner marker. The widget
 *  runtime owns layout, so the rows are read back rather than
 *  predicted. */

/** Buffer line of each card's header, resolved the same way as the
 *  level banners. Tab moves widget focus but the host only scrolls the
 *  pane for a focused *text* widget — a focused button on a long
 *  document can land off-screen, so the focus event asks the host to
 *  scroll to that card's own fold widget. */


/** One line of the Contents section: what it says, the widget it goes
 *  to, and how deep it sits.
 *
 *  Recorded by `banner()` and `card()` as the page is built rather than
 *  listed in a table beside them. A table is a second copy of the
 *  outline, and a second copy of anything on this page has gone stale
 *  at least once (the level captions the jump keys used to match as
 *  strings). The builder already walks the page top to bottom in
 *  order; letting it say what it is building costs one push per
 *  section and cannot disagree with the page. */
type OutlineEntry = { label: string; key: string; depth: number };

let outline: OutlineEntry[] = [];

/** The entry the Contents section marks, by widget key.
 *
 *  A key, not an index into `outline` — that list is discarded and
 *  rebuilt on every `render()`, which is every keystroke in the finder,
 *  and an index into it is only stable while every `card()` call is
 *  unconditional. One card built behind a condition and the mark would
 *  quietly point at a different section.
 *
 *  It follows *focus*, not the caret, and only when focus lands on a
 *  heading — a fold arrow or a level banner. A plugin cannot ask where
 *  its own widgets were painted, so it cannot map an arbitrary caret row
 *  back to a section; what it can see is that the reader focused this
 *  heading, and with the caret driving focus that is most of the way
 *  there. The mark stays put while they read a card's body, which is
 *  the behaviour you want anyway. */
let outlineSelected = "";

const LEVEL_MARK: Record<string, string> = {
  "1": "LEVEL 1 · JUST EDIT",
  "2": "LEVEL 2 · IT'S A PROJECT NOW",
  "3": "LEVEL 3 · RUN THE WHOLE SHOP",
};

// ── Small helpers ────────────────────────────────────────────────────

function line(segments: StyledSegment[]): WidgetSpec {
  return raw([styledRow(segments)]);
}

function plain(t: string, fg?: string): WidgetSpec {
  return line([{ text: t, style: fg ? { fg } : undefined }]);
}

/** A bullet whose marker is structure and whose text is content — the
 *  two were the same colour, which threw away the marker as a scanning
 *  aid and painted the sentence as a comment.
 *
 *  Wrapped at render time, with a hanging indent. Hand-wrapping set
 *  these to the width of a wide terminal and left them breaking in the
 *  wrong places on a narrow one — the same mistake the door copy made. */
function bullet(t: string): WidgetSpec {
  const lines = wrap(t, Math.max(20, measure() - 6));
  return col(
    ...lines.map((l, i) =>
      line(
        i === 0
          ? [
            { text: "  · ", style: { fg: C.gutter } },
            { text: l, style: { fg: C.body } },
          ]
          : [{ text: "    " + l, style: { fg: C.body } }]
      )
    ),
  );
}

function blank(): WidgetSpec {
  return raw([styledRow([{ text: "" }])]);
}

/** `n` blank rows on a terminal with the height to spend them, one when
 *  there isn't.
 *
 *  Air between blocks is most of what separates a composed page from a
 *  dense one, and single blank rows everywhere packed the first
 *  viewport into a slab. But that viewport also has to *be* one
 *  viewport — the doors, the verbs and the scroll hint all land above
 *  the fold — so on a short terminal the air is the first thing to go.
 *  The rhythm is a function of the height available, not a constant. */
function air(n: number): WidgetSpec[] {
  const rows = viewportHeight() >= 38 ? n : 1;
  return Array.from({ length: rows }, () => blank());
}

function viewportHeight(): number {
  const vp = editor.getViewport();
  return vp && vp.height > 0 ? vp.height : 40;
}

/** Centre a run of styled text by letting the host do it.
 *
 *  Two flex spacers split the leftover width evenly, so the row lands
 *  on the axis of whatever the panel's content width actually is —
 *  after the compose column is centred in the pane, after the gutter
 *  and scrollbar take their columns, after a dock drag resizes the
 *  split. The plugin never learns any of those numbers.
 *
 *  This replaced a pad computed from `measure()`, which was wrong by
 *  two columns in a way worth recording: the widget layout was sizing
 *  rows to the *split* while the renderer painted them into the
 *  narrower compose column, so a flex spacer overfilled and the host
 *  wrapped the row. That is fixed in the host (`widget_panel_width`
 *  now honours `compose_width`), and with the two widths agreeing the
 *  page can simply ask to be centred. */
function centred(segs: StyledSegment[]): WidgetSpec {
  return centredRow(line(segs));
}

/** The same, for a row of widgets rather than styled text. */
function centredRow(...parts: WidgetSpec[]): WidgetSpec {
  return row(flexSpacer(), ...parts, flexSpacer());
}

function accel(action: string): string {
  return editor.getKeybindingLabel(action, "normal") ??
    editor.getKeybindingLabel(action, "global") ?? "";
}

const VERBS: [string, string, string][] = [
  ["act_open", "Open file", "open"],
  ["act_recent", "Command palette", "quick_open"],
  ["act_new", "New buffer", "new"],
];

/** The three verbs, each with its live keybinding.
 *
 *  The keys used to sit at the right edge of the measure, flushed there
 *  by the width of the column rather than by anything to do with the
 *  verb — `Ctrl+O` ended up sixty columns from `Open file` and read as
 *  belonging to nothing. They are a column of their own now, four
 *  spaces past the longest label, so the eye can carry a label to its
 *  key without crossing the page. */
/** The three verbs on one centred line, each key beside its own label.
 *
 *  They used to be a stacked list with the keys in a column of their
 *  own, which was an answer to the keys having drifted sixty columns
 *  from their labels. Sitting them side by side answers the same thing
 *  more directly — a key one space after the verb it belongs to can't
 *  drift at all — and it costs two rows of the first viewport, which is
 *  the scarcest space on the page. */
function verbs(): WidgetSpec[] {
  const parts: WidgetSpec[] = [];
  VERBS.forEach(([key, label, action], i) => {
    const acc = accel(action);
    if (i > 0) parts.push(line([{ text: "    " }]));
    // The marker is outside the button: an underline that includes it
    // starts two columns before the word it marks.
    parts.push(line([{ text: "▸ ", style: { fg: C.mark } }]));
    parts.push(button(label, { key, bare: true, style: LINK, hoverStyle: HOVER_LINK }));
    if (acc) {
      parts.push(line([
        { text: " " },
        { text: acc, style: { fg: C.key, bold: true } },
      ]));
    }
  });
  return [centredRow(...parts)];
}

/** The parts of the UI that are not the text, and the one line that says
 *  they exist.
 *
 *  A first-viewport block on purpose. Everything above it is about
 *  opening a file and typing; this is the first thing that says the
 *  editor has *furniture*, and it says it by giving you the furniture
 *  rather than by describing it — every one of these opens the real
 *  thing, here, now.
 *
 *  `Contents` is this page's own outline in the sidebar, which is also
 *  the answer to "this page is long": the section is already there when
 *  the page opens, and this brings it back if it has been closed, or
 *  puts the keyboard in it if it is buried under the explorer. */
const UI_FEATURES: [string, string, string][] = [
  ["ui_explorer", "File explorer", "toggle_file_explorer"],
  // No action of its own — the page mounts and focuses its own section.
  ["ui_contents", "Contents", ""],
  ["ui_terminal", "Terminal", "open_terminal"],
  ["ui_split", "Split the pane", "split_vertical"],
  ["ui_settings", "Settings", "open_settings"],
];

function uiFeatures(): WidgetSpec[] {
  const parts: WidgetSpec[] = [];
  UI_FEATURES.forEach(([key, label, action], i) => {
    if (i > 0) parts.push(line([{ text: "   ·   ", style: { fg: C.gutter } }]));
    parts.push(button(label, { key, bare: true, style: LINK, hoverStyle: HOVER_LINK }));
    const acc = action ? accel(action) : "";
    if (acc) {
      parts.push(line([
        { text: " " },
        { text: acc, style: { fg: C.key, bold: true } },
      ]));
    }
  });
  return [
    rule("THE UI, WHEN YOU WANT IT"),
    blank(),
    centredRow(...parts),
    blank(),
    centred([
      {
        text: "Each one opens the real thing. Contents is this page's outline, in the sidebar.",
        style: { fg: C.muted },
      },
    ]),
  ];
}

/** A section heading: fold arrow at the rail, title, then a leader rule
 *  running out to the hint. A rule is the typographic answer to a wide
 *  gap between a label and its value — and unlike a flex spacer it can
 *  be computed exactly, so the hint never drifts a hundred columns from
 *  the thing it describes. Narrow: the hint goes first, then the rule. */
function heading(id: string, title: string, hint: string, framed = false): WidgetSpec {
  const open = !folded.has(id);
  // A framed card's heading rules to the width of its own box. Ruling
  // to the page instead left the leader running sixteen columns past
  // the frame below it, so the heading and the card it names looked
  // like two unrelated things.
  const M = framed ? cardMeasure() : measure();
  const segs: StyledSegment[] = [{ text: title, style: { fg: C.accent, bold: true } }];
  const gap = M - 2 - title.length - hint.length - 2;
  if (gap >= 4) {
    segs.push({ text: " " + "─".repeat(gap) + " ", style: { fg: C.frame } });
    segs.push({ text: hint, style: { fg: C.muted } });
  } else {
    const g = M - 2 - title.length - 1;
    if (g >= 2) segs.push({ text: " " + "─".repeat(g), style: { fg: C.frame } });
  }
  return row(
    button(open ? "▾" : "▸", {
      key: `fold:${id}`,
      bare: true,
      hoverStyle: GLOW,
    }),
    spacer(1),
    line(segs),
  );
}

/** A foldable section.
 *
 *  `framed` draws the box. It is reserved for the sections holding real,
 *  touchable data — the finder, git, themes, the dock — so the frame
 *  means "your data is in here and you can touch it" rather than
 *  "section", which it said ten times in the loudest colour on the page.
 *  Reading material gets the heading and nothing else. */
function card(
  id: string,
  title: string,
  hint: string,
  body: () => WidgetSpec[],
  framed = false,
): WidgetSpec {
  outline.push({ label: title, key: `fold:${id}`, depth: 1 });
  const head = heading(id, title, hint, framed);
  // Air before every section, not one blank row. Below the fold the
  // page was a stack of headings and boxes at single-row spacing, which
  // reads as one dense column however well each part is set — the first
  // viewport got its rhythm and the rest of the document did not.
  if (folded.has(id)) return col(...air(2), head);
  if (framed) return col(...air(2), head, toCardWidth(col(...body())));
  return col(...air(2), head, ...body());
}

/** Constrain a framed card to the card measure rather than the page's.
 *
 *  A box drawn to the full measure is mostly empty: the finder's paths,
 *  the git card's file names and the workspace rows are all short, so
 *  the frame ran forty columns past its own content and the card read
 *  as a room rather than a card. Prose still sets to the full measure —
 *  it is the *frames* that were too wide, not the page. */
function toCardWidth(child: WidgetSpec): WidgetSpec {
  return row(labeledSection({ child, widthCols: cardMeasure() }));
}

/** Framed cards sit inside the page's measure by a margin on each side,
 *  capped so a wide terminal doesn't stretch one into a room. */
function cardMeasure(): number {
  return Math.max(24, Math.min(measure() - 8, 72));
}

/** A centred heading with a short rule out either side.
 *
 *  Short on purpose: run out to the measure and the rule stops marking
 *  a heading and becomes a divider across the page — heavier than the
 *  words it sets off, and at 87 of 88 columns too wide to centre at
 *  all, so the whole heading sat two columns right of the page. */
const RULE_ARM = 6;

function rule(label: string): WidgetSpec {
  const arm = Math.max(2, Math.min(RULE_ARM, Math.floor((measure() - label.length - 6) / 2)));
  return centred([
    { text: "─".repeat(arm) + "  ", style: { fg: C.frame } },
    { text: label, style: { fg: C.body, bold: true } },
    { text: "  " + "─".repeat(arm), style: { fg: C.frame } },
  ]);
}

function banner(level: string, sub: string): WidgetSpec {
  const mark = LEVEL_MARK[level];
  outline.push({ label: mark, key: `level:${level}`, depth: 0 });
  // Computed, not a hardcoded 40: the rule used to stop at column 64 on
  // a wide terminal and overflow the pane on a narrow one. Heavy stroke
  // so the top of the hierarchy is also the strongest horizontal.
  const tail = Math.max(3, measure() - 5 - mark.length - 1);
  return col(
    blank(),
    blank(),
    // The caption is a keyed button so the banner has an identity the
    // host can find. `scrollToWidget` then answers "take me to level 2"
    // from the panel's own hit areas — where this widget actually
    // landed — instead of the page reading its own painted text back and
    // matching this very caption as a string.
    row(
      line([{ text: "━━━━ ", style: { fg: C.frame } }]),
      button(mark, {
        key: `level:${level}`,
        bare: true,
        focusable: false,
        style: { fg: C.title, bold: true },
        hoverStyle: GLOW,
      }),
      line([{ text: " " + "━".repeat(tail), style: { fg: C.frame } }]),
    ),
    // A rule and the line under it, set tight, read as one two-line
    // heading — the description looked like a subtitle *of the rule*
    // rather than the opening sentence of the level it introduces. The
    // air is what makes the banner a banner and the sentence prose.
    blank(),
    line([{ text: "  " + sub, style: { fg: C.body } }]),
    blank(),
  );
}

// ── The wordmark ─────────────────────────────────────────────────────

const ART = [
  "███████╗██████╗ ███████╗███████╗██╗  ██╗",
  "██╔════╝██╔══██╗██╔════╝██╔════╝██║  ██║",
  "█████╗  ██████╔╝█████╗  ███████╗███████║",
  "██╔══╝  ██╔══██╗██╔══╝  ╚════██║██╔══██║",
  "██║     ██║  ██║███████╗███████║██║  ██║",
  "╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝",
];

/** ANSI-Shadow is a two-material face: `█` block faces and a `╔╗╚╝║═`
 *  bevel. Painting both in one colour flattened the mark into a slab;
 *  recessing the bevel gives it the depth the glyph set was drawn for.
 *
 *  One key for the face, one for the bevel — both resolved by the host
 *  at paint time, so switching theme repaints the mark with no help
 *  from this plugin, like every other colour on the page.
 *
 *  Two richer versions were tried and both were wrong. Interpolating a
 *  true gradient means reading the theme's JSON and emitting literal
 *  RGB: baked values stop tracking the theme, and themes like
 *  `terminal` store colour *names* rather than triplets, so there is
 *  nothing to interpolate. Banding across several accent keys keeps the
 *  host resolving them, but a ramp of *independent semantic* keys is
 *  only a ramp when a theme happens to make them hue-adjacent — on
 *  `dark` (teal → light blue → blue) it read beautifully and on `light`
 *  it went teal → navy → magenta, which is a stripe, not a light. */

function artLine(l: string): WidgetSpec {
  const segs: StyledSegment[] = [];
  let i = 0;
  while (i < l.length) {
    const face = l[i] === "█";
    let j = i;
    while (j < l.length && (l[j] === "█") === face) j++;
    segs.push({
      text: l.slice(i, j),
      style: { fg: face ? C.art : C.frame, bold: face },
    });
    i = j;
  }
  // Every row of the mark is the same width, so centring each row
  // independently keeps the mark rigid.
  return centred(segs);
}

function hero(): WidgetSpec[] {
  const wide = viewportWidth() >= 60;
  const art = wide
    ? ART.map((l) => artLine(l))
    : [centred([{ text: "fresh", style: { fg: C.art, bold: true } }])];
  const tag = viewportWidth() >= 70
    ? "A terminal text editor and IDE.  It grows when your work does."
    : "It grows when your work does.";
  return [
    // The off switch rides the top edge, right-aligned, clear of the
    // mark: a control for "I don't want this screen" belongs where
    // someone who doesn't want the screen looks first, and putting it
    // above the wordmark keeps the mark the first thing *read*.
    ...startupRow(),
    blank(),
    ...art,
    ...air(2),
    centred([{ text: tag, style: { fg: C.muted, italic: true } }]),
    blank(),
    chipsRow(),
  ];
}

/** The three chips, centred, each on its own background block.
 *
 *  A chip is a claim about the product, and a block behind it is what
 *  makes it read as a chip rather than as three more words of prose —
 *  the same job a pill does on a web page, done with the one thing a
 *  cell grid has: a background. */
function chipsRow(): WidgetSpec {
  const chips = ["single static binary", "zero configuration", "open source"];
  const segs: StyledSegment[] = [];
  chips.forEach((c, i) => {
    if (i > 0) segs.push({ text: "  ·  ", style: { fg: C.gutter } });
    segs.push({ text: ` ${c} `, style: { fg: C.mark, bg: C.chipBg } });
  });
  return centred(segs);
}

/** The startup switch, right-aligned on the page's top edge.
 *
 *  A bare button rather than a `toggle`: it draws the same `[✓]` box,
 *  but a button can say what it does under the pointer and a toggle
 *  cannot. */
function startupRow(): WidgetSpec[] {
  // The page's first row says so itself, rather than the outline naming
  // `startupToggle` from a table: a table is a string that has to match a
  // widget key defined somewhere else, and `scrollToWidget` on a key that
  // no longer exists does nothing at all — a silent first row in the
  // Contents section, which is exactly what building the outline as the
  // page is built exists to rule out.
  outline.push({ label: "Top of the page", key: "startupToggle", depth: 0 });
  const on = showOnStartup();
  const label = `${on ? "[✓]" : "[ ]"} Show this screen on startup`;
  return [
    row(
      flexSpacer(),
      button(label, {
        key: "startupToggle",
        bare: true,
        style: LINK,
        hoverStyle: HOVER_LINK,
      }),
    ),
  ];
}

// ── The three doors ──────────────────────────────────────────────────

type Door = { n: string; head: string; sub: string; body: string };

const DOORS: Door[] = [
  {
    n: "1",
    head: "[1] JUST EDIT TEXT",
    sub: "Open a file & go",
    body: "Notes, configs, huge logs. Standard keys and full mouse. Nothing to learn first.",
  },
  {
    n: "2",
    head: "[2] CLASSIC IDE",
    sub: "Code with LSP & git",
    body: "Completions, goto and hover, hunk-level diff review, splits, themes, plugins.",
  },
  {
    n: "3",
    head: "[3] ORCHESTRATE",
    sub: "Run agents in parallel",
    body: "One worktree per task. claude, codex, aider and remotes. Tour the diffs.",
  },
];

/** Greedy word wrap. The door bodies used to be hand-wrapped arrays,
 *  which set them to the width of a third of a wide terminal — and then
 *  kept that ragged 22-column column when the doors stacked full-width
 *  on a narrow one. Wrapping at render time lets one sentence set
 *  itself correctly at either size. */
function wrap(text: string, width: number): string[] {
  const out: string[] = [];
  let line = "";
  for (const w of text.split(" ")) {
    if (!line) line = w;
    else if (cols(line) + 1 + cols(w) <= width) line += " " + w;
    else {
      out.push(line);
      line = w;
    }
  }
  if (line) out.push(line);
  return out;
}

/** Display columns, not UTF-16 units.
 *
 *  `wrap` measured with `.length`, which happens to be right for ASCII
 *  and wrong for everything else — `日本語` is three units and six
 *  columns. Every wrapped string on this page was ASCII, so nothing
 *  showed it; the moment a card wrapped the translated-UI sentence it
 *  would have set that line to twice its intended width. */
function cols(s: string): number {
  try {
    return editor.stringWidth(s);
  } catch (_e) {
    return s.length;
  }
}

/** Inner text width of a framed card.
 *
 *  Measured against a real render rather than reasoned about, because
 *  the budget is spent in three places and it is easy to miss one: the
 *  card's own two border columns, the section's one column of padding
 *  on each side, and the two-space indent each body row carries in its
 *  own string. A 49-column card therefore sets its prose to 43. Getting
 *  this two columns wrong does not misalign anything — it clips the
 *  tail of the line and eats the words. */
function cardTextWidth(): number {
  return Math.max(16, cardMeasure() - 6);
}

/** Prose inside a framed card, wrapped to the card at render time.
 *
 *  These bodies were hand-split into two string literals set to a wide
 *  card's width. A card narrower than that does not re-flow them — the
 *  host clips each row at the border, so the tail of the first line is
 *  replaced by `…` and those words are simply gone from the page. At 60
 *  columns the git card lost "by-side diff, review"; the theme card
 *  rendered "Configurable st…", destroying "status". Wrapping to the
 *  card's own width is what the doors have always done. */
function bodyText(text: string, fg: string = C.body): WidgetSpec[] {
  return wrap(text, cardTextWidth()).map((l) => plain("  " + l, fg));
}

/** Inner text width of one door, at whichever layout is in force. */
function doorTextWidth(): number {
  const wide = viewportWidth() >= 96;
  return wide
    ? Math.max(12, Math.floor(measure() / 3) - 5)
    : Math.max(12, measure() - 8);
}

/** Every interior row of a door is a full-width bare button carrying the
 *  card's own key. Two things follow: a click anywhere inside the card
 *  jumps to its level (a `labeledSection` emits no hit of its own, so
 *  the frame alone could never be routed), and the hover highlight —
 *  which the renderer applies to *every* widget sharing the hovered key
 *  — lights the whole card at once instead of one line of it. */
function doorRow(d: Door, label: string, verb = false): WidgetSpec {
  return row(
    button(label, {
      key: `jump:${d.n}`,
      bare: true,
      // Full width so the whole card is one target, which is also why
      // nothing in it is underlined: the mark would run the padding.
      fullWidth: true,
      // One Tab stop per card, on the row that names the action.
      focusable: verb,
      hoverStyle: GLOW,
    }),
  );
}

function doorCard(d: Door, rows: number): WidgetSpec {
  const body = wrap(d.body, doorTextWidth());
  const pad = Math.max(0, rows - body.length);
  return labeledSection({
    label: d.head,
    // The card's own key is the key of the control filling it, so the
    // frame and its legend light with the rows rather than watching
    // them light. A section is never itself hovered — it emits no hit.
    key: `jump:${d.n}`,
    hoverStyle: GLOW,
    // `widthPct` applies only to a Block child of a Row, and stacked
    // doors are wrapped in one for exactly that reason: a section left
    // to fill a `Col` takes the whole PANEL width, which on a pane a
    // little wider than the measure is wider than the compose area the
    // host clips to — and the card's top border wrapped.
    // Exact columns, so three doors and two gutters add up to the
    // measure. As an integer percent this did not divide: rounding up
    // overflowed the panel and the host wrapped the third card onto a
    // line of its own, rounding down left three columns of slack that
    // all landed on the right.
    widthCols: viewportWidth() >= 96
      ? Math.floor((measure() - DOOR_GAP * 2) / 3)
      : measure(),
    child: col(
      doorRow(d, d.sub),
      doorRow(d, " "),
      ...body.map((b) => doorRow(d, b)),
      // `labeledSection` sizes to its own child, so an uneven row of
      // doors closes its boxes at different rows and reads as broken
      // rather than as three peers. Pad to the tallest.
      ...Array.from({ length: pad }, () => doorRow(d, " ")),
      doorRow(d, " "),
      // The one focusable stop in the card, so Tab advances a card at a
      // time. The digit lives here rather than flushed right: a
      // full-width button pads its own label, so there is no column to
      // align a second fragment to.
      doorRow(d, "jump ↓ · click, or Enter", true),
    ),
  });
}

/** Columns between the doors, taken out of the cards rather than added
 *  to the row: the three still have to add up to the measure. Three
 *  boxes sharing a wall read as one grid; separated they read as three
 *  peers, which is what they are. */
const DOOR_GAP = 2;

function doors(): WidgetSpec[] {
  const wide = viewportWidth() >= 96;
  const w = doorTextWidth();
  const rows = Math.max(...DOORS.map((d) => wrap(d.body, w).length));
  const cards = DOORS.map((d) => doorCard(d, rows));
  return [
    ...air(3),
    // A heading between two rules, centred. The words alone, dim and at
    // the margin, read as one more line of prose; a rule through them is
    // what makes the page break here.
    rule("WHAT BRINGS YOU HERE?"),
    blank(),
    wide
      ? centredRow(cards[0], spacer(DOOR_GAP), cards[1], spacer(DOOR_GAP), cards[2])
      : col(...cards.map((c) => centredRow(c))),
  ];
}

// ── Level 1 ──────────────────────────────────────────────────────────

function fuzzy(query: string, s: string): boolean {
  if (!query) return true;
  const q = query.toLowerCase();
  const t = s.toLowerCase();
  let qi = 0;
  for (let i = 0; i < t.length && qi < q.length; i++) {
    if (t[i] === q[qi]) qi++;
  }
  return qi === q.length;
}

function recomputeHits(): void {
  const files = repoFiles ?? [];
  const out: FinderItem[] = [];
  for (const f of files) {
    if (fuzzy(finderQuery, f)) {
      out.push({ path: f });
      if (out.length >= 200) break;
    }
  }
  finderHits = out;
  if (finderIndex >= finderHits.length) finderIndex = 0;
}

function finderCard(): WidgetSpec {
  return card("finder", "Pick up where you left off", "live — type in it", () => {
    const rows: WidgetSpec[] = [
      blank(),
      textInput(finderQuery, {
        key: "finderField",
        cursorByte: finderCursor,
        label: " find",
        fullWidth: true,
      }),
      blank(),
    ];
    if (repoFiles === null) {
      rows.push(plain(repoFilesLoading ? "  scanning…" : "  not a git repo — Ctrl+P finds files anywhere", C.muted));
    } else if (finderHits.length === 0) {
      rows.push(plain("  no match", C.muted));
    } else {
      // Rendered as rows rather than a List widget: a List's items are
      // emitted at their natural width and the enclosing section's
      // right border cannot be reached from inside one, so every row
      // ended in a `…` clip marker where the frame should be. `raw`
      // rows are padded to the section by the host, so the card stays a
      // card. Selection is ours to track either way — `finderIndex` was
      // already the model.
      // Window the visible slice around the selection. The list drew
      // hits 0..5 unconditionally, so walking the selection past the
      // sixth moved a marker that was never on screen — the list did
      // not follow it, and `Enter` then opened a file the reader could
      // not see. `i` stays the ABSOLUTE index so `hit:<i>` keeps naming
      // the same result a click means.
      const shown = Math.min(finderHits.length, FINDER_ROWS);
      const start = Math.max(
        0,
        Math.min(
          finderIndex - FINDER_ROWS + 1 > 0 ? finderIndex - FINDER_ROWS + 1 : 0,
          finderHits.length - FINDER_ROWS,
        ),
      );
      if (start > 0) {
        rows.push(plain(`    … ${start} above`, C.muted));
      }
      for (let i = start; i < start + shown && i < finderHits.length; i++) {
        const h = finderHits[i];
        const on = i === finderIndex;
        // One full-width button per result, marker and all — not a
        // marker beside a button sized to the path.
        //
        // A result you can see is a result you should be able to click,
        // and a row that looks like a target across its whole width has
        // to behave like one. Hit areas are the control's own cells, so
        // the path-width button was clickable only on its glyphs: a
        // click in the space right of a short path did nothing. The
        // marker could not simply sit outside a `fullWidth` sibling
        // either — an inline child of a `row` is laid out at the FULL
        // panel width, so the padded button would have started after
        // the marker and run past the card's right border.
        //
        // Which settles the underline question the other way for these
        // rows. `LINK` marks a word that is a link, and its underline
        // runs every cell the button owns — including `fullWidth`
        // padding, so an underlined full-width row paints a rule across
        // the card. A result row is a row of a list, not a link in
        // prose: it is marked by being a row with a selection marker,
        // and it lights under the pointer the way a menu entry does.
        // `focusable: false` so Tab still makes one stop at this card
        // rather than six.
        rows.push(
          button((on ? "   ▸ " : "     ") + h.path, {
            key: `hit:${i}`,
            bare: true,
            fullWidth: true,
            focusable: false,
            style: on ? SELECTED_ROW : undefined,
            hoverStyle: HOVER_ROW,
          }),
        );
      }
      const below = finderHits.length - (start + shown);
      if (below > 0) {
        rows.push(plain(`    … and ${below} more`, C.muted));
      }
    }
    rows.push(blank());
    // Wrapped to the card, not hand-broken to the page: these two lines
    // were split by hand at the old full-measure width, so narrowing the
    // frame clipped the first one mid-word.
    for (
      const l of wrap(
        "Fresh remembers your cursor position in every file. Hot Exit restores unsaved buffers after a crash — even unnamed scratch ones.",
        Math.max(20, cardMeasure() - 6),
      )
    ) {
      rows.push(plain("  " + l, C.body));
    }
    rows.push(blank());
    return rows;
  }, true);
}

function level1(): WidgetSpec[] {
  return [
    banner("1", "Open a file. Type. Save. Fresh stays out of the way."),
    finderCard(),
    blank(),
    card("ugly", "Built for the ugly files too", "big files, odd encodings", () => [
      blank(),
      bullet("Multi-GB files open without blocking the UI — logs, dumps, CSVs."),
      bullet("Instant startup; text appears as you type. Small memory footprint."),
      bullet("Encodings beyond UTF-8: UTF-16, GBK, Shift-JIS, Latin-1 and more."),
      bullet("Project-wide search & replace with regex — even across unsaved buffers."),
      blank(),
    ]),
    blank(),
    card("editorvar", "Make it your $EDITOR", "shell setup", () => [
      blank(),
      plain("  # Use Fresh for commit messages and rebases", C.muted),
      plain("  git config --global core.editor \"fresh --wait\"", C.value),
      blank(),
      plain("  # Keep a project session alive across terminal disconnects", C.muted),
      plain("  fresh -a myproject", C.value),
      blank(),
    ]),
  ];
}

// ── Level 2 ──────────────────────────────────────────────────────────

const SAMPLE_CODE = [
  "  pub struct UserStore {",
  "      users: HashMap<u64, User>,",
  "  }",
  "",
  "  impl UserStore {",
  "      pub fn active_users(&self) -> impl Iterator<Item = &User> {",
  "          self.users.values().filter(|u| u.is_active)",
  "      }",
  "  }",
];

/** The same file, short enough for a narrow box. A code block does not
 *  truncate — it *wraps*, and a wrapped line eats a row of a fixed-height
 *  widget, so a sample too wide for its box loses its own tail. */
const SAMPLE_CODE_NARROW = [
  "pub struct UserStore {",
  "    users: HashMap<u64, User>,",
  "}",
];

/** Columns the sample actually has inside its box: the measure, less
 *  the two-column inset, the section's own border and padding, and the
 *  column the panel keeps for a scrollbar. Measured against the drawn
 *  box rather than derived — a guess here shows up as a wrapped line. */
function sampleWidth(): number {
  return Math.max(16, measure() - 11);
}

function sampleLines(): string[] {
  const w = sampleWidth();
  const longest = Math.max(...SAMPLE_CODE.map((l) => l.length));
  return longest + 1 <= w ? SAMPLE_CODE : SAMPLE_CODE_NARROW;
}

/** The sample, padded to a rectangle.
 *
 *  A markdown code block paints its background over the text it has and
 *  no further, so a ragged sample renders as a ragged grey shape rather
 *  than as a block of code. Trailing spaces inside the fence become
 *  NBSP and carry the background with them, which is what squares it
 *  off. (Leading spaces do the same, which is why the page margin must
 *  never be written into the sample: it painted a slab of code
 *  background across the whole margin.) */
function sample(): string {
  const lines = sampleLines();
  const longest = Math.max(...lines.map((l) => l.length));
  const w = Math.min(longest + 2, sampleWidth());
  const body = lines.map((l) => (l.length >= w ? l : l + " ".repeat(w - l.length)));
  return ["```rust", ...body, "```"].join("\n");
}

function level2(): WidgetSpec[] {
  return [
    banner("2", "Language servers, git review, themes — here the whole time, waiting."),
    card("lsp", "Language smarts, zero setup", "real syntax highlighting", () => [
      blank(),
      // The sample sits in its own rounded box, labelled with the file
      // it is pretending to be, and inset from the prose around it — a
      // listing, not a paragraph.
      //
      // The margin goes AROUND the widget, never inside its text: the
      // markdown renderer turns leading spaces in a code fence into
      // NBSP and paints the code background across them, so an indent
      // written into the sample became a grey slab the width of the
      // whole left margin. (The background inside the box is the
      // host's `ui.inline_code_bg`, shared with every hover popup and
      // the markdown preview, so it is not this page's to switch off.)
      row(
        spacer(2),
        labeledSection({
          label: "src/store.rs",
          widthCols: measure() - 4,
          child: text({
            value: sample(),
            rows: sampleLines().length,
            markdown: true,
            readOnly: true,
            fieldWidth: sampleWidth(),
            // Deliberately keyless: a keyed widget joins the Tab cycle,
            // and a read-only sample is something to look at, not a
            // stop on the way to the next control.
          }),
        }),
      ),
      blank(),
      bullet(
        "Open a file and the language server starts itself. Hover, goto, references, rename, code actions and diagnostics, with no setup.",
      ),
      bullet("Configs shipped for Python, TypeScript, Rust, Go, Java, C/C++ and more."),
      bullet("Run multiple servers per language with merged completions."),
      blank(),
    ]),
    blank(),
    gitCard(),
    blank(),
    reviewCard(),
    blank(),
    themeCard(),
    blank(),
    card("power", "Power tools when your hands get fast", "optional, all of it", () => [
      blank(),
      bullet("Multi-cursor and block selection, keyboard macros, sort lines."),
      bullet("Command palette with prefix routing: > commands · # buffers · : lines."),
      bullet("Vi mode with operators, motions and text objects — if that's your thing."),
      bullet("TypeScript plugins, sandboxed in QuickJS. No node_modules on disk."),
      bullet("Tabs, split panes, integrated terminal, markdown preview."),
      blank(),
    ]),
  ];
}

function gitCard(): WidgetSpec {
  return card("git", "Review your diff before it reviews you", "your working tree", () => {
    const rows: WidgetSpec[] = [blank()];
    if (!gitProbed) {
      rows.push(plain("  reading git status…", C.muted));
    } else if (!gitBranch && gitDirty.length === 0) {
      rows.push(...bodyText("not a git repo — open one and this card fills in.", C.muted));
    } else {
      rows.push(
        line([
          { text: "  on ", style: { fg: C.muted } },
          { text: gitBranch || "(detached)", style: { fg: C.value } },
          { text: "   ", style: {} },
          {
            text: gitDirty.length === 0 ? "working tree clean" : `${gitDirty.length} changed`,
            style: { fg: gitDirty.length === 0 ? C.ok : C.body },
          },
        ]),
      );
      rows.push(blank());
      for (const f of gitDirty.slice(0, 6)) {
        rows.push(
          line([
            { text: "   " + f.slice(0, 2), style: { fg: statusFg(f.slice(0, 2)) } },
            { text: " " + f.slice(3), style: { fg: C.body } },
          ]),
        );
      }
      if (gitDirty.length > 6) {
        rows.push(plain(`   … and ${gitDirty.length - 6} more`, C.muted));
      }
    }
    rows.push(blank());
    rows.push(
      row(
        spacer(2),
        button("PR branch log", { key: "act_review", hoverStyle: GLOW }),
        spacer(2),
        button("Git log", { key: "act_gitlog", hoverStyle: GLOW }),
      ),
    );
    rows.push(blank());
    rows.push(
      ...bodyText(
        "Hunk-level stage / unstage / discard. Side-by-side diff, review notes, git gutter, git grep.",
      ),
    );
    rows.push(blank());
    return rows;
  }, true);
}

/** Two columns: a key chord and what it does. The keys sit in a column
 *  of their own so the eye can run down them — which is how a key table
 *  is read, and is not how prose is read. */
function keyRows(pairs: [string, string][]): WidgetSpec[] {
  const width = Math.max(...pairs.map(([k]) => cols(k)));
  return pairs.map(([k, what]) =>
    line([
      { text: "  " },
      { text: k + " ".repeat(Math.max(0, width - cols(k))), style: { fg: C.key, bold: true } },
      { text: "  " + what, style: { fg: C.body } },
    ])
  );
}

/** Review Diff — the code-review tool, which is a different thing from
 *  the git card above it and is why that card stops where it does.
 *
 *  Every key here is one `audit_mode.ts` binds in `review-mode`, and
 *  every command named is one it registers. A welcome screen that
 *  teaches a chord the editor does not have is worse than one that
 *  teaches nothing. */
function reviewCard(): WidgetSpec {
  return card("review", "Review Diff — read a change like a reviewer", "a tool, not a view", () => [
    blank(),
    ...bodyText(
      "Your whole working tree as one diff you move through, stage from, and leave notes on. "
        + "The palette opens it three ways: on the working tree, on a range or branch, or on a stash.",
    ),
    blank(),
    ...keyRows([
      ["n / p", "next / previous hunk"],
      [", / .", "previous / next file"],
      ["1 2 0", "one column · side by side · auto"],
      ["F / C", "the files and comments panels (both start hidden)"],
      ["Tab", "move between panels; arrows act on the focused one"],
    ]),
    blank(),
    ...bodyText("Stage what you have read, hunk by hunk, without leaving the diff:", C.muted),
    ...keyRows([
      ["s u d", "stage · unstage · discard the hunk under the cursor"],
      ["S U D", "the same, for the whole file"],
      ["v", "select a line range first, and act on just those lines"],
    ]),
    blank(),
    ...bodyText("And leave the review behind you:", C.muted),
    ...keyRows([
      ["c", "comment on the line under the cursor"],
      ["] / [", "walk the comments you have left"],
      ["e", "export the session as Markdown"],
    ]),
    blank(),
    // One button, and only one that means anything from here: the
    // working tree is a thing this page can point at, and "this file" is
    // not — the current file, pressed from the welcome screen, is the
    // welcome screen. The other two openings are palette commands, and
    // the prose above names them.
    row(
      spacer(2),
      button("Review the working tree", { key: "act_review_diff", hoverStyle: GLOW }),
    ),
    blank(),
  ], true);
}

function themeCard(): WidgetSpec {
  return card("themes", "Make it yours", "restyles the editor, live", () => {
    const buttons: WidgetSpec[] = [spacer(2)];
    for (const name of themeNames.slice(0, 6)) {
      buttons.push(
        line([{
          text: name === activeTheme ? "● " : "  ",
          style: { fg: C.accent },
        }]),
        button(name, {
          key: `theme:${name}`,
          bare: true,
          style: LINK,
          hoverStyle: HOVER_LINK,
        }),
        spacer(2),
      );
    }
    return [
      blank(),
      row(...buttons),
      blank(),
      ...bodyText(
        "Live theme editor with \"Inspect Theme at Cursor\". Configurable status bar. "
          + "UI translated to 日本語, 한국어, 中文, Tiếng Việt and more.",
      ),
      blank(),
    ];
  }, true);
}

// ── Level 3 ──────────────────────────────────────────────────────────

/** Idle is a healthy state. It used to be painted `syntax.constant`
 *  amber, which told the reader something was wrong. */
function stateGlyph(w: Workspace): { text: string; fg: string } {
  if (w.kind === "discovered") return { text: "○", fg: C.muted };
  if (w.agentState === "working") return { text: "●", fg: C.ok };
  return { text: "◐", fg: C.muted };
}

function orchestratorCard(): WidgetSpec {
  return card("orch", "The Orchestrator dock", "your workspaces", () => {
    const rows: WidgetSpec[] = [blank()];
    if (workspaces.length === 0) {
      rows.push(...bodyText("No workspaces yet. Cut one and an agent starts inside it.", C.muted));
    } else {
      for (const w of workspaces.slice(0, 6)) {
        const g = stateGlyph(w);
        rows.push(
          row(
            spacer(2),
            line([{ text: g.text, style: { fg: g.fg } }]),
            spacer(1),
            button(w.name, {
              key: `ws:${w.windowId}`,
              bare: true,
              style: LINK,
              hoverStyle: HOVER_LINK,
            }),
            flexSpacer(),
            line([{ text: w.branch, style: { fg: C.muted } }]),
            spacer(2),
          ),
        );
      }
      rows.push(blank());
      rows.push(
        line([
          { text: "  ● working   ", style: { fg: C.ok } },
          { text: "◐ idle   ", style: { fg: C.muted } },
          { text: "○ discovered worktree", style: { fg: C.muted } },
        ]),
      );
    }
    rows.push(blank());
    // The first two are the Orchestrator's own handlers. Offer them only
    // when that plugin is actually loaded — a button whose action no
    // plugin defines fails silently in the log, which is worse than an
    // absent button. `Open the dock` is a built-in action and always
    // holds.
    const orchLoaded = editor.getPluginApi("orchestrator") !== null;
    const actions: WidgetSpec[] = [spacer(2)];
    if (orchLoaded) {
      actions.push(
        button("New workspace…", { key: "act_ws_new", hoverStyle: GLOW }),
        spacer(2),
        button("Run agent here…", { key: "act_ws_agent", hoverStyle: GLOW }),
        spacer(2),
      );
    }
    actions.push(button("Open the dock", { key: "act_ws_dock", hoverStyle: GLOW }));
    rows.push(row(...actions));
    rows.push(blank());
    rows.push(
      ...bodyText(
        "One workspace per git worktree, each with its own terminals and agent. "
          + "Sessions resume after a restart. Leave the rest running.",
      ),
    );
    rows.push(blank());
    return rows;
  }, true);
}

function level3(): WidgetSpec[] {
  return [
    banner("3", "One workspace per git worktree. An agent in each. Hop with an arrow key."),
    orchestratorCard(),
    blank(),
    card("remote", "Your other machines are workspaces too", "SSH + detachable daemon", () => [
      blank(),
      plain("  # Edit nginx config on prod — saves transfer only the patch", C.muted),
      plain("  fresh deploy@prod:/etc/nginx/nginx.conf", C.value),
      blank(),
      plain("  # Open a file in an already-running daemon", C.muted),
      plain("  fresh --cmd daemon open-file myproject src/main.rs:42", C.value),
      blank(),
    ]),
  ];
}

// ── Footer ───────────────────────────────────────────────────────────

function footer(): WidgetSpec[] {
  return [
    blank(),
    // Not `divider`, which rules the whole pane: at this width the page
    // ended with a line twice as long as anything above it.
    // Ruled to the cards' own edges, not inset from them.
    line([{ text: "─".repeat(Math.max(4, measure() - 2)), style: { fg: C.frame } }]),
    blank(),
    plain("  That's the whole ladder. Most days you'll live on rung one — the rest", C.value),
    plain("  keeps up when you climb.", C.value),
    blank(),
  ];
}

// ── Assembly ─────────────────────────────────────────────────────────

/** The page's text column. Long enough for the longest hand-wrapped
 *  line plus air, capped so a very wide terminal doesn't stretch a
 *  paragraph across the room. Without it every card was a 147-column
 *  box around 70 columns of text, and nothing could look composed. */
const MEASURE = 88;

function measure(): number {
  // Less two: a rule computed to exactly the viewport width wraps, and a
  // wrapped rule is a broken one. `raw` rows flow through at their own
  // width, so the pane is the only backstop.
  return Math.min(Math.max(20, viewportWidth() - 2), MEASURE);
}


/** Hand the page's column to the host's own page-view machinery.
 *
 *  `setLayoutHints({ composeWidth })` is what markdown compose mode
 *  uses: the host centres the render area to that width and paints the
 *  flanking margins as paper-on-desk — `ui.compose_margin_bg` outside,
 *  a one-column paper edge inside. Doing it here rather than padding
 *  every row means the margin is a real margin: the code sample no
 *  longer carries a slab of its own background across it (the markdown
 *  renderer turns leading spaces in a fence into NBSP and paints the
 *  code background over them), and the panel never has to know where
 *  the page sits in the pane.
 *
 *  One column of slack over the measure, so the panel the host builds
 *  from this hint is exactly `measure()` wide.
 *
 *  It asked for two, which was right when `widget_panel_width` took the
 *  full gutter while composing and stale the moment that was fixed to
 *  hold back a single column. The leftover column was the page's lean:
 *  the panel came out one wider than the measure, so rows the page
 *  emits at its own width sat at the panel's left edge while blocks the
 *  layout engine centred picked up the spare column — the level rules
 *  and the door cards disagreeing with each other by one column, on the
 *  same page. Matching the hint to what the host actually reserves
 *  leaves no slack for the two to disagree about.
 *
 *  Below the cap the hint is dropped — the host skips composing when
 *  the width it is given is not narrower than the pane, but saying so
 *  plainly keeps a resize from leaving a stale hint behind. */
function applyComposeWidth(): void {
  if (bufferId === null) return;
  const w = measure() + 1;
  editor.setLayoutHints(
    bufferId,
    null,
    w < viewportWidth() ? { composeWidth: w } : {},
  );
}

/** The width of *this page's* pane.
 *
 *  `getViewport()` reports the **active split**, which is this buffer
 *  only while it holds focus. Split the window and work in the other
 *  pane and every measurement here was taken from that pane instead —
 *  the page laid itself out to a width it does not occupy. The
 *  `viewport_changed` payload carries the right number, addressed to
 *  this buffer, so it is cached on the way past and preferred; the
 *  active-split reading is the fallback for the first render, before
 *  any resize has been reported. */
let paneWidth = 0;

function viewportWidth(): number {
  if (paneWidth > 0) return paneWidth;
  const vp = editor.getViewport();
  return vp && vp.width > 0 ? vp.width : 100;
}

function buildSpec(): WidgetSpec {
  return col(
    ...hero(),
    ...doors(),
    ...air(2),
    ...uiFeatures(),
    ...air(2),
    ...verbs(),
    ...air(2),
    // It teaches keybindings, so the keys should look like keys — the
    // verbs two lines above already paint theirs in `ui.help_key_fg`.
    // And no box: a frame is an alert shape, which is the wrong shape
    // for the one message on the page whose job is to lower a pulse.
    centred([
      { text: "Nothing to learn first. It works like you'd expect: ", style: { fg: C.body } },
      { text: "Ctrl+S", style: { fg: C.key, bold: true } },
      { text: " saves,", style: { fg: C.body } },
    ]),
    centred([
      { text: "Ctrl+Z", style: { fg: C.key, bold: true } },
      { text: " undoes, ", style: { fg: C.body } },
      { text: "Ctrl+F", style: { fg: C.key, bold: true } },
      { text: " finds, ", style: { fg: C.body } },
      { text: "Ctrl+C/V", style: { fg: C.key, bold: true } },
      { text: " copy-paste — and the mouse just works.", style: { fg: C.body } },
    ]),
    centred([{ text: "Click, drag, scroll, select.", style: { fg: C.body } }]),
    ...air(2),
    // The scroll hint carries `Esc` because this is the page's one line
    // about what else you can do with it, and closing was the only thing
    // the page did that it never said it did. `Esc` on a document closes
    // nothing anywhere else in the editor, so a reader had no way to
    // learn it but to lose the page by accident — and no way to know
    // that losing it is undoable. Both halves are on the line now.
    centred([
      { text: "▼ ", style: { fg: C.mark } },
      { text: "scroll — the rest is here when you need it", style: { fg: C.muted } },
    ]),
    centred([
      { text: "Esc", style: { fg: C.key, bold: true } },
      { text: " closes this page · ", style: { fg: C.muted } },
      { text: "Welcome", style: { fg: C.key, bold: true } },
      { text: " in the palette brings it back", style: { fg: C.muted } },
    ]),
    ...level1(),
    ...level2(),
    ...level3(),
    ...footer(),
  );
}

/** Re-paint the page.
 *
 *  A panel repaint keeps the pane's scroll position, so there is
 *  nothing to save and restore here. There used to be: a capture of
 *  the top line and a `scrollTopTo` after the repaint. It was not only
 *  unnecessary, it was the one thing on the page that scrolled by
 *  itself — the restore travels through the host's *reveal* path,
 *  which lands a line off, so every keystroke in the finder walked the
 *  whole document up the screen. */
function render(): void {
  if (!panel) return;
  outline = [];
  panel.set(buildSpec());
  publishContents();
}

// ── The Contents section ─────────────────────────────────────────────
//
// The page is long, and a long document in this editor gets an outline
// in the sidebar — that is what Markdown files do (`markdown_toc.ts`,
// the first consumer of the sidebar-sections API). This page is a
// document; it gets the same thing, from the same API, and the button
// on the page is there because a sidebar section can be closed, scrolled
// past, or hidden behind the explorer, and a reader who cannot see it
// has no way to ask for it back.
//
// Its own id, distinct from the page's mounted panel: `widget_event`
// carries the panel id, and both arrive at the same hook.

const CONTENTS_PANEL_ID = 7301;
let contentsMounted = false;

/** Rows to ask the sidebar for: every entry, within reason. The section
 *  is a fixed, known-length outline rather than a file's headings, so
 *  there is a right answer and it is "all of it" — capped so the
 *  explorer above it keeps a workable share of the column. */
function contentsRows(): number {
  return Math.max(6, Math.min(outline.length, 16));
}

/** Where the marked key sits in the outline as it stands now — the one
 *  place an index is computed, at the moment it is used. */
function outlineIndex(): number {
  return outline.findIndex((e) => e.key === outlineSelected);
}

function contentsSpec(): WidgetSpec {
  return tree({
    key: "toc",
    nodes: outline.map((e) => ({
      // One column for "you are here". A tree paints its selection band
      // only while it has the keyboard, and this section spends its life
      // blurred — so without a glyph in the text it would say nothing at
      // all about where in the page the reader is, which is half of what
      // an outline is for.
      text: { text: (e.key === outlineSelected ? "▸ " : "  ") + e.label },
      depth: e.depth,
      hasChildren: false,
    })),
    // The widget key each row goes to. It is already unique per row and
    // it is the thing the row *means*, so there is nothing to look up
    // on the way back: the activation handler scrolls to the key it was
    // handed.
    itemKeys: outline.map((e) => e.key),
    selectedIndex: outlineIndex(),
    // Nothing folds: the outline is three levels of cards, and a
    // disclosure triangle on every level would be two states to keep for
    // a list that fits.
    expandedKeys: [],
    itemHeight: 1,
    cardBorders: false,
    // The sidebar is a couple of dozen columns wide; one column per
    // level is enough to read the nesting.
    indentCols: 1,
  });
}

/** Publish the outline. Mounts the section the first time and replaces
 *  its content after that — `mountSidebarSection` on an id that is
 *  already a section would replace it in place too, but a remount is
 *  also a re-*mount*, and this page repaints on every keystroke in the
 *  finder. */
function publishContents(): void {
  if (!contentsMounted) return;
  editor.updateFloatingWidget(CONTENTS_PANEL_ID, contentsSpec());
  editor.widgetMutate(CONTENTS_PANEL_ID, {
    kind: "setSelectedIndex",
    widgetKey: "toc",
    index: outlineIndex(),
  });
}

/** Put the section in the sidebar — quietly, unless `focus` says the
 *  reader asked for it.
 *
 *  `startBlurred` is not a detail here. Mounting a section used to open
 *  the sidebar *column* whatever the mount said, and the file explorer
 *  came up with it: opening this page took thirty-five columns off a
 *  hundred-and-twenty-column terminal on every launch, from someone who
 *  had deliberately closed the sidebar. It also stole them from a page
 *  that had already composed against the full width, so the three doors
 *  fell out of their row and every row wrapped.
 *
 *  A blurred mount is silent now (`Editor::reveal_sidebar`): the outline
 *  is there for a reader whose sidebar is open, and waits in a hidden
 *  column for one whose is not. The `Contents` button is what opens it —
 *  which is why the button exists at all, since a section can be closed,
 *  collapsed, or buried under the explorer. */
function mountContents(focus: boolean): void {
  if (!contentsMounted) {
    editor.mountSidebarSection(
      CONTENTS_PANEL_ID,
      contentsSpec(),
      "Welcome — Contents",
      contentsRows(),
      { closable: true, startBlurred: true },
    );
    contentsMounted = true;
  } else {
    publishContents();
  }
  if (focus) editor.floatingPanelControl(CONTENTS_PANEL_ID, "focus", 0);
}

function unmountContents(): void {
  if (!contentsMounted) return;
  editor.unmountFloatingWidget(CONTENTS_PANEL_ID);
  contentsMounted = false;
}

/** Mark the outline entry for a heading that just took focus.
 *
 *  Anything else — a button in a card's body, a theme swatch, the
 *  finder's field — leaves the mark where it is: those are *inside* the
 *  section the mark already names. */
function markOutlineFor(widgetKey: string): void {
  if (!contentsMounted) return;
  if (!widgetKey.startsWith("fold:") && !widgetKey.startsWith("level:")) return;
  if (widgetKey === outlineSelected) return;
  if (!outline.some((e) => e.key === widgetKey)) return;
  outlineSelected = widgetKey;
  // Both, and both are needed. `setSelectedIndex` is the *selection* —
  // where an arrow key would start if the reader put the keyboard in
  // here — and a blurred tree does not paint one, which is most of the
  // time: the whole point is to say where you are while you read the
  // page, not while you drive the sidebar. The caret glyph in the label
  // is what actually shows on a blurred section, so the spec goes too.
  editor.widgetMutate(CONTENTS_PANEL_ID, {
    kind: "setSelectedIndex",
    widgetKey: "toc",
    index: outlineIndex(),
  });
  editor.updateFloatingWidget(CONTENTS_PANEL_ID, contentsSpec());
}



// ── Data probes ──────────────────────────────────────────────────────

async function probeRepoFiles(): Promise<void> {
  if (repoFilesLoading) return;
  repoFilesLoading = true;
  try {
    const res = await editor.spawnProcess("git", ["ls-files"], editor.getCwd());
    if (res.exit_code === 0) {
      repoFiles = res.stdout.split("\n").filter((l) => l.length > 0).slice(0, 5000);
      recomputeHits();
    } else {
      repoFiles = null;
    }
  } catch (_e) {
    repoFiles = null;
  }
  repoFilesLoading = false;
  render();
}

async function probeGit(): Promise<void> {
  try {
    const st = await editor.spawnProcess("git", ["status", "--porcelain", "-b"], editor.getCwd());
    if (st.exit_code === 0) {
      const lines = st.stdout.split("\n").filter((l) => l.length > 0);
      const head = lines.find((l) => l.startsWith("## "));
      if (head) {
        gitBranch = head.slice(3).split("...")[0].split(" ")[0];
      }
      gitDirty = lines.filter((l) => !l.startsWith("## "));
    }
  } catch (_e) {
    // Not a repo, or no git on PATH: the card says so.
  }
  gitProbed = true;
  render();
}

/** `getAllThemes()` answers with the registry *object* — canonical key
 *  to theme data — not a list, so the names are its keys. Builtins are
 *  asked for separately and put first: they are the ones every install
 *  has, which is what a welcome screen should offer. */
function probeThemes(): void {
  const names: string[] = [];
  const push = (v: unknown) => {
    if (v && typeof v === "object") {
      for (const k of Object.keys(v as Record<string, unknown>)) {
        if (k.length > 0 && !k.startsWith("_") && !names.includes(k)) names.push(k);
      }
    }
  };
  try {
    push(editor.getBuiltinThemes());
    push(editor.getAllThemes());
  } catch (_e) {
    // A theme registry we can't read just means no swatches on the card.
  }
  themeNames = names;
}

type OrchestratorApi = {
  listWorkspaces?: () => Array<Record<string, unknown>>;
  focusWorkspace?: (id: number) => unknown;
};

function probeWorkspaces(): void {
  try {
    const api = editor.getPluginApi("orchestrator") as OrchestratorApi | null;
    if (!api?.listWorkspaces) return;
    const rows = api.listWorkspaces();
    workspaces = rows.map((r) => ({
      name: String(r.name ?? ""),
      branch: String(r.branch ?? ""),
      agentState: String(r.agentState ?? "idle"),
      kind: String(r.kind ?? "live"),
      active: r.active === true,
      windowId: typeof r.windowId === "number" ? r.windowId : 0,
    }));
  } catch (_e) {
    workspaces = [];
  }
}

// ── Config ───────────────────────────────────────────────────────────

editor.defineConfigBoolean("showOnStartup", {
  default: true,
  description: "Open the welcome screen when Fresh starts, as a tab behind whatever is already open.",
});

/** The footer toggle writes plugin global state, which persists across
 *  restarts; the declared config field is the fallback, so the Settings
 *  UI still owns the setting for anyone who never touches the toggle.
 *  Same precedence the dashboard uses for its own auto-open override. */
function showOnStartup(): boolean {
  const override = editor.getGlobalState("showOnStartup");
  if (typeof override === "boolean") return override;
  const cfg = (editor.getPluginConfig() ?? {}) as { showOnStartup?: boolean };
  return cfg.showOnStartup !== false;
}

// ── Lifecycle ────────────────────────────────────────────────────────

/** `force` is the reader asking for the page by name — the `Welcome`
 *  command — which both overrides `showOnStartup` and brings the page to
 *  the front. Startup passes `false`: the page is created as a tab
 *  behind whatever is already open and nothing else about the workspace
 *  is consulted — not how many buffers there are, not what the host's
 *  untitled seed means, not any other setting. One concern: open at
 *  startup, or don't. */
async function openWelcome(force: boolean): Promise<void> {
  if (bufferId !== null) {
    editor.showBuffer(bufferId);
    return;
  }
  if (opening) return;
  if (!force && !showOnStartup()) return;
  opening = true;
  readActiveTheme();
  try {
    const res = await editor.createVirtualBuffer({
      name: "Welcome",
      mode: "welcome",
      readOnly: true,
      showLineNumbers: false,
      // A real cursor. It used to be hidden, and hiding it cost far
      // more than it saved: `show_cursors: false` blocks the native
      // movement actions, so this plugin had to reimplement scrolling —
      // its own model of the top line, a reveal that compensated for
      // the host's, page keys computed by hand, and a ceiling so
      // holding Down past the end didn't buy dead Up presses. All of
      // that is the host's job, and it does it correctly. The caret
      // also answers a question the page was posing without meaning
      // to: clicks still seated an invisible cursor, so clicking a word
      // lit its occurrences and the reader could see no reason why.
      showCursors: true,
      editingDisabled: true,
      // The caret's row means nothing on a page laid out by widgets, and a
      // lit band across the centred wordmark reads as a selection.
      highlightCurrentLine: false,
      // Startup never takes the view. The page arrives as a tab, in one
      // step: opening and then switching back is two visible switches,
      // and the reader watches their buffer get displaced and returned
      // for no reason they asked for. Only the palette brings it forward.
      background: !force,
    });
    bufferId = res.bufferId;
    // Nothing focused is this page's resting state: it opens as
    // something to read, and Tab is what turns it into something to
    // operate. Without saying so, the host re-seeds focus onto the
    // first tabbable widget on every repaint — so clearing focus did
    // not clear it, and leaving the finder parked focus on the startup
    // toggle, off screen, where the next Space switched the page off.
    panel = new WidgetPanel(bufferId, undefined, {
      autoFocusFirst: false,
      // The caret and the focus ring are one thing on this page. It is a
      // document with a real cursor in it, so two independent "where am
      // I" markers is one too many: Tab used to move focus while the
      // caret stayed three cards above, and an arrow key used to move
      // the caret while Enter still fired whatever the last Tab had
      // left focused — off screen, unasked for. The host maintains both
      // directions now (`WidgetPanelOptions.focusFollowsCursor`), which
      // is also what makes "nothing focused" the common case rather
      // than a corner: most rows of this page are prose.
      focusFollowsCursor: true,
    });
    // Re-assert the caret *after* the panel exists. `createVirtualBuffer`
    // took `showCursors: true` and mounting a widget panel then cleared
    // it — panel buffers default to no caret — so the page ran with
    // movement actions enabled and nothing on screen to show where the
    // cursor was. `Down` moved it and the reader saw nothing until the
    // cursor reached the viewport edge and the page finally scrolled.
    // This is the order `setBufferShowCursors`'s own docs prescribe.
    editor.setBufferShowCursors(bufferId, true);
    applyComposeWidth();
    probeThemes();
    probeWorkspaces();
    render();
    // No section yet. The page is created as a *background* tab at
    // startup, and an outline of a page nobody is looking at would sit
    // in the sidebar under the file they are — where the Markdown
    // outline goes, and in place of it. `buffer_activated` is the moment
    // this page is the one being read; `showBuffer` below fires it.
    if (force) editor.showBuffer(bufferId);
    void probeRepoFiles();
    void probeGit();
  } catch (e) {
    editor.error(`welcome: ${e}`);
  }
  opening = false;
}

function closeWelcome(): void {
  if (bufferId === null) return;
  const id = bufferId;
  unmountContents();
  panel?.unmount();
  panel = null;
  bufferId = null;
  editor.closeBuffer(id, true);
}

registerHandler("welcome_open", () => {
  void openWelcome(true);
});
editor.registerCommand(
  "Welcome",
  "Open the welcome screen",
  "welcome_open",
);

registerHandler("welcomeOnReady", async () => {
  await openWelcome(false);
});
/** Only ever housekeeping for *this* page's own buffer.
 *
 *  Emptying the workspace deliberately does nothing. The page is a
 *  startup surface, not an empty-workspace surface: it used to reopen
 *  itself whenever the last buffer went away, which made closing your
 *  final file feel like the editor undoing the close — and made
 *  "close everything" impossible to express. Startup is the one moment
 *  the reader has not just told us what they wanted.
 *
 *  Nor does opening a file close the page. It used to "step aside" for
 *  a file when nobody had touched it, which needed the plugin to keep
 *  score of engagement and of whether the page had ever been on screen,
 *  and still deleted a tab the reader had not asked to close. A tab is
 *  a tab: the reader closes it. */
registerHandler("welcomeOnBufferClosed", (e: { buffer_id: number }) => {
  // The tab's `×` / `Ctrl+W` route: the buffer is gone and we were not
  // the ones who asked, so drop our handle on it.
  if (bufferId !== null && e.buffer_id === bufferId) {
    unmountContents();
    panel = null;
    bufferId = null;
  }
});
/** Everything about the page's shape a resize can change: the measure,
 *  the three widths the layout switches on, and whether there is room
 *  for full spacing. Above the measure's cap most of this stops moving
 *  — which is the point.
 *
 *  A repaint replaces the buffer's whole content, so it has to be worth
 *  doing. Deduping on the raw width was not enough: `measure()` is
 *  `min(width - 2, 88)`, so on any terminal wider than about 90 columns
 *  every width change lands on the same layout, and the commonest width
 *  change of all is a single column — the scrollbar appearing the first
 *  time the reader scrolls. That repaint could not alter a row.
 *
 *  Height is in the key because `air()` reads it: below 38 rows the
 *  page drops to single-row spacing. An earlier version of this
 *  function said height was ignored "because every list pins its own
 *  visibleRows", which was true of the lists and false of the spacing —
 *  so crossing 38 rows restyled nothing until something else forced a
 *  repaint. */
function layoutKey(): string {
  const w = viewportWidth();
  return [measure(), w >= 60, w >= 70, w >= 96, viewportHeight() >= 38].join(",");
}

// `viewport_changed` fires on scroll and on every height change as
// well as on resize, and a repaint replaces the buffer's whole content
// — which cancels an open prompt (the command palette shortens the
// pane by a row) and fights the reader for the viewport. So it repaints
// only when `layoutKey` actually moves.
//
// The payload's width is recorded first, because it is the authority:
// it is this buffer's pane, where `getViewport()` is whichever split
// happens to be active.
let lastKey = "";
/** `viewport_changed` reaches the *active* buffer of a split only, so a
 *  page sitting behind a file hears nothing — not a resize, not the
 *  switch that finally shows it. Its `composeWidth` hint and its spec
 *  are then whatever the terminal was when it was created: resize from
 *  140 columns to 70 while the page is a background tab and it paints,
 *  when you turn to it, at a measure the pane cannot hold.
 *
 *  Coming to the front is the moment to catch up. `layoutKey` is
 *  recomputed from scratch — `paneWidth` is cleared first, because the
 *  cached one belongs to a pane this buffer may never have had — and
 *  the page repaints if anything about its shape moved. */
registerHandler("welcomeOnBufferActivated", (e: { buffer_id: number }) => {
  // The Contents section is *this page's* outline, so it lives and dies
  // with this page being the one you are looking at. Leaving it up over
  // a source file would be a table of contents for something that is not
  // on screen — and it would sit where the Markdown outline goes, which
  // is the same column and the same idea.
  if (bufferId === null) return;
  if (e.buffer_id !== bufferId) {
    unmountContents();
    return;
  }
  if (editor.getActiveBufferId() !== bufferId) return;
  // Blurred, so a closed sidebar stays closed — which means a reader who
  // keeps it closed gets no outline and no sign that one exists except
  // the `Contents` button in the first viewport. That is the trade: this
  // page does not get to open a column the reader shut, and the button
  // is the recovery.
  mountContents(false);
  editor.setTimeout(0, "welcomeCatchUpOnShow");
});
registerHandler("welcomeCatchUpOnShow", async () => {
  if (bufferId === null || editor.getActiveBufferId() !== bufferId) return;
  paneWidth = 0;
  const key = layoutKey();
  if (key === lastKey) return;
  lastKey = key;
  applyComposeWidth();
  // The hint has to be *in* before the panel is laid out against it:
  // `widget_panel_width` reads it while it processes the update, and a
  // repaint issued in the same breath as the hint reads the old one.
  await editor.flush();
  render();
});
registerHandler(
  "welcomeOnViewportChanged",
  (d: { buffer_id: number; width: number }) => {
    if (bufferId === null || d.buffer_id !== bufferId) return;
    if (typeof d.width === "number" && d.width > 0) paneWidth = d.width;
    const key = layoutKey();
    if (key === lastKey) return;
    lastKey = key;
    applyComposeWidth();
    render();
  },
);

editor.on("ready", "welcomeOnReady");
editor.on("buffer_activated", "welcomeOnBufferActivated");
editor.on("buffer_closed", "welcomeOnBufferClosed");
editor.on("viewport_changed", "welcomeOnViewportChanged");

// ── Keyboard ─────────────────────────────────────────────────────────

function dispatch(action: ReturnType<typeof widgetKey>): void {
  panel?.command(action);
}

function jumpTo(level: string): void {
  if (bufferId === null) return;
  editor.scrollToWidget(bufferId, `level:${level}`);
  // A level banner is deliberately not focusable, so the `focus` event
  // that normally moves the outline's mark never comes for one. Say it
  // here instead: pressing `2` is exactly the reader telling us where
  // they are.
  markOutlineFor(`level:${level}`);
}

// The digits, `/` and Space are bound here *and* are ordinary
// characters. Each handler used to begin by asking whether the finder
// had focus and, if so, handing the character back to the panel —
// because the mode's bindings were consulted before the text path, so a
// bound key never reached a focused field on its own. The host settles
// that now: a focused text widget takes a printable key ahead of the
// mode's bindings, so these handlers only ever run when nothing is
// being typed into, and they can simply do their job.

registerHandler("welcome_tab", () => dispatch(widgetKey("Tab")));
registerHandler("welcome_shift_tab", () => dispatch(widgetKey("Shift+Tab")));
/** Activate whatever has focus.
 *
 *  Enter is the only key that gets here. Space used to as well, and is
 *  gone with the rest of the printable bindings — it is a character a
 *  reader can mean to type into the finder, and Enter already says
 *  "activate this" everywhere else in the editor.
 *
 *  Two cases are handled here rather than forwarded to the panel:
 *
 *  - the finder field, because a single-line `Text` widget treats Enter
 *    as advance-focus, and opening the pick is this field's whole
 *    purpose;
 *  - a fold arrow, because the host's own activation advances focus
 *    afterwards, and if that lands on a text widget the host scrolls the
 *    pane to it. Folding a card by keyboard therefore worked and then
 *    dropped the reader two cards away. Folding by *click* never did,
 *    which is what named the culprit. */
function activateFocused(): boolean {
  if (finderFocused()) {
    openFinderHit(finderIndex);
    return true;
  }
  if (lastFocusedWidget.startsWith("fold:")) {
    activateKey(lastFocusedWidget);
    return true;
  }
  return false;
}

registerHandler("welcome_enter", () => {
  if (activateFocused()) return;
  dispatch(widgetKey("Enter"));
});
/** Walk the finder's marked hit, and say whether it moved.
 *
 *  It no longer wraps, and the caller falls through to an ordinary caret
 *  move when it declines. Wrapping made the finder a trap: the caret now
 *  focuses the field just by arriving on its row, so a reader walking
 *  down the page fell in and could never walk out again — every Down
 *  cycled a list instead of moving the caret, with nothing on screen to
 *  say why. Falling out of either end is how a list inside a document
 *  should behave. */
function moveFinder(delta: number): boolean {
  if (finderHits.length === 0) return false;
  const next = finderIndex + delta;
  if (next < 0 || next >= finderHits.length) return false;
  finderIndex = next;
  render();
  return true;
}

/** Whether Up/Down belong to the finder's result list rather than to the
 *  page.
 *
 *  Not simply "is the field focused". With an empty query every file in
 *  the repo is a hit, so a reader who has typed nothing — who has merely
 *  arrived at the field with the caret — would have to walk two hundred
 *  results to get past the card. A query is what makes the list
 *  something you are navigating rather than something that happens to be
 *  under you. */
function finderOwnsVerticalKeys(): boolean {
  return finderFocused() && finderQuery.length > 0;
}
/** Movement keys decide only *who* moves — the focused widget, or the
 *  editor. The moving itself is the host's, which is why none of the
 *  scroll arithmetic that used to live here does any more. */
/** The only movement keys this page still names.
 *
 *  Everything else — Left/Right, Home/End, the page keys — the host now
 *  routes on its own: to the focused text widget when there is one, to
 *  the buffer otherwise. This page keeps Up and Down because on it they
 *  mean something the host cannot know: with the finder focused they
 *  walk its *hits*, which are plugin state, not text in the field. */
registerHandler("welcome_up", () => {
  if (finderOwnsVerticalKeys() && moveFinder(-1)) return;
  editor.executeAction("move_up");
});
registerHandler("welcome_down", () => {
  if (finderOwnsVerticalKeys() && moveFinder(1)) return;
  editor.executeAction("move_down");
});


registerHandler("welcome_backspace", () => dispatch(widgetKey("Backspace")));
registerHandler("welcome_delete", () => dispatch(widgetKey("Delete")));
/** Escape leaves the finder before it leaves the page: a reader who
 *  pressed it to get out of the search field did not ask to close the
 *  document they were reading. */
registerHandler("welcome_close", () => {
  if (finderFocused()) {
    lastFocusedWidget = "";
    panel?.setFocusKey("");
    render();
    return;
  }
  closeWelcome();
});


// A mode that declares `allowTextInput` owns the keyboard: the host
// blocks unbound Ctrl-/Alt-modified keys so a focused text field can
// never be hijacked by Open or Save. That is the right default and it
// means the handful of accelerators this page promises have to be
// named. Each one forwards to the real action, so a rebound key keeps
// working — the labels on the page come from the same resolver.
const FORWARDED: Array<[string, string]> = [
  ["C-p", "quick_open"],
  ["C-o", "open"],
  ["C-n", "new"],
  ["C-b", "toggle_file_explorer"],
  ["M-o", "toggle_dock_focus"],
  ["M-/", "open_live_grep"],
  ["F1", "show_help"],
];
for (const [, action] of FORWARDED) {
  registerHandler(`welcome_do_${action}`, () => editor.executeAction(action));
}

registerHandler("mode_text_input", (args: { text: string }) => {
  if (!panel || !args?.text) return;
  // Every character typed into a focused field arrives here, including
  // the ones this page also binds: the host gives a focused text widget
  // the key before it consults the mode's bindings.
  panel.command(textInputChar(args.text));
});

editor.defineMode(
  "welcome",
  [
    // **Nothing printable is bound here.** Every key this page claims is
    // one no text field wants: Tab, Enter, Escape, the arrows, Backspace
    // and Delete, and the handful of Ctrl-/Alt- accelerators the page
    // promises in print.
    //
    // Four went, and the reason is the same for all four. `1` / `2` / `3`
    // jumped to a level, `0` went to the top, `/` put focus in the
    // finder and Space activated the focused control — and each is a
    // character a reader can perfectly well mean to *type* into the
    // field this page carries. The host does hand a focused text widget
    // the key ahead of the mode's bindings, so they were not actually
    // stolen; but "it works because of a precedence rule" is a worse
    // answer than not binding them, and the page has better ways to say
    // all four now: the caret navigates, Enter activates, the Contents
    // section jumps, and the door cards are Tab stops that jump when you
    // press Enter on them.
    ["Tab", "welcome_tab"],
    // Shift+Tab is its own key code, not Tab carrying a modifier: the
    // terminal sends CSI Z and the parser yields `BackTab`. `S-Tab`
    // parses to Tab+Shift and matched nothing, so back-tab was dead on
    // this page. Both spellings are bound because the event does still
    // carry SHIFT alongside the BackTab code.
    ["BackTab", "welcome_shift_tab"],
    ["S-BackTab", "welcome_shift_tab"],
    ["Return", "welcome_enter"],
    ["Up", "welcome_up"],
    ["Down", "welcome_down"],
    ["Backspace", "welcome_backspace"],
    ["Delete", "welcome_delete"],
    ["Escape", "welcome_close"],
    ...FORWARDED.map(([k, action]) => [k, `welcome_do_${action}`] as [string, string]),
  ],
  true,
  true,
);

// ── Widget events ────────────────────────────────────────────────────

function activateKey(k: string): void {
  if (k.startsWith("fold:")) {
    const id = k.slice(5);
    if (folded.has(id)) folded.delete(id);
    else folded.add(id);
    // The reader's line is the cursor's line, and the host keeps the
    // cursor where it was across a repaint — so folding no longer needs
    // the plugin to save and restore a scroll position. It used to:
    // repainting a panel holding the finder field pulled the pane to
    // that widget, and folding by keyboard dropped the reader two cards
    // from the one they had just folded.
    render();
    return;
  }
  if (k.startsWith("hit:")) {
    const i = Number(k.slice(4));
    if (Number.isFinite(i)) {
      finderIndex = i;
      openFinderHit(i);
    }
    return;
  }
  if (k.startsWith("jump:")) {
    jumpTo(k.slice(5));
    return;
  }
  if (k.startsWith("theme:")) {
    const name = k.slice(6);
    editor.applyTheme(name);
    activeTheme = name;
    render();
    return;
  }
  if (k.startsWith("ws:")) {
    const id = Number(k.slice(3));
    const api = editor.getPluginApi("orchestrator") as OrchestratorApi | null;
    api?.focusWorkspace?.(id);
    return;
  }
  if (k === "ui_contents") {
    // Mount it if the reader closed it, raise it either way, and put the
    // keyboard in it — the whole reason this button exists is that a
    // sidebar section can be closed or buried.
    mountContents(true);
    return;
  }
  const uiFeature = UI_FEATURES.find(([key]) => key === k);
  if (uiFeature && uiFeature[2]) {
    editor.executeAction(uiFeature[2]);
    return;
  }
  switch (k) {
    case "act_open":
      editor.executeAction("open");
      return;
    case "act_recent":
      editor.executeAction("quick_open");
      return;
    case "act_new":
      editor.executeAction("new");
      return;
    case "act_review_diff":
      editor.executeAction("start_review_diff");
      return;
    case "act_review":
      // Renamed by #3098, along with the palette entry: the command
      // opens a commit list for `base..HEAD`, not a review session, and
      // everything still called "Review …" now opens the review tool.
      // The old name has no handler at all, and `executeAction` fails
      // for an unowned name only in the log (see the note on
      // `show_git_log` below) — so this button would have gone silently
      // dead on merging master, with its label still promising a review.
      editor.executeAction("start_branch_log");
      return;
    case "act_gitlog":
      // The git-log plugin's handler is `show_git_log`; `git_log` is
      // only the palette label. An unknown name is dispatched as a
      // plugin action, finds no handler in any context, and fails in
      // the log rather than on screen — so the name has to be right.
      editor.executeAction("show_git_log");
      return;
    case "act_ws_new":
      editor.executeAction("orchestrator_new");
      return;
    case "act_ws_agent":
      editor.executeAction("orchestrator_run_agent");
      return;
    case "act_ws_dock":
      editor.executeAction("toggle_dock_focus");
      return;
    case "startupToggle": {
      // Say so. This is the one control on the page that changes a
      // persisted setting, it is reachable from the keyboard, and the
      // only feedback used to be the checkbox glyph itself — which is
      // no feedback at all if the switch is off screen. Revealing the
      // focused widget (see the `focus` branch) means it no longer can
      // be, but a setting that survives the session should confirm
      // itself either way.
      const next = !showOnStartup();
      editor.setGlobalState("showOnStartup", next);
      // Kept short: the status field clips well under 40 columns, and a
      // message that ends in `reo...` is worse than a shorter one.
      editor.setStatus(
        next ? "Welcome: shown on startup" : "Welcome: hidden — reopen via palette",
      );
      render();
      return;
    }
    default:
      return;
  }
}


function openFinderHit(index: number): void {
  const hit = finderHits[index];
  if (!hit) return;
  editor.openFile(hit.path);
}

/** The Contents section's own events.
 *
 *  Routing is decided by `panel_id` at the call site, so this returns
 *  nothing: it used to answer "did I handle it", and the one caller
 *  discarded the answer. */
function handleContentsEvent(args: {
  event_type: string;
  payload?: unknown;
}): void {
  const payload = (args.payload ?? {}) as { index?: unknown; via?: unknown };
  if (args.event_type === "cancel") {
    // The section's `×`: the host has already unmounted it. The button
    // on the page is how it comes back.
    contentsMounted = false;
    return;
  }
  if (args.event_type === "select" || args.event_type === "activate") {
    const index = typeof payload.index === "number" ? payload.index : -1;
    const entry = outline[index];
    if (!entry || bufferId === null) return;
    outlineSelected = entry.key;
    // **An arrow key browses; a click or Enter goes.** Both halves are
    // deliberate, and the first is why an arrow does not scroll the page.
    //
    // Going means moving the caret (`scrollToWidget` seats it), and on
    // this page the caret is what focus follows — so a scroll on every
    // arrow key would hand the keyboard to the page and the reader would
    // lose the outline they were walking after one press. `markdown_toc`
    // makes the same split under the name "cursor mode": browsing an
    // outline is not navigating it, and Enter is what commits.
    const goThere = args.event_type === "activate" ||
      (payload as { via?: unknown }).via === "click";
    if (!goThere) return;
    editor.scrollToWidget(bufferId, entry.key, "top");
    // `"top"` rather than a minimal reveal: the reader asked to be taken
    // to this section, not to be shown that it exists.
    editor.floatingPanelControl(CONTENTS_PANEL_ID, "blur", 0);
    editor.showBuffer(bufferId);
  }
}

editor.on("widget_event", (args) => {
  if (args.panel_id === CONTENTS_PANEL_ID) {
    handleContentsEvent(args);
    return;
  }
  if (!panel || args.panel_id !== panel.id()) return;
  const k = typeof args.widget_key === "string" ? args.widget_key : "";

  // A `focus` event is the host stating where focus now is, and `""` is
  // one of the things it can state — `focusFollowsCursor` clears focus
  // every time the caret lands on prose, which is most of this page.
  // Mirroring only non-empty keys left this proxy naming a widget
  // nothing was focused on, so `Enter` on a paragraph still opened the
  // finder's marked hit.
  //
  // And the `focus` branch is the *only* writer. There used to be a
  // second one here, running for every event type, and the page has
  // widgets the host never focuses: `doorRow` marks six of a card's
  // seven rows `focusable: false`, and the finder's results are keyed
  // individually and not focusable at all. Clicking either fires
  // `activate` without moving focus, so that writer left the mirror
  // naming a widget nothing was focused on — with host focus still in
  // the finder field, `finderFocused()` went false while the field was
  // still taking what you typed, and Up/Down stopped walking the
  // results.
  if (args.event_type === "focus") {
    lastFocusedWidget = k;
    markOutlineFor(k);
    // No reveal here any more. The page used to call `scrollToWidget`
    // with `"minimal"`, which *is* a caret move — and under
    // `focusFollowsCursor` the host has already made the only correct
    // one. Repeating it was actively wrong in the case the host is
    // careful about: a card several rows tall anchors at its top, so
    // arrowing up into a door card's last row focused the card and this
    // then threw the caret back over everything the reader had just
    // walked past.
    return;
  }

  if (args.event_type === "change" && k === "finderField") {
    const payload = args.payload as { value?: string; cursorByte?: number } | undefined;
    if (typeof payload?.value !== "string") return;
    finderQuery = payload.value;
    finderCursor = typeof payload.cursorByte === "number" ? payload.cursorByte : finderQuery.length;
    recomputeHits();
    render();
    return;
  }

  if (args.event_type === "activate") {
    if (k === "finderField") {
      openFinderHit(finderIndex);
      return;
    }
    activateKey(k);
  }
});

editor.debug("Welcome screen plugin loaded");
