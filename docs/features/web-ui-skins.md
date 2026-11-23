# Web UI Skins

The browser UI (`fresh --web`, and the desktop app) dresses everything around the buffer — the menu bar, tabs, explorer, dock, status bar, menus, dialogs, the window frame — in a **skin**. The editor's colour [theme](./themes.md) still paints the buffer; a skin decides how everything around it looks, and where it goes.

Seven skins come with Fresh: **Cosmos** (wallpaper and a hardware bezel), **macOS Light**, **macOS Dark**, **Compact** (a quiet frame that follows your editor theme), **Winamp Classic** and **Bridge** (a starship console), plus **Neon Hive** as an example to copy. Pick one from the pill at the right end of the menu bar, or cycle with **Ctrl/Cmd+Alt+T**. The choice is per browser: two windows on the same session can wear different skins.

A skin is a folder of ordinary web files, written with ordinary tools. It needs no build step and runs no JavaScript.

```
my-skin/
  skin.css          the skin (required)
  theme.json        an editor theme to go with it (optional)
  furniture.html    decorative markup for the window frame (optional)
  img/metal.png     any images or fonts the CSS uses (optional)
```

## Installing a skin

Open the skin picker (the pill at the right end of the menu bar):

- **Install from folder…** picks a skin folder. Images and fonts are copied into the browser as `data:` URLs, so the skin keeps working offline.
- **Install from files…** picks `skin.css` together with the files it references.
- **Install from URL…** takes the URL of a `skin.css`, or of its folder (ending in `/`). Relative `url(…)`s keep pointing at that site. GitHub `…/blob/…` and `…/tree/…` links work as-is. The server must allow cross-origin requests, which `raw.githubusercontent.com` does.

Installed skins are stored in the browser (`localStorage`), so you install once and the skin is there on every load. Storage is per browser and per address, and holds about 5 MB. A skin installed from a URL has a **↻** button that fetches it again; **✕** removes a skin.

If a skin ever hides the picker, load the page with `?skin=cosmos` (for example `http://127.0.0.1:8137/?skin=cosmos`) to switch back.

## Developing a skin: the skins folder

`fresh --web` also serves every skin folder under

- `$FRESH_WEB_SKINS`, if that environment variable is set, else
- `<config dir>/web-skins/` (on Linux, `~/.config/fresh/web-skins/`).

These skins appear in the picker under **Skins folder**. While one is on screen the page watches its files, and **saving any file re-applies the skin within a second**, so you can tune it against the real, running editor. A quick start is to copy a built-in skin:

```sh
mkdir -p ~/.config/fresh/web-skins
cp -r crates/fresh-editor/web-ui/skins/30-compact ~/.config/fresh/web-skins/my-skin
# edit @id / @name in ~/.config/fresh/web-skins/my-skin/skin.css, then:
fresh --web
```

The folder name is the skin's id unless the header sets `@id`.

For a fuller example, `crates/fresh-editor/web-ui/example-skins/neon-hive/` rearranges the window into a honeycomb of neon modules with live system readouts and cables between them, and bundles a matching editor theme. Copy it into your skins folder to try it.

## `skin.css`

### The header

The stylesheet starts with a comment block that names the skin:

```css
/* ==FreshSkin==
@id           neon-night
@name         Neon Night
@description  Dark glass with a magenta accent
@author       Your Name
@version      1.0.0
@extends      compact
@frame        full
@scale        1
@furniture    furniture.html
@editor-theme theme.json
==/FreshSkin== */
```

| Key | Meaning |
|---|---|
| `@name` | Shown in the picker. Required. |
| `@id` | `a–z`, `0–9` and `-`, 1–48 characters. Defaults to the folder name. It may not reuse a built-in id. |
| `@description`, `@author`, `@version` | Shown in the picker; informational. |
| `@extends` | Another skin's id. Its CSS (and furniture, frame, scale, editor theme and explorer side) come first and yours layers on top, so a palette variant can be a few lines — macOS Dark is one. |
| `@frame` | `full`: the window is the editor. `bezel`: the editor sits inset inside the `#device` frame, with the page's background (your wallpaper) around it, as in Cosmos and Winamp. Default `full`. |
| `@scale` | Density, from `0.5` to `1.5`: the size of the chrome's text and of the buffer's font (Compact uses `0.92`). |
| `@furniture` | The HTML file whose markup is built inside `#device`. |
| `@editor-theme` | The editor theme that goes with the skin: a `.json` file in the folder (a folder's `theme.json` is found even without this line; URL installs need it), or the name of an installed theme such as `builtin://nord`. See [The editor theme](#the-editor-theme). |
| `@keys` | `words` (the default: `Ctrl+Shift+P`) or `symbols` (the Mac's: `⌃⇧P`): how key hints are written in menus, the palette and the status bar. |
| `@explorer-side` | `left` or `right`: a setting the skin proposes, applied through the editor's own setting when the skin is picked, with the same opt-out as its editor theme. |

### Colours: custom properties

The chrome is drawn from custom properties. A skin sets them on `:root.theme-<id>`:

```css
:root.theme-neon-night {
  --fg: #e6e1f0;          --muted: #9a93ad;       /* chrome text, secondary text */
  --accent: #ff2fa0;      --on-accent: #fff;      /* the solid highlight, and the ink on it */
  --ui-accent: #ff2fa0;                           /* focus rings, the active tab's edge */
  --menu-bg: #16131f;     --menu-fg: #e6e1f0;     /* the menu bar and its menus */
  --tab-bg: #16131f;      --tab-fg: #9a93ad;      /* the tab strip */
  --tab-active-bg: var(--bg);  --tab-active-fg: var(--ed-fg);
  --status-bg: #16131f;   --status-fg: #b9b1cc;
  --popup-bg: #221d2e;    --popup-fg: #e6e1f0;    /* the palette, dialogs, completion */
  --bg2: #221d2e;         --bg3: #1b1725;         /* side panels (explorer, dock) */
  --border: #3a3150;
  --hairline: rgba(255,255,255,.08);  --hairline-strong: rgba(255,255,255,.15);
  --hover: rgba(255,255,255,.06);
  --shadow: 0 12px 32px rgba(0,0,0,.5);  --modal-shadow: 0 24px 64px rgba(0,0,0,.6);
  --field-bg: #1b1725;  --field-fg: #e6e1f0;  --field-border: #3a3150;  /* text fields, selects */
  --r-sm: 4px;  --r-md: 6px;  --r-lg: 10px;
  --font-family: "Inter", system-ui, sans-serif;  /* the chrome's font (Cosmos uses var(--mono-family)) */
  --ui-font-size: 13px;   --mono-font-size: 15px; /* the chrome's text, and the buffer's */
}
```

Leave out any property to keep its default, which comes from the editor theme; that is how Cosmos follows whatever theme the editor uses. Two kinds of property belong to the editor theme and a skin never sets them: `--bg`, the buffer's ground, and `--ed-fg` / `--ed-muted`, the buffer's own ink — so a light skin with dark `--fg` still leaves the buffer's text readable on a dark editor theme. Use them for surfaces that stand on the buffer's ground, as the active tab does above. Where a skin sets a solid fill (`--accent`, `--ui-accent`) without the ink drawn on it (`--on-accent`, `--on-ui-accent`), the page picks a readable ink itself.

### Following the editor theme

`<html>` carries `editor-dark` or `editor-light` depending on which way the editor's ground leans. Use them to derive a frame one shade off the buffer, as Compact does:

```css
:root.theme-neon-night:where(.editor-dark)  { --menu-bg: color-mix(in srgb, var(--bg), #fff 4%); }
:root.theme-neon-night:where(.editor-light) { --menu-bg: color-mix(in srgb, var(--bg), #000 4%); }
```

Wrapping the mode class in `:where()` keeps the rule as specific as `:root.theme-<id>`, so a skin that `@extends` yours can still override it.

### Structure: it is all CSS

The browser UI is an ordinary web page that lays itself out, so a skin can restyle, resize and move any of it with plain CSS scoped under `.theme-<id>` (set on both `<html>` and `<body>`):

```css
body.theme-neon-night:not(.mobile) { background: url(img/wallpaper.jpg) center/cover; }
.theme-neon-night .tab.active { box-shadow: inset 0 -2px 0 var(--ui-accent); }
.theme-neon-night #statusbar { height: 28px; }
```

A web font comes in with `@import` at the top of `skin.css`, as Bridge brings its condensed lettering; give a local fallback for when there is no network. Each skin's `@import`s are moved ahead of every rule, so they work in a skin that `@extends` another, too:

```css
@import "https://fonts.googleapis.com/css2?family=Antonio:wght@400;600&display=swap";
:root.theme-neon-night { --font-family: "Antonio", "Arial Narrow", sans-serif; }
```

The page's parts:

| Selector | What it is |
|---|---|
| `#app` | the window: `#menubar`, `#workbench`, `#statusbar`, top to bottom |
| `#menubar` | `#menus` (the `.mtop` titles) and `#themeswitch` (the skin picker: `#themebtn`, `#thememenu`) |
| `#workbench` | `#dock` (the Orchestrator, at the left edge) and `#dock-sash`, `#sidebar` (the explorer, `.ex-head` / `.ex-row`) and `#sash`, `#center`; the columns are placed with `order` |
| `#center` | `#editors` (the panes: `.split`, `.pane`, `.tabs` / `.tab`, `.pane-body`, `.view`) |
| `#statusbar` | `.sitem`s; the document's name is `.sname`, messages `.smsg`, the language server `.slsp` (with `lsp-on`, `lsp-off`, `lsp-warning`, `lsp-error`, `lsp-offdismissed`), the palette's key `.spalette` |
| `.dropdown`, `.mitem` | menus, and their rows (`.dropdown.context` for right-click menus) |
| `.ask`, `.ask.palette`, `.ask-row` | the palette and other prompts; `.ask.dialog` for questions, with `.btn` / `.btn.primary` |
| `.popup`, `.popup-item` | completion, hover and code-action pop-ups |
| `.settings`, `.floating-panel` | the settings screen; plugin panels shown on top |

The buffer itself is drawn on a canvas at whole cells. Restyle around it freely, but keep the buffer's font monospace (`--mono-family`).

Layout is yours too. Neon Hive turns the window into three columns with a grid — `#workbench` and `#center` step aside with `display: contents`, and the dock, the editor and the explorer become columns whose margins leave room for its furniture:

```css
body.theme-neon-hive:not(.mobile) #app {
  display: grid; grid-template-columns: auto minmax(0, 1fr) auto; grid-template-rows: auto minmax(0, 1fr) auto;
}
body.theme-neon-hive:not(.mobile) :is(#workbench, #center) { display: contents; }
body.theme-neon-hive:not(.mobile) #sidebar { grid-area: 1 / 3 / 4 / 4; margin: 34px 22px 270px 72px; }
```

The client's own controls can move into your frame, too. Bridge takes the menu bar out of the window's flow (`position: fixed`) and stacks its titles as the frame's column of buttons, and makes the status bar the frame's bottom bar:

```css
body.theme-bridge.frame-bezel #menubar { position: fixed; left: var(--shell-pad); width: var(--br-col); flex-direction: column; }
body.theme-bridge.frame-bezel #menus { flex-direction: column; gap: 4px; }
```

When `#menus` is laid out as a column, menus open beside their titles instead of below them. Winamp moves the Orchestrator's menu and close button into its own window's title bar the same way, by their `data-wkey` names.

`<body>` carries `mobile` on a narrow screen (the bezel frame is off there; guard desktop layout with `:not(.mobile)`), and `sidebar-left` or `sidebar-right`, the explorer's side.

### Where everything is

For furniture that sits beside a part of the window, the page publishes where each part is, as custom properties on `:root` in pixels from `#app`'s top-left corner:

| Property | Meaning |
|---|---|
| `--<id>-x`, `--<id>-y`, `--<id>-w`, `--<id>-h` | the part's box (`-w` / `-h` are `0px` while it is hidden) |
| `--app-x`, `--app-y`, `--app-w`, `--app-h` | the client itself, in the window (dialogs, the palette and menus are kept inside it, so a skin's frame stays whole) |

Ids: `menubar`, `sidebar`, `editors`, `dock`, `statusbar`. `<body>` also carries `has-<id>` while a part is shown. Inside `#device`, `#app` starts at `var(--bezel-side)`, `var(--bezel-top)`, so furniture can sit exactly around a part. Write selectors that use these classes as `body.theme-<id>.has-dock …`: `theme-<id>` is on `<html>` too.

## The frame and `furniture.html`

With `@frame bezel`, `#app` is inset inside `#device` by `--shell-pad` (the margin of wallpaper) and `--bezel-top`, `--bezel-side`, `--bezel-bot` (the frame itself); set them on `:root.theme-<id>`. The frame starts as the Cosmos hardware: a machined device with a rim. Restyle `.theme-<id> #device` and its `::before` to make it yours, or set `background: none` to have no device at all.

`furniture.html` is decorative markup built inside `#device`: title bars, LEDs, screws, rails. Style it from `skin.css`.

```html
<div class="nn-title"><span class="nn-caption">NEON NIGHT</span></div>
<i class="nn-screw tl"></i><i class="nn-screw tr"></i>
```

It is decoration only: it is pointer-transparent and hidden from assistive technology. The page rebuilds it from `div`, `span`, `i`, `b`, `em`, `strong`, `small` and `p` elements, keeping only their `class`, their `data-bind` / `data-bind-var` attributes (see [Live values](#live-values)) and their text. Scripts, other elements and every other attribute are dropped. A bezel skin can reuse the Cosmos kit's pieces: `dv-screen`, `dv-top`, `dv-bottom`, `dv-side` (`dv-l`, `dv-r`), `dv-label`, `dv-grow`, `dv-leds` (`g`, `y`, `a`), `dv-screw` (`tl`, `tr`, `bl`, `br`).

A title-bar host, `#mactitle` (with `.mt-lights` and `.mt-name`, which always holds the active document's name), is hidden by default; a skin may show and style it, as macOS does.

## Live values

Furniture can show live data. Fresh publishes a fixed set of values, and a skin binds to them in `furniture.html`:

```html
<b data-bind="skin_data:load">–</b>                            <!-- the value as text -->
<span class="gauge" data-bind-var="--v:skin_data:cpu"></span>  <!-- as a custom property -->
```

```css
.theme-my-skin .gauge { background: conic-gradient(var(--accent) calc(var(--v, 0) * 1%), transparent 0); }
```

The element's own text is the placeholder until a value arrives. A number comes through unitless, so CSS can scale it with `calc()`.

| Value | Meaning |
|---|---|
| `skin_data:cpu`, `skin_data:mem` | percent of CPU busy and of memory in use, 0–100 |
| `skin_data:memText` | memory as "used / total G" |
| `skin_data:load` | 1, 5 and 15-minute load averages |
| `skin_data:uptime` | how long the machine has been up |
| `skin_data:errors`, `skin_data:warnings` | diagnostics across open files |
| `skin_data:buffers` | open files |
| `skin_data:time` | the time, "HH:MM" |

The system figures (`cpu`, `mem`, `memText`, `load`, `uptime`) are read where the editor runs, and appear where the platform reports them. A skin never brings code: the data comes from Fresh. A plugin can publish values of its own with `editor.setSkinValue(name, value)`; a skin shows them as `data-bind="<plugin name>:<name>"`.

## The editor theme

A skin dresses the chrome; the buffer's syntax colours, selection and cursor come from the editor's [theme](./themes.md). To ship both as one look, put a `theme.json` next to `skin.css`. It is an ordinary Fresh theme file, the same format the theme editor saves:

```json
{
  "name": "neon-night",
  "extends": "builtin://dark",
  "editor": { "bg": [18, 8, 31], "selection_bg": [74, 20, 90] },
  "syntax": { "keyword": [255, 47, 160], "string": [35, 229, 255] }
}
```

Use another file name with `@editor-theme my-theme.json`, or pair the skin with an installed theme instead of bundling one: `@editor-theme builtin://nord`. A skin that `@extends` another inherits its editor theme unless it brings its own.

When you **pick** the skin, its editor theme is applied as well, exactly as if you had chosen it in the editor. This happens on a pick, not on every page load. A bundled theme is saved as `<config dir>/themes/web-skins/<skin id>.json`, so it also appears in the editor's theme list (in the terminal too).

The editor theme is not per-browser the way a skin is. It is the editor's own setting: every window attached to the session, including a terminal, uses it, and it is saved in `config.json`. To keep your editor theme (and settings such as the explorer's side), untick **Also apply …** under the skin in the picker. That choice is remembered for that skin. Switching to a skin without an editor theme never changes it.

In the skins folder, saving `theme.json` re-applies it live, like `skin.css`. A URL has no folder listing, so a skin meant to be installed from a URL declares its theme with `@editor-theme theme.json`; adding that line always is the simplest habit.

## Checking a skin

Walk every surface in both a dark and a light editor theme: the menu bar and a menu with a submenu, tabs (active, inactive, modified, in an inactive pane), the explorer (selected and current file), a split, the dock (open a plugin panel), the status bar with a message, the command palette, **Open File** (the file browser), a confirmation (close a modified buffer), completion and hover pop-ups (with a language server), the settings screen and **Keyboard Shortcuts**, a right-click menu in the explorer, the skin picker itself, and a narrow window. The built-in skins in `crates/fresh-editor/web-ui/skins/` are complete examples; Winamp restyles nearly every surface.

## Safety

A skin is CSS and inert HTML: it cannot run script. Its CSS can still load resources from other sites (fonts, images) and change what the page looks like, so install skins from sources you trust, as you would a browser extension's styles.
