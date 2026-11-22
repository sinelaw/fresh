# Web UI Skins

The browser UI (`fresh --web`) dresses its chrome (menus, tabs, dock, dialogs, window frame) in a **skin**. The editor's colour [theme](./themes.md) still paints every buffer cell; a skin decides how everything around the buffer looks.

Five skins ship built in: **Cosmos** (wallpaper and a hardware bezel), **macOS Light**, **macOS Dark**, **Compact** (a quiet frame that follows your editor theme), and **Winamp Classic**. Pick one from the pill in the top-right corner, or cycle with **Ctrl/Cmd+Alt+T**.

A skin is a folder of ordinary web files, so you write it with ordinary tools. You need no build step and no JavaScript.

```
my-skin/
  skin.css          the skin (required)
  furniture.html    decorative markup for the window frame (optional)
  img/metal.png     any images or fonts the CSS uses (optional)
```

## Installing a skin

Open the skin picker (the pill in the top-right corner):

- **Install from folder…** picks a skin folder. Images and fonts are copied into the browser as `data:` URLs, so the skin keeps working offline.
- **Install from files…** picks `skin.css` together with the files it references.
- **Install from URL…** takes the URL of a `skin.css`, or of its folder (ending in `/`). Relative `url(…)`s keep pointing at that site. GitHub `…/blob/…` and `…/tree/…` links work as-is. The server must allow cross-origin requests, which `raw.githubusercontent.com` does.

Installed skins are stored in the browser (`localStorage`), so you install once and the skin is there on every load. Storage is per browser and per address, and it holds about 5 MB. A skin installed from a URL has a **↻** button that fetches it again. **✕** removes a skin.

If a skin ever hides the picker, load the page with `?skin=cosmos` (for example `http://127.0.0.1:8137/?skin=cosmos`) to switch back.

## Developing a skin: the skins folder

`fresh --web` also serves every skin folder under

- `$FRESH_WEB_SKINS`, if that environment variable is set, else
- `<config dir>/web-skins/` (on Linux, `~/.config/fresh/web-skins/`).

These skins appear in the picker under **Skins folder**. While one is active the page watches its files, and **saving any file re-applies the skin within a second**, so you can tune it against the real, running editor. A quick start is to copy a built-in skin:

```sh
mkdir -p ~/.config/fresh/web-skins
cp -r crates/fresh-editor/web-ui/skins/30-compact ~/.config/fresh/web-skins/my-skin
# edit @id / @name in ~/.config/fresh/web-skins/my-skin/skin.css, then:
fresh --web
```

The folder name is the skin's id unless the header sets `@id`.

## `skin.css`

### The header

The stylesheet starts with a comment block that names the skin:

```css
/* ==FreshSkin==
@id          neon-night
@name        Neon Night
@description Dark glass with a magenta accent
@author      Your Name
@version     1.0.0
@extends     compact
@frame       full
@scale       1
@menu        flow
@furniture   furniture.html
==/FreshSkin== */
```

| Key | Meaning |
|---|---|
| `@name` | Shown in the picker. Required. |
| `@id` | `a–z`, `0–9` and `-`, 1–48 characters. Defaults to the folder name. It may not reuse a built-in id. |
| `@description`, `@author`, `@version` | Shown in the picker; informational. |
| `@extends` | Another skin's id. Its CSS (and furniture, frame, scale and menu) come first and yours layers on top, so a palette variant can be a few lines. |
| `@frame` | `full`: the editor fills the window. `bezel`: the editor sits inset inside the `#device` frame with the wallpaper around it, as in Cosmos and Winamp. Default `full`. |
| `@scale` | Density, from `0.5` to `1.5`, applied under the user's own zoom (Compact uses `0.92`). |
| `@menu` | `cells`: menu titles sit at the terminal's columns. `flow`: they are evenly spaced. |
| `@furniture` | The HTML file whose markup is built inside `#device`. |

### Colours: CSS custom properties

The chrome is drawn from custom properties. A skin sets them on `:root.theme-<id>`:

```css
:root.theme-neon-night {
  --fg: #e6e1f0;           /* chrome text */
  --muted: #9a93ad;        /* secondary text */
  --bg2: #221d2e;          /* panels, popups */
  --bg3: #1b1725;          /* menus */
  --shell: #16131f;        /* window frame / toolbar */
  --status-bg: #16131f;  --status-fg: #b9b1cc;
  --menuhi: #ff2fa0;       /* solid highlight fill */
  --accent: #ff2fa0;     --ui-accent: #ff2fa0;
  --border: #3a3150;
  --surface: #221d2e;    --surface-2: #2a2338;
  --hairline: rgba(255,255,255,.08);  --hairline-strong: rgba(255,255,255,.15);
  --hover: rgba(255,255,255,.06);
  --sel: color-mix(in srgb, var(--ui-accent) 25%, transparent);
  --shadow: 0 12px 32px rgba(0,0,0,.5);
  --r-sm: 4px;  --r-md: 6px;  --r-lg: 10px;
}
```

Leave out any property to keep the editor theme's value. This is how Cosmos follows whatever theme the editor uses. Do not set `--bg`: it is the buffer's ground, and the syntax colours are chosen against it. Where a skin sets a solid fill (`--menuhi`, `--accent`) without the ink drawn on it (`--on-menuhi`, `--on-accent`), the page picks a readable ink itself.

### Following the editor theme

The editor theme's colours stay available (`var(--bg)` is the buffer ground). `<html>` carries `editor-dark` or `editor-light` depending on which way that ground leans. Use these to derive a frame one shade off the buffer, as Compact does:

```css
:root.theme-neon-night:where(.editor-dark) {
  --shell: color-mix(in srgb, var(--bg), #fff 4%);
}
:root.theme-neon-night:where(.editor-light) {
  --shell: color-mix(in srgb, var(--bg), #000 4%);
}
```

Wrapping the mode class in `:where()` keeps the rule as specific as `:root.theme-<id>`, so a skin that `@extends` yours can still override it.

### Structure

Anything else is ordinary CSS scoped under `.theme-<id>`, which is set on both `<html>` and `<body>`. That covers fonts, radii, the menu bar, tabs, status bar, dock, dialogs, wallpaper, and the `#device` frame:

```css
.theme-neon-night { --font-family: "Inter", system-ui, sans-serif; }
body.theme-neon-night:not(.mobile) { background: url(img/wallpaper.jpg) center/cover; }
.theme-neon-night .tab.active { box-shadow: inset 0 -2px 0 var(--ui-accent); }
```

- Relative `url(…)`s resolve against the skin folder.
- Keep the buffer monospace. The cell grid is pinned to `--mono-family`, so changing `--font-family` restyles the chrome only.
- Never put padding or borders on rows drawn over the cell grid; tint them with colour and inset shadows instead.
- Some chrome stands on the editor's own ground: the active tab and panels mounted in a pane. A light skin with dark `--fg` on a dark editor theme can make that text vanish, so check the skin against both a dark and a light editor theme, and give such surfaces their own background.
- `docs/internal/web-ui-theme-checklist.md` in the repository walks every surface worth checking.
- The built-in skins in `crates/fresh-editor/web-ui/skins/` are complete examples. Winamp restyles nearly every surface.

The skin's stylesheet is inserted after the page's base rules and before a few rules that must win over every skin (status-bar tone pills, popup row geometry, file-icon colours). Override those with a more specific selector.

## `furniture.html`

Decorative markup built inside the `#device` host: title bars, LEDs, screws, side rails. Style it from `skin.css`.

```html
<div class="nn-title"><span class="nn-caption">NEON NIGHT</span></div>
<i class="nn-screw tl"></i><i class="nn-screw tr"></i>
```

It is decoration only: it is pointer-transparent and hidden from assistive tech. The page rebuilds it from `div`, `span`, `i`, `b`, `em`, `strong`, `small` and `p` elements, keeping only their `class` attribute and text. Scripts, other elements and every other attribute are dropped. A `@frame bezel` skin can also reuse the Cosmos bezel kit classes (`dv-screen`, `dv-top`, `dv-bottom`, `dv-side`, `dv-label`, `dv-leds`, `dv-screw`) from the base stylesheet.

A title-bar host, `#mactitle` (with `.mt-lights` and `.mt-name`, which always holds the active document's name), is hidden by default; a skin may show and style it, as macOS does.

## Safety

A skin is CSS and inert HTML: it cannot run script. Its CSS can still load resources from other sites (fonts, images) and can change what the page looks like, so install skins from sources you trust, as you would a browser extension's styles.
