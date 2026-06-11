# Clickable bare URLs in LSP hover popups (issue #603)

## Problem

Some LSP servers (e.g. pyrefly) embed plain URLs in their hover documentation,
for example `See https://docs.example.com/path for details`. Today those URLs
render as inert text — you cannot click them to open the page.

## What already works

The popup link plumbing is already complete:

- `view/markdown.rs` stores an optional `link_url` on each `StyledSpan` and
  exposes `StyledLine::link_at_column`.
- `Popup::link_at_position` (`view/popup.rs`) maps a click position to a URL.
- The popup click handlers (`app/mouse_input.rs`, `view/popup_mouse.rs`) open
  that URL via `open::that`.

`parse_markdown` populates `link_url` for CommonMark `[text](url)` and `<url>`
links, so those are *already* clickable in hover popups. The gap is **bare**
URLs: CommonMark (and therefore `parse_markdown`) does not autolink a raw
`https://…`, and plain-text (non-markdown) hovers never get any link metadata.

## Design

Add a single post-processing pass, `markdown::linkify_bare_urls(&mut [StyledLine])`,
that scans each span's text for `http://` / `https://` URLs and splits the span
into plain + link segments, tagging the link segment with `link_url` and the
existing link style (underlined cyan). Spans that already carry a `link_url`
(markdown links) are left untouched, so nothing is double-processed.

Apply it once in `app/lsp_requests.rs` to the parsed hover content (both the
markdown and the plain-text branch). Because it reuses the same `link_url`
metadata, the existing click handling makes bare URLs clickable for free — no
changes to the mouse layer.

URL boundary heuristic: consume RFC-3986 URL characters, then strip trailing
sentence punctuation (`. , ; : ! ? ) ] } " '`) so `…details.` and `(see https://x)`
behave naturally. A scheme with no host (`http://` alone) is not linkified.

## Testing

- Unit tests on `linkify_bare_urls` (markdown.rs): bare URL → link span; leading
  and trailing prose preserved; trailing punctuation trimmed; multiple URLs per
  line; existing markdown link spans untouched; `http` without `://` ignored.
- Component test on `Popup::link_at_position` proving a bare URL in a Markdown
  popup resolves to a clickable URL at the right column.
- E2E (`tests/e2e/lsp.rs`) with a fake LSP whose hover contains a bare URL:
  hover the symbol, render, and assert the URL cells render with the link style
  (cyan + underline) — observable rendered output, no real browser launch.
