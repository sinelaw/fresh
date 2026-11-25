// Elements drawn from HTML strings (as Monaco draws its view lines), kept
// by their HTML: an element whose HTML is already on screen is reused, just
// moved, so a redraw writes only what changed.

// What was drawn, by its HTML: each HTML's elements.
export function poolOf(drawn) {
  const pool = new Map();
  for (const d of drawn) {
    const list = pool.get(d.html);
    if (list !== undefined) list.push(d);
    else pool.set(d.html, [d]);
  }
  return pool;
}

// An element showing `html`: one from `pool` (taken from it), or made.
export function take(pool, html) {
  const hit = (pool.get(html) ?? []).pop();
  if (hit !== undefined) return hit;
  const box = document.createElement("div");
  box.innerHTML = html;
  const el = box.firstChild;
  return { el: el ?? box, html };
}

// Whether two lists of drawn elements are the same elements in order.
export function sameDrawn(a, b) {
  return a.length === b.length && a.every((x, i) => x.el === b[i].el);
}
