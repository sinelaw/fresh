// The page's own elements (index.html).

// The element `selector` names. The page's elements are there by
// construction: a missing one is a broken page, said at once.
export function $(selector) {
  const el = document.querySelector(selector);
  if (el === null) throw new Error(`The page has no ${selector}`);
  return el;
}

// The element `selector` names inside `el`, which made it (a missing one
// is a broken page, said at once).
export function within(el, selector) {
  const found = el.querySelector(selector);
  if (found === null) throw new Error(`No ${selector} in its element`);
  return found;
}

// `text` as HTML text.
/** function escapeHtml(text: String) => String */
export function escapeHtml(text) {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&#39;");
}

// `cls` on `el` or off it.
export function setClass(el, cls, on) {
  if (on) el.classList.add(cls);
  else el.classList.remove(cls);
}
