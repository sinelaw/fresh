// Sashes: the handles that resize what they sit between — the editor's
// splits, the sidebar, the bottom dock, the terminal and the right dock.
//
// A sash is an element of no size in the layout (a split's sash carries the
// split's dividing line as its border) whose grip reaches a few pixels to
// either side. Dragging it measures what it resizes once, when the drag
// starts, and then only writes sizes, at most once a frame: a drag never
// reads layout after writing it. A double click puts the default size back.

/**
 * Make `el` a sash along `axis` ("x": it moves left and right, "y": up and
 * down). `o.begin()` measures and returns the drag's context; `o.drag(ctx, d)`
 * applies a move of `d` pixels from where the drag started; `o.end()` runs
 * once it is let go; `o.reset()` on a double click.
 */
export function sash(el, axis, o) {
  el.classList.add("sash", axis);
  el.addEventListener("pointerdown", (e) => {
    if (e.button !== 0) return;
    e.preventDefault();
    el.setPointerCapture(e.pointerId);
    el.classList.add("dragging");
    // Nothing is selected while dragging, wherever the pointer goes.
    document.documentElement.classList.add("sash-dragging");
    const at = (ev) => (axis === "x" ? ev.clientX : ev.clientY);
    const from = at(e);
    const ctx = o.begin();
    let to = from, frame = 0;
    const move = (ev) => {
      to = at(ev);
      if (!frame) frame = requestAnimationFrame(() => { frame = 0; o.drag(ctx, to - from); });
    };
    const up = () => {
      el.removeEventListener("pointermove", move);
      el.removeEventListener("pointerup", up);
      el.removeEventListener("pointercancel", up);
      if (frame) { cancelAnimationFrame(frame); o.drag(ctx, to - from); }
      el.classList.remove("dragging");
      document.documentElement.classList.remove("sash-dragging");
      o.end();
    };
    el.addEventListener("pointermove", move);
    el.addEventListener("pointerup", up);
    el.addEventListener("pointercancel", up);
  });
  if (o.reset) el.addEventListener("dblclick", () => o.reset());
}

export const clamp = (x, lo, hi) => Math.max(lo, Math.min(hi, x));
