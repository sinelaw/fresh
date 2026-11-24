// Line diff: Myers' O(ND) algorithm over two arrays of lines. Returns the
// edits as {kind: "+" | "-", a, b}: "-" deletes line a of the old text
// (at line b of the new), "+" inserts line b of the new (after line a of
// the old). Null when the texts differ in more than `limit` lines.

export function diffLines(a, b, limit = 4000) {
  const N = a.length, M = b.length, max = N + M;
  const o = max + 1; // v[o + k]: the furthest x on diagonal k
  const v = new Int32Array(2 * max + 3);
  // Per step d, the diagonals -d-1 ..= d+1 as they were before it: all the
  // backtrack reads (keeping the whole array per step is quadratic memory).
  const trace = [];
  for (let d = 0; d <= max; d++) {
    trace.push(v.slice(o - d - 1, o + d + 2));
    for (let k = -d; k <= d; k += 2) {
      let x = k === -d || (k !== d && v[o + k - 1] < v[o + k + 1]) ? v[o + k + 1] : v[o + k - 1] + 1;
      let y = x - k;
      while (x < N && y < M && a[x] === b[y]) { x++; y++; }
      v[o + k] = x;
      if (x >= N && y >= M) return backtrack(trace, N, M);
    }
    if (d > limit) return null;
  }
  return [];
}

function backtrack(trace, N, M) {
  const edits = [];
  let x = N, y = M;
  for (let d = trace.length - 1; d > 0; d--) {
    const vv = trace[d];
    const at = (k) => vv[k + d + 1];
    const k = x - y;
    const prevK = k === -d || (k !== d && at(k - 1) < at(k + 1)) ? k + 1 : k - 1;
    const px = at(prevK), py = px - prevK;
    while (x > px && y > py) { x--; y--; }
    edits.push(x === px ? { kind: "+", a: x, b: py } : { kind: "-", a: px, b: y });
    x = px; y = py;
  }
  return edits.reverse();
}

// The two texts walked in order: [" " | "-" | "+", line, old index, new
// index] for every line, from the edits `diffLines` returns.
function walk(a, b, edits) {
  const out = [];
  let i = 0, j = 0;
  const same = (toA, toB) => { while (i < toA && j < toB) { out.push([" ", a[i], i, j]); i++; j++; } };
  for (const e of edits) {
    if (e.kind === "-") { same(e.a, Infinity); out.push(["-", a[i], i, j]); i++; }
    else { same(Infinity, e.b); out.push(["+", b[j], i, j]); j++; }
  }
  same(a.length, b.length);
  return out;
}

// For each line of `b`, the line of `a` it is unchanged from, or -1. Null
// when the texts differ too much to say.
export function mapLines(a, b) {
  const edits = diffLines(a, b);
  if (!edits) return null;
  const map = new Int32Array(b.length).fill(-1);
  for (const [k, , i, j] of walk(a, b, edits)) if (k === " ") map[j] = i;
  return map;
}

// A unified diff of `a` to `b` with `context` lines around each change:
// "" when they are equal, null when they differ too much.
export function unifiedDiff(a, b, nameA, nameB, context = 3) {
  const edits = diffLines(a, b);
  if (!edits) return null;
  if (!edits.length) return "";
  const lines = walk(a, b, edits);
  let out = `--- ${nameA}\n+++ ${nameB}\n`;
  for (let s = 0; s < lines.length;) {
    // The next change, and the hunk around it: changes closer than twice
    // the context share one.
    while (s < lines.length && lines[s][0] === " ") s++;
    if (s >= lines.length) break;
    let e = s;
    for (let k = s; k < lines.length && k - e <= 2 * context; k++) if (lines[k][0] !== " ") e = k;
    const from = Math.max(0, s - context), to = Math.min(lines.length, e + context + 1);
    const hunk = lines.slice(from, to);
    const na = hunk.filter((l) => l[0] !== "+").length, nb = hunk.filter((l) => l[0] !== "-").length;
    const sa = hunk[0][2] + (na ? 1 : 0), sb = hunk[0][3] + (nb ? 1 : 0);
    out += `@@ -${sa},${na} +${sb},${nb} @@\n` + hunk.map((l) => l[0] + l[1]).join("\n") + "\n";
    s = to;
  }
  return out;
}
