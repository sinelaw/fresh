// Line diff: Myers' O(ND) algorithm over two arrays of lines. Returns the
// edits as {kind: "+" | "-", a, b}: "-" deletes line a of the old text
// (at line b of the new), "+" inserts line b of the new (after line a of
// the old). Null when the texts differ in more than `limit` lines.

export function diffLines(a, b, limit = 4000) {
  const N = a.length, M = b.length, max = N + M;
  const v = new Int32Array(2 * max + 2);
  const trace = [];
  for (let d = 0; d <= max; d++) {
    trace.push(v.slice());
    for (let k = -d; k <= d; k += 2) {
      let x = k === -d || (k !== d && v[max + k - 1] < v[max + k + 1]) ? v[max + k + 1] : v[max + k - 1] + 1;
      let y = x - k;
      while (x < N && y < M && a[x] === b[y]) { x++; y++; }
      v[max + k] = x;
      if (x >= N && y >= M) return backtrack(trace, max, N, M);
    }
    if (d > limit) return null;
  }
  return [];
}

function backtrack(trace, max, N, M) {
  const edits = [];
  let x = N, y = M;
  for (let d = trace.length - 1; d > 0; d--) {
    const vv = trace[d];
    const k = x - y;
    const prevK = k === -d || (k !== d && vv[max + k - 1] < vv[max + k + 1]) ? k + 1 : k - 1;
    const px = vv[max + prevK], py = px - prevK;
    while (x > px && y > py) { x--; y--; }
    edits.push(x === px ? { kind: "+", a: x, b: py } : { kind: "-", a: px, b: y });
    x = px; y = py;
  }
  return edits.reverse();
}
