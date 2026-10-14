// Inventory of the engine's built-ins: every own property path reachable from
// globalThis (and constructors' prototypes) up to a fixed depth.
(function () {
  const out = new Set();
  function walk(obj, path, depth, stack) {
    if (obj === null || (typeof obj !== "object" && typeof obj !== "function")) return;
    if (depth > 3 || stack.includes(obj)) return;
    stack.push(obj);
    for (const k of Reflect.ownKeys(obj)) {
      const name = typeof k === "symbol" ? "[" + k.description + "]" : String(k);
      const p = path ? path + "." + name : name;
      out.add(p);
      let v;
      try {
        v = obj[k];
      } catch (e) {
        continue;
      }
      walk(v, p, depth + 1, stack);
    }
    stack.pop();
  }
  walk(globalThis, "", 0, []);
  return [...out].sort().join("\n");
})()
