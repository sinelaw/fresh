// A permissive stand-in for the host `editor` object, shared by both engine
// runners so they see identical input. Every property is another stub; every
// call goes through the native `__host_call` (counting host calls) and returns
// a stub. Handlers the plugin registers (registerCommand / on) are recorded so
// the runner can fire each one afterwards.
//
// Stubs are not real values, so plenty of plugin code fails on them — the same
// way on both engines. Only the *differences* between engines matter.
(function () {
  const handlers = [];
  const handler = {
    get(target, prop, receiver) {
      if (prop === Symbol.toPrimitive) return (hint) => (hint === "number" ? 0 : "");
      if (prop === Symbol.iterator) return function* () {};
      if (typeof prop === "symbol") return undefined;
      if (prop === "then") return undefined;
      if (prop === "toString" || prop === "toJSON") return () => "";
      if (prop === "valueOf") return () => 0;
      if (prop === "bind" || prop === "call" || prop === "apply") {
        return Function.prototype[prop].bind(receiver);
      }
      if (Object.prototype.hasOwnProperty.call(target.__props, prop)) return target.__props[prop];
      return makeStub(target.__path + "." + String(prop));
    },
    set(target, prop, value) {
      target.__props[prop] = value;
      return true;
    },
    has() {
      return true;
    },
    apply(target, thisArg, args) {
      __host_call(target.__path);
      const path = target.__path;
      if (path.endsWith(".registerCommand") && typeof args[2] === "string") handlers.push(args[2]);
      if (path.endsWith(".on") && typeof args[1] === "string") handlers.push(args[1]);
      return makeStub(path + "()");
    },
    construct(target) {
      return makeStub("new " + target.__path);
    },
  };
  function makeStub(path) {
    const f = function () {};
    f.__path = path;
    f.__props = Object.create(null);
    return new Proxy(f, handler);
  }
  globalThis.editor = makeStub("editor");
  globalThis.__pluginName__ = "compat";

  const results = [];
  globalThis.__fireAll = function () {
    for (const name of handlers) {
      const rec = { name, status: "pending" };
      results.push(rec);
      try {
        const f = globalThis[name];
        if (typeof f !== "function") {
          rec.status = "missing";
          continue;
        }
        const r = f(makeStub("arg"));
        if (r instanceof Promise) {
          r.then(
            () => (rec.status = "ok"),
            (e) => ((rec.status = "error"), (rec.error = String((e && e.message) || e))),
          );
        } else {
          rec.status = "ok";
        }
      } catch (e) {
        rec.status = "error";
        rec.error = String((e && e.message) || e);
      }
    }
  };
  globalThis.__report = () => JSON.stringify(results);
})();
