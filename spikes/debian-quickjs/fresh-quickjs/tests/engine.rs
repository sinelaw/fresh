//! What the plugin runtime needs from a JS engine, exercised against the
//! system QuickJS. Each test drops its runtime at the end, and QuickJS asserts
//! in `JS_FreeRuntime` that no object is still referenced, so a refcount bug in
//! the wrapper aborts the test run (Debian's libquickjs keeps that assertion).

use fresh_quickjs::{Context, Error, Runtime, Type};
use serde_json::json;
use std::cell::RefCell;
use std::rc::Rc;

fn setup() -> (Runtime, Context) {
    let rt = Runtime::new().unwrap();
    let ctx = Context::new(&rt).unwrap();
    (rt, ctx)
}

#[test]
fn evaluates_numbers_and_strings() {
    let (_rt, ctx) = setup();
    assert_eq!(ctx.eval("6 * 7", "t.js").unwrap().as_f64(), Some(42.0));
    assert_eq!(ctx.eval("0.5 + 0.25", "t.js").unwrap().as_f64(), Some(0.75));
    let s = ctx.eval("'héllo ' + '🦀'.repeat(2)", "t.js").unwrap();
    assert_eq!(s.type_of(), Type::String);
    assert_eq!(s.to_string().unwrap(), "héllo 🦀🦀");
    assert_eq!(ctx.eval("true", "t.js").unwrap().as_bool(), Some(true));
    assert!(ctx.eval("undefined", "t.js").unwrap().is_undefined());
    assert!(ctx.eval("null", "t.js").unwrap().is_null());
}

#[test]
fn long_concatenations_read_back_as_strings() {
    // Bellard's QuickJS builds long concatenations as rope strings
    // (JS_TAG_STRING_ROPE), a tag quickjs-ng and rquickjs do not have.
    let (_rt, ctx) = setup();
    let v = ctx
        .eval(
            "let s = ''; for (let i = 0; i < 2000; i++) s += 'ab'; s",
            "t.js",
        )
        .unwrap();
    assert_eq!(v.type_of(), Type::String);
    assert_eq!(v.to_string().unwrap().len(), 4000);
}

#[test]
fn reports_exceptions_with_name_message_and_stack() {
    let (_rt, ctx) = setup();
    let err = ctx
        .eval(
            "function f() { throw new TypeError('bad'); }\nf();",
            "plugin.js",
        )
        .unwrap_err();
    match err {
        Error::Exception {
            name,
            message,
            stack,
        } => {
            assert_eq!(name, "TypeError");
            assert_eq!(message, "bad");
            let stack = stack.unwrap();
            assert!(stack.contains("plugin.js"), "stack: {stack}");
        }
        other => panic!("unexpected {other:?}"),
    }
    // Thrown non-Error values come back as their string form.
    let err = ctx.eval("throw 'plain'", "t.js").unwrap_err();
    assert_eq!(err.to_string(), "plain");
}

#[test]
fn reports_syntax_errors_without_running() {
    let (_rt, ctx) = setup();
    assert!(ctx.compile_check("let x = 1;", "ok.js").is_ok());
    let err = ctx.compile_check("let = ;", "broken.js").unwrap_err();
    assert!(
        matches!(err, Error::Exception { ref name, .. } if name == "SyntaxError"),
        "{err:?}"
    );
    // compile_check must not have defined anything.
    assert!(ctx.eval("typeof x", "t.js").unwrap().to_string().unwrap() == "undefined");
}

#[test]
fn native_functions_receive_args_and_return_values() {
    let (_rt, ctx) = setup();
    let calls = Rc::new(RefCell::new(Vec::new()));
    let seen = calls.clone();
    let f = ctx
        .function("record", 2, move |ctx, _this, args| {
            let parts: Vec<String> = args.iter().map(|a| a.to_string().unwrap()).collect();
            seen.borrow_mut().push(parts.join(","));
            Ok(ctx.int(args.len() as i64))
        })
        .unwrap();
    ctx.global().set("record", f).unwrap();
    let n = ctx.eval("record('a', 1, {}) + record()", "t.js").unwrap();
    assert_eq!(n.as_f64(), Some(3.0));
    assert_eq!(
        *calls.borrow(),
        vec!["a,1,[object Object]".to_string(), String::new()]
    );
    assert_eq!(
        ctx.eval("record.name", "t.js")
            .unwrap()
            .to_string()
            .unwrap(),
        "record"
    );
}

#[test]
fn native_errors_are_catchable_in_js() {
    let (_rt, ctx) = setup();
    let f = ctx
        .function("fail", 0, |_ctx, _this, _args| {
            Err(Error::Other("no such buffer".into()))
        })
        .unwrap();
    ctx.global().set("fail", f).unwrap();
    let msg = ctx
        .eval(
            "try { fail(); 'no' } catch (e) { e instanceof Error ? e.message : 'wrong' }",
            "t.js",
        )
        .unwrap();
    assert_eq!(msg.to_string().unwrap(), "no such buffer");
    // Uncaught, it surfaces as an exception on the Rust side.
    assert_eq!(
        ctx.eval("fail()", "t.js").unwrap_err().to_string(),
        "Error: no such buffer"
    );
}

#[test]
fn native_panics_become_js_exceptions() {
    let (_rt, ctx) = setup();
    let f = ctx
        .function("boom", 0, |_ctx, _this, _args| panic!("boom"))
        .unwrap();
    ctx.global().set("boom", f).unwrap();
    let err = ctx.eval("boom()", "t.js").unwrap_err();
    assert!(err.to_string().contains("panic"), "{err}");
}

#[test]
fn calls_js_functions_from_rust() {
    let (_rt, ctx) = setup();
    ctx.eval("globalThis.add = (a, b) => a + b;", "t.js")
        .unwrap();
    let add = ctx.global().get("add").unwrap();
    assert!(add.is_function());
    let r = add
        .call(&ctx.undefined(), &[ctx.int(2), ctx.float(0.5)])
        .unwrap();
    assert_eq!(r.as_f64(), Some(2.5));
}

#[test]
fn json_round_trips_through_serde() {
    let (_rt, ctx) = setup();
    let input = json!({
        "path": "/tmp/naïve.rs",
        "line": 12,
        "ratio": 0.5,
        "flags": [true, false, null],
        "nested": { "emoji": "🦀", "empty": [] }
    });
    let v = ctx.from_json(&input).unwrap();
    assert!(v.is_object());
    ctx.global().set("input", v).unwrap();
    let out = ctx
        .eval(
            "({ ...input, line: input.line + 1, len: input.flags.length })",
            "t.js",
        )
        .unwrap()
        .to_json()
        .unwrap();
    assert_eq!(out["line"], 13);
    assert_eq!(out["len"], 3);
    assert_eq!(out["nested"]["emoji"], "🦀");
    assert_eq!(out["path"], "/tmp/naïve.rs");
    assert!(ctx.eval("[1, 2]", "t.js").unwrap().is_array());
}

/// The plugin runtime's async pattern: a `_fooStart` host method returns a
/// callback id, JS parks a promise on it, and the host later resolves it by
/// calling `_resolveCallback` and running the job queue.
#[test]
fn host_resolved_promises_drive_async_plugin_code() {
    let (rt, ctx) = setup();
    let next_id = Rc::new(RefCell::new(0));
    let ids = next_id.clone();
    let start = ctx
        .function("_readFileStart", 1, move |ctx, _this, _args| {
            *ids.borrow_mut() += 1;
            Ok(ctx.int(*ids.borrow()))
        })
        .unwrap();
    ctx.global().set("_readFileStart", start).unwrap();
    ctx.eval(
        r#"
        globalThis._pending = new Map();
        globalThis._resolveCallback = (id, v) => { _pending.get(id).resolve(v); _pending.delete(id); };
        globalThis.readFile = (p) => {
            const id = _readFileStart(p);
            return new Promise((resolve, reject) => _pending.set(id, { resolve, reject }));
        };
        globalThis.result = 'pending';
        (async () => {
            const text = await readFile('a.txt');
            result = 'got ' + text.length;
        })();
        "#,
        "plugin.js",
    )
    .unwrap();
    rt.execute_pending_jobs().unwrap();
    assert_eq!(
        ctx.eval("result", "t.js").unwrap().to_string().unwrap(),
        "pending"
    );

    let resolve = ctx.global().get("_resolveCallback").unwrap();
    resolve
        .call(&ctx.undefined(), &[ctx.int(1), ctx.string("hello")])
        .unwrap();
    assert!(rt.is_job_pending());
    rt.execute_pending_jobs().unwrap();
    assert_eq!(
        ctx.eval("result", "t.js").unwrap().to_string().unwrap(),
        "got 5"
    );
}

#[test]
fn errors_thrown_in_jobs_are_reported() {
    let (rt, ctx) = setup();
    ctx.eval(
        "Promise.resolve().then(() => { throw new RangeError('later'); });",
        "t.js",
    )
    .unwrap();
    // An unhandled rejection is not a job failure; the job itself succeeded.
    assert!(rt.execute_pending_jobs().is_ok());
}

#[test]
fn contexts_share_a_runtime_but_not_globals() {
    // One context per plugin, as the runtime does today.
    let rt = Runtime::new().unwrap();
    let a = Context::new(&rt).unwrap();
    let b = Context::new(&rt).unwrap();
    a.eval("globalThis.mine = 1", "a.js").unwrap();
    assert_eq!(
        b.eval("typeof mine", "b.js").unwrap().to_string().unwrap(),
        "undefined"
    );
}

#[test]
fn memory_limit_turns_runaway_allocation_into_an_exception() {
    let (rt, ctx) = setup();
    rt.set_memory_limit(8 * 1024 * 1024);
    let err = ctx
        .eval("let a = []; for (;;) a.push('x'.repeat(1024));", "t.js")
        .unwrap_err();
    assert!(err.to_string().contains("out of memory"), "{err}");
}

#[test]
fn deadline_interrupts_runaway_scripts() {
    let (rt, ctx) = setup();
    rt.set_deadline(Some(
        std::time::Instant::now() + std::time::Duration::from_millis(50),
    ));
    let err = ctx.eval("for (;;) {}", "t.js").unwrap_err();
    assert!(err.to_string().contains("interrupted"), "{err}");
    rt.set_deadline(None);
    assert_eq!(ctx.eval("1 + 1", "t.js").unwrap().as_f64(), Some(2.0));
}

#[test]
fn values_outliving_their_context_handle_keep_it_alive() {
    let v = {
        let (_rt, ctx) = setup();
        ctx.eval("({ a: [1, 2, 3] })", "t.js").unwrap()
    };
    // Runtime and context are only reachable through `v` now.
    assert_eq!(
        v.get("a").unwrap().get_index(2).unwrap().as_f64(),
        Some(3.0)
    );
}

#[test]
fn many_native_functions_are_freed_with_the_runtime() {
    let (_rt, ctx) = setup();
    for i in 0..1000 {
        let f = ctx
            .function("f", 0, move |ctx, _this, _args| Ok(ctx.int(i)))
            .unwrap();
        ctx.global().set(&format!("f{i}"), f).unwrap();
    }
    assert_eq!(ctx.eval("f999()", "t.js").unwrap().as_f64(), Some(999.0));
}
