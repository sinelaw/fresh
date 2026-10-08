//! The behaviour Fresh relies on from its JS engine, run against whichever
//! `fresh-js` backend is built: rquickjs by default, the system QuickJS with
//! `RUSTFLAGS="--cfg fresh_js_system"`. The same assertions passing on both is
//! what makes the backends interchangeable for the plugin runtime.
//!
//! Each test frees its runtime at the end, and QuickJS asserts in
//! `JS_FreeRuntime` that no object is still referenced, so a reference leak
//! aborts the test binary.

use fresh_js::function::{Opt, Rest};
use fresh_js::{Context, Ctx, Function, Object, Runtime, Value};

#[derive(fresh_js::class::Trace, fresh_js::JsLifetime)]
#[fresh_js::class]
struct Api {
    #[qjs(skip_trace)]
    base: i32,
}

#[fresh_js::methods(rename_all = "camelCase")]
impl Api {
    fn add_base(&self, n: i32) -> i32 {
        self.base + n
    }

    fn greet(&self, name: Opt<String>) -> String {
        format!("hello {}", name.0.unwrap_or_else(|| "world".into()))
    }

    fn count<'js>(&self, _ctx: Ctx<'js>, items: Rest<Value<'js>>) -> usize {
        items.0.len()
    }

    #[qjs(rename = "_raw")]
    fn raw_name(&self) -> bool {
        true
    }

    #[qjs(skip)]
    fn helper(x: i32) -> i32 {
        x
    }
}

fn with_ctx<R>(f: impl for<'js> FnOnce(Ctx<'js>) -> R) -> R {
    let rt = Runtime::new().unwrap();
    let ctx = Context::full(&rt).unwrap();
    ctx.with(f)
}

#[test]
fn runtime_and_context_tear_down() {
    with_ctx(|ctx| {
        let v: i32 = ctx.eval("1 + 2").unwrap();
        assert_eq!(v, 3);
    });
}

#[test]
fn class_instance_on_globals_tears_down() {
    with_ctx(|ctx| {
        let api = fresh_js::Class::instance(ctx.clone(), Api { base: 10 }).unwrap();
        ctx.globals().set("api", api).unwrap();
        let v: i32 = ctx.eval("api.addBase(5)").unwrap();
        assert_eq!(v, 15);
        assert_eq!(Api::helper(2), 2);
    });
}

#[test]
fn bound_methods_tear_down() {
    with_ctx(|ctx| {
        let api = fresh_js::Class::instance(ctx.clone(), Api { base: 1 }).unwrap();
        ctx.globals().set("api", api).unwrap();
        let v: String = ctx
            .eval("const g = api.greet.bind(api); globalThis.keep = g; g('x')")
            .unwrap();
        assert_eq!(v, "hello x");
    });
}

#[test]
fn native_closures_tear_down() {
    with_ctx(|ctx| {
        let console = Object::new(ctx.clone()).unwrap();
        let f = Function::new(ctx.clone(), |_ctx: Ctx, args: Rest<Value>| args.0.len()).unwrap();
        console.set("log", f).unwrap();
        ctx.globals().set("console", console).unwrap();
        let n: i32 = ctx.eval("console.log(1, 2, 3)").unwrap();
        assert_eq!(n, 3);
    });
}

#[test]
fn promises_and_jobs_tear_down() {
    with_ctx(|ctx| {
        ctx.eval::<(), _>("globalThis.r = 0; Promise.resolve(4).then(v => { r = v; });")
            .unwrap();
        while ctx.execute_pending_job() {}
        let r: i32 = ctx.eval("r").unwrap();
        assert_eq!(r, 4);
    });
}

#[test]
fn an_unattached_closure_is_freed_with_the_runtime() {
    with_ctx(|ctx| {
        let _f = Function::new(ctx.clone(), || 1).unwrap();
    });
}

#[test]
fn values_handed_to_the_engine_release_their_context() {
    with_ctx(|ctx| {
        let o = Object::new(ctx.clone()).unwrap();
        o.set("x", 1).unwrap();
        ctx.globals().set("o", o).unwrap();
    });
}

#[test]
fn native_call_results_release_their_context() {
    with_ctx(|ctx| {
        let f = Function::new(ctx.clone(), || 1).unwrap();
        let v: i32 = f.call(()).unwrap();
        assert_eq!(v, 1);
    });
}

// ── rquickjs behaviour the plugin runtime depends on ──────────────────────

fn eval_err(src: &str) -> String {
    with_ctx(|ctx| {
        let api = fresh_js::Class::instance(ctx.clone(), Api { base: 0 }).unwrap();
        ctx.globals().set("api", api).unwrap();
        let msg: String = ctx
            .eval(format!(
                "try {{ {src}; 'no error' }} catch (e) {{ e.constructor.name + ': ' + e.message }}"
            ))
            .unwrap();
        msg
    })
}

#[test]
fn methods_are_exported_under_rquickjs_names() {
    with_ctx(|ctx| {
        let api = fresh_js::Class::instance(ctx.clone(), Api { base: 0 }).unwrap();
        ctx.globals().set("api", api).unwrap();
        let names: String = ctx
            .eval("['addBase','greet','count','_raw','helper'].map(n => n + ':' + typeof api[n]).join(',')")
            .unwrap();
        assert_eq!(
            names,
            "addBase:function,greet:function,count:function,_raw:function,helper:undefined"
        );
    });
}

#[test]
fn optional_and_rest_parameters_follow_argument_count() {
    with_ctx(|ctx| {
        let api = fresh_js::Class::instance(ctx.clone(), Api { base: 0 }).unwrap();
        ctx.globals().set("api", api).unwrap();
        let out: String = ctx
            .eval("[api.greet(), api.greet('a'), api.count(), api.count(1, 'x', {})].join(',')")
            .unwrap();
        assert_eq!(out, "hello world,hello a,0,3");
    });
}

#[test]
fn conversion_errors_match_rquickjs_messages() {
    assert_eq!(
        eval_err("api.greet(undefined)"),
        "TypeError: Error converting from js 'undefined' into type 'string'"
    );
    assert_eq!(
        eval_err("api.addBase('1')"),
        "TypeError: Error converting from js 'string' into type 'i32'"
    );
    assert_eq!(
        eval_err("api.addBase()"),
        "TypeError: Error calling function with 0 argument(s) while 1 where expected"
    );
}

#[test]
fn numbers_are_range_checked_like_rquickjs() {
    with_ctx(|ctx| {
        let neg: fresh_js::Result<u64> = ctx.eval("-1");
        assert_eq!(
            neg.unwrap_err().to_string(),
            "Error converting from js 'f64' into type 'u64': Underflow"
        );
        let big: fresh_js::Result<u8> = ctx.eval("300");
        assert_eq!(
            big.unwrap_err().to_string(),
            "Error converting from js 'i32' into type 'u8': Overflow"
        );
        let f: i32 = ctx.eval("2.9").unwrap();
        assert_eq!(f, 2);
        let n: Option<String> = ctx.eval("null").unwrap();
        assert_eq!(n, None);
    });
}

#[test]
fn eval_is_strict_by_default() {
    with_ctx(|ctx| {
        let r: fresh_js::Result<()> = ctx.eval("undeclared = 1");
        assert!(r.unwrap_err().is_exception());
        let e = ctx.catch();
        // An Error object is `Type::Exception`, but `is_exception()` means the
        // engine's exception marker, which a caught value never is.
        assert_eq!(e.type_of(), fresh_js::Type::Exception);
        assert!(!e.is_exception());
        let msg = e.as_exception().unwrap().message().unwrap();
        assert!(msg.contains("undeclared"), "{msg}");
    });
}

#[test]
fn rope_strings_read_back_as_strings() {
    // Bellard's QuickJS builds long concatenations as ropes (a tag rquickjs
    // never sees on quickjs-ng).
    with_ctx(|ctx| {
        let s: String = ctx
            .eval("let s = ''; for (let i = 0; i < 2000; i++) s += 'ab'; s")
            .unwrap();
        assert_eq!(s.len(), 4000);
        let v: Value = ctx.eval("s").unwrap();
        assert!(v.is_string());
        assert_eq!(v.type_of(), fresh_js::Type::String);
    });
}

#[test]
fn types_are_classified_like_rquickjs() {
    with_ctx(|ctx| {
        let types: Vec<&str> = [
            "undefined",
            "null",
            "true",
            "1",
            "1.5",
            "'s'",
            "[]",
            "(() => 1)",
            "class A {}",
            "Promise.resolve()",
            "new Error('x')",
            "({})",
            "10n",
        ]
        .iter()
        .map(|src| {
            ctx.eval::<Value, _>(format!("({src})"))
                .unwrap()
                .type_name()
        })
        .collect();
        assert_eq!(
            types,
            [
                "undefined",
                "null",
                "bool",
                "int",
                "float",
                "string",
                "array",
                "function",
                "constructor",
                "promise",
                "exception",
                "object",
                "big_int"
            ]
        );
    });
}

#[test]
fn objects_arrays_and_maps_convert() {
    with_ctx(|ctx| {
        let obj: Object = ctx.eval("({ b: 2, a: 1 })").unwrap();
        let keys: Vec<String> = obj
            .keys::<String>()
            .collect::<fresh_js::Result<_>>()
            .unwrap();
        assert_eq!(keys, ["b", "a"]);
        assert!(obj.contains_key("a").unwrap());
        let props: std::collections::HashMap<String, i32> = obj
            .props::<String, i32>()
            .collect::<fresh_js::Result<_>>()
            .unwrap();
        assert_eq!(props["b"], 2);
        let arr: fresh_js::Array = ctx.eval("[1, 2, 3]").unwrap();
        assert_eq!(arr.len(), 3);
        let v: Vec<u8> = ctx.eval("[1, 2, 3]").unwrap();
        assert_eq!(v, [1, 2, 3]);
        let back: Value = fresh_js::IntoJs::into_js(vec!["x".to_string()], &ctx).unwrap();
        ctx.globals().set("back", back).unwrap();
        let joined: String = ctx.eval("back.join('|')").unwrap();
        assert_eq!(joined, "x");
    });
}

#[test]
fn serde_round_trips_plain_data() {
    #[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
    struct Info {
        path: String,
        line: u32,
        tags: Vec<String>,
        missing: Option<u32>,
    }
    with_ctx(|ctx| {
        let info = Info {
            path: "/tmp/naïve.rs".into(),
            line: 7,
            tags: vec!["🦀".into()],
            missing: None,
        };
        let v = fresh_js::serde::to_value(ctx.clone(), &info).unwrap();
        ctx.globals().set("info", v).unwrap();
        let line: u32 = ctx.eval("info.line + 1").unwrap();
        assert_eq!(line, 8);
        let null: bool = ctx.eval("info.missing === null").unwrap();
        assert!(null);
        let back: Info =
            fresh_js::serde::from_value(ctx.eval::<Value, _>("({ ...info })").unwrap()).unwrap();
        assert_eq!(back, info);
    });
}

#[test]
fn persistent_values_outlive_with() {
    let rt = Runtime::new().unwrap();
    let ctx = Context::full(&rt).unwrap();
    let saved = ctx.with(|ctx| {
        let obj: Object = ctx.eval("({ n: 41 })").unwrap();
        fresh_js::Persistent::save(&ctx, obj)
    });
    let n: i32 = ctx.with(|ctx| {
        let obj = saved.clone().restore(&ctx).unwrap();
        obj.get("n").unwrap()
    });
    assert_eq!(n, 41);
    drop(saved);
}

#[test]
fn calling_js_from_rust_and_back() {
    with_ctx(|ctx| {
        let add: Function = ctx.eval("(a, b) => a + b").unwrap();
        let r: f64 = add.call((2, 0.5)).unwrap();
        assert_eq!(r, 2.5);
        let thrower: Function = ctx.eval("() => { throw new RangeError('nope') }").unwrap();
        let err = thrower.call::<_, ()>(()).unwrap_err();
        assert!(err.is_exception());
        let e = ctx.catch();
        assert_eq!(e.as_exception().unwrap().message().as_deref(), Some("nope"));
    });
}

#[test]
fn rejection_tracker_sees_unhandled_rejections() {
    use std::cell::RefCell;
    use std::rc::Rc;
    let rt = Runtime::new().unwrap();
    let seen = Rc::new(RefCell::new(Vec::new()));
    let sink = seen.clone();
    rt.set_host_promise_rejection_tracker(Some(Box::new(move |_ctx, _p, reason, handled| {
        if !handled {
            let msg = reason
                .as_exception()
                .and_then(|e| e.message())
                .unwrap_or_default();
            sink.borrow_mut().push(msg);
        }
    })));
    let ctx = Context::full(&rt).unwrap();
    ctx.with(|ctx| {
        ctx.eval::<(), _>("Promise.reject(new Error('lost'))")
            .unwrap();
        while ctx.execute_pending_job() {}
    });
    assert_eq!(*seen.borrow(), ["lost"]);
}

#[test]
#[should_panic(expected = "boom from rust")]
fn panics_in_native_calls_resume_in_rust() {
    with_ctx(|ctx| {
        let f = Function::new(ctx.clone(), || -> i32 { panic!("boom from rust") }).unwrap();
        ctx.globals().set("boom", f).unwrap();
        let _: fresh_js::Result<()> = ctx.eval("boom()");
    });
}

#[test]
fn interrupt_handler_stops_runaway_scripts() {
    let rt = Runtime::new().unwrap();
    let start = std::time::Instant::now();
    rt.set_interrupt_handler(Some(Box::new(move || {
        start.elapsed() > std::time::Duration::from_millis(50)
    })));
    let ctx = Context::full(&rt).unwrap();
    ctx.with(|ctx| {
        let r: fresh_js::Result<()> = ctx.eval("for (;;) {}");
        assert!(r.unwrap_err().is_exception());
        let e = ctx.catch();
        let msg = e.as_exception().unwrap().message().unwrap();
        assert!(msg.contains("interrupted"), "{msg}");
    });
}
