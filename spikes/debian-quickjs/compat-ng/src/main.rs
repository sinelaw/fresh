//! The `compat_system` example, on quickjs-ng through rquickjs (what Fresh runs
//! today), so the two engines' results can be compared line by line.

use rquickjs::context::EvalOptions;
use rquickjs::function::Rest;
use rquickjs::{Context, Function, Object, Runtime, Value};
use serde_json::json;
use std::cell::Cell;
use std::rc::Rc;
use std::time::{Duration, Instant};

const STUB: &str = include_str!("../../compat/stub.js");
const PROBE: &str = include_str!("../../compat/probe.js");

fn main() {
    let arg = std::env::args()
        .nth(1)
        .expect("usage: compat-ng <dir> | --probe");
    if arg == "--probe" {
        let rt = Runtime::new().unwrap();
        let ctx = Context::full(&rt).unwrap();
        ctx.with(|ctx| {
            let s: String = ctx.eval(PROBE).unwrap();
            println!("{s}");
        });
        return;
    }
    let dir = std::path::PathBuf::from(arg);
    let bootstrap = std::fs::read_to_string(dir.join("__bootstrap.js")).unwrap();
    let mut files: Vec<_> = std::fs::read_dir(&dir)
        .unwrap()
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|e| e == "js") && !p.ends_with("__bootstrap.js"))
        .collect();
    files.sort();
    for path in files {
        let name = path.file_stem().unwrap().to_string_lossy().into_owned();
        let source = std::fs::read_to_string(&path).unwrap();
        println!("{}", run_plugin(&name, &source, &bootstrap));
    }
}

fn error_string(ctx: &rquickjs::Ctx, e: rquickjs::Error) -> String {
    if matches!(e, rquickjs::Error::Exception) {
        let exc = ctx.catch();
        if let Some(obj) = exc.as_object() {
            let name: Option<String> = obj.get("name").ok();
            let message: Option<String> = obj.get("message").ok();
            return match (name, message) {
                (Some(n), Some(m)) if !n.is_empty() => format!("{n}: {m}"),
                (_, Some(m)) => m,
                _ => "exception".into(),
            };
        }
        return exc
            .as_string()
            .and_then(|s| s.to_string().ok())
            .unwrap_or_else(|| "exception".into());
    }
    e.to_string()
}

fn run_jobs(rt: &Runtime) -> Result<(), String> {
    loop {
        match rt.execute_pending_job() {
            Ok(true) => continue,
            Ok(false) => return Ok(()),
            Err(e) => {
                return Err(e
                    .0
                    .with(|ctx| error_string(&ctx, rquickjs::Error::Exception)));
            }
        }
    }
}

fn run_plugin(name: &str, source: &str, bootstrap: &str) -> serde_json::Value {
    let rt = Runtime::new().unwrap();
    rt.set_memory_limit(256 << 20);
    let deadline: Rc<Cell<Option<Instant>>> = Rc::new(Cell::new(None));
    let d = deadline.clone();
    rt.set_interrupt_handler(Some(Box::new(
        move || matches!(d.get(), Some(t) if Instant::now() >= t),
    )));
    let ctx = Context::full(&rt).unwrap();
    let host_calls = Rc::new(Cell::new(0u32));

    ctx.with(|ctx| {
        let global = ctx.globals();
        let counter = host_calls.clone();
        let host_call = Function::new(ctx.clone(), move |_args: Rest<Value>| {
            counter.set(counter.get() + 1);
        })
        .unwrap();
        global.set("__host_call", host_call).unwrap();
        let console = Object::new(ctx.clone()).unwrap();
        for level in ["log", "info", "warn", "error", "debug"] {
            console
                .set(
                    level,
                    Function::new(ctx.clone(), |_args: Rest<Value>| {}).unwrap(),
                )
                .unwrap();
        }
        global.set("console", console).unwrap();
        ctx.eval::<(), _>(STUB).unwrap();
        ctx.eval::<(), _>(bootstrap).unwrap();
    });

    let soon = || Some(Instant::now() + Duration::from_secs(5));
    deadline.set(soon());
    let wrapped = format!("(function() {{ {source} }})();");
    let load = ctx
        .with(|ctx| {
            let mut opts = EvalOptions::default();
            opts.global = true;
            opts.filename = Some(format!("{name}.js"));
            ctx.eval_with_options::<(), _>(wrapped.as_bytes(), opts)
                .map_err(|e| error_string(&ctx, e))
        })
        .and_then(|_| run_jobs(&rt));
    deadline.set(soon());
    let fire = ctx
        .with(|ctx| {
            ctx.eval::<(), _>("__fireAll()")
                .map_err(|e| error_string(&ctx, e))
        })
        .and_then(|_| run_jobs(&rt));
    deadline.set(None);
    let handlers: serde_json::Value = ctx.with(|ctx| {
        ctx.eval::<String, _>("__report()")
            .ok()
            .map(|s| serde_json::from_str(&s).unwrap())
            .unwrap_or(json!([]))
    });

    json!({
        "plugin": name,
        "load": load.err(),
        "fire": fire.err(),
        "host_calls": host_calls.get(),
        "handlers": handlers,
    })
}
