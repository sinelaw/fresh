//! Run the dumped bundled plugins (see `compat/README` in the spike) on the
//! system QuickJS through `fresh-quickjs`, printing one JSON line per plugin.
//! `compat-ng` does the same on quickjs-ng through rquickjs.
//!
//! ```sh
//! cargo run --example compat_system -- <dumped-js-dir>          # plugins
//! cargo run --example compat_system -- --probe                  # built-ins
//! ```

use fresh_quickjs::{Context, Runtime};
use serde_json::json;
use std::cell::Cell;
use std::rc::Rc;
use std::time::{Duration, Instant};

const STUB: &str = include_str!("../../compat/stub.js");
const PROBE: &str = include_str!("../../compat/probe.js");

fn main() {
    let arg = std::env::args()
        .nth(1)
        .expect("usage: compat_system <dir> | --probe");
    if arg == "--probe" {
        let rt = Runtime::new().unwrap();
        let ctx = Context::new(&rt).unwrap();
        println!(
            "{}",
            ctx.eval(PROBE, "probe.js").unwrap().to_string().unwrap()
        );
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

fn run_plugin(name: &str, source: &str, bootstrap: &str) -> serde_json::Value {
    let rt = Runtime::new().unwrap();
    rt.set_memory_limit(256 << 20);
    let ctx = Context::new(&rt).unwrap();
    let host_calls = Rc::new(Cell::new(0u32));

    let global = ctx.global();
    let counter = host_calls.clone();
    let host_call = ctx
        .function("__host_call", 1, move |ctx, _this, _args| {
            counter.set(counter.get() + 1);
            Ok(ctx.undefined())
        })
        .unwrap();
    global.set("__host_call", host_call).unwrap();
    let console = ctx.object().unwrap();
    for level in ["log", "info", "warn", "error", "debug"] {
        let f = ctx
            .function(level, 0, |ctx, _this, _args| Ok(ctx.undefined()))
            .unwrap();
        console.set(level, f).unwrap();
    }
    global.set("console", console).unwrap();

    ctx.eval(STUB, "stub.js").unwrap();
    ctx.eval(bootstrap, "bootstrap.js").unwrap();

    let deadline = || Some(Instant::now() + Duration::from_secs(5));
    rt.set_deadline(deadline());
    // Wrapped exactly as QuickJsBackend::execute_js does it.
    let wrapped = format!("(function() {{ {source} }})();");
    let load = ctx
        .eval(&wrapped, &format!("{name}.js"))
        .map(drop)
        .and_then(|_| rt.execute_pending_jobs().map(drop));
    rt.set_deadline(deadline());
    let fire = ctx
        .eval("__fireAll()", "fire.js")
        .map(drop)
        .and_then(|_| rt.execute_pending_jobs().map(drop));
    rt.set_deadline(None);
    let handlers: serde_json::Value = ctx
        .eval("__report()", "report.js")
        .and_then(|v| v.to_string())
        .map(|s| serde_json::from_str(&s).unwrap())
        .unwrap_or(json!([]));

    json!({
        "plugin": name,
        "load": load.err().map(|e| e.to_string()),
        "fire": fire.err().map(|e| e.to_string()),
        "host_calls": host_calls.get(),
        "handlers": handlers,
    })
}
