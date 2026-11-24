//! Embed the web client (`web/`) into the binary.
use std::path::Path;

fn walk(dir: &Path, base: &Path, out: &mut Vec<String>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir).unwrap().flatten().collect();
    entries.sort_by_key(|e| e.path());
    for e in entries {
        let p = e.path();
        if p.is_dir() {
            walk(&p, base, out);
        } else {
            let rel = p.strip_prefix(base).unwrap().to_string_lossy().replace('\\', "/");
            out.push(format!(
                "    ({:?}, include_bytes!({:?})),\n",
                rel,
                p.canonicalize().unwrap().to_string_lossy()
            ));
        }
    }
}

fn main() {
    let web = Path::new(env!("CARGO_MANIFEST_DIR")).join("web");
    println!("cargo:rerun-if-changed=web");
    let mut items = Vec::new();
    walk(&web, &web, &mut items);
    let src = format!(
        "pub static ASSETS: &[(&str, &[u8])] = &[\n{}];\n",
        items.concat()
    );
    let out = Path::new(&std::env::var("OUT_DIR").unwrap()).join("assets.rs");
    std::fs::write(out, src).unwrap();
}
