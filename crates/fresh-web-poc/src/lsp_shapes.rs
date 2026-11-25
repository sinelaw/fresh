//! Language servers' answers in one shape each, for the client.
//!
//! The protocol lets a server answer most requests in several shapes: one
//! item, a list or `null`; a `Location` or a `LocationLink`; a
//! `DocumentSymbol` tree or flat `SymbolInformation`; a `Command` or a
//! `CodeAction`; hover contents as a string, a `MarkupContent` or a list of
//! `MarkedString`s. `lsp.call` answers in exactly one shape per method
//! (below), so the client reads a typed value instead of testing shapes.
//! A method not listed here is answered as the server answered.
//!
//! Positions and ranges stay the protocol's (`{line, character}`, UTF-16
//! columns); files are paths (a location outside the file system is
//! dropped).

use crate::lsp;
use serde_json::{json, Value};

/// `r`, the server's answer to `method`, in its one shape.
pub fn plain(method: &str, r: Value) -> Value {
    match method {
        // [{path, range}]
        "textDocument/definition"
        | "textDocument/declaration"
        | "textDocument/typeDefinition"
        | "textDocument/implementation"
        | "textDocument/references" => list(r).iter().filter_map(location).collect(),
        // [Symbol]: {name, kind, container, range, children: [Symbol]}
        "textDocument/documentSymbol" => list(r).iter().map(document_symbol).collect(),
        // [{name, kind, container, path, range}]
        "workspace/symbol" => list(r).iter().filter_map(workspace_symbol).collect(),
        // the contents as Markdown text, or null
        "textDocument/hover" => hover_text(&r["contents"]),
        // [{label, detail, insert}]
        "textDocument/completion" => {
            let items = if r["items"].is_array() {
                r["items"].clone()
            } else {
                r
            };
            list(items).iter().map(completion).collect()
        }
        // [{title, kind, edit: [FileEdit] | null, command: {command, arguments} | null}]
        "textDocument/codeAction" => list(r).iter().map(code_action).collect(),
        // [FileEdit]: {path, edits: [{range, newText}]}
        "textDocument/rename" => workspace_edit(&r),
        // [{range, newText}]
        "textDocument/formatting" | "textDocument/rangeFormatting" => {
            list(r).iter().map(text_edit).collect()
        }
        // {label, documentation} of the active signature, or null
        "textDocument/signatureHelp" => signature(&r),
        // [{position, label, paddingLeft, paddingRight}]
        "textDocument/inlayHint" => list(r).iter().map(inlay_hint).collect(),
        // [{start, end}]: document lines
        "textDocument/foldingRange" => list(r)
            .iter()
            .map(|f| json!({"start": f["startLine"], "end": f["endLine"]}))
            .collect(),
        // [[range]]: each position's ranges, innermost first
        "textDocument/selectionRange" => list(r).iter().map(selection_ranges).collect(),
        _ => r,
    }
}

/// One item, a list of them, or none, as a list.
fn list(r: Value) -> Vec<Value> {
    match r {
        Value::Array(a) => a,
        Value::Null => vec![],
        one => vec![one],
    }
}

/// A string field, or null.
fn text_or_null(v: &Value) -> Value {
    v.as_str().map_or(Value::Null, |s| json!(s))
}

/// A range, or the empty one at the start where there is none.
fn range(v: &Value) -> Value {
    if v["start"].is_object() && v["end"].is_object() {
        v.clone()
    } else {
        json!({"start": {"line": 0, "character": 0}, "end": {"line": 0, "character": 0}})
    }
}

/// A `file://` URI as a path.
fn path(uri: &Value) -> Option<String> {
    Some(lsp::path_of(uri.as_str()?)?.to_string_lossy().into_owned())
}

/// A `Location` or a `LocationLink` as `{path, range}`.
fn location(l: &Value) -> Option<Value> {
    let (uri, r) = if l["targetUri"].is_string() {
        (&l["targetUri"], &l["targetSelectionRange"])
    } else {
        (&l["uri"], &l["range"])
    };
    Some(json!({"path": path(uri)?, "range": range(r)}))
}

/// A `DocumentSymbol` (its range: the name's) or a `SymbolInformation`.
fn document_symbol(s: &Value) -> Value {
    let r = if s["selectionRange"].is_object() {
        &s["selectionRange"]
    } else if s["location"].is_object() {
        &s["location"]["range"]
    } else {
        &s["range"]
    };
    json!({
        "name": s["name"].as_str().unwrap_or_default(),
        "kind": s["kind"].as_u64().unwrap_or(0),
        "container": text_or_null(&s["containerName"]),
        "range": range(r),
        "children": list(s["children"].clone()).iter().map(document_symbol).collect::<Vec<_>>(),
    })
}

/// A `SymbolInformation` or `WorkspaceSymbol` in a file.
fn workspace_symbol(s: &Value) -> Option<Value> {
    Some(json!({
        "name": s["name"].as_str().unwrap_or_default(),
        "kind": s["kind"].as_u64().unwrap_or(0),
        "container": text_or_null(&s["containerName"]),
        "path": path(&s["location"]["uri"])?,
        "range": range(&s["location"]["range"]),
    }))
}

/// Hover contents (a string, `MarkupContent`, a `MarkedString` or a list
/// of them) as one Markdown text; null when empty.
fn hover_text(c: &Value) -> Value {
    let part = |x: &Value| match x {
        Value::String(s) => s.clone(),
        o => o["value"].as_str().unwrap_or_default().to_string(),
    };
    let text = match c {
        Value::Array(a) => a.iter().map(part).collect::<Vec<_>>().join("\n\n"),
        Value::Null => String::new(),
        one => part(one),
    };
    if text.trim().is_empty() {
        Value::Null
    } else {
        json!(text)
    }
}

fn completion(i: &Value) -> Value {
    let label = i["label"].as_str().unwrap_or_default();
    let insert = i["textEdit"]["newText"]
        .as_str()
        .or(i["insertText"].as_str())
        .unwrap_or(label);
    json!({"label": label, "detail": text_or_null(&i["detail"]), "insert": insert})
}

/// A `Command` as `{command, arguments}`, its arguments as JSON text (the
/// client sends them back as they are).
fn command(c: &Value) -> Value {
    let args = if c["arguments"].is_array() {
        c["arguments"].clone()
    } else {
        json!([])
    };
    json!({"command": c["command"], "arguments": args.to_string()})
}

/// A `CodeAction` or a bare `Command`.
fn code_action(a: &Value) -> Value {
    let bare = a["command"].is_string();
    json!({
        "title": a["title"].as_str().unwrap_or_default(),
        "kind": text_or_null(&a["kind"]),
        "edit": if a["edit"].is_object() { workspace_edit(&a["edit"]) } else { Value::Null },
        "command": if bare { command(a) } else if a["command"].is_object() { command(&a["command"]) } else { Value::Null },
    })
}

fn text_edit(e: &Value) -> Value {
    json!({"range": range(&e["range"]), "newText": e["newText"].as_str().unwrap_or_default()})
}

/// A `WorkspaceEdit` (its `changes` or its `documentChanges`' text edits)
/// as `[{path, edits}]`, a file once.
fn workspace_edit(e: &Value) -> Value {
    let mut files: Vec<(String, Vec<Value>)> = vec![];
    let mut add = |uri: &Value, edits: &Value| {
        let Some(p) = path(uri) else { return };
        let edits = list(edits.clone())
            .iter()
            .map(text_edit)
            .collect::<Vec<_>>();
        match files.iter_mut().find(|(q, _)| *q == p) {
            Some((_, es)) => es.extend(edits),
            None => files.push((p, edits)),
        }
    };
    if let Some(changes) = e["changes"].as_object() {
        for (uri, edits) in changes {
            add(&json!(uri), edits);
        }
    }
    for dc in list(e["documentChanges"].clone()) {
        if dc["textDocument"].is_object() {
            add(&dc["textDocument"]["uri"], &dc["edits"]);
        }
    }
    files
        .into_iter()
        .map(|(p, edits)| json!({"path": p, "edits": edits}))
        .collect()
}

fn signature(r: &Value) -> Value {
    let active = r["activeSignature"].as_u64().unwrap_or(0) as usize;
    let Some(s) = r["signatures"].get(active) else {
        return Value::Null;
    };
    let doc = &s["documentation"];
    let documentation = match doc {
        Value::String(t) => json!(t),
        o => text_or_null(&o["value"]),
    };
    json!({"label": s["label"].as_str().unwrap_or_default(), "documentation": documentation})
}

fn inlay_hint(h: &Value) -> Value {
    let label = match &h["label"] {
        Value::String(s) => s.clone(),
        parts => list(parts.clone())
            .iter()
            .map(|p| p["value"].as_str().unwrap_or_default().to_string())
            .collect(),
    };
    json!({
        "position": h["position"],
        "label": label,
        "paddingLeft": h["paddingLeft"].as_bool().unwrap_or(false),
        "paddingRight": h["paddingRight"].as_bool().unwrap_or(false),
    })
}

/// A `SelectionRange` and its parents, innermost first.
fn selection_ranges(s: &Value) -> Value {
    let mut out = vec![];
    let mut x = s;
    while x.is_object() {
        out.push(range(&x["range"]));
        x = &x["parent"];
    }
    Value::Array(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    const R: fn(u64) -> Value =
        |l| json!({"start": {"line": l, "character": 0}, "end": {"line": l, "character": 3}});

    #[test]
    fn locations_and_links_are_one_list() {
        assert_eq!(plain("textDocument/definition", Value::Null), json!([]));
        let one = plain(
            "textDocument/definition",
            json!({"uri": "file:///a.rs", "range": R(1)}),
        );
        assert_eq!(one, json!([{"path": "/a.rs", "range": R(1)}]));
        let link = json!([{"targetUri": "file:///b.rs", "targetRange": R(0), "targetSelectionRange": R(2)}, {"uri": "jdt://x", "range": R(0)}]);
        assert_eq!(
            plain("textDocument/definition", link),
            json!([{"path": "/b.rs", "range": R(2)}])
        );
    }

    #[test]
    fn symbols_are_one_tree() {
        let tree = json!([{"name": "S", "kind": 23, "range": R(0), "selectionRange": R(1), "children": [{"name": "f", "kind": 8, "range": R(2), "selectionRange": R(2)}]}]);
        assert_eq!(
            plain("textDocument/documentSymbol", tree),
            json!([{"name": "S", "kind": 23, "container": null, "range": R(1), "children": [{"name": "f", "kind": 8, "container": null, "range": R(2), "children": []}]}])
        );
        let flat = json!([{"name": "g", "kind": 12, "containerName": "m", "location": {"uri": "file:///a.py", "range": R(4)}}]);
        assert_eq!(
            plain("textDocument/documentSymbol", flat),
            json!([{"name": "g", "kind": 12, "container": "m", "range": R(4), "children": []}])
        );
    }

    #[test]
    fn hover_is_text_or_null() {
        assert_eq!(
            plain(
                "textDocument/hover",
                json!({"contents": {"kind": "markdown", "value": "x"}})
            ),
            json!("x")
        );
        assert_eq!(
            plain(
                "textDocument/hover",
                json!({"contents": ["a", {"language": "rust", "value": "b"}]})
            ),
            json!("a\n\nb")
        );
        assert_eq!(plain("textDocument/hover", Value::Null), Value::Null);
    }

    #[test]
    fn actions_commands_and_edits() {
        let r = json!([
            {"title": "Run", "command": "x.run", "arguments": [1]},
            {"title": "Fix", "kind": "quickfix", "edit": {"changes": {"file:///a.rs": [{"range": R(0), "newText": "y"}]}}},
        ]);
        assert_eq!(
            plain("textDocument/codeAction", r),
            json!([
                {"title": "Run", "kind": null, "edit": null, "command": {"command": "x.run", "arguments": "[1]"}},
                {"title": "Fix", "kind": "quickfix", "edit": [{"path": "/a.rs", "edits": [{"range": R(0), "newText": "y"}]}], "command": null},
            ])
        );
        let rename = json!({"documentChanges": [{"textDocument": {"uri": "file:///a.rs", "version": 1}, "edits": [{"range": R(1), "newText": "z"}]}, {"kind": "create", "uri": "file:///n.rs"}]});
        assert_eq!(
            plain("textDocument/rename", rename),
            json!([{"path": "/a.rs", "edits": [{"range": R(1), "newText": "z"}]}])
        );
        assert_eq!(plain("textDocument/rename", Value::Null), json!([]));
    }

    #[test]
    fn completion_hints_folds_and_selections() {
        let c = json!({"isIncomplete": false, "items": [{"label": "len", "detail": "fn", "textEdit": {"newText": "len()"}}, {"label": "x"}]});
        assert_eq!(
            plain("textDocument/completion", c),
            json!([{"label": "len", "detail": "fn", "insert": "len()"}, {"label": "x", "detail": null, "insert": "x"}])
        );
        let h = json!([{"position": {"line": 0, "character": 1}, "label": [{"value": ": "}, {"value": "i32"}], "paddingLeft": true}]);
        assert_eq!(
            plain("textDocument/inlayHint", h),
            json!([{"position": {"line": 0, "character": 1}, "label": ": i32", "paddingLeft": true, "paddingRight": false}])
        );
        assert_eq!(
            plain(
                "textDocument/foldingRange",
                json!([{"startLine": 1, "endLine": 4}])
            ),
            json!([{"start": 1, "end": 4}])
        );
        let s = json!([{"range": R(1), "parent": {"range": R(2)}}]);
        assert_eq!(
            plain("textDocument/selectionRange", s),
            json!([[R(1), R(2)]])
        );
        assert_eq!(
            plain(
                "textDocument/signatureHelp",
                json!({"signatures": [{"label": "f(a)", "documentation": {"value": "d"}}]})
            ),
            json!({"label": "f(a)", "documentation": "d"})
        );
    }
}
