//! Behavioural tests for the translation store, driven through the public
//! API only. The randomized cases run a seeded generator over a few hundred
//! catalogs and templates each; the fixed cases pin the specific edges those
//! properties are blind to.

mod common;

use common::{distinct_words, locale_guard, Rng};
use fresh_i18n::{available_locales, register_locales, set_locale, t, translate, translate_in};
use serde_json::{json, Map, Value};

/// Build a nested JSON object from `paths`, each of which is a segment list.
fn nested(paths: &[(Vec<String>, String)]) -> String {
    let mut root = Map::new();
    for (segments, value) in paths {
        let (last, parents) = segments.split_last().expect("non-empty path");
        let mut node = &mut root;
        for segment in parents {
            node = node
                .entry(segment.clone())
                .or_insert_with(|| Value::Object(Map::new()))
                .as_object_mut()
                .expect("generated paths never collide");
        }
        node.insert(last.clone(), Value::String(value.clone()));
    }
    Value::Object(root).to_string()
}

/// A catalog of `count` entries whose paths share no prefix, so nesting them
/// can never turn one entry's leaf into another's parent.
fn catalog(rng: &mut Rng, count: usize) -> Vec<(Vec<String>, String)> {
    distinct_words(rng, count, 1, 6)
        .into_iter()
        .map(|root| {
            let depth = rng.between(0, 3);
            let mut segments = vec![root];
            segments.extend((0..depth).map(|_| rng.word(1, 5)));
            (segments, rng.text(0, 24))
        })
        .collect()
}

fn dotted(segments: &[String]) -> String {
    segments.join(".")
}

// ---------------------------------------------------------------------------
// Lookup
// ---------------------------------------------------------------------------

#[test]
fn every_registered_string_comes_back_verbatim() {
    let _guard = locale_guard();
    let mut rng = Rng::new(0x5eed_0001);

    for round in 0..64 {
        let count = rng.between(1, 12);
        let entries = catalog(&mut rng, count);
        let code = format!("x-verbatim-{round}");
        register_locales(&[(&code, &nested(&entries))]);
        set_locale(&code);

        for (segments, value) in &entries {
            let key = dotted(segments);
            assert_eq!(t!(&key), *value, "key {key} in {code}");
        }
    }
}

#[test]
fn a_key_that_names_a_subtree_is_not_a_translation() {
    let _guard = locale_guard();
    register_locales(&[(
        "x-subtree",
        &json!({"menu": {"file": {"open": "Open"}}}).to_string(),
    )]);
    set_locale("x-subtree");

    assert_eq!(t!("menu.file.open"), "Open");
    for prefix in ["menu", "menu.file"] {
        assert_eq!(t!(prefix), prefix, "{prefix} names an object, not a string");
    }
}

#[test]
fn an_unregistered_key_translates_to_itself() {
    let _guard = locale_guard();
    let mut rng = Rng::new(0x5eed_0002);

    let entries = catalog(&mut rng, 8);
    register_locales(&[("x-absent", &nested(&entries))]);
    set_locale("x-absent");

    for _ in 0..256 {
        let key = dotted(&[rng.word(1, 6), rng.word(1, 6), rng.word(1, 6)]);
        if entries.iter().any(|(s, _)| dotted(s) == key) {
            continue;
        }
        assert_eq!(t!(&key), key);
    }
}

#[test]
fn keys_prefixed_with_underscore_are_invisible_at_every_depth() {
    let _guard = locale_guard();
    register_locales(&[(
        "x-meta",
        &json!({
            "_version": "1.0",
            "kept": "value",
            "outer": {"_hidden": "no", "shown": "yes"},
            "_branch": {"leaf": "no"}
        })
        .to_string(),
    )]);
    set_locale("x-meta");

    assert_eq!(t!("kept"), "value");
    assert_eq!(t!("outer.shown"), "yes");
    for hidden in ["_version", "outer._hidden", "_branch.leaf"] {
        assert_eq!(t!(hidden), hidden, "{hidden} should not be reachable");
    }
}

#[test]
fn the_active_locale_wins_then_the_fallback_then_the_key() {
    let _guard = locale_guard();
    register_locales(&[
        (
            "x-both",
            &json!({"shared": "active", "only_active": "a"}).to_string(),
        ),
        (
            "en",
            &json!({"shared": "english", "only_english": "e"}).to_string(),
        ),
    ]);

    set_locale("x-both");
    assert_eq!(t!("shared"), "active");
    assert_eq!(t!("only_active"), "a");
    assert_eq!(t!("only_english"), "e");
    assert_eq!(t!("only_nowhere"), "only_nowhere");

    // With the fallback locale active, its own strings are all there is.
    set_locale("en");
    assert_eq!(t!("shared"), "english");
    assert_eq!(t!("only_active"), "only_active");
}

#[test]
fn lookup_walks_down_to_shorter_language_tags() {
    let _guard = locale_guard();
    register_locales(&[("x-zh", &json!({"hello": "base"}).to_string())]);

    for tag in ["x-zh", "x-zh-Hant", "x-zh-Hant-TW", "x-zh-Hant-TW-x-priv"] {
        set_locale(tag);
        assert_eq!(t!("hello"), "base", "{tag} should reach x-zh");
    }

    // A sibling tag is not an ancestor and must not be reached.
    set_locale("x-zhz");
    assert_eq!(t!("hello"), "hello");
}

#[test]
fn an_unknown_locale_falls_back_rather_than_failing() {
    let _guard = locale_guard();
    register_locales(&[("en", &json!({"greet": "Hello"}).to_string())]);
    set_locale("nonexistent-locale");
    assert_eq!(t!("greet"), "Hello");
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

#[test]
fn re_registering_a_locale_replaces_its_catalog() {
    let _guard = locale_guard();
    register_locales(&[("x-replace", &json!({"a": "first", "b": "kept"}).to_string())]);
    set_locale("x-replace");
    assert_eq!(t!("a"), "first");

    register_locales(&[("x-replace", &json!({"a": "second"}).to_string())]);
    assert_eq!(t!("a"), "second");
    assert_eq!(t!("b"), "b", "the replaced catalog no longer has b");
}

#[test]
fn available_locales_stay_sorted_and_unique() {
    let mut rng = Rng::new(0x5eed_0003);

    for _ in 0..32 {
        let count = rng.between(1, 6);
        let mut codes: Vec<String> = distinct_words(&mut rng, count, 2, 5)
            .into_iter()
            .map(|w| format!("x-avail-{w}"))
            .collect();
        rng.shuffle(&mut codes);
        let pairs: Vec<(&str, &str)> = codes.iter().map(|c| (c.as_str(), "{}")).collect();
        register_locales(&pairs);
        register_locales(&pairs); // registering twice must not duplicate

        let available = available_locales();
        assert!(
            available.windows(2).all(|w| w[0] < w[1]),
            "available_locales must be sorted and free of duplicates: {available:?}"
        );
        for code in &codes {
            assert!(available.contains(&code.as_str()), "{code} missing");
        }
    }
}

#[test]
fn a_malformed_catalog_is_inert_rather_than_fatal() {
    let _guard = locale_guard();
    register_locales(&[
        ("x-broken", "{ this is not json"),
        ("en", &json!({"greet": "Hello"}).to_string()),
    ]);
    set_locale("x-broken");

    assert!(available_locales().contains(&"x-broken"));
    assert_eq!(translate_in("x-broken", "greet"), None);
    assert_eq!(t!("greet"), "Hello", "the fallback still answers");
}

#[test]
fn non_string_values_are_skipped_but_their_siblings_survive() {
    let _guard = locale_guard();
    register_locales(&[(
        "x-types",
        &json!({"count": 3, "on": true, "nothing": null, "text": "ok"}).to_string(),
    )]);
    set_locale("x-types");

    assert_eq!(t!("text"), "ok");
    for skipped in ["count", "on", "nothing"] {
        assert_eq!(t!(skipped), skipped);
    }
}

#[test]
fn lookups_from_many_threads_agree() {
    let _guard = locale_guard();
    register_locales(&[("x-threads", &json!({"key": "value"}).to_string())]);
    set_locale("x-threads");

    let handles: Vec<_> = (0..8)
        .map(|n| {
            std::thread::spawn(move || {
                // Every thread races the same first parse, and half of them
                // register further catalogs while the others read.
                for i in 0..64 {
                    if n % 2 == 0 {
                        let code = format!("x-threads-{n}-{i}");
                        register_locales(&[(&code, "{}")]);
                    }
                    assert_eq!(translate_in("x-threads", "key"), Some("value"));
                }
            })
        })
        .collect();
    for handle in handles {
        handle.join().expect("no thread panicked");
    }
}

// ---------------------------------------------------------------------------
// Interpolation
// ---------------------------------------------------------------------------

/// A template of literal chunks interleaved with `%{name}` placeholders,
/// paired with the substitution it must produce.
struct Template {
    text: String,
    args: Vec<(String, String)>,
    expected: String,
}

fn template(rng: &mut Rng, names: &[String]) -> Template {
    let args: Vec<(String, String)> = names.iter().map(|n| (n.clone(), rng.word(1, 8))).collect();

    let mut text = String::new();
    let mut expected = String::new();
    for _ in 0..rng.between(1, 6) {
        let chunk = rng.text(0, 12);
        text.push_str(&chunk);
        expected.push_str(&chunk);
        let (name, value) = rng.pick(&args).clone();
        text.push_str(&format!("%{{{name}}}"));
        expected.push_str(&value);
    }
    let tail = rng.text(0, 12);
    text.push_str(&tail);
    expected.push_str(&tail);

    Template {
        text,
        args,
        expected,
    }
}

fn borrowed(args: &[(String, String)]) -> Vec<(&str, String)> {
    args.iter().map(|(k, v)| (k.as_str(), v.clone())).collect()
}

#[test]
fn every_named_placeholder_is_substituted() {
    let _guard = locale_guard();
    let mut rng = Rng::new(0x5eed_0004);

    for round in 0..128 {
        let count = rng.between(1, 4);
        let names = distinct_words(&mut rng, count, 1, 6);
        let t = template(&mut rng, &names);
        let code = format!("x-interp-{round}");
        register_locales(&[(&code, &json!({ "msg": t.text }).to_string())]);
        set_locale(&code);

        assert_eq!(
            fresh_i18n::translate_with_args("msg", &borrowed(&t.args)),
            t.expected,
            "template {:?}",
            t.text
        );
    }
}

#[test]
fn substitution_does_not_depend_on_argument_order_and_settles_in_one_pass() {
    let _guard = locale_guard();
    let mut rng = Rng::new(0x5eed_0005);

    for round in 0..128 {
        let count = rng.between(1, 4);
        let names = distinct_words(&mut rng, count, 1, 6);
        let t = template(&mut rng, &names);
        let code = format!("x-order-{round}");
        register_locales(&[(&code, &json!({ "msg": t.text }).to_string())]);
        set_locale(&code);

        let once = fresh_i18n::translate_with_args("msg", &borrowed(&t.args)).into_owned();

        let mut shuffled = t.args.clone();
        rng.shuffle(&mut shuffled);
        assert_eq!(
            fresh_i18n::translate_with_args("msg", &borrowed(&shuffled)),
            once,
            "argument order changed the result"
        );

        // Substituted values are never rescanned for placeholders.
        register_locales(&[(&code, &json!({ "again": once.clone() }).to_string())]);
        assert_eq!(
            fresh_i18n::translate_with_args("again", &borrowed(&t.args)),
            once,
            "a second pass changed an already-substituted string"
        );
    }
}

#[test]
fn a_placeholder_with_no_argument_is_left_alone() {
    let _guard = locale_guard();
    let mut rng = Rng::new(0x5eed_0006);

    for round in 0..128 {
        let count = rng.between(1, 4);
        let names = distinct_words(&mut rng, count, 1, 6);
        let t = template(&mut rng, &names);
        let code = format!("x-unbound-{round}");
        register_locales(&[(&code, &json!({ "msg": t.text }).to_string())]);
        set_locale(&code);

        assert_eq!(
            fresh_i18n::translate_with_args("msg", &[]),
            t.text,
            "no arguments must leave the template untouched"
        );
    }
}

#[test]
fn arbitrary_brace_soup_is_handled_without_panicking() {
    let _guard = locale_guard();
    const ALPHABET: &[char] = &['%', '{', '}', 'a', 'b', ' ', '\\', 'é'];
    let mut rng = Rng::new(0x5eed_0007);

    register_locales(&[("x-soup", "{}")]);
    set_locale("x-soup");

    for _ in 0..512 {
        let len = rng.between(0, 24);
        let soup: String = (0..len).map(|_| *rng.pick(ALPHABET)).collect();

        // A missing key is its own template, so this exercises the scanner
        // over text that was never meant to be a translation.
        assert_eq!(fresh_i18n::translate_with_args(&soup, &[]), soup);

        let args = [("a", "A".to_string()), ("b", "B".to_string())];
        let with_args = fresh_i18n::translate_with_args(&soup, &args);
        assert_eq!(
            with_args,
            fresh_i18n::translate_with_args(&soup, &args),
            "substitution is not deterministic for {soup:?}"
        );
        if !soup.contains("%{") {
            assert_eq!(with_args, soup, "text with no placeholder was rewritten");
        }
    }
}

// ---------------------------------------------------------------------------
// The macro
// ---------------------------------------------------------------------------

#[test]
fn the_macro_accepts_every_shape_the_editor_uses() {
    let _guard = locale_guard();
    register_locales(&[(
        "x-macro",
        &json!({
            "plain": "Copy",
            "one": "Found %{count} of them",
            "two": "%{old} became %{new}",
            "keyword": "%{type} register must be 0-9"
        })
        .to_string(),
    )]);
    set_locale("x-macro");

    // A literal key, with and without arguments.
    assert_eq!(t!("plain"), "Copy");
    assert_eq!(t!("one", count = 3), "Found 3 of them");
    assert_eq!(t!("two", old = "a", new = String::from("b")), "a became b");

    // An argument name that is a Rust keyword, written as a string literal.
    assert_eq!(
        t!("keyword", "type" = "digit"),
        "digit register must be 0-9"
    );

    // Trailing commas, in both arms.
    assert_eq!(t!("plain",), "Copy");
    assert_eq!(t!("one", count = 3,), "Found 3 of them");

    // A key that is an expression rather than a literal.
    let owned = String::from("plain");
    let field = ("plain", 0);
    assert_eq!(t!(&owned), "Copy");
    assert_eq!(t!(owned.as_str()), "Copy");
    assert_eq!(t!(field.0), "Copy");
    assert_eq!(t!(if true { "plain" } else { "one" }), "Copy");

    // The result is a `Cow`, so both borrowed and owned uses compile.
    let as_str: &str = &t!("plain");
    assert_eq!(as_str, "Copy");
    assert_eq!(t!("plain").into_owned(), "Copy");
    assert_eq!(translate("plain").to_string(), "Copy");
}

#[test]
fn an_argument_whose_value_is_not_a_string_is_formatted_with_display() {
    let _guard = locale_guard();
    register_locales(&[("x-display", &json!({"msg": "%{a}/%{b}/%{c}"}).to_string())]);
    set_locale("x-display");

    let path = std::path::Path::new("/tmp/x");
    assert_eq!(
        t!("msg", a = 42u64, b = 'x', c = path.display()),
        "42/x//tmp/x"
    );
}
