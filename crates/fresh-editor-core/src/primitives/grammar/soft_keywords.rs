// Contextual ("soft") keywords missing from the grammars syntect bundles.
//
// `include!`d by `build.rs` as well as compiled into the crate, so the
// build-time packdump and the runtime grammar builder apply the same patch.
// Inner doc comments and `crate::` paths are both unusable here: the file is
// pasted into `build.rs`, which is its own crate root.

/// The keyword patches, grouped by syntect grammar name.
///
/// syntect 5.3 ships the Sublime `Python` and `C++` grammars as they stood
/// before Python 3.10's `match`/`case` and before C++11/20's `final`,
/// `requires` and `co_await`/`co_return`/`co_yield`. None of those tokens is
/// mentioned anywhere in the bundled pack, so the editor scopes them as plain
/// identifiers (issues #3509, #3644).
///
/// We cannot fix this the usual way. The defaults reach us as a pre-linked
/// binary packdump, and `{{variable}}` substitution already happened when the
/// upstream YAML was parsed, so the `variables` map on a loaded definition is
/// inert — appending a keyword there changes nothing. The only remaining seam
/// is the pattern list of a context, so we prepend one match per keyword to the
/// contexts where that keyword is actually legal.
///
/// Scoping them per-context, rather than from one flat keyword list, is what
/// keeps the soft keywords dark where they are ordinary identifiers: `match`
/// and `case` are matched only in the compound-statement context and only on a
/// line that reads like `match subject:`, and `final` only between a class or
/// struct name and its `:`/`{`. So `match = re.match(...)`, `case = match` and
/// `int final = 0;` keep rendering as plain identifiers.
///
/// Each entry is `(context, regex, scope)`. Two invariants:
///   * Patterns are only ever prepended to an **existing** context. `build()`
///     numbers contexts by sorted name, so adding or removing a context key
///     would shift the `ContextId`s already baked into the packdump's links.
///   * A missing context is a hard build error rather than a silently dropped
///     fix, so a syntect bump that renames one fails loudly here.
type KeywordPatch = (&'static str, &'static str, &'static str);

const SOFT_KEYWORD_PATCHES: &[(&str, &[KeywordPatch])] = &[
    (
        "Python",
        &[(
            // `match`/`case` are keywords only at the head of a compound
            // statement. Require a `:` to close the line (a trailing comment is
            // fine) and reject the shapes that make them identifiers: a
            // following `=` (assignment), `:` (annotation), `(` (call), or a
            // leading `.` (attribute access).
            "block-statements",
            r"(?<![.\w])(?:match|case)\b(?!\s*[=:,()])(?=.*:\s*(?:#.*)?$)",
            "keyword.control.flow.python",
        )],
    ),
    (
        "C++",
        &[
            // `class Foo final {` / `class Foo final : Base`. This context is
            // only entered after a class name, so a variable called `final`
            // elsewhere is never considered here.
            (
                "data-structures-class-definition-after-identifier",
                r"\bfinal\b(?=\s*[:{])",
                "storage.modifier.c++",
            ),
            // `struct S final : Base`.
            (
                "data-structures-struct-definition-after-identifier",
                r"\bfinal\b(?=\s*[:{])",
                "storage.modifier.c++",
            ),
            // `requires` clauses and `requires`-expressions. Unlike `final`,
            // `requires` is a reserved word as of C++20 rather than a
            // context-sensitive identifier, so it is matched unconditionally —
            // scoping it only in some positions would colour the same token
            // two different ways within one file. `expressions` is the context
            // the other operand-position keywords live in, and it is reached
            // both from `template-common` (so a `requires` clause trailing a
            // `template <...>` header is covered) and from `statements`.
            ("expressions", r"\brequires\b", "keyword.other.c++"),
            // A `requires` clause trailing a `template <...>` header sits at
            // namespace scope, where the grammar has already committed to
            // "this might be a function declaration" and so never reaches
            // `expressions`.
            ("global-modifier", r"\brequires\b", "keyword.other.c++"),
            (
                "statements",
                r"\bco_(?:await|return|yield)\b",
                "keyword.control.c++",
            ),
        ],
    ),
];

/// Prepend the soft-keyword patterns for `syntax`, if any apply to it.
pub(crate) fn patch_soft_keywords(syntax: &mut syntect::parsing::SyntaxDefinition) {
    use syntect::parsing::syntax_definition::{MatchOperation, MatchPattern, Pattern};
    use syntect::parsing::{Regex, Scope};

    let Some((_, patches)) = SOFT_KEYWORD_PATCHES
        .iter()
        .find(|(name, _)| *name == syntax.name)
    else {
        return;
    };

    for (context_name, regex, scope) in *patches {
        let context = syntax.contexts.get_mut(*context_name).unwrap_or_else(|| {
            panic!(
                "{}: no context {:?} to attach the soft-keyword patch to. A syntect \
                 upgrade probably renamed it — re-point this patch (issues #3509, #3644) \
                 rather than dropping it.",
                syntax.name, context_name
            )
        });
        context.patterns.insert(
            0,
            Pattern::Match(MatchPattern {
                has_captures: false,
                regex: Regex::new((*regex).to_string()),
                scope: vec![Scope::new(scope).expect("soft-keyword scope parses")],
                captures: None,
                operation: MatchOperation::None,
                with_prototype: None,
            }),
        );
    }
}
