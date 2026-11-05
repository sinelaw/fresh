// Contextual ("soft") keywords missing from the grammars syntect bundles.
//
// `include!`d by `build.rs` as well as compiled into the crate, so the
// build-time packdump and the runtime grammar builder apply the same patch.
// Inner doc comments and `crate::` paths are both unusable here: the file is
// pasted into `build.rs`, which is its own crate root.

/// The keyword patches, grouped by syntect grammar name.
///
/// syntect 5.3 ships the Sublime `Python` grammar as it stood before Python
/// 3.10's `match`/`case`, which it therefore scopes as plain identifiers
/// (issue #3509).
///
/// Unlike C/C++ — fixed by vendoring the current upstream grammars, see
/// `CPP_GRAMMAR` — Python cannot be fixed that way. Upstream's `match`/`case`
/// support is built on `branch_point`/`branch`/`fail`, and syntect implements
/// none of the three: it reads only `push`/`set`/`pop` and silently ignores the
/// rest, so the entry pattern (a zero-width `(?=\bcase\b)` whose only action
/// is `branch`) would scope nothing at all. The newest upstream grammar is also
/// `version: 2`, which syntect does not recognise and would misparse as v1.
///
/// So the keyword is added where syntect can act on it: the pattern list of the
/// context. We prepend one match to the contexts where it is actually legal.
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

const SOFT_KEYWORD_PATCHES: &[(&str, &[KeywordPatch])] = &[(
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
)];

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
