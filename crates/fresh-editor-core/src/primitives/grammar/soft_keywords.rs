// Contextual ("soft") keywords missing from the grammars syntect bundles.
//
// `include!`d by `build.rs` as well as compiled into the crate, so the
// build-time packdump and the runtime grammar builder apply the same patch.
// Inner doc comments and `crate::` paths are both unusable here: the file is
// pasted into `build.rs`, which is its own crate root.

/// A patch: `(context, regex, scope)`. The regex must contain exactly one
/// capture group, around the keyword itself; `scope` is applied to that group,
/// not to the whole match, so an anchor may consume the leading indentation
/// without colouring it.
type KeywordPatch = (&'static str, &'static str, &'static str);

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
/// context. We prepend one match to the context where it is legal, which is
/// what keeps these soft keywords dark where they are ordinary identifiers —
/// `match = re.match(...)`, `case = match` and `match[0]` all stay plain.
///
/// A missing context is a loud failure rather than a silently dropped fix: the
/// `panic!` below fails the build (`build.rs` calls this), so a syntect upgrade
/// that renames a context cannot quietly un-fix the issue.
const SOFT_KEYWORD_PATCHES: &[(&str, &[KeywordPatch])] = &[(
    "Python",
    &[(
        // `match`/`case` are keywords only at the head of a compound statement,
        // so the pattern is anchored to the start of the line. The anchor is
        // load-bearing: `block-statements` stays on the stack after `elif`,
        // which — unlike `if`/`while`/`for`/`with`/`except` — scopes its
        // keyword without pushing a sub-context. Without `^` this fired on real
        // code such as CPython's `fractions.py`:
        //     elif match["align"] is not None and ...:
        // It also bounds the cost of the closing lookahead to one attempt per
        // line instead of one per candidate token.
        //
        // Then: reject the shapes that make the word an identifier — a
        // following `=` (assignment), `:` (annotation), `(` (call) or `,` — and
        // require a `:` to close the header, optionally followed by a comment.
        // `\s*$` rather than a bare `$` so the trailing newline syntect feeds us
        // is absorbed under both regex engines: onig's `$` is a line anchor,
        // fancy-regex's (used for `wasm`) is end-of-text.
        //
        // Known gaps, both rare and deliberate. A parenthesised subject
        // (`match (a, b):`) is skipped because `(` is what protects
        // `match(...)` call statements, and an inline suite (`case 1: pass`) is
        // skipped because telling that `:` from a dict literal's needs a parser.
        "block-statements",
        r"^[ \t]*(match|case)\b(?!\s*[=:,()])(?=[^\n]*:[^\S\n]*(?:#[^\n]*)?\s*$)",
        "keyword.control.flow.python",
    )],
)];

/// Prepend the soft-keyword patterns for `syntax`, if any apply to it.
///
/// Patterns are only ever prepended to an existing context; no context key is
/// added or removed. That keeps the patch to "one more alternative, tried
/// first" — `build()` re-numbers and re-links every context by name afterwards,
/// so adding a key would be sound too, but removing a referenced one would not.
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
                 upgrade probably renamed it — re-point this patch (issue #3509) \
                 rather than dropping it.",
                syntax.name, context_name
            )
        });
        context.patterns.insert(
            0,
            Pattern::Match(MatchPattern {
                // No backreferences in the pattern; `has_captures` is about
                // those, not about the capture-to-scope map below.
                has_captures: false,
                regex: Regex::new((*regex).to_string()),
                // Empty: the scope goes on capture group 1 (the keyword) so the
                // leading indentation the anchor consumes stays unscoped.
                scope: Vec::new(),
                captures: Some(vec![(
                    1,
                    vec![Scope::new(scope).expect("soft-keyword scope parses")],
                )]),
                operation: MatchOperation::None,
                with_prototype: None,
            }),
        );
    }
}
