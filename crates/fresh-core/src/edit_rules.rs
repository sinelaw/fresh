//! Declarative edit rules: what Enter, Tab and Shift+Tab do to a line, given
//! as data and applied by the editor synchronously, inside the key's own
//! edit.
//!
//! Rules come from two places:
//!
//! - a language's `edit_rules` setting (`languages.<id>.edit_rules`: the
//!   built-in defaults, the user's and project's config.json, a language
//!   pack), held per buffer as [`ConfiguredEditRule`]s; and
//! - plugins, which register an [`EditRuleSet`] for some language ids at run
//!   time (`registerEditRules`), kept in the [`EditRuleRegistry`].
//!
//! When one of the keys is pressed, the editor splits the cursor's line at
//! the cursor and asks [`BufferEditRules::apply`] for a rewrite: the
//! buffer's configured rules are tried first, in order, then the
//! plugin-registered sets for its language, in registration order. The
//! first rule that applies wins, and when none does the key does what it
//! always does. Nothing here knows about any particular language: the rules
//! are regular expressions and templates (see [`EditRule`] for the format).
//!
//! The rules run in the editor rather than in a plugin because a plugin
//! handler runs asynchronously: keys typed after Enter were inserted before
//! the handler's edit landed (#3813), and a mode that claimed the keys fought
//! with vi's (#3814).

use crate::api::{EditRule, EditRuleSet, EditRuleTrigger};
use regex::Regex;

/// A reference to a capture group, by number (of the `before` pattern) or by
/// name (of `before`, else of `after`).
#[derive(Debug, Clone, PartialEq, Eq)]
enum CaptureRef {
    Index(usize),
    Name(String),
}

/// One piece of a parsed template.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Piece {
    Text(String),
    Capture(CaptureRef),
    Inc(CaptureRef),
    Cycle(CaptureRef, Vec<String>),
    Dedent(CaptureRef),
    Indent,
}

/// A parsed template; see [`EditRule`] for the syntax.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Template(Vec<Piece>);

fn parse_capture_ref(s: &str) -> Result<CaptureRef, String> {
    if s.is_empty() {
        return Err("empty capture reference".to_string());
    }
    if s.bytes().all(|b| b.is_ascii_digit()) {
        return s
            .parse()
            .map(CaptureRef::Index)
            .map_err(|_| format!("capture number `{s}` is too large"));
    }
    let mut chars = s.chars();
    let first = chars.next().unwrap_or('0');
    if (first.is_ascii_alphabetic() || first == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
    {
        return Ok(CaptureRef::Name(s.to_string()));
    }
    Err(format!("`{s}` is not a capture number or group name"))
}

fn parse_template(src: &str) -> Result<Template, String> {
    let mut pieces = Vec::new();
    let mut text = String::new();
    let mut chars = src.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // `\n`, `\t`, `\\`: a line break, a tab, a backslash, so a template
            // can be written on one line (a Settings field is one line). A
            // backslash before anything else is itself.
            match chars.peek().copied() {
                Some('n') => text.push('\n'),
                Some('t') => text.push('\t'),
                Some('\\') => text.push('\\'),
                _ => {
                    text.push('\\');
                    continue;
                }
            }
            chars.next();
            continue;
        }
        if c != '$' {
            text.push(c);
            continue;
        }
        let piece = match chars.next() {
            Some('$') => {
                text.push('$');
                continue;
            }
            Some(d) if d.is_ascii_digit() => {
                Piece::Capture(CaptureRef::Index(d as usize - '0' as usize))
            }
            Some('{') => {
                let mut body = String::new();
                loop {
                    match chars.next() {
                        Some('}') => break,
                        Some(ch) => body.push(ch),
                        None => return Err("unterminated `${`".to_string()),
                    }
                }
                if body == "indent" {
                    Piece::Indent
                } else if let Some(r) = body.strip_prefix("inc:") {
                    Piece::Inc(parse_capture_ref(r)?)
                } else if let Some(r) = body.strip_prefix("dedent:") {
                    Piece::Dedent(parse_capture_ref(r)?)
                } else if let Some(rest) = body.strip_prefix("cycle:") {
                    let (r, list) = rest
                        .split_once(':')
                        .ok_or("`${cycle:REF:a|b|…}` needs a list of values")?;
                    let values: Vec<String> = list.split('|').map(str::to_string).collect();
                    if values.iter().any(String::is_empty) {
                        return Err("`${cycle}` values must not be empty".to_string());
                    }
                    Piece::Cycle(parse_capture_ref(r)?, values)
                } else {
                    Piece::Capture(parse_capture_ref(&body)?)
                }
            }
            _ => return Err("`$` must be followed by a digit, `{` or `$`".to_string()),
        };
        if !text.is_empty() {
            pieces.push(Piece::Text(std::mem::take(&mut text)));
        }
        pieces.push(piece);
    }
    if !text.is_empty() {
        pieces.push(Piece::Text(text));
    }
    Ok(Template(pieces))
}

impl Template {
    fn capture_refs(&self) -> impl Iterator<Item = &CaptureRef> {
        self.0.iter().filter_map(|p| match p {
            Piece::Capture(r) | Piece::Inc(r) | Piece::Dedent(r) | Piece::Cycle(r, _) => Some(r),
            Piece::Text(_) | Piece::Indent => None,
        })
    }

    /// The template's text, or `None` when a function has nothing to work
    /// with (the rule then does not apply).
    fn expand(&self, caps: &Captures<'_>, ctx: &LineContext<'_>) -> Option<String> {
        let mut out = String::new();
        for piece in &self.0 {
            match piece {
                Piece::Text(t) => {
                    if ctx.line_ending == "\n" {
                        out.push_str(t);
                    } else {
                        out.push_str(&t.replace('\n', ctx.line_ending));
                    }
                }
                Piece::Capture(r) => out.push_str(caps.get(r)),
                Piece::Inc(r) => out.push_str(&increment(caps.get(r))?),
                Piece::Cycle(r, values) => {
                    let value = caps.get(r);
                    let at = values.iter().position(|v| v == value)?;
                    out.push_str(&values[(at + 1) % values.len()]);
                }
                Piece::Dedent(r) => out.push_str(dedent_once(caps.get(r), ctx.tab_width)?),
                Piece::Indent => out.push_str(ctx.indent_unit),
            }
        }
        Some(out)
    }
}

/// `value` as a decimal number plus one, keeping its zero-padding width.
fn increment(value: &str) -> Option<String> {
    if value.is_empty() || !value.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let n: u64 = value.parse().ok()?;
    Some(format!(
        "{:0width$}",
        n.checked_add(1)?,
        width = value.len()
    ))
}

/// `indent` with one indentation level taken off its start: a leading tab,
/// or `width` leading spaces. `None` when it holds less than a level.
fn dedent_once(indent: &str, width: usize) -> Option<&str> {
    if let Some(rest) = indent.strip_prefix('\t') {
        return Some(rest);
    }
    if width > 0 && indent.len() >= width && indent.as_bytes()[..width].iter().all(|&b| b == b' ') {
        return Some(&indent[width..]);
    }
    None
}

/// The capture groups of one rule's match.
struct Captures<'t> {
    before: Option<regex::Captures<'t>>,
    after: Option<regex::Captures<'t>>,
}

impl<'t> Captures<'t> {
    fn get(&self, r: &CaptureRef) -> &'t str {
        let found = match r {
            CaptureRef::Index(i) => self.before.as_ref().and_then(|c| c.get(*i)),
            CaptureRef::Name(n) => self
                .before
                .as_ref()
                .and_then(|c| c.name(n))
                .or_else(|| self.after.as_ref().and_then(|c| c.name(n))),
        };
        found.map_or("", |m| m.as_str())
    }
}

/// Where a key was pressed: the cursor's line split at the cursor, and the
/// buffer's indentation settings.
#[derive(Debug, Clone, Copy)]
pub struct LineContext<'a> {
    /// The line's text from its start to the cursor.
    pub before: &'a str,
    /// The line's text from the cursor to its end, without the line break.
    pub after: &'a str,
    /// One indentation level: `"\t"`, or `tab_width` spaces.
    pub indent_unit: &'a str,
    /// Columns per indentation level (the buffer's `tab_size`).
    pub tab_width: usize,
    /// The buffer's line ending, written for a line break in a template.
    pub line_ending: &'a str,
}

/// A rule's result: the line's new text before and after the cursor. The
/// cursor ends up between the two.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineEdit {
    pub before: String,
    pub after: String,
}

impl LineEdit {
    /// Whether this rewrite only adds text at the cursor, leaving the line's
    /// existing text as it was (a list marker continued on a new line, but
    /// not an empty item cleared or a bullet cycled).
    pub fn only_inserts(&self, ctx: &LineContext<'_>) -> bool {
        self.before.starts_with(ctx.before) && self.after == ctx.after
    }
}

/// Which rewrites a key may take from the rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RuleReach {
    /// Any: the user's own Enter / Tab / Shift+Tab.
    #[default]
    Line,
    /// Only those that add text at the cursor ([`LineEdit::only_inserts`]);
    /// a rule that would rewrite the line is passed over for the next one.
    /// For the Enter of `insert_line_below` (vi's `o`: line end, then
    /// Enter): the command opens a line, so clearing or rewriting the line
    /// it opens from is not its to do.
    InsertOnly,
}

impl RuleReach {
    fn accepts(self, edit: &LineEdit, ctx: &LineContext<'_>) -> bool {
        match self {
            RuleReach::Line => true,
            RuleReach::InsertOnly => edit.only_inserts(ctx),
        }
    }
}

/// An [`EditRule`] with its patterns compiled and templates parsed.
#[derive(Debug, Clone)]
pub struct CompiledEditRule {
    on: EditRuleTrigger,
    before: Option<Regex>,
    after: Option<Regex>,
    insert: Option<Template>,
    replace_before: Option<Template>,
    replace_after: Option<Template>,
}

impl CompiledEditRule {
    /// Compile one rule, or say what is wrong with it.
    pub fn compile(rule: &EditRule) -> Result<Self, String> {
        let regex = |field: &str, src: &Option<String>| -> Result<Option<Regex>, String> {
            src.as_deref()
                .map(|s| Regex::new(s).map_err(|e| format!("{field}: {e}")))
                .transpose()
        };
        let template = |field: &str, src: &Option<String>| -> Result<Option<Template>, String> {
            src.as_deref()
                .map(|s| parse_template(s).map_err(|e| format!("{field}: {e}")))
                .transpose()
        };
        let compiled = Self {
            on: rule.on,
            before: regex("before", &rule.before)?,
            after: regex("after", &rule.after)?,
            insert: template("insert", &rule.insert)?,
            replace_before: template("replace_before", &rule.replace_before)?,
            replace_after: template("replace_after", &rule.replace_after)?,
        };
        if compiled.insert.is_none()
            && compiled.replace_before.is_none()
            && compiled.replace_after.is_none()
        {
            return Err("a rule needs `insert`, `replace_before` or `replace_after`".to_string());
        }
        compiled.check_capture_refs()?;
        Ok(compiled)
    }

    /// Every capture a template names exists in the patterns.
    fn check_capture_refs(&self) -> Result<(), String> {
        let groups = self.before.as_ref().map_or(1, Regex::captures_len);
        let named = |n: &str| {
            [&self.before, &self.after]
                .into_iter()
                .flatten()
                .any(|re| re.capture_names().flatten().any(|g| g == n))
        };
        for (field, t) in [
            ("insert", &self.insert),
            ("replace_before", &self.replace_before),
            ("replace_after", &self.replace_after),
        ] {
            for r in t.iter().flat_map(Template::capture_refs) {
                match r {
                    CaptureRef::Index(i) if *i >= groups => {
                        return Err(format!(
                            "{field}: `${i}` but `before` has {} group(s)",
                            groups - 1
                        ))
                    }
                    CaptureRef::Name(n) if !named(n) => {
                        return Err(format!("{field}: no group named `{n}`"))
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    /// The key this rule answers.
    pub fn trigger(&self) -> EditRuleTrigger {
        self.on
    }

    /// The rewrite this rule makes at `ctx`, or `None` when it does not apply.
    pub fn apply(&self, ctx: &LineContext<'_>) -> Option<LineEdit> {
        let before = match &self.before {
            Some(re) => Some(re.captures(ctx.before)?),
            None => None,
        };
        let after = match &self.after {
            Some(re) => Some(re.captures(ctx.after)?),
            None => None,
        };
        let caps = Captures { before, after };
        // Outer `None`: a template had nothing to work with. Inner: no template.
        let expand = |t: &Option<Template>| -> Option<Option<String>> {
            match t {
                None => Some(None),
                Some(t) => t.expand(&caps, ctx).map(Some),
            }
        };
        let mut new_before = expand(&self.replace_before)?.unwrap_or_else(|| ctx.before.into());
        new_before.push_str(&expand(&self.insert)?.unwrap_or_default());
        let new_after = expand(&self.replace_after)?.unwrap_or_else(|| ctx.after.into());
        Some(LineEdit {
            before: new_before,
            after: new_after,
        })
    }
}

/// One rule of a language's `edit_rules` setting: the rule as written and
/// its compiled form.
///
/// Deserializing compiles the rule, so a rule with a bad pattern or template
/// fails right there, where config loading can drop just that element (with
/// a warning naming it), rather than at the first key press. It serializes
/// as the [`EditRule`] it was written as.
#[derive(Debug, Clone)]
pub struct ConfiguredEditRule {
    rule: EditRule,
    compiled: CompiledEditRule,
}

impl ConfiguredEditRule {
    /// Compile `rule`, or say what is wrong with it.
    ///
    /// A line break written as a real one in a template is kept as the
    /// escape `\n` (see [`escape_line_breaks`]): it means the same, and the
    /// rule as written is what the Settings UI shows and writes back, in a
    /// one-line field.
    pub fn new(mut rule: EditRule) -> Result<Self, String> {
        for template in [
            &mut rule.insert,
            &mut rule.replace_before,
            &mut rule.replace_after,
        ]
        .into_iter()
        .flatten()
        {
            if template.contains('\n') {
                *template = escape_line_breaks(template);
            }
        }
        let compiled = CompiledEditRule::compile(&rule)?;
        Ok(Self { rule, compiled })
    }

    /// The rule as written.
    pub fn rule(&self) -> &EditRule {
        &self.rule
    }

    /// The rule, compiled.
    pub fn compiled(&self) -> &CompiledEditRule {
        &self.compiled
    }
}

/// `template` with each real line break written as the escape `\n`, which
/// the template means the same by. A lone backslash right before a line
/// break stood for a backslash, so it is written `\\`: left alone, it would
/// pair with the `\n` written for the break and read as `\\` + `n`.
fn escape_line_breaks(template: &str) -> String {
    let mut out = String::with_capacity(template.len() + 2);
    let mut chars = template.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\n' => out.push_str("\\n"),
            '\\' => match chars.peek().copied() {
                // Already an escape: keep it as it is.
                Some(next @ ('n' | 't' | '\\')) => {
                    out.push('\\');
                    out.push(next);
                    chars.next();
                }
                Some('\n') => out.push_str("\\\\"),
                _ => out.push('\\'),
            },
            other => out.push(other),
        }
    }
    out
}

impl PartialEq for ConfiguredEditRule {
    fn eq(&self, other: &Self) -> bool {
        self.rule == other.rule
    }
}

impl serde::Serialize for ConfiguredEditRule {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.rule.serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for ConfiguredEditRule {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let rule = EditRule::deserialize(deserializer)?;
        Self::new(rule).map_err(serde::de::Error::custom)
    }
}

impl schemars::JsonSchema for ConfiguredEditRule {
    fn schema_name() -> std::borrow::Cow<'static, str> {
        EditRule::schema_name()
    }

    fn schema_id() -> std::borrow::Cow<'static, str> {
        EditRule::schema_id()
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        EditRule::json_schema(generator)
    }
}

/// An [`EditRuleSet`], compiled.
#[derive(Debug, Clone)]
pub struct CompiledEditRuleSet {
    languages: Vec<String>,
    rules: Vec<CompiledEditRule>,
}

impl CompiledEditRuleSet {
    /// Compile a set, or say which rule is wrong and why.
    pub fn compile(set: &EditRuleSet) -> Result<Self, String> {
        if set.languages.is_empty() {
            return Err("`languages` must name at least one language id".to_string());
        }
        let rules = set
            .rules
            .iter()
            .enumerate()
            .map(|(i, r)| CompiledEditRule::compile(r).map_err(|e| format!("rules[{i}].{e}")))
            .collect::<Result<_, _>>()?;
        Ok(Self {
            languages: set.languages.clone(),
            rules,
        })
    }

    fn applies_to(&self, language: &str) -> bool {
        self.languages.iter().any(|l| l == language)
    }
}

/// Every registered edit rule set, in registration order, keyed by the
/// plugin that registered it and its id within that plugin.
#[derive(Debug, Default)]
pub struct EditRuleRegistry {
    sets: Vec<(String, String, CompiledEditRuleSet)>,
}

impl EditRuleRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add `owner`'s set `id`, replacing (in place) one already under that
    /// name.
    pub fn register(&mut self, owner: &str, id: &str, set: CompiledEditRuleSet) {
        match self.sets.iter_mut().find(|(o, i, _)| o == owner && i == id) {
            Some(entry) => entry.2 = set,
            None => self.sets.push((owner.to_string(), id.to_string(), set)),
        }
    }

    /// Drop `owner`'s set `id`. Whether there was one.
    pub fn unregister(&mut self, owner: &str, id: &str) -> bool {
        let len = self.sets.len();
        self.sets.retain(|(o, i, _)| !(o == owner && i == id));
        self.sets.len() != len
    }

    /// Drop every set `owner` registered; how many there were.
    pub fn unregister_owner(&mut self, owner: &str) -> usize {
        let len = self.sets.len();
        self.sets.retain(|(o, _, _)| o != owner);
        len - self.sets.len()
    }

    /// Whether any rule answers `trigger` in `language`: when not, the key
    /// needs no look at the line.
    pub fn has_rules(&self, language: &str, trigger: EditRuleTrigger) -> bool {
        self.sets
            .iter()
            .filter(|(_, _, s)| s.applies_to(language))
            .any(|(_, _, s)| s.rules.iter().any(|r| r.on == trigger))
    }

    /// The first rule for `trigger` in `language` that applies at `ctx`,
    /// across sets in registration order, and its rewrite.
    pub fn apply(
        &self,
        language: &str,
        trigger: EditRuleTrigger,
        ctx: &LineContext<'_>,
    ) -> Option<LineEdit> {
        self.apply_within(language, trigger, ctx, RuleReach::Line)
    }

    /// [`Self::apply`], taking only a rewrite `reach` allows.
    pub fn apply_within(
        &self,
        language: &str,
        trigger: EditRuleTrigger,
        ctx: &LineContext<'_>,
        reach: RuleReach,
    ) -> Option<LineEdit> {
        self.sets
            .iter()
            .filter(|(_, _, s)| s.applies_to(language))
            .flat_map(|(_, _, s)| s.rules.iter())
            .filter(|r| r.on == trigger)
            .filter_map(|r| r.apply(ctx))
            .find(|e| reach.accepts(e, ctx))
    }
}

/// The edit rules that apply in one buffer: its language's configured
/// rules, then the plugin-registered sets for the language. The first rule
/// that applies wins, so a configured rule shadows a plugin's rule for the
/// same line, and `"edit_rules": []` leaves only the plugins' rules.
#[derive(Clone, Copy)]
pub struct BufferEditRules<'a> {
    /// The buffer's language id.
    pub language: &'a str,
    /// The language's `edit_rules` setting, compiled.
    pub configured: &'a [CompiledEditRule],
    /// The plugin-registered sets, when the caller has them.
    pub registry: Option<&'a EditRuleRegistry>,
}

impl BufferEditRules<'_> {
    /// Whether any rule answers `trigger` here: when not, the key needs no
    /// look at the line.
    pub fn has_rules(&self, trigger: EditRuleTrigger) -> bool {
        self.configured.iter().any(|r| r.on == trigger)
            || self
                .registry
                .is_some_and(|r| r.has_rules(self.language, trigger))
    }

    /// The first rule for `trigger` that applies at `ctx`, and its rewrite.
    pub fn apply(&self, trigger: EditRuleTrigger, ctx: &LineContext<'_>) -> Option<LineEdit> {
        self.apply_within(trigger, ctx, RuleReach::Line)
    }

    /// [`Self::apply`], taking only a rewrite `reach` allows: a rule whose
    /// rewrite it doesn't is passed over for the next.
    pub fn apply_within(
        &self,
        trigger: EditRuleTrigger,
        ctx: &LineContext<'_>,
        reach: RuleReach,
    ) -> Option<LineEdit> {
        self.configured
            .iter()
            .filter(|r| r.on == trigger)
            .filter_map(|r| r.apply(ctx))
            .find(|e| reach.accepts(e, ctx))
            .or_else(|| {
                self.registry
                    .and_then(|r| r.apply_within(self.language, trigger, ctx, reach))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(on: EditRuleTrigger) -> EditRule {
        EditRule {
            on,
            before: None,
            after: None,
            insert: None,
            replace_before: None,
            replace_after: None,
        }
    }

    fn ctx<'a>(before: &'a str, after: &'a str) -> LineContext<'a> {
        LineContext {
            before,
            after,
            indent_unit: "    ",
            tab_width: 4,
            line_ending: "\n",
        }
    }

    fn edit(before: &str, after: &str) -> Option<LineEdit> {
        Some(LineEdit {
            before: before.to_string(),
            after: after.to_string(),
        })
    }

    fn compile(r: EditRule) -> CompiledEditRule {
        CompiledEditRule::compile(&r).unwrap()
    }

    /// The built-in Markdown `edit_rules` (fresh-editor-core's default
    /// `languages.markdown`), written here as the worked example of the
    /// format.
    fn markdown_lists() -> EditRuleSet {
        let r = |on, before: &str, after: Option<&str>, f: fn(&mut EditRule)| {
            let mut rule = rule(on);
            rule.before = Some(before.to_string());
            rule.after = after.map(str::to_string);
            f(&mut rule);
            rule
        };
        use EditRuleTrigger::*;
        EditRuleSet {
            languages: vec!["markdown".into()],
            rules: vec![
                r(
                    Newline,
                    r"^[ \t]*(?:[-*+] \[[ xX]\] |\d+\. |[-*+] )\s*$",
                    Some(r"^\s*$"),
                    |r| {
                        r.replace_before = Some(String::new());
                        r.replace_after = Some(String::new());
                    },
                ),
                r(Newline, r"^([ \t]*)([-*+]) \[[ xX]\] ", None, |r| {
                    r.insert = Some("\n$1$2 [ ] ".into())
                }),
                r(Newline, r"^([ \t]*)(\d+)\. ", None, |r| {
                    r.insert = Some("\n$1${inc:2}. ".into())
                }),
                r(Newline, r"^([ \t]*)([-*+]) ", None, |r| {
                    r.insert = Some("\n$1$2 ".into())
                }),
                r(Newline, r"^([ \t]+)", None, |r| {
                    r.insert = Some("\n$1".into())
                }),
                r(
                    Indent,
                    r"^([ \t]*)([-*+]) (\[[ xX]\] )?\s*$",
                    Some(r"^\s*$"),
                    |r| {
                        r.replace_before = Some("$1${indent}${cycle:2:*|-|+} $3".into());
                        r.replace_after = Some(String::new());
                    },
                ),
                r(
                    Dedent,
                    r"^([ \t]*)([-*+]) (\[[ xX]\] )?\s*$",
                    Some(r"^\s*$"),
                    |r| {
                        r.replace_before = Some("${dedent:1}${cycle:2:+|-|*} $3".into());
                        r.replace_after = Some(String::new());
                    },
                ),
            ],
        }
    }

    fn markdown_registry() -> EditRuleRegistry {
        let mut reg = EditRuleRegistry::new();
        reg.register(
            "markdown",
            "lists",
            CompiledEditRuleSet::compile(&markdown_lists()).unwrap(),
        );
        reg
    }

    #[test]
    fn markdown_newline_continues_and_clears_list_items() {
        let reg = markdown_registry();
        let nl = |b, a| reg.apply("markdown", EditRuleTrigger::Newline, &ctx(b, a));
        assert_eq!(nl("- one", ""), edit("- one\n- ", ""));
        assert_eq!(nl("  * one", "two"), edit("  * one\n  * ", "two"));
        assert_eq!(nl("9. nine", ""), edit("9. nine\n10. ", ""));
        assert_eq!(nl("- [x] done", ""), edit("- [x] done\n- [ ] ", ""));
        // An empty item: the marker goes, whatever whitespace follows.
        assert_eq!(nl("- ", ""), edit("", ""));
        assert_eq!(nl("  1. ", "  "), edit("", ""));
        assert_eq!(nl("- [ ] ", ""), edit("", ""));
        // An empty marker with text after the cursor still continues.
        assert_eq!(nl("- ", "text"), edit("- \n- ", "text"));
        // Plain lines: indentation is copied; nothing at all falls through.
        assert_eq!(nl("    text", ""), edit("    text\n    ", ""));
        assert_eq!(nl("text", ""), None);
        assert_eq!(nl("-not a list", ""), None);
    }

    /// A command's Enter (vi's `o`) takes only rewrites that add text at the
    /// cursor: on an empty item the clearing rule is passed over for the
    /// one that continues the marker, so the line opened from keeps its text.
    #[test]
    fn insert_only_reach_passes_over_rules_that_rewrite_the_line() {
        let reg = markdown_registry();
        let configured: Vec<CompiledEditRule> = markdown_lists()
            .rules
            .iter()
            .map(|r| compile(r.clone()))
            .collect();
        let only = BufferEditRules {
            language: "markdown",
            configured: &configured,
            registry: None,
        };
        let open =
            |b, a| only.apply_within(EditRuleTrigger::Newline, &ctx(b, a), RuleReach::InsertOnly);
        assert_eq!(open("- ", ""), edit("- \n- ", ""));
        assert_eq!(open("1. ", ""), edit("1. \n2. ", ""));
        assert_eq!(open("- [ ] ", ""), edit("- [ ] \n- [ ] ", ""));
        assert_eq!(open("- one", ""), edit("- one\n- ", ""));
        // The key itself still clears the item.
        assert_eq!(
            only.apply(EditRuleTrigger::Newline, &ctx("- ", "")),
            edit("", "")
        );
        // The registry honours it the same way.
        let tab =
            |reach| reg.apply_within("markdown", EditRuleTrigger::Indent, &ctx("* ", ""), reach);
        assert_eq!(tab(RuleReach::Line), edit("    - ", ""));
        assert_eq!(tab(RuleReach::InsertOnly), None);
    }

    #[test]
    fn markdown_tab_and_shift_tab_cycle_blank_bullets() {
        let reg = markdown_registry();
        let tab = |b, a| reg.apply("markdown", EditRuleTrigger::Indent, &ctx(b, a));
        let untab = |b, a| reg.apply("markdown", EditRuleTrigger::Dedent, &ctx(b, a));
        assert_eq!(tab("* ", ""), edit("    - ", ""));
        assert_eq!(tab("    - ", ""), edit("        + ", ""));
        assert_eq!(tab("+ ", ""), edit("    * ", ""));
        assert_eq!(tab("- [x] ", " "), edit("    + [x] ", ""));
        // Not blank, or an ordered item: the normal Tab.
        assert_eq!(tab("- item", ""), None);
        assert_eq!(tab("1. ", ""), None);
        assert_eq!(untab("    - ", ""), edit("* ", ""));
        assert_eq!(untab("        + ", ""), edit("    - ", ""));
        assert_eq!(untab("\t* [ ] ", ""), edit("+ [ ] ", ""));
        // No whole level to remove: falls through to the normal Shift+Tab.
        assert_eq!(untab("- ", ""), None);
        assert_eq!(untab("  - ", ""), None);
    }

    #[test]
    fn rules_apply_only_to_their_languages_and_trigger() {
        let reg = markdown_registry();
        assert!(reg.has_rules("markdown", EditRuleTrigger::Newline));
        assert!(!reg.has_rules("rust", EditRuleTrigger::Newline));
        assert_eq!(
            reg.apply("rust", EditRuleTrigger::Newline, &ctx("- one", "")),
            None
        );
    }

    #[test]
    fn a_comment_continuation_rule_for_another_language() {
        // Nothing Markdown about the engine: `//` comments continue in C.
        let mut r = rule(EditRuleTrigger::Newline);
        r.before = Some(r"^(?P<lead>\s*//+ ?)\S".into());
        r.insert = Some("\n${lead}".into());
        let set = EditRuleSet {
            languages: vec!["c".into(), "cpp".into()],
            rules: vec![r],
        };
        let mut reg = EditRuleRegistry::new();
        reg.register("p", "c", CompiledEditRuleSet::compile(&set).unwrap());
        assert_eq!(
            reg.apply("cpp", EditRuleTrigger::Newline, &ctx("  // note", "")),
            edit("  // note\n  // ", "")
        );
        assert_eq!(
            reg.apply("c", EditRuleTrigger::Newline, &ctx("int x;", "")),
            None
        );
    }

    #[test]
    fn templates_substitute_captures_and_functions() {
        let mut r = rule(EditRuleTrigger::Newline);
        r.before = Some(r"^(?<n>\d+)(x)?".into());
        r.after = Some(r"(?<tail>.*)".into());
        r.replace_before = Some("[$1|$2|${n}|${inc:n}|$$|${tail}]".into());
        let c = compile(r);
        assert_eq!(
            c.apply(&ctx("41", "end")).unwrap().before,
            "[41||41|42|$|end]"
        );
        assert_eq!(
            c.apply(&ctx("007x", "")).unwrap().before,
            "[007|x|007|008|$|]"
        );
        assert_eq!(c.apply(&ctx("x", "")), None, "`before` must match");
    }

    #[test]
    fn increment_keeps_padding_and_rejects_non_numbers() {
        assert_eq!(increment("1").as_deref(), Some("2"));
        assert_eq!(increment("9").as_deref(), Some("10"));
        assert_eq!(increment("09").as_deref(), Some("10"));
        assert_eq!(increment("007").as_deref(), Some("008"));
        assert_eq!(increment(""), None);
        assert_eq!(increment("a1"), None);
        assert_eq!(increment("99999999999999999999999"), None);
    }

    #[test]
    fn cycle_wraps_and_fails_outside_the_list() {
        let mut r = rule(EditRuleTrigger::Indent);
        r.before = Some(r"(\S)$".into());
        r.insert = Some("${cycle:1:a|b|c}".into());
        let c = compile(r);
        assert_eq!(c.apply(&ctx("a", "")).unwrap().before, "ab");
        assert_eq!(c.apply(&ctx("c", "")).unwrap().before, "ca");
        assert_eq!(c.apply(&ctx("z", "")), None);
    }

    #[test]
    fn indent_and_dedent_follow_the_buffer_settings() {
        let mut r = rule(EditRuleTrigger::Dedent);
        r.before = Some(r"^(\s*)x$".into());
        r.replace_before = Some("${dedent:1}x${indent}".into());
        let c = compile(r);
        let tabs = LineContext {
            indent_unit: "\t",
            tab_width: 2,
            ..ctx("", "")
        };
        assert_eq!(
            c.apply(&LineContext {
                before: "\t\tx",
                ..tabs
            })
            .unwrap()
            .before,
            "\tx\t"
        );
        assert_eq!(
            c.apply(&LineContext {
                before: "   x",
                ..tabs
            })
            .unwrap()
            .before,
            " x\t"
        );
        assert_eq!(
            c.apply(&LineContext {
                before: " x",
                ..tabs
            }),
            None
        );
        assert_eq!(c.apply(&ctx("     x", "")).unwrap().before, " x    ");
        assert_eq!(dedent_once("   ", 4), None);
        assert_eq!(dedent_once("", 0), None);
    }

    #[test]
    fn line_breaks_use_the_buffer_line_ending() {
        let mut r = rule(EditRuleTrigger::Newline);
        r.insert = Some("\n> ".into());
        let c = compile(r);
        let crlf = LineContext {
            line_ending: "\r\n",
            ..ctx("> quote", "")
        };
        assert_eq!(c.apply(&crlf).unwrap().before, "> quote\r\n> ");
    }

    #[test]
    fn replace_after_and_insert_place_the_cursor_between() {
        let mut r = rule(EditRuleTrigger::Newline);
        r.after = Some(r"^(?<rest>.*)$".into());
        r.insert = Some("<".into());
        r.replace_after = Some(">${rest}".into());
        assert_eq!(compile(r).apply(&ctx("a", "b")), edit("a<", ">b"));
    }

    #[test]
    fn invalid_rules_are_rejected_with_a_reason() {
        let bad = |f: fn(&mut EditRule)| {
            let mut r = rule(EditRuleTrigger::Newline);
            r.insert = Some(String::new());
            f(&mut r);
            CompiledEditRule::compile(&r).unwrap_err()
        };
        assert!(bad(|r| r.before = Some("(".into())).starts_with("before:"));
        assert!(bad(|r| r.insert = Some("$x".into())).starts_with("insert:"));
        assert!(bad(|r| r.insert = Some("${oops".into())).contains("unterminated"));
        assert!(bad(|r| r.insert = Some("$1".into())).contains("0 group"));
        assert!(bad(|r| r.insert = Some("${name}".into())).contains("no group named"));
        assert!(bad(|r| r.insert = Some("${cycle:0:a||b}".into())).contains("empty"));
        assert!(bad(|r| r.insert = Some("${cycle:0}".into())).contains("list"));
        assert!(bad(|r| r.insert = None).contains("needs"));
        let err = CompiledEditRuleSet::compile(&EditRuleSet {
            languages: vec!["x".into()],
            rules: vec![rule(EditRuleTrigger::Indent)],
        })
        .unwrap_err();
        assert!(err.starts_with("rules[0]."), "{err}");
        assert!(CompiledEditRuleSet::compile(&EditRuleSet {
            languages: vec![],
            rules: vec![],
        })
        .is_err());
    }

    #[test]
    fn the_rule_format_rejects_unknown_fields() {
        let err = serde_json::from_value::<EditRuleSet>(serde_json::json!({
            "languages": ["x"],
            "rules": [{ "on": "newline", "insert": "", "replace_befor": "" }]
        }))
        .unwrap_err();
        assert!(err.to_string().contains("replace_befor"), "{err}");
        // snake_case (config.json) and camelCase (plugins) both work.
        for key in ["replace_before", "replaceBefore"] {
            let ok: EditRuleSet = serde_json::from_value(serde_json::json!({
                "languages": ["x"],
                "rules": [{ "on": "dedent", "before": "^a", key: "" }]
            }))
            .unwrap();
            assert_eq!(ok.rules[0].on, EditRuleTrigger::Dedent);
            assert_eq!(ok.rules[0].replace_before.as_deref(), Some(""));
        }
    }

    #[test]
    fn registry_replaces_and_unregisters_by_owner() {
        let set = |insert: &str| {
            let mut r = rule(EditRuleTrigger::Newline);
            r.insert = Some(insert.into());
            CompiledEditRuleSet::compile(&EditRuleSet {
                languages: vec!["t".into()],
                rules: vec![r],
            })
            .unwrap()
        };
        let nl = |reg: &EditRuleRegistry| {
            reg.apply("t", EditRuleTrigger::Newline, &ctx("", ""))
                .map(|e| e.before)
        };
        let mut reg = EditRuleRegistry::new();
        reg.register("a", "one", set("A1"));
        reg.register("b", "one", set("B1"));
        assert_eq!(nl(&reg).as_deref(), Some("A1"), "first registered wins");
        reg.register("a", "one", set("A2"));
        assert_eq!(
            nl(&reg).as_deref(),
            Some("A2"),
            "re-registering replaces in place"
        );
        assert!(reg.unregister("a", "one"));
        assert!(!reg.unregister("a", "one"));
        assert_eq!(nl(&reg).as_deref(), Some("B1"));
        reg.register("b", "two", set("B2"));
        assert_eq!(reg.unregister_owner("b"), 2);
        assert_eq!(nl(&reg), None);
        assert!(!reg.has_rules("t", EditRuleTrigger::Newline));
    }

    #[test]
    fn a_configured_rule_compiles_when_deserialized() {
        let ok: Vec<ConfiguredEditRule> = serde_json::from_value(serde_json::json!([
            { "on": "newline", "before": "^(#+ )", "insert": "\n$1" }
        ]))
        .unwrap();
        assert_eq!(
            ok[0].compiled().apply(&ctx("## x", "")),
            edit("## x\n## ", "")
        );
        // Written back as written, but for the line break, which is kept as
        // the escape `\n` (it means the same and fits a one-line field): no
        // nulls for the fields left out.
        assert_eq!(
            serde_json::to_value(&ok[0]).unwrap(),
            serde_json::json!({ "on": "newline", "before": "^(#+ )", "insert": r"\n$1" })
        );
        let again: ConfiguredEditRule =
            serde_json::from_value(serde_json::to_value(&ok[0]).unwrap()).unwrap();
        assert_eq!(again, ok[0], "the written form reads back the same");
        // A bad pattern, template or field fails to deserialize, naming why.
        let err = |v: serde_json::Value| {
            serde_json::from_value::<ConfiguredEditRule>(v)
                .unwrap_err()
                .to_string()
        };
        let bad_regex = err(serde_json::json!({ "on": "newline", "before": "(", "insert": "" }));
        assert!(bad_regex.starts_with("before:"), "{bad_regex}");
        let bad_template = err(serde_json::json!({ "on": "indent", "insert": "${nope:1}" }));
        assert!(bad_template.starts_with("insert:"), "{bad_template}");
        let unknown = err(serde_json::json!({ "on": "dedent", "insert": "", "when": "x" }));
        assert!(unknown.contains("when"), "{unknown}");
    }

    /// `\n`, `\t` and `\\` in a template are a line break, a tab and a
    /// backslash; a backslash before anything else is itself. A real line
    /// break means the same as `\n`.
    #[test]
    fn templates_take_backslash_escapes() {
        let apply = |insert: &str| {
            compile(rule_with_insert(EditRuleTrigger::Newline, insert)).apply(&ctx("a", ""))
        };
        assert_eq!(apply(r"\n- "), edit("a\n- ", ""));
        assert_eq!(apply(r"\t|"), edit("a\t|", ""));
        assert_eq!(apply(r"\\n"), edit("a\\n", ""));
        assert_eq!(apply(r"\x\"), edit("a\\x\\", ""));
        assert_eq!(apply("\n- "), apply(r"\n- "));
        // CRLF buffers get their line ending for the escape too.
        let crlf = LineContext {
            line_ending: "\r\n",
            ..ctx("a", "")
        };
        assert_eq!(
            compile(rule_with_insert(EditRuleTrigger::Newline, r"\n- ")).apply(&crlf),
            edit("a\r\n- ", "")
        );
    }

    /// A configured rule keeps a real line break in a template as `\n`, so
    /// it reads (and is written back) on one line; what it does is the same.
    #[test]
    fn configured_rules_write_line_breaks_as_the_escape() {
        let configured = |insert: &str| {
            ConfiguredEditRule::new(rule_with_insert(EditRuleTrigger::Newline, insert)).unwrap()
        };
        for (written, kept) in [
            ("\n- ", r"\n- "),
            (r"\n- ", r"\n- "),
            ("a\\\nb", r"a\\\nb"),
            ("a\\\\\nb", r"a\\\nb"),
            (r"\x", r"\x"),
        ] {
            let rule = configured(written);
            assert_eq!(rule.rule().insert.as_deref(), Some(kept), "{written:?}");
            let raw = compile(rule_with_insert(EditRuleTrigger::Newline, written));
            assert_eq!(
                rule.compiled().apply(&ctx("x", "")),
                raw.apply(&ctx("x", "")),
                "{written:?} means the same written either way"
            );
        }
    }

    #[test]
    fn configured_rules_are_tried_before_plugin_rules() {
        let compiled = |insert: &str| {
            CompiledEditRule::compile(&rule_with_insert(EditRuleTrigger::Newline, insert)).unwrap()
        };
        let mut reg = EditRuleRegistry::new();
        reg.register(
            "p",
            "s",
            CompiledEditRuleSet::compile(&EditRuleSet {
                languages: vec!["t".into()],
                rules: vec![rule_with_insert(EditRuleTrigger::Newline, "P")],
            })
            .unwrap(),
        );
        let configured = [compiled("C")];
        let rules = |configured| BufferEditRules {
            language: "t",
            configured,
            registry: Some(&reg),
        };
        let nl = |r: BufferEditRules<'_>| {
            r.apply(EditRuleTrigger::Newline, &ctx("", ""))
                .map(|e| e.before)
        };
        assert_eq!(nl(rules(&configured)).as_deref(), Some("C"));
        // No configured rules (`"edit_rules": []`): the plugin's apply.
        assert_eq!(nl(rules(&[])).as_deref(), Some("P"));
        assert!(rules(&configured).has_rules(EditRuleTrigger::Newline));
        assert!(!rules(&configured).has_rules(EditRuleTrigger::Indent));
        let alone = BufferEditRules {
            language: "other",
            configured: &configured,
            registry: None,
        };
        assert!(alone.has_rules(EditRuleTrigger::Newline));
        assert_eq!(nl(alone).as_deref(), Some("C"));
    }

    fn rule_with_insert(on: EditRuleTrigger, insert: &str) -> EditRule {
        let mut r = rule(on);
        r.insert = Some(insert.into());
        r
    }
}
