//! The backslash escapes a replacement template takes, shared by search &
//! replace and edit-rule templates so both read a template the same way.

/// Interpret the common backslash escapes in a replacement template:
/// `\n`, `\t`, `\r` become their control-character equivalents and `\\`
/// becomes a single backslash. Unknown escapes (e.g. `\q`) are preserved
/// verbatim so users who didn't mean to type an escape sequence don't get
/// surprising behaviour. A trailing lone backslash is likewise passed
/// through unchanged.
pub fn interpret_escapes(template: &str) -> String {
    let mut out = String::with_capacity(template.len());
    let mut chars = template.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('t') => out.push('\t'),
            Some('r') => out.push('\r'),
            Some('\\') => out.push('\\'),
            Some(other) => {
                out.push('\\');
                out.push(other);
            }
            None => out.push('\\'),
        }
    }
    out
}
