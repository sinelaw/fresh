//! LSP snippet parser and expander.
//!
//! Parses LSP snippet syntax and expands it to plain text with cursor positioning.
//! Supports:
//! - `$0` - final cursor position
//! - `$n` - tabstops (expanded as empty, cursor goes to $0 or end)
//! - `${n:text}` - tabstops with default text (uses the default)
//! - `${n|choice1,choice2|}` - choices (uses first choice)
//! - `\\$` - escaped dollar sign

/// Result of expanding a snippet
#[derive(Debug, Clone, PartialEq)]
pub struct ExpandedSnippet {
    /// The expanded plain text
    pub text: String,
    /// Cursor offset from start of inserted text (where $0 was, or end if no $0)
    pub cursor_offset: usize,
}

/// Expand an LSP snippet to plain text
///
/// # Examples
/// ```
/// use fresh::primitives::snippet::expand_snippet;
///
/// // Simple function call
/// let result = expand_snippet("foo($0)");
/// assert_eq!(result.text, "foo()");
/// assert_eq!(result.cursor_offset, 4); // cursor inside parens
///
/// // With placeholder
/// let result = expand_snippet("foo(${1:arg})");
/// assert_eq!(result.text, "foo(arg)");
/// assert_eq!(result.cursor_offset, 8); // cursor at end (no $0)
/// ```
pub fn expand_snippet(snippet: &str) -> ExpandedSnippet {
    let mut result = String::new();
    let mut cursor_offset: Option<usize> = None;
    let mut chars = snippet.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Escape sequence
            if let Some(&next) = chars.peek() {
                if next == '$' || next == '\\' || next == '}' {
                    result.push(chars.next().unwrap());
                    continue;
                }
            }
            result.push(c);
        } else if c == '$' {
            // Snippet placeholder
            if let Some(&next) = chars.peek() {
                if next == '{' {
                    // ${...} syntax
                    chars.next(); // consume '{'
                    let (expanded, is_final) = parse_brace_placeholder(&mut chars);
                    if is_final {
                        cursor_offset = Some(result.len());
                    }
                    result.push_str(&expanded);
                } else if next.is_ascii_digit() {
                    // $n syntax
                    let mut num = String::new();
                    while let Some(&d) = chars.peek() {
                        if d.is_ascii_digit() {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    if num == "0" {
                        cursor_offset = Some(result.len());
                    }
                    // Other tabstops expand to nothing
                } else {
                    // Not a valid placeholder, keep the $
                    result.push(c);
                }
            } else {
                result.push(c);
            }
        } else {
            result.push(c);
        }
    }

    ExpandedSnippet {
        cursor_offset: cursor_offset.unwrap_or(result.len()),
        text: result,
    }
}

/// Parse a ${...} placeholder
/// Returns (expanded_text, is_final_cursor)
fn parse_brace_placeholder(chars: &mut std::iter::Peekable<std::str::Chars>) -> (String, bool) {
    let mut content = String::new();
    let mut depth = 1;

    // Collect everything until matching '}'
    while let Some(c) = chars.next() {
        if c == '{' {
            depth += 1;
            content.push(c);
        } else if c == '}' {
            depth -= 1;
            if depth == 0 {
                break;
            }
            content.push(c);
        } else if c == '\\' {
            // Handle escapes inside braces
            if let Some(&next) = chars.peek() {
                if next == '$' || next == '\\' || next == '}' || next == '|' {
                    content.push(chars.next().unwrap());
                    continue;
                }
            }
            content.push(c);
        } else {
            content.push(c);
        }
    }

    // Parse the content: n or n:default or n|choices|
    let (tabstop, default) = parse_placeholder_content(&content);

    let is_final = tabstop == Some(0);
    (default, is_final)
}

/// Parse placeholder content like "1", "1:default", or "1|a,b,c|"
/// Returns (tabstop_number, default_text)
fn parse_placeholder_content(content: &str) -> (Option<u32>, String) {
    // Find the tabstop number
    let mut chars = content.chars().peekable();
    let mut num_str = String::new();

    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() {
            num_str.push(chars.next().unwrap());
        } else {
            break;
        }
    }

    let tabstop = num_str.parse::<u32>().ok();

    // Check what follows
    match chars.peek() {
        Some(':') => {
            // Default text: ${n:default}
            chars.next(); // consume ':'
            let default: String = chars.collect();
            // Recursively expand nested snippets in default
            let expanded = expand_snippet(&default);
            (tabstop, expanded.text)
        }
        Some('|') => {
            // Choices: ${n|choice1,choice2|}
            chars.next(); // consume '|'
            let choices: String = chars.collect();
            // Remove trailing '|' if present
            let choices = choices.trim_end_matches('|');
            // Use first choice
            let first_choice = choices.split(',').next().unwrap_or("");
            (tabstop, first_choice.to_string())
        }
        _ => {
            // Just a tabstop number, no default
            (tabstop, String::new())
        }
    }
}

/// Check if a string contains snippet syntax
pub fn is_snippet(text: &str) -> bool {
    let mut chars = text.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Skip escaped character
            chars.next();
        } else if c == '$' {
            // Check if it's a valid placeholder
            if let Some(&next) = chars.peek() {
                if next == '{' || next.is_ascii_digit() {
                    return true;
                }
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plain_text() {
        let result = expand_snippet("hello world");
        assert_eq!(result.text, "hello world");
        assert_eq!(result.cursor_offset, 11); // end of text
    }

    #[test]
    fn test_final_cursor() {
        let result = expand_snippet("foo($0)");
        assert_eq!(result.text, "foo()");
        assert_eq!(result.cursor_offset, 4); // inside parens
    }

    #[test]
    fn test_tabstop_no_default() {
        let result = expand_snippet("$1");
        assert_eq!(result.text, "");
        assert_eq!(result.cursor_offset, 0);
    }

    #[test]
    fn test_tabstop_with_default() {
        let result = expand_snippet("foo(${1:arg})");
        assert_eq!(result.text, "foo(arg)");
        assert_eq!(result.cursor_offset, 8); // end (no $0)
    }

    #[test]
    fn test_multiple_tabstops() {
        let result = expand_snippet("fn ${1:name}(${2:args}) { $0 }");
        assert_eq!(result.text, "fn name(args) {  }");
        assert_eq!(result.cursor_offset, 16); // where $0 was
    }

    #[test]
    fn test_choices() {
        let result = expand_snippet("${1|public,private,protected|}");
        assert_eq!(result.text, "public");
        assert_eq!(result.cursor_offset, 6);
    }

    #[test]
    fn test_escaped_dollar() {
        let result = expand_snippet("cost: \\$100");
        assert_eq!(result.text, "cost: $100");
        assert_eq!(result.cursor_offset, 10);
    }

    #[test]
    fn test_nested_placeholder() {
        let result = expand_snippet("${1:foo${2:bar}}");
        assert_eq!(result.text, "foobar");
        assert_eq!(result.cursor_offset, 6);
    }

    #[test]
    fn test_function_with_params() {
        // Typical rust-analyzer completion
        let result = expand_snippet("println!($0)");
        assert_eq!(result.text, "println!()");
        assert_eq!(result.cursor_offset, 9);
    }

    #[test]
    fn test_is_snippet() {
        assert!(is_snippet("foo($0)"));
        assert!(is_snippet("${1:arg}"));
        assert!(is_snippet("$1"));
        assert!(!is_snippet("foo()"));
        assert!(!is_snippet("\\$100")); // escaped
    }

    #[test]
    fn test_empty_default() {
        let result = expand_snippet("foo(${1:})");
        assert_eq!(result.text, "foo()");
        assert_eq!(result.cursor_offset, 5);
    }
}
