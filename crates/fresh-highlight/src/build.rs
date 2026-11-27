//! Building a syntax set with extra grammars: the default set (an
//! uncompressed packdump, as `Highlighter`'s consumers load it) and
//! `.sublime-syntax` grammars registered at runtime, built into one set and
//! dumped in the same format.

use std::fmt;
use syntect::parsing::{ParseSyntaxError, SyntaxDefinition, SyntaxSet};

/// A `.sublime-syntax` grammar to add.
#[derive(Debug, Clone, Copy)]
pub struct Grammar<'a> {
    /// Names the grammar in errors, and is its name when its YAML has none
    /// (natively, a grammar file's stem).
    pub name: &'a str,
    /// The grammar's YAML.
    pub source: &'a str,
}

#[derive(Debug)]
pub enum BuildError {
    /// The packdump does not load.
    Packdump(bincode::Error),
    /// Grammars that failed to parse, each with its name, in the order given.
    Grammars(Vec<(String, ParseSyntaxError)>),
    /// The built set does not serialize.
    Dump(bincode::Error),
}

impl fmt::Display for BuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuildError::Packdump(error) => write!(f, "cannot load the syntax set: {error}"),
            BuildError::Grammars(failed) => {
                write!(f, "grammars failed to parse:")?;
                failed
                    .iter()
                    .try_for_each(|(name, error)| write!(f, " {name:?}: {error};"))
            }
            BuildError::Dump(error) => write!(f, "cannot dump the syntax set: {error}"),
        }
    }
}

impl std::error::Error for BuildError {}

/// The syntax set of `packdump` (uncompressed, as
/// `syntect::dumps::from_uncompressed_data` loads it) with `grammars` added
/// after its own syntaxes, so that one of a name already in the set shadows
/// it, dumped in the same format. Every grammar must parse: those that do
/// not are the error, by name.
pub fn build_syntax_set(packdump: &[u8], grammars: &[Grammar]) -> Result<Vec<u8>, BuildError> {
    let defaults: SyntaxSet =
        syntect::dumps::from_uncompressed_data(packdump).map_err(BuildError::Packdump)?;
    let (parsed, failed) = grammars.iter().fold(
        (Vec::new(), Vec::new()),
        |(mut parsed, mut failed), grammar| {
            match SyntaxDefinition::load_from_str(grammar.source, true, Some(grammar.name)) {
                Ok(syntax) => parsed.push(syntax),
                Err(error) => failed.push((grammar.name.to_string(), error)),
            }
            (parsed, failed)
        },
    );
    if !failed.is_empty() {
        return Err(BuildError::Grammars(failed));
    }
    let mut builder = defaults.into_builder();
    parsed.into_iter().for_each(|syntax| builder.add(syntax));
    dump_syntax_set(&builder.build()).map_err(BuildError::Dump)
}

/// `syntaxes` as an uncompressed packdump: in memory, what
/// `syntect::dumps::dump_to_uncompressed_file` writes to a file.
pub fn dump_syntax_set(syntaxes: &SyntaxSet) -> Result<Vec<u8>, bincode::Error> {
    bincode::serialize(syntaxes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Category, Highlighter};

    const TINY: &str = "\
%YAML 1.2
---
name: Tiny
file_extensions: [tiny]
scope: source.tiny
contexts:
  main:
    - match: '\\bkw\\b'
      scope: keyword.control.tiny
    - match: '#.*'
      scope: comment.line.tiny
";

    fn defaults() -> Vec<u8> {
        dump_syntax_set(&SyntaxSet::load_defaults_newlines()).unwrap()
    }

    #[test]
    fn a_built_set_holds_the_defaults_and_the_grammars_added() {
        let built = build_syntax_set(
            &defaults(),
            &[Grammar {
                name: "tiny",
                source: TINY,
            }],
        )
        .unwrap();
        let set: SyntaxSet = syntect::dumps::from_uncompressed_data(&built).unwrap();
        assert!(set.find_syntax_by_name("Tiny").is_some());
        assert!(set.find_syntax_by_name("Rust").is_some());
    }

    /// A document opened in an added grammar is coloured by it.
    #[test]
    fn a_document_in_an_added_grammar_has_its_categories() {
        let built = build_syntax_set(
            &defaults(),
            &[Grammar {
                name: "tiny",
                source: TINY,
            }],
        )
        .unwrap();
        let mut h = Highlighter::new(syntect::dumps::from_uncompressed_data(&built).unwrap());
        h.open(1, "Tiny", "x kw y # note").unwrap();
        while h.work(1 << 16).unwrap() {}
        let runs: Vec<(u32, u32, Category)> = h.take(1).unwrap()[0]
            .runs
            .iter()
            .map(|r| (r.start, r.len, r.cat))
            .collect();
        assert_eq!(runs, [(2, 2, Category::Keyword), (7, 6, Category::Comment)]);
    }

    #[test]
    fn every_grammar_that_fails_to_parse_is_named() {
        let failed = build_syntax_set(
            &defaults(),
            &[
                Grammar {
                    name: "broken",
                    source: "contexts: [not, a, map",
                },
                Grammar {
                    name: "tiny",
                    source: TINY,
                },
                Grammar {
                    name: "no-contexts",
                    source: "%YAML 1.2\n---\nscope: source.x\n",
                },
            ],
        );
        match failed {
            Err(BuildError::Grammars(failed)) => assert_eq!(
                failed
                    .iter()
                    .map(|(name, _)| name.as_str())
                    .collect::<Vec<_>>(),
                ["broken", "no-contexts"]
            ),
            other => panic!("{other:?}"),
        }
    }

    #[test]
    fn a_packdump_that_does_not_load_is_refused() {
        assert!(matches!(
            build_syntax_set(b"not a packdump", &[]),
            Err(BuildError::Packdump(_))
        ));
    }
}
