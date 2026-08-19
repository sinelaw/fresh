//! Identity for reconciliation.
//!
//! A [`Key`] distinguishes "the same logical child at a new position" from "a
//! different child at the same position". That distinction exists only in the
//! domain model, so keys are supplied by the caller and never inferred.

use std::fmt;
use std::rc::Rc;

/// A reconciliation key. Cheap to clone; compared by value.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Key {
    Int(u64),
    Str(Rc<str>),
    /// A namespace plus an ordinal, so two unrelated lists cannot collide.
    Pair(Rc<str>, u64),
}

impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Key::Int(i) => write!(f, "#{i}"),
            Key::Str(s) => write!(f, "#{s}"),
            Key::Pair(s, i) => write!(f, "#{s}:{i}"),
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

macro_rules! key_from_int {
    ($($t:ty),*) => { $(
        impl From<$t> for Key {
            fn from(v: $t) -> Self { Key::Int(v as u64) }
        }
    )* };
}
key_from_int!(u8, u16, u32, u64, usize);

impl From<&str> for Key {
    fn from(v: &str) -> Self {
        Key::Str(Rc::from(v))
    }
}

impl From<String> for Key {
    fn from(v: String) -> Self {
        Key::Str(Rc::from(v.as_str()))
    }
}

impl From<Rc<str>> for Key {
    fn from(v: Rc<str>) -> Self {
        Key::Str(v)
    }
}

macro_rules! key_from_pair {
    ($($t:ty),*) => { $(
        impl From<(&str, $t)> for Key {
            fn from(v: (&str, $t)) -> Self { Key::Pair(Rc::from(v.0), v.1 as u64) }
        }
    )* };
}
key_from_pair!(u8, u16, u32, u64, usize);

/// The chain of keys from the root to an element, for diagnostics. Positions
/// without a key print as `.`, because position is the implicit key.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct KeyPath(pub Vec<Option<Key>>);

impl KeyPath {
    pub fn push(&mut self, k: Option<Key>) {
        self.0.push(k);
    }

    pub fn child(&self, k: Option<Key>) -> KeyPath {
        let mut p = self.clone();
        p.push(k);
        p
    }
}

impl fmt::Display for KeyPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, seg) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, "/")?;
            }
            match seg {
                Some(k) => write!(f, "{k}")?,
                None => write!(f, ".")?,
            }
        }
        Ok(())
    }
}

impl fmt::Debug for KeyPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
