use std::str::Chars;

pub struct LoadedLine {
    chars: String,
}

impl LoadedLine {
    pub fn empty() -> LoadedLine {
        LoadedLine {
            chars: String::new(),
        }
    }
    pub fn new(chars: String) -> LoadedLine {
        LoadedLine { chars }
    }
    pub fn len(&self) -> usize {
        self.chars.len()
    }
    pub fn push(&mut self, c: char) {
        self.chars.push(c);
    }
    pub fn insert(&mut self, index: usize, c: char) {
        self.chars.insert(index, c);
    }
    pub fn remove(&mut self, index: usize) {
        self.chars.remove(index);
    }
    pub fn extend(&mut self, line: LoadedLine) {
        self.chars.extend(line.chars.chars());
    }
    /*  pub fn char_get_mut(&mut self, index: usize) -> Option<&mut char> {
        self.chars.as_bytes_mut().get_mut(index).map(|u| u as char)
    }
    pub fn char_get(&self, index: usize) -> Option<&str> {
        self.chars.get(index..index + 1).map(|s| s.chars())
    } */
    pub fn split_off(&mut self, x: usize) -> String {
        self.chars.split_off(x)
    }
    pub fn chars_iter(&self) -> Chars<'_> {
        self.chars.chars()
    }
}
