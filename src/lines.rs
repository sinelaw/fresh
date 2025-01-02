pub struct LoadedLine {
    chars: Vec<char>,
}

impl LoadedLine {
    pub fn empty() -> LoadedLine {
        LoadedLine { chars: vec![] }
    }
    pub fn new(chars: &[char]) -> LoadedLine {
        LoadedLine {
            chars: Vec::from(chars),
        }
    }
    pub fn from_vec(chars: Vec<char>) -> LoadedLine {
        LoadedLine { chars: chars }
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
        self.chars.extend(line.chars);
    }
    pub fn char_get_mut(&mut self, index: usize) -> Option<&mut char> {
        self.chars.get_mut(index)
    }
    pub fn char_get(&self, index: usize) -> Option<&char> {
        self.chars.get(index)
    }
    pub fn split_off(&mut self, x: usize) -> Vec<char> {
        self.chars.split_off(x)
    }
    pub fn chars_iter(&self) -> std::slice::Iter<'_, char> {
        self.chars.iter()
    }
}
