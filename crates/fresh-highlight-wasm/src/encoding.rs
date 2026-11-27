//! Reading the compact encodings the `hl_*` calls take a list in: a byte
//! string of little-endian `u32` words and UTF-8 texts, each text behind its
//! byte length (`[len u32][utf8]`), and each list behind its item count
//! (`[n u32]` then the items). Nothing is padded or aligned.

/// A reader over an encoded argument, named `what` in its errors.
pub struct Reader<'a> {
    bytes: &'a [u8],
    what: &'static str,
}

impl<'a> Reader<'a> {
    pub fn new(bytes: &'a [u8], what: &'static str) -> Self {
        Reader { bytes, what }
    }

    pub fn word(&mut self) -> Result<u32, String> {
        let (word, rest) = self
            .bytes
            .split_first_chunk::<4>()
            .ok_or_else(|| format!("{} ends inside a word", self.what))?;
        self.bytes = rest;
        Ok(u32::from_le_bytes(*word))
    }

    /// A word that counts or places bytes.
    pub fn size(&mut self) -> Result<usize, String> {
        self.word().map(|word| word as usize)
    }

    /// `[len u32][utf8]`.
    pub fn text(&mut self) -> Result<&'a str, String> {
        let len = self.size()?;
        if len > self.bytes.len() {
            return Err(format!("{} ends inside a text of {len} bytes", self.what));
        }
        let (text, rest) = self.bytes.split_at(len);
        self.bytes = rest;
        std::str::from_utf8(text).map_err(|e| format!("{}: a text is not UTF-8: {e}", self.what))
    }

    /// `[n u32]` then `n` items, each read by `item`.
    pub fn list<T>(
        &mut self,
        mut item: impl FnMut(&mut Self) -> Result<T, String>,
    ) -> Result<Vec<T>, String> {
        let n = self.word()?;
        (0..n).map(|_| item(self)).collect()
    }

    /// Done reading: nothing may be left over.
    pub fn finish(self) -> Result<(), String> {
        if self.bytes.is_empty() {
            Ok(())
        } else {
            Err(format!(
                "{} has {} bytes past its end",
                self.what,
                self.bytes.len()
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn encode(words_and_texts: &[Result<u32, &str>]) -> Vec<u8> {
        words_and_texts
            .iter()
            .flat_map(|item| match item {
                Ok(word) => word.to_le_bytes().to_vec(),
                Err(text) => (text.len() as u32)
                    .to_le_bytes()
                    .into_iter()
                    .chain(text.bytes())
                    .collect(),
            })
            .collect()
    }

    #[test]
    fn a_list_of_words_and_texts_reads_back() {
        let bytes = encode(&[Ok(2), Ok(7), Err("é a"), Ok(9), Err("")]);
        let mut reader = Reader::new(&bytes, "the list");
        let items = reader
            .list(|r| Ok((r.word()?, r.text()?.to_string())))
            .unwrap();
        reader.finish().unwrap();
        assert_eq!(items, [(7, "é a".to_string()), (9, String::new())]);
    }

    #[test]
    fn a_truncated_or_overlong_encoding_is_refused() {
        let bytes = encode(&[Ok(1), Err("abc")]);
        let mut short = Reader::new(&bytes[..bytes.len() - 1], "the list");
        assert!(short.list(|r| r.text().map(str::len)).is_err());
        let mut long = Reader::new(&bytes, "the list");
        long.word().unwrap();
        assert!(long.finish().is_err());
    }
}
