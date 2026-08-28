#![allow(dead_code)]

//! A seeded generator, so the randomized tests explore a wide input space
//! while still failing the same way twice.

pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Self {
        Self(seed | 1)
    }

    fn next_u64(&mut self) -> u64 {
        // xorshift64*
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }

    /// A number in `0..n`.
    pub fn below(&mut self, n: usize) -> usize {
        assert!(n > 0);
        (self.next_u64() % n as u64) as usize
    }

    /// A number in `lo..=hi`.
    pub fn between(&mut self, lo: usize, hi: usize) -> usize {
        lo + self.below(hi - lo + 1)
    }

    pub fn pick<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        &items[self.below(items.len())]
    }

    /// A lowercase word, usable as a translation-key segment.
    pub fn word(&mut self, lo: usize, hi: usize) -> String {
        let len = self.between(lo, hi);
        (0..len)
            .map(|_| (b'a' + self.below(26) as u8) as char)
            .collect()
    }

    /// Free text with no `%` in it, so it cannot be mistaken for a placeholder.
    pub fn text(&mut self, lo: usize, hi: usize) -> String {
        const ALPHABET: &[char] = &['a', 'z', ' ', '\'', '.', '{', '}', 'é', '。'];
        let len = self.between(lo, hi);
        (0..len).map(|_| *self.pick(ALPHABET)).collect()
    }

    pub fn shuffle<T>(&mut self, items: &mut [T]) {
        for i in (1..items.len()).rev() {
            let j = self.below(i + 1);
            items.swap(i, j);
        }
    }
}

/// Distinct lowercase words, so generated keys and locale codes never collide.
pub fn distinct_words(rng: &mut Rng, count: usize, lo: usize, hi: usize) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    while seen.len() < count {
        seen.insert(rng.word(lo, hi));
    }
    let mut words: Vec<_> = seen.into_iter().collect();
    words.sort();
    words
}

/// The active locale is process-global, so tests that steer it take turns.
/// Under `cargo nextest` each test gets its own process and this is free.
pub static LOCALE: std::sync::Mutex<()> = std::sync::Mutex::new(());

pub fn locale_guard() -> std::sync::MutexGuard<'static, ()> {
    LOCALE.lock().unwrap_or_else(|e| e.into_inner())
}
