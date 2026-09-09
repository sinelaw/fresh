//! Encoding detection heuristics
//!
//! This module contains heuristic functions for distinguishing between similar
//! single-byte encodings that cannot be reliably distinguished by statistical
//! detection alone (e.g., chardetng).

/// Check if sample has Windows-1250 (Central European) specific byte patterns
///
/// Windows-1250 and Windows-1252 share most byte values, but differ in the 0x80-0x9F range
/// and some bytes in 0xA0-0xFF. This function looks for bytes that are:
/// 1. Undefined in Windows-1252 but valid in Windows-1250 (definitive indicators)
/// 2. Have different meanings in the 0x80-0x9F range that suggest Central European text
///
/// # Definitive Windows-1250 indicators (undefined in Windows-1252)
///
/// - `0x8D` = Ť (T with caron) - Czech/Slovak
/// - `0x8F` = Ź (Z with acute) - Polish
/// - `0x9D` = ť (t with caron) - Czech/Slovak
///
/// # Strong Windows-1250 indicators (0x80-0x9F range, clearly different meanings)
///
/// - `0x8C` = Ś (S with acute) vs Œ in Windows-1252 - Polish
/// - `0x9C` = ś (s with acute) vs œ in Windows-1252 - Polish
/// - `0x9F` = ź (z with acute) vs Ÿ in Windows-1252 - Polish
///
/// # Note: Bytes in 0xA0-0xFF range are NOT used as indicators
///
/// These bytes have ambiguous meanings that could be either encoding:
/// - `0xA3` = Ł vs £ (pound sign is common in Western European text)
/// - `0xA5` = Ą vs ¥ (yen sign is common in financial text)
/// - `0xB3` = ł vs ³ (superscript 3 is common)
/// - `0xB9` = ą vs ¹ (superscript 1 is common)
/// - `0xBF` = ż vs ¿ (inverted question mark is common in Spanish)
///
/// # Returns
///
/// `true` if the sample contains patterns that indicate Windows-1250 encoding,
/// `false` otherwise (suggesting Windows-1252 or another encoding).
pub fn has_windows1250_pattern(sample: &[u8]) -> bool {
    // Definitive indicators: bytes undefined in Windows-1252 but valid in Windows-1250
    // If any of these are present, it's definitely Windows-1250
    const DEFINITIVE_1250_BYTES: [u8; 3] = [0x8D, 0x8F, 0x9D];

    // Strong indicators: bytes in 0x80-0x9F range that have clearly different meanings
    // These are less ambiguous than the 0xA0-0xFF range bytes
    const STRONG_1250_BYTES: [u8; 3] = [
        0x8C, 0x9C, 0x9F, // Ś, ś, ź (Polish) vs Œ, œ, Ÿ in 1252
    ];

    let mut definitive_count = 0;
    let mut strong_count = 0;

    for &byte in sample {
        if DEFINITIVE_1250_BYTES.contains(&byte) {
            definitive_count += 1;
        }
        if STRONG_1250_BYTES.contains(&byte) {
            strong_count += 1;
        }
    }

    // If we have any definitive indicators, it's Windows-1250
    if definitive_count > 0 {
        return true;
    }

    // If we have multiple strong indicators from 0x80-0x9F range, likely Windows-1250
    if strong_count >= 2 {
        return true;
    }

    false
}

/// Check if sample has Windows-1251 (Cyrillic) specific byte patterns
///
/// Windows-1251 maps bytes 0xC0-0xDF to Cyrillic uppercase letters (А-Я) and
/// bytes 0xE0-0xFF to Cyrillic lowercase letters (а-я). In Windows-1252 the
/// same ranges are accented Latin uppercase (À-ß) and lowercase (à-ÿ).
///
/// Distinguishing Windows-1251 from Windows-1252 is hard when chardetng is
/// not confident, because both encodings use the same byte ranges for very
/// different character sets. This heuristic intentionally only fires when
/// there is strong evidence of Cyrillic text:
///
/// 1. The sample contains **both** a Cyrillic uppercase letter (0xC0-0xDE)
///    **and** a Cyrillic lowercase letter (0xE0-0xFF). In Latin-1 text, runs
///    of accented uppercase letters are extremely rare, so mixing uppercase
///    and lowercase high bytes is a strong Cyrillic signal. Note: 0xDF is
///    excluded from the "uppercase" test because it decodes to ß (sharp s,
///    lowercase) in Latin-1 / Windows-1252.
/// 2. The sample contains Ё (0xA8) or ё (0xB8) with at least one Cyrillic
///    lowercase letter. These bytes decode to ¨ / ¸ in Windows-1252 which
///    almost never appear next to other high bytes in real text.
///
/// # Returns
///
/// `true` if the sample contains patterns that strongly indicate Windows-1251.
pub fn has_windows1251_pattern(sample: &[u8]) -> bool {
    let mut has_cyrillic_upper = false; // 0xC0-0xDE (А-Ю in Windows-1251)
    let mut has_cyrillic_lower = false; // 0xE0-0xFF (а-я in Windows-1251)
    let mut has_yo = false; // 0xA8 (Ё) or 0xB8 (ё)

    for &byte in sample {
        // 0xDF excluded: it is ß in Latin-1/Windows-1252, a common lowercase
        // letter in German, so we don't treat it as a Cyrillic-upper signal.
        if (0xC0..=0xDE).contains(&byte) {
            has_cyrillic_upper = true;
        }
        if (0xE0..=0xFF).contains(&byte) {
            has_cyrillic_lower = true;
        }
        if byte == 0xA8 || byte == 0xB8 {
            has_yo = true;
        }
    }

    // Strong signal: mix of uppercase and lowercase Cyrillic. Real Russian
    // text almost always contains both cases (sentence starts, proper nouns),
    // while Latin-1 text of the form "Café résumé" only uses lowercase
    // accented letters.
    if has_cyrillic_upper && has_cyrillic_lower {
        return true;
    }

    // Ё / ё plus a lowercase Cyrillic letter is also a strong Cyrillic signal
    // since these Windows-1252 characters (¨ and ¸) almost never cluster with
    // other high bytes in real text.
    if has_yo && has_cyrillic_lower {
        return true;
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_definitive_bytes_t_caron() {
        // Byte 0x9D (ť) is undefined in Windows-1252, definitive Windows-1250
        let with_t_caron = [0x6D, 0x9D, 0x73, 0x74, 0x6F]; // mťsto
        assert!(
            has_windows1250_pattern(&with_t_caron),
            "Byte 0x9D (ť) should trigger Windows-1250 detection"
        );
    }

    #[test]
    fn test_definitive_bytes_z_acute_upper() {
        // Byte 0x8F (Ź) is undefined in Windows-1252, definitive Windows-1250
        let with_z_acute_upper = [0x8F, 0x72, 0xF3, 0x64, 0xB3, 0x6F]; // Źródło
        assert!(
            has_windows1250_pattern(&with_z_acute_upper),
            "Byte 0x8F (Ź) should trigger Windows-1250 detection"
        );
    }

    #[test]
    fn test_definitive_bytes_t_caron_upper() {
        // Byte 0x8D (Ť) is undefined in Windows-1252, definitive Windows-1250
        let with_t_caron_upper = [0x8D, 0x65, 0x73, 0x74]; // Ťest
        assert!(
            has_windows1250_pattern(&with_t_caron_upper),
            "Byte 0x8D (Ť) should trigger Windows-1250 detection"
        );
    }

    #[test]
    fn test_strong_indicators_0x80_range() {
        // Polish text with ś (0x9C) and Ś (0x8C) - strong indicators from 0x80-0x9F range
        let polish_text = [
            0x9C, 0x77, 0x69, 0x65, 0x74, 0x79, 0x20, // "świety " (holy)
            0x8C, 0x77, 0x69, 0x61, 0x74, // "Świat" (world)
        ];
        assert!(
            has_windows1250_pattern(&polish_text),
            "Multiple Polish characters (ś, Ś) should trigger Windows-1250"
        );
    }

    #[test]
    fn test_ambiguous_bytes_not_strong_indicators() {
        // Bytes in 0xA0-0xFF range are ambiguous and should NOT trigger Windows-1250
        // Polish "żółć" (bile/yellow color) - ż(0xBF) ó(0xF3) ł(0xB3) ć(0xE6)
        // None of these are in the strong indicator list anymore
        let zolc = [0xBF, 0xF3, 0xB3, 0xE6];
        assert!(
            !has_windows1250_pattern(&zolc),
            "Ambiguous bytes (0xBF, 0xB3) should NOT trigger Windows-1250"
        );

        // ą (0xB9) and ł (0xB3) are also ambiguous
        let polish_text = [
            0x6D, 0xB9, 0x6B, 0x61, 0x20, // "mąka " (flour) - but could be m¹ka
            0x6D, 0xB3, 0x6F, 0x64, 0x79, // "młody" (young) - but could be m³ody
        ];
        assert!(
            !has_windows1250_pattern(&polish_text),
            "Ambiguous bytes (0xB9, 0xB3) should NOT trigger Windows-1250"
        );
    }

    #[test]
    fn test_pound_and_yen_not_indicators() {
        // £ (0xA3) and ¥ (0xA5) are common in Western European and financial text
        // They should NOT trigger Windows-1250 detection
        let currency = [0x31, 0x30, 0xA3, 0x20, 0x31, 0x30, 0xA5]; // "10£ 10¥"
        assert!(
            !has_windows1250_pattern(&currency),
            "Currency symbols (£, ¥) should not trigger Windows-1250"
        );
    }

    #[test]
    fn test_pure_ascii() {
        let ascii = b"Hello, World!";
        assert!(
            !has_windows1250_pattern(ascii),
            "Pure ASCII should not trigger Windows-1250"
        );
    }

    #[test]
    fn test_windows1252_french() {
        // French "Café résumé" - uses é (0xE9) which is the same in both encodings
        let french = [
            0x43, 0x61, 0x66, 0xE9, 0x20, // "Café "
            0x72, 0xE9, 0x73, 0x75, 0x6D, 0xE9, // "résumé"
        ];
        assert!(
            !has_windows1250_pattern(&french),
            "French text should not trigger Windows-1250"
        );
    }

    #[test]
    fn test_windows1251_upper_and_lower() {
        // Russian word "Привет" (Hello) in Windows-1251 mixes uppercase
        // П (0xCF, in 0xC0-0xDE) with lowercase ривет (0xF0 0xE8 0xE2 0xE5 0xF2,
        // in 0xE0-0xFF). This upper+lower combination is the strong signal.
        let privet: &[u8] = &[0xCF, 0xF0, 0xE8, 0xE2, 0xE5, 0xF2];
        assert!(
            has_windows1251_pattern(privet),
            "Cyrillic upper + lower should trigger Windows-1251"
        );
    }

    #[test]
    fn test_windows1251_with_yo() {
        // ё (0xB8) appearing with lowercase Cyrillic letters is a strong indicator.
        // "ёжик" (little hedgehog): ё=0xB8 ж=0xE6 и=0xE8 к=0xEA
        // 0xB8 itself is outside the letter ranges, but the ж/и/к are lowercase
        // Cyrillic. has_yo && has_cyrillic_lower triggers the heuristic.
        let yozhik: &[u8] = &[0xB8, 0xE6, 0xE8, 0xEA];
        assert!(
            has_windows1251_pattern(yozhik),
            "ё + lowercase Cyrillic letters should trigger Windows-1251"
        );
    }

    #[test]
    fn test_windows1251_sentence_with_spaces() {
        // "Привет мир" (Hello world) in Windows-1251
        let hello_world: &[u8] = &[
            0xCF, 0xF0, 0xE8, 0xE2, 0xE5, 0xF2, // Привет
            0x20, // space
            0xEC, 0xE8, 0xF0, // мир
        ];
        assert!(
            has_windows1251_pattern(hello_world),
            "Russian sentence should trigger Windows-1251"
        );
    }

    #[test]
    fn test_not_windows1251_french() {
        // "Café résumé" — only lowercase high bytes (0xE9), no Ё/ё.
        let french = [
            0x43, 0x61, 0x66, 0xE9, 0x20, // "Café "
            0x72, 0xE9, 0x73, 0x75, 0x6D, 0xE9, // "résumé"
        ];
        assert!(
            !has_windows1251_pattern(&french),
            "French text should not trigger Windows-1251"
        );
    }

    #[test]
    fn test_not_windows1251_ascii() {
        let ascii = b"Hello, World!";
        assert!(
            !has_windows1251_pattern(ascii),
            "Pure ASCII should not trigger Windows-1251"
        );
    }

    #[test]
    fn test_not_windows1251_lowercase_only_latin1() {
        // Latin-1 text with runs of lowercase accented letters must NOT trigger
        // Windows-1251 — this is what prop_latin1_text_preserved generates.
        // "ééééé" (5 é's) has no uppercase signal, so the heuristic rejects it.
        let only_lower = [0xE9, 0xE9, 0xE9, 0xE9, 0xE9];
        assert!(
            !has_windows1251_pattern(&only_lower),
            "Runs of lowercase Latin-1 letters should NOT trigger Windows-1251"
        );
    }

    #[test]
    fn test_not_windows1251_ambiguous_polish() {
        // Polish "żółć" (4 lowercase high bytes) must NOT trigger Windows-1251.
        let zolc = [0xBF, 0xF3, 0xB3, 0xE6];
        assert!(
            !has_windows1251_pattern(&zolc),
            "Lowercase-only Polish bytes should NOT trigger Windows-1251"
        );
    }

    #[test]
    fn test_not_windows1251_sharp_s_excluded() {
        // ß (0xDF) must not count as "Cyrillic upper" because it is a common
        // German lowercase letter in Latin-1 / Windows-1252.
        let german = [0x53, 0x74, 0x72, 0x61, 0xDF, 0x65]; // "Straße"
        assert!(
            !has_windows1251_pattern(&german),
            "German ß should not trigger Windows-1251"
        );
    }

    #[test]
    fn test_czech_pangram() {
        // "Příliš žluťoučký kůň úpěl ďábelské ódy" contains ť (0x9D)
        let czech_pangram: &[u8] = &[
            0x50, 0xF8, 0xED, 0x6C, 0x69, 0x9A, 0x20, // "Příliš "
            0x9E, 0x6C, 0x75, 0x9D, 0x6F, 0x75, 0xE8, 0x6B, 0xFD, 0x20, // "žluťoučký "
            0x6B, 0xF9, 0xF2, 0x20, // "kůň "
            0xFA, 0x70, 0xEC, 0x6C, 0x20, // "úpěl "
            0xEF, 0xE1, 0x62, 0x65, 0x6C, 0x73, 0x6B, 0xE9, 0x20, // "ďábelské "
            0xF3, 0x64, 0x79, // "ódy"
        ];
        assert!(
            has_windows1250_pattern(czech_pangram),
            "Czech pangram should trigger Windows-1250 (contains ť = 0x9D)"
        );
    }
}
