#[cfg(test)]
mod debug_i18n {
    use std::collections::HashSet;
    use std::fs;
    use std::path::Path;

    #[test]
    fn print_missing_keys() {
        let locales_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("locales");
        let en_content =
            fs::read_to_string(locales_dir.join("en.json")).expect("Failed to read en.json");
        let en_json: serde_json::Value =
            serde_json::from_str(&en_content).expect("Failed to parse en.json");
        let en_keys: HashSet<String> = en_json
            .as_object()
            .unwrap()
            .keys()
            .filter(|k| !k.starts_with('_'))
            .cloned()
            .collect();

        let mut entries = fs::read_dir(&locales_dir).unwrap();
        while let Some(Ok(entry)) = entries.next() {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                let locale = path.file_stem().and_then(|s| s.to_str()).unwrap();
                if locale == "en" {
                    continue;
                }

                let content = fs::read_to_string(&path).unwrap();
                let json: serde_json::Value = serde_json::from_str(&content).unwrap();
                let loc_keys: HashSet<String> = json
                    .as_object()
                    .unwrap()
                    .keys()
                    .filter(|k| !k.starts_with('_'))
                    .cloned()
                    .collect();

                let missing: Vec<_> = en_keys.difference(&loc_keys).collect();
                if !missing.is_empty() {
                    println!("Locale '{}' is missing: {:?}", locale, missing);
                }
            }
        }
    }
}
