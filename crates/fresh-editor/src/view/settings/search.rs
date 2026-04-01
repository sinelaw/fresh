//! Search functionality for settings
//!
//! Provides fuzzy search over setting names and descriptions,
//! with support for highlighting matching categories.

use super::items::{SettingControl, SettingItem, SettingsPage};

/// Describes a match found inside a composite control (Map, TextList)
#[derive(Debug, Clone)]
pub enum DeepMatch {
    /// Matched a map entry key (e.g., "python" in the languages map)
    MapKey {
        /// The key that matched
        key: String,
        /// Index of the entry in the map
        entry_index: usize,
    },
    /// Matched a value nested inside a map entry (e.g., a field value)
    MapValue {
        /// The parent map key
        key: String,
        /// Index of the entry in the map
        entry_index: usize,
        /// JSON pointer to the matched field within the entry value
        field_path: String,
        /// The matched text
        matched_text: String,
    },
    /// Matched a TextList item
    TextListItem {
        /// The item text that matched
        text: String,
        /// Index of the item in the list
        item_index: usize,
    },
}

/// A search result with match information
#[derive(Debug, Clone)]
pub struct SearchResult {
    /// Index of the page (category) containing this result
    pub page_index: usize,
    /// Index of the item within the page
    pub item_index: usize,
    /// The setting item
    pub item: SettingItem,
    /// Breadcrumb path (e.g., "Editor > Appearance")
    pub breadcrumb: String,
    /// Match score (higher = better match)
    pub score: i32,
    /// Character indices that matched in the name (for highlighting)
    pub name_matches: Vec<usize>,
    /// Character indices that matched in the description (for highlighting)
    pub description_matches: Vec<usize>,
    /// If this result matched something inside a composite control
    pub deep_match: Option<DeepMatch>,
}

/// Perform fuzzy search over all settings
pub fn search_settings(pages: &[SettingsPage], query: &str) -> Vec<SearchResult> {
    if query.is_empty() {
        return Vec::new();
    }

    let query_lower = query.to_lowercase();
    let mut results = Vec::new();

    for (page_index, page) in pages.iter().enumerate() {
        for (item_index, item) in page.items.iter().enumerate() {
            // Try to match the name
            let (name_score, name_matches) = fuzzy_match(&item.name.to_lowercase(), &query_lower);

            // Try to match the description
            let (desc_score, desc_matches) = item
                .description
                .as_ref()
                .map(|d| fuzzy_match(&d.to_lowercase(), &query_lower))
                .unwrap_or((0, Vec::new()));

            // Also check path for matches
            let (path_score, _) = fuzzy_match(&item.path.to_lowercase(), &query_lower);

            // Total score is the best of the three
            let total_score = name_score.max(desc_score).max(path_score);

            if total_score > 0 {
                results.push(SearchResult {
                    page_index,
                    item_index,
                    item: item.clone(),
                    breadcrumb: page.name.clone(),
                    score: total_score,
                    name_matches,
                    description_matches: desc_matches,
                    deep_match: None,
                });
            }

            // Search inside composite controls (Map entries, TextList items)
            search_composite_control(
                &mut results,
                page_index,
                item_index,
                item,
                &page.name,
                &query_lower,
            );
        }
    }

    // Sort by score (descending), then by name (ascending)
    results.sort_by(|a, b| {
        b.score
            .cmp(&a.score)
            .then_with(|| a.item.name.cmp(&b.item.name))
    });

    results
}

/// Search inside composite controls (Map, TextList) for deep matches
fn search_composite_control(
    results: &mut Vec<SearchResult>,
    page_index: usize,
    item_index: usize,
    item: &SettingItem,
    page_name: &str,
    query_lower: &str,
) {
    match &item.control {
        SettingControl::Map(map_state) => {
            for (entry_idx, (key, value)) in map_state.entries.iter().enumerate() {
                // Match on map key
                let (key_score, key_matches) = fuzzy_match(&key.to_lowercase(), query_lower);
                if key_score > 0 {
                    results.push(SearchResult {
                        page_index,
                        item_index,
                        item: item.clone(),
                        breadcrumb: format!("{} > {}", page_name, key),
                        score: key_score,
                        name_matches: key_matches,
                        description_matches: Vec::new(),
                        deep_match: Some(DeepMatch::MapKey {
                            key: key.clone(),
                            entry_index: entry_idx,
                        }),
                    });
                    // Skip searching nested values when the key already matches,
                    // to avoid duplicate results like "grammar: bash" alongside "bash"
                    continue;
                }

                // Match on nested string values within the map entry
                search_json_value(
                    results,
                    page_index,
                    item_index,
                    item,
                    page_name,
                    key,
                    entry_idx,
                    value,
                    "",
                    query_lower,
                );
            }
        }
        SettingControl::TextList(list_state) => {
            for (list_idx, text) in list_state.items.iter().enumerate() {
                let (score, matches) = fuzzy_match(&text.to_lowercase(), query_lower);
                if score > 0 {
                    results.push(SearchResult {
                        page_index,
                        item_index,
                        item: item.clone(),
                        breadcrumb: format!("{} > {}", page_name, item.name),
                        score,
                        name_matches: matches,
                        description_matches: Vec::new(),
                        deep_match: Some(DeepMatch::TextListItem {
                            text: text.clone(),
                            item_index: list_idx,
                        }),
                    });
                }
            }
        }
        _ => {}
    }
}

/// Recursively search JSON values for string matches
#[allow(clippy::too_many_arguments)]
fn search_json_value(
    results: &mut Vec<SearchResult>,
    page_index: usize,
    item_index: usize,
    item: &SettingItem,
    page_name: &str,
    map_key: &str,
    entry_index: usize,
    value: &serde_json::Value,
    path: &str,
    query_lower: &str,
) {
    match value {
        serde_json::Value::String(s) => {
            let (score, _) = fuzzy_match(&s.to_lowercase(), query_lower);
            if score > 0 {
                // Use the field name from path as the display name
                let field_name = path.rsplit('/').next().unwrap_or(path).to_string();
                let display_name = if field_name.is_empty() {
                    s.clone()
                } else {
                    format!("{}: {}", field_name, s)
                };
                results.push(SearchResult {
                    page_index,
                    item_index,
                    item: item.clone(),
                    breadcrumb: format!("{} > {}", page_name, map_key),
                    score,
                    name_matches: Vec::new(),
                    description_matches: Vec::new(),
                    deep_match: Some(DeepMatch::MapValue {
                        key: map_key.to_string(),
                        entry_index,
                        field_path: path.to_string(),
                        matched_text: display_name,
                    }),
                });
            }
        }
        serde_json::Value::Object(obj) => {
            for (k, v) in obj {
                let child_path = format!("{}/{}", path, k);
                search_json_value(
                    results,
                    page_index,
                    item_index,
                    item,
                    page_name,
                    map_key,
                    entry_index,
                    v,
                    &child_path,
                    query_lower,
                );
            }
        }
        serde_json::Value::Array(arr) => {
            for (i, v) in arr.iter().enumerate() {
                let child_path = format!("{}/{}", path, i);
                search_json_value(
                    results,
                    page_index,
                    item_index,
                    item,
                    page_name,
                    map_key,
                    entry_index,
                    v,
                    &child_path,
                    query_lower,
                );
            }
        }
        _ => {}
    }
}

/// Perform fuzzy matching on a string
/// Returns (score, matched_indices)
fn fuzzy_match(text: &str, pattern: &str) -> (i32, Vec<usize>) {
    if pattern.is_empty() {
        return (0, Vec::new());
    }

    let text_chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();

    let mut score = 0;
    let mut matched_indices = Vec::new();
    let mut pattern_idx = 0;
    let mut prev_match_idx: Option<usize> = None;

    for (text_idx, &text_char) in text_chars.iter().enumerate() {
        if pattern_idx < pattern_chars.len() && text_char == pattern_chars[pattern_idx] {
            matched_indices.push(text_idx);

            // Score bonuses
            score += 10; // Base match score

            // Consecutive matches bonus
            if let Some(prev) = prev_match_idx {
                if text_idx == prev + 1 {
                    score += 15; // Consecutive match bonus
                }
            }

            // Word boundary bonus (start of word)
            if text_idx == 0
                || text_chars.get(text_idx.wrapping_sub(1)) == Some(&' ')
                || text_chars.get(text_idx.wrapping_sub(1)) == Some(&'_')
            {
                score += 20; // Word start bonus
            }

            // Exact prefix bonus
            if text_idx == pattern_idx {
                score += 5; // Matches in same position as pattern
            }

            prev_match_idx = Some(text_idx);
            pattern_idx += 1;
        }
    }

    // Did we match all pattern characters?
    if pattern_idx == pattern_chars.len() {
        // Bonus for shorter matches (more specific)
        let length_bonus = (100 - text_chars.len().min(100) as i32) / 10;
        score += length_bonus;

        // Exact match bonus
        if text == pattern {
            score += 100;
        }

        (score, matched_indices)
    } else {
        // Didn't match all characters
        (0, Vec::new())
    }
}

/// Check if a query matches a setting (simple substring match)
pub fn matches_query(item: &SettingItem, query: &str) -> bool {
    let query_lower = query.to_lowercase();

    item.name.to_lowercase().contains(&query_lower)
        || item
            .description
            .as_ref()
            .map(|d| d.to_lowercase().contains(&query_lower))
            .unwrap_or(false)
        || item.path.to_lowercase().contains(&query_lower)
}

/// Get indices of categories that have matching items
pub fn matching_categories(pages: &[SettingsPage], query: &str) -> Vec<usize> {
    if query.is_empty() {
        return Vec::new();
    }

    pages
        .iter()
        .enumerate()
        .filter(|(_, page)| page.items.iter().any(|item| matches_query(item, query)))
        .map(|(idx, _)| idx)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::controls::ToggleState;
    use crate::view::settings::items::SettingControl;

    fn make_item(name: &str, description: Option<&str>, path: &str) -> SettingItem {
        SettingItem {
            path: path.to_string(),
            name: name.to_string(),
            description: description.map(String::from),
            control: SettingControl::Toggle(ToggleState::new(false, name)),
            default: None,
            modified: false,
            layer_source: crate::config_io::ConfigLayer::System,
            read_only: false,
            is_auto_managed: false,
            nullable: false,
            is_null: false,
            section: None,
            is_section_start: false,
            layout_width: 0,
        }
    }

    fn make_page(name: &str, items: Vec<SettingItem>) -> SettingsPage {
        SettingsPage {
            name: name.to_string(),
            path: format!("/{}", name.to_lowercase()),
            description: None,
            items,
            subpages: Vec::new(),
        }
    }

    #[test]
    fn test_fuzzy_match_exact() {
        let (score, indices) = fuzzy_match("line_numbers", "line");
        assert!(score > 0);
        assert_eq!(indices, vec![0, 1, 2, 3]);
    }

    #[test]
    fn test_fuzzy_match_prefix() {
        let (score, indices) = fuzzy_match("tab_size", "tab");
        assert!(score > 0);
        assert_eq!(indices, vec![0, 1, 2]);
    }

    #[test]
    fn test_fuzzy_match_scattered() {
        let (score, indices) = fuzzy_match("line_numbers", "lnm");
        assert!(score > 0);
        // 'l' at 0, 'n' at 2 (first n in "line"), 'm' at 7 (in "numbers")
        assert_eq!(indices, vec![0, 2, 7]);
    }

    #[test]
    fn test_fuzzy_match_no_match() {
        let (score, indices) = fuzzy_match("hello", "xyz");
        assert_eq!(score, 0);
        assert!(indices.is_empty());
    }

    #[test]
    fn test_search_settings_empty_query() {
        let pages = vec![make_page(
            "Editor",
            vec![make_item(
                "Line Numbers",
                Some("Show line numbers"),
                "/line_numbers",
            )],
        )];

        let results = search_settings(&pages, "");
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_settings_name_match() {
        let pages = vec![make_page(
            "Editor",
            vec![
                make_item("Line Numbers", Some("Show line numbers"), "/line_numbers"),
                make_item("Tab Size", Some("Spaces per tab"), "/tab_size"),
            ],
        )];

        let results = search_settings(&pages, "line");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].item.name, "Line Numbers");
        assert_eq!(results[0].breadcrumb, "Editor");
    }

    #[test]
    fn test_search_settings_description_match() {
        let pages = vec![make_page(
            "Editor",
            vec![make_item(
                "Tab Size",
                Some("Number of spaces per tab character"),
                "/tab_size",
            )],
        )];

        let results = search_settings(&pages, "spaces");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].item.name, "Tab Size");
    }

    #[test]
    fn test_search_settings_path_match() {
        let pages = vec![make_page(
            "Editor",
            vec![make_item("Tab Size", None, "/editor/tab_size")],
        )];

        let results = search_settings(&pages, "editor");
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_matching_categories() {
        let pages = vec![
            make_page(
                "Editor",
                vec![make_item("Line Numbers", None, "/line_numbers")],
            ),
            make_page("Theme", vec![make_item("Theme Name", None, "/theme")]),
        ];

        let matches = matching_categories(&pages, "line");
        assert_eq!(matches, vec![0]);

        let matches = matching_categories(&pages, "theme");
        assert_eq!(matches, vec![1]);
    }

    #[test]
    fn test_search_ranking() {
        let pages = vec![make_page(
            "Editor",
            vec![
                make_item("Tab", None, "/tab"),                 // Exact match
                make_item("Tab Size", None, "/tab_size"),       // Prefix match
                make_item("Default Tab", None, "/default_tab"), // Contains match
            ],
        )];

        let results = search_settings(&pages, "tab");
        assert_eq!(results.len(), 3);
        // Exact match should be first
        assert_eq!(results[0].item.name, "Tab");
        // Then prefix match
        assert_eq!(results[1].item.name, "Tab Size");
        // Then contains match (scored lower due to position)
        assert_eq!(results[2].item.name, "Default Tab");
    }

    fn make_map_item(
        name: &str,
        path: &str,
        entries: Vec<(String, serde_json::Value)>,
    ) -> SettingItem {
        use crate::view::controls::MapState;
        let mut map_state = MapState::new(name);
        map_state.entries = entries;
        SettingItem {
            path: path.to_string(),
            name: name.to_string(),
            description: None,
            control: SettingControl::Map(map_state),
            default: None,
            modified: false,
            layer_source: crate::config_io::ConfigLayer::System,
            read_only: false,
            is_auto_managed: false,
            nullable: false,
            is_null: false,
            section: None,
            is_section_start: false,
            layout_width: 0,
        }
    }

    fn make_text_list_item(name: &str, path: &str, items: Vec<String>) -> SettingItem {
        use crate::view::controls::TextListState;
        let mut list_state = TextListState::new(name);
        list_state.items = items;
        SettingItem {
            path: path.to_string(),
            name: name.to_string(),
            description: None,
            control: SettingControl::TextList(list_state),
            default: None,
            modified: false,
            layer_source: crate::config_io::ConfigLayer::System,
            read_only: false,
            is_auto_managed: false,
            nullable: false,
            is_null: false,
            section: None,
            is_section_start: false,
            layout_width: 0,
        }
    }

    #[test]
    fn test_search_map_key() {
        let pages = vec![make_page(
            "Languages",
            vec![make_map_item(
                "Languages",
                "/languages",
                vec![
                    ("python".to_string(), serde_json::json!({})),
                    ("rust".to_string(), serde_json::json!({})),
                ],
            )],
        )];

        let results = search_settings(&pages, "python");
        // Should find a deep match on the map key "python"
        let deep_results: Vec<_> = results.iter().filter(|r| r.deep_match.is_some()).collect();
        assert!(!deep_results.is_empty(), "Should find map key 'python'");
        assert!(
            matches!(&deep_results[0].deep_match, Some(DeepMatch::MapKey { key, .. }) if key == "python")
        );
        assert_eq!(deep_results[0].breadcrumb, "Languages > python");
    }

    #[test]
    fn test_search_map_key_no_duplicate_nested_values() {
        // When a map key matches, nested values inside that entry should NOT
        // produce additional results (avoids duplicates like "grammar: bash"
        // alongside the "bash" key match).
        let pages = vec![make_page(
            "General",
            vec![make_map_item(
                "Languages",
                "/languages",
                vec![(
                    "bash".to_string(),
                    serde_json::json!({"grammar": "bash", "files": ["1.bash"]}),
                )],
            )],
        )];

        let results = search_settings(&pages, "bash");
        let deep_results: Vec<_> = results.iter().filter(|r| r.deep_match.is_some()).collect();
        // Should only have the MapKey match, not MapValue matches for "grammar: bash" etc.
        assert_eq!(
            deep_results.len(),
            1,
            "Expected exactly 1 deep match (MapKey), got {}: {:?}",
            deep_results.len(),
            deep_results
                .iter()
                .map(|r| &r.deep_match)
                .collect::<Vec<_>>()
        );
        assert!(
            matches!(&deep_results[0].deep_match, Some(DeepMatch::MapKey { key, .. }) if key == "bash")
        );
    }

    #[test]
    fn test_search_map_nested_value() {
        let pages = vec![make_page(
            "LSP",
            vec![make_map_item(
                "LSP",
                "/lsp",
                vec![(
                    "rust".to_string(),
                    serde_json::json!({"command": "rust-analyzer", "args": ["--stdio"]}),
                )],
            )],
        )];

        let results = search_settings(&pages, "rust-analyzer");
        let deep_results: Vec<_> = results
            .iter()
            .filter(|r| matches!(&r.deep_match, Some(DeepMatch::MapValue { .. })))
            .collect();
        assert!(
            !deep_results.is_empty(),
            "Should find nested value 'rust-analyzer'"
        );
    }

    #[test]
    fn test_search_text_list_item() {
        let pages = vec![make_page(
            "Editor",
            vec![make_text_list_item(
                "File Extensions",
                "/file_extensions",
                vec!["py".to_string(), "rs".to_string(), "js".to_string()],
            )],
        )];

        let results = search_settings(&pages, "py");
        let deep_results: Vec<_> = results
            .iter()
            .filter(|r| matches!(&r.deep_match, Some(DeepMatch::TextListItem { .. })))
            .collect();
        assert!(!deep_results.is_empty(), "Should find text list item 'py'");
    }

    #[test]
    fn test_deep_match_ranks_higher_than_fuzzy_noise() {
        let pages = vec![
            make_page(
                "Editor",
                vec![
                    // "python" fuzzy-matches scattered chars in long names
                    make_item(
                        "Leading Spaces",
                        Some("Show space indicators for leading whitespace"),
                        "/editor/whitespace_spaces_leading",
                    ),
                ],
            ),
            make_page(
                "Languages",
                vec![make_map_item(
                    "Languages",
                    "/languages",
                    vec![("python".to_string(), serde_json::json!({}))],
                )],
            ),
        ];

        let results = search_settings(&pages, "python");
        assert!(!results.is_empty());
        // The map key exact match on "python" should rank first
        assert!(
            matches!(&results[0].deep_match, Some(DeepMatch::MapKey { key, .. }) if key == "python"),
            "Map key 'python' should rank above fuzzy noise, got: {:?}",
            results[0].deep_match
        );
    }
}
