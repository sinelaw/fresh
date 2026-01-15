//! Search functionality for settings
//!
//! Provides fuzzy search over setting names and descriptions,
//! with support for highlighting matching categories.

use super::items::{SettingItem, SettingsPage};

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
                });
            }
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
}
