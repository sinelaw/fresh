//! A separate string registry for plugin-supplied translations, keyed by
//! plugin name so a plugin's strings can be added and removed with it.

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::RwLock;

/// plugin name -> locale -> key -> translated string
type PluginStringsMap = HashMap<String, HashMap<String, HashMap<String, String>>>;

static PLUGIN_STRINGS: Lazy<RwLock<PluginStringsMap>> = Lazy::new(|| RwLock::new(HashMap::new()));

/// Register a plugin's strings, in the form `{ "en": { "key": "value" }, .. }`.
pub fn register_plugin_strings(
    plugin_name: &str,
    strings: HashMap<String, HashMap<String, String>>,
) {
    PLUGIN_STRINGS
        .write()
        .unwrap()
        .insert(plugin_name.to_string(), strings);
}

/// Translate one of a plugin's strings in the active locale, falling back to
/// the fallback locale and then to `key` itself.
pub fn translate_plugin_string(
    plugin_name: &str,
    key: &str,
    args: &HashMap<String, String>,
) -> String {
    let locale = crate::locale();
    let all_strings = PLUGIN_STRINGS.read().unwrap();

    let Some(plugin_map) = all_strings.get(plugin_name) else {
        return key.to_string();
    };
    let lang_map = plugin_map
        .get(&locale)
        .or_else(|| plugin_map.get(crate::store::FALLBACK_LOCALE));
    let Some(template) = lang_map.and_then(|m| m.get(key)) else {
        return key.to_string();
    };

    let args: Vec<(&str, String)> = args.iter().map(|(k, v)| (k.as_str(), v.clone())).collect();
    crate::interpolate(template, &args)
}

/// Forget a plugin's strings.
pub fn unregister_plugin_strings(plugin_name: &str) {
    PLUGIN_STRINGS.write().unwrap().remove(plugin_name);
}
