//! Memoised expansion of `MenuConfig` against the active `ThemeRegistry`.
//!
//! Without this cache, `MenuRenderer::render` rescans the themes directory
//! and re-deserialises every theme JSON file on every frame to expand any
//! `DynamicSubmenu` items in the menu config. The cache builds the
//! expansion once per registry instance and reuses it until the registry
//! is replaced (e.g. by `reload_themes`), at which point callers
//! `invalidate` and re-`update`.

use std::path::Path;
use std::sync::Arc;

use crate::config::{MenuConfig, MenuExt};
use crate::view::theme::ThemeRegistry;

#[derive(Default)]
pub struct ExpandedMenusCache {
    cached: Option<(Arc<ThemeRegistry>, MenuConfig)>,
}

impl ExpandedMenusCache {
    /// Ensure the cache holds an expansion built against `registry`. No-op
    /// when the cached entry was already built against the same `Arc`
    /// instance (compared via `Arc::ptr_eq`).
    pub fn update(&mut self, registry: &Arc<ThemeRegistry>, base: &MenuConfig, themes_dir: &Path) {
        if let Some((cached_reg, _)) = &self.cached {
            if Arc::ptr_eq(cached_reg, registry) {
                return;
            }
        }
        let mut menus = base.clone();
        for menu in &mut menus.menus {
            menu.expand_dynamic_items(themes_dir);
        }
        self.cached = Some((Arc::clone(registry), menus));
    }

    /// The currently-cached expansion, or `None` if not yet built / just
    /// invalidated.
    pub fn get(&self) -> Option<&MenuConfig> {
        self.cached.as_ref().map(|(_, m)| m)
    }

    /// Drop the cached expansion. The next `update` will rebuild.
    pub fn invalidate(&mut self) {
        self.cached = None;
    }
}
