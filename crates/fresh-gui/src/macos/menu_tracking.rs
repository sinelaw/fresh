//! Detect when the macOS menu bar is being tracked (user is browsing menus).
//!
//! Modifying `NSMenuItem` properties (enabled, title, checked) while the menu
//! bar is in tracking mode can cause the highlighted menu to jump to the
//! leftmost item — a known macOS issue that affects apps with custom event
//! loops (winit, GLFW, SDL, etc.).
//!
//! This module observes `NSMenuDidBeginTrackingNotification` and
//! `NSMenuDidEndTrackingNotification` via `NSNotificationCenter` and exposes a
//! simple [`is_menu_tracking`] predicate so callers can defer menu mutations.

use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Once;

use block2::RcBlock;
use objc2_foundation::{NSNotification, NSNotificationCenter, NSString};

/// `true` while the user is browsing the native menu bar.
static MENU_TRACKING: AtomicBool = AtomicBool::new(false);

/// Guard to ensure observers are only installed once.
static INSTALL_ONCE: Once = Once::new();

/// Returns `true` if the macOS menu bar is currently in tracking mode
/// (i.e. the user clicked the menu bar and is hovering over menus).
pub fn is_menu_tracking() -> bool {
    MENU_TRACKING.load(Ordering::Relaxed)
}

/// Install `NSNotificationCenter` observers for menu begin/end tracking.
///
/// Safe to call multiple times — observers are only installed on the first
/// call.  Must be called from the main thread (after `NSApplication` is
/// initialized).  The observers remain active for the lifetime of the
/// process.
pub fn install_tracking_observers() {
    INSTALL_ONCE.call_once(|| {
        // SAFETY: We are on the main thread (called from winit's `resumed`).
        // The notification names are well-known AppKit constants.
        unsafe { install_observers_inner() }
    });
}

unsafe fn install_observers_inner() {
    unsafe {
        let center = NSNotificationCenter::defaultCenter();

        // NSMenuDidBeginTrackingNotification
        let begin_name = NSString::from_str("NSMenuDidBeginTrackingNotification");
        let begin_block = RcBlock::new(|_notif: NonNull<NSNotification>| {
            tracing::info!("Menu tracking began");
            MENU_TRACKING.store(true, Ordering::Relaxed);
        });
        let _begin_observer = center.addObserverForName_object_queue_usingBlock(
            Some(&begin_name),
            None,
            None,
            &begin_block,
        );
        // Leak the observer so it stays alive for the process lifetime.
        std::mem::forget(_begin_observer);

        // NSMenuDidEndTrackingNotification
        let end_name = NSString::from_str("NSMenuDidEndTrackingNotification");
        let end_block = RcBlock::new(|_notif: NonNull<NSNotification>| {
            tracing::info!("Menu tracking ended");
            MENU_TRACKING.store(false, Ordering::Relaxed);
        });
        let _end_observer = center.addObserverForName_object_queue_usingBlock(
            Some(&end_name),
            None,
            None,
            &end_block,
        );
        std::mem::forget(_end_observer);
    }
}
