//! Host-driven plugin timers — the machinery behind `editor.setInterval` and
//! `editor.setTimeout`.
//!
//! # Why the host owns these
//!
//! A plugin can already drive periodic work itself, with a detached
//! `while (alive) { await editor.delay(ms); … }` loop — the bundled
//! `dashboard.ts` does exactly that, successfully. So this is not a
//! capability that was missing; it is the same capability with the
//! bookkeeping moved to the side that can actually do it.
//!
//! What a plugin-side loop makes the author responsible for:
//!
//! - **Surviving a throw.** The loop is a detached async function, so a
//!   rejection anywhere in its body ends it with nowhere to report. The panel
//!   just stops updating. dashboard.ts only survives this because every await
//!   in its refresh path is individually guarded, with a timeout race and a
//!   stale-while-revalidate fallback — careful code that a first attempt will
//!   not have. A timer handler's throw is caught here, logged with the timer
//!   and plugin that produced it, and the next tick still fires.
//! - **Cancellation.** Nothing stops a loop when its plugin is unloaded or
//!   reloaded; it runs until its own guard happens to notice, which means the
//!   guard has to be an identity check rather than a boolean or a reload
//!   leaves two loops racing. Timers are cancelled by the host on unload.
//! - **The first iteration**, which is one period late unless the author also
//!   does the work once before entering the loop.
//!
//! A loop is still the better shape when each iteration depends on the last,
//! or when one ticker drives many items on their own schedules — again
//! dashboard.ts, which ticks at 1s and re-runs a section only once its own TTL
//! has expired, so ambient cost tracks the sum of the sections' rates instead
//! of tick-rate × section-count.
//!
//! # Lifetime
//!
//! Timers are owned by the plugin that created them and cancelled when it is
//! unloaded or reloaded (the runtime tracks the ids and sends
//! `ClearPluginTimer` for each on cleanup). That matters most during plugin
//! development, where a hot-reload would otherwise leave every previous copy's
//! timers ticking alongside the new one.

use std::time::Instant;

/// Floor on a timer's period.
///
/// A plugin asking for `0` (or `1`) means "as often as possible", which on a
/// host-driven timer would mean "every tick, forever" — enough to keep the
/// editor busy doing nothing. 16ms is roughly a frame; nothing a plugin
/// renders can usefully go faster, since the editor won't repaint faster
/// either.
pub const MIN_PLUGIN_TIMER_MS: u64 = 16;

/// One live `setInterval` / `setTimeout`.
#[derive(Debug, Clone)]
pub struct PluginTimer {
    /// Id minted by the plugin runtime; what `clearInterval` names.
    pub id: u64,
    /// Owning plugin, so unload can cancel what it left behind.
    pub plugin_name: String,
    /// Global JS function the fire dispatches to.
    pub handler_name: String,
    /// Period, already clamped to [`MIN_PLUGIN_TIMER_MS`].
    pub interval: std::time::Duration,
    /// When this timer is next due.
    pub next_fire: Instant,
    /// `false` for a one-shot, which is dropped after it fires.
    pub repeat: bool,
}

impl PluginTimer {
    /// Build a timer due `interval_ms` after `now`, clamping the period.
    pub fn new(
        id: u64,
        plugin_name: String,
        handler_name: String,
        interval_ms: u64,
        repeat: bool,
        now: Instant,
    ) -> Self {
        let interval = std::time::Duration::from_millis(interval_ms.max(MIN_PLUGIN_TIMER_MS));
        Self {
            id,
            plugin_name,
            handler_name,
            interval,
            next_fire: now + interval,
            repeat,
        }
    }

    /// Re-arm a repeating timer relative to `now`.
    ///
    /// Deliberately `now + interval` rather than `next_fire + interval`: after
    /// the editor has been blocked (a big paste, a slow LSP round-trip) the
    /// latter would have accumulated a backlog of due-in-the-past fires and
    /// would burn them all off in a burst. A plugin polling every 30s wants
    /// "again in 30s", not "the four I missed, right now".
    pub fn rearm(&mut self, now: Instant) {
        self.next_fire = now + self.interval;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn period_is_clamped_to_the_floor() {
        let now = Instant::now();
        let t = PluginTimer::new(1, "p".into(), "h".into(), 0, true, now);
        assert_eq!(t.interval, Duration::from_millis(MIN_PLUGIN_TIMER_MS));
        assert!(t.next_fire > now, "a timer is never due immediately");
    }

    #[test]
    fn a_normal_period_is_left_alone() {
        let now = Instant::now();
        let t = PluginTimer::new(1, "p".into(), "h".into(), 30_000, true, now);
        assert_eq!(t.interval, Duration::from_secs(30));
    }

    /// Re-arming after a long stall schedules one fire from now, not a
    /// backlog of the ones that came due while the editor was busy.
    #[test]
    fn rearm_does_not_accumulate_missed_fires() {
        let start = Instant::now();
        let mut t = PluginTimer::new(1, "p".into(), "h".into(), 100, true, start);

        let much_later = start + Duration::from_secs(10);
        t.rearm(much_later);

        assert_eq!(t.next_fire, much_later + Duration::from_millis(100));
    }
}
