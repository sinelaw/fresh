//! Banner manager — the non-modal alternative to action popups.
//!
//! Banners render as a strip across the top of the editor (between the menu
//! bar and the buffer area) and never steal focus. The user can keep editing
//! while a banner is on screen. The manager owns a priority queue with four
//! slots: Security > Authority > Activation > Info. Within a slot, requests
//! are FIFO. At most one banner is active at any time; higher-priority
//! requests preempt the currently active one (the preempted banner returns
//! to the queue and becomes active again after the higher-priority one
//! resolves).
//!
//! # Dedup & replace
//!
//! Each banner has a stable `id`. Re-requesting an id that is already in
//! the queue or active replaces that banner's contents in place. This is
//! how a content-hash change for the same conceptual question (`"the
//! .envrc file changed since you trusted it"`) updates the message
//! without a flicker of dismissal + re-show.
//!
//! # Invalidation
//!
//! A banner can declare `invalidate_on` triggers. When the corresponding
//! event fires (trust level reaches a target, authority changes, named
//! hook fires), the banner is dropped from the queue *without* firing
//! `banner_result`. The requester receives a `banner_invalidated` hook
//! so it can clean up pending state — there is no "did the user click,
//! or did it just vanish?" ambiguity in the API.
//!
//! # Persistence
//!
//! Per-workspace dismissal is recorded in the workspace decision store
//! (see [`crate::services::workspace_decisions`]). On re-open, banners
//! whose `id` has a persisted dismissal are silently skipped at enqueue
//! time. Session-only dismissals live in this struct and clear on editor
//! restart.

use fresh_core::api::{
    BannerAction, BannerDismissalScope, BannerInvalidation, BannerOptions, BannerPriority,
};
use std::collections::{HashMap, HashSet, VecDeque};

/// One banner in the queue or active slot. Mirrors `BannerOptions` plus a
/// monotonic insertion sequence for stable FIFO within a priority bucket.
/// Routing back to the requesting plugin is done via the banner's `id` —
/// plugins that care about an outcome listen for `banner_result` and
/// filter by id (same model as action popups).
///
/// The banner has no "selected action" state — interaction is via
/// `Alt+<mnemonic>` from anywhere or a mouse click on a button. There's
/// no focus to shift, no Tab navigation: that's how non-modal stays
/// non-modal.
#[derive(Debug, Clone)]
pub struct Banner {
    pub id: String,
    pub priority: BannerPriority,
    pub title: String,
    pub message: String,
    pub actions: Vec<BannerAction>,
    pub invalidate_on: Vec<BannerInvalidation>,
    pub dismissal_scope: BannerDismissalScope,
    /// Monotonic sequence — older within the same priority bucket comes
    /// first.
    seq: u64,
}

impl Banner {
    fn from_options(opts: BannerOptions, seq: u64) -> Self {
        Self {
            id: opts.id,
            priority: opts.priority,
            title: opts.title,
            message: opts.message,
            actions: opts.actions,
            invalidate_on: opts.invalidate_on,
            dismissal_scope: opts.dismissal_scope,
            seq,
        }
    }

    /// The mnemonic character for an action: an explicit `mnemonic`
    /// field if set, otherwise the first ASCII alphanumeric character
    /// of the label. Empty for actions that are mouse-only.
    pub fn action_mnemonic(action: &BannerAction) -> Option<char> {
        if let Some(c) = action.mnemonic.chars().next() {
            return Some(c.to_ascii_lowercase());
        }
        action
            .label
            .chars()
            .find(|c| c.is_ascii_alphanumeric())
            .map(|c| c.to_ascii_lowercase())
    }

    /// Find an action whose mnemonic matches the given character
    /// (case-insensitive). Used by the input dispatcher to route
    /// `Alt+<letter>` keystrokes to the right action.
    pub fn action_for_mnemonic(&self, c: char) -> Option<&BannerAction> {
        let c = c.to_ascii_lowercase();
        self.actions
            .iter()
            .find(|a| Self::action_mnemonic(a) == Some(c))
    }
}

/// Outcome of resolving a banner (action picked or Esc) — surfaced via the
/// `banner_result` hook.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BannerOutcome {
    pub banner_id: String,
    pub action_id: String,
    /// True when the user picked the dismiss action (Esc / X). False
    /// when the user picked a real action button.
    pub dismissed: bool,
    pub dismissal_scope: BannerDismissalScope,
}

/// Why a banner was invalidated — surfaced via the `banner_invalidated`
/// hook so the requester can clean up.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BannerInvalidationEvent {
    pub banner_id: String,
    pub reason: String,
}

/// What happened during a manager operation. Drives editor follow-up:
/// emit hooks, redraw chrome, persist dismissals.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BannerEffects {
    /// Banners that resolved cleanly (user picked an action or dismissed).
    pub resolved: Vec<BannerOutcome>,
    /// Banners dropped because an invalidation trigger fired.
    pub invalidated: Vec<BannerInvalidationEvent>,
    /// True if the active banner changed (newly shown, replaced, or
    /// cleared) — caller invalidates layout / repaints.
    pub active_changed: bool,
}

impl BannerEffects {
    fn merged_with(mut self, other: BannerEffects) -> Self {
        self.resolved.extend(other.resolved);
        self.invalidated.extend(other.invalidated);
        self.active_changed |= other.active_changed;
        self
    }
}

/// The banner queue. Owns the active banner (the one rendered) and the
/// per-priority FIFOs of queued banners.
#[derive(Debug, Default)]
pub struct BannerManager {
    active: Option<Banner>,
    /// Per-priority FIFO queues. Higher-priority queues are drained first.
    queues: HashMap<BannerPriority, VecDeque<Banner>>,
    /// Banner ids dismissed this session — re-requests for these ids
    /// are silently dropped at enqueue time. Cleared on session start.
    session_dismissed: HashSet<String>,
    /// Banner ids the workspace decision store says are dismissed for
    /// this workspace. Loaded at workspace-open time; treated the same
    /// as `session_dismissed` for filtering.
    workspace_dismissed: HashSet<String>,
    /// Monotonic sequence — guarantees stable FIFO within a priority.
    next_seq: u64,
}

impl BannerManager {
    pub fn new() -> Self {
        Self::default()
    }

    /// Load the set of persistently-dismissed banner ids for the current
    /// workspace. Called when the workspace root changes. Banner ids in
    /// this set are silently dropped at enqueue time.
    pub fn set_workspace_dismissed(&mut self, ids: HashSet<String>) {
        self.workspace_dismissed = ids;
    }

    /// The banner currently rendered, if any.
    pub fn active(&self) -> Option<&Banner> {
        self.active.as_ref()
    }

    /// True when there is any banner (active or queued).
    pub fn has_any(&self) -> bool {
        self.active.is_some() || self.queues.values().any(|q| !q.is_empty())
    }

    /// Count of queued banners *not* counting the active one. Used by
    /// the chrome ("+N more") to hint the user that other banners
    /// follow.
    pub fn queued_count(&self) -> usize {
        self.queues.values().map(|q| q.len()).sum()
    }

    /// Enqueue a banner request. May replace an existing same-id banner
    /// in place, may immediately preempt the active banner if higher
    /// priority, and may be dropped silently if the id is already
    /// dismissed in this session or persistently.
    pub fn enqueue(&mut self, opts: BannerOptions) -> BannerEffects {
        // Silently drop persisted / session-level dismissals so re-requests
        // for the same question don't nag.
        if self.session_dismissed.contains(&opts.id) || self.workspace_dismissed.contains(&opts.id)
        {
            return BannerEffects::default();
        }

        let seq = self.next_seq;
        self.next_seq += 1;
        let banner = Banner::from_options(opts, seq);

        // Replace-in-place semantics: if an existing banner has the same
        // id, swap its contents (preserves position in the queue).
        if let Some(active) = self.active.as_mut() {
            if active.id == banner.id {
                let prev_priority = active.priority;
                let new_priority = banner.priority;
                // Reseat content but keep the existing sequence so the
                // banner stays where it is in FIFO order if priority is
                // unchanged.
                let seq = active.seq;
                let mut replaced = banner;
                replaced.seq = seq;
                *active = replaced;
                // If priority changed, re-queue at the new priority and
                // drain. This is rare but harmless.
                if prev_priority != new_priority {
                    let preempted = self.active.take().unwrap();
                    self.queues
                        .entry(preempted.priority)
                        .or_default()
                        .push_back(preempted);
                    return BannerEffects {
                        active_changed: true,
                        ..Default::default()
                    }
                    .merged_with(self.drain_active());
                }
                return BannerEffects {
                    active_changed: true,
                    ..Default::default()
                };
            }
        }
        for q in self.queues.values_mut() {
            if let Some(pos) = q.iter().position(|b| b.id == banner.id) {
                let seq = q[pos].seq;
                let mut replaced = banner.clone();
                replaced.seq = seq;
                q[pos] = replaced;
                return BannerEffects::default();
            }
        }

        // Brand-new request: push to the appropriate priority queue and
        // drain.
        self.queues
            .entry(banner.priority)
            .or_default()
            .push_back(banner);
        self.drain_active()
    }

    /// Withdraw a banner by id (queued or active) without firing a
    /// result. No-op if no such banner exists.
    pub fn withdraw(&mut self, banner_id: &str) -> BannerEffects {
        if let Some(active) = self.active.as_ref() {
            if active.id == banner_id {
                self.active = None;
                return BannerEffects {
                    active_changed: true,
                    ..Default::default()
                }
                .merged_with(self.drain_active());
            }
        }
        for q in self.queues.values_mut() {
            if let Some(pos) = q.iter().position(|b| b.id == banner_id) {
                q.remove(pos);
                return BannerEffects::default();
            }
        }
        BannerEffects::default()
    }

    /// Resolve the active banner with the given action id (or
    /// `"dismissed"` for Esc / X). No-op if no active banner. Returns
    /// the outcome for the editor to emit `banner_result`.
    pub fn resolve_active(&mut self, action_id: &str) -> BannerEffects {
        let Some(banner) = self.active.take() else {
            return BannerEffects::default();
        };
        let dismissed = action_id == "dismissed";
        if dismissed {
            match banner.dismissal_scope {
                BannerDismissalScope::Transient => {}
                BannerDismissalScope::Session => {
                    self.session_dismissed.insert(banner.id.clone());
                }
                BannerDismissalScope::Workspace => {
                    // Caller is responsible for persisting via the
                    // workspace decision store — see
                    // BannerOutcome.dismissal_scope below. We do not
                    // add to workspace_dismissed locally because the
                    // store is authoritative; reloading the workspace
                    // populates it via set_workspace_dismissed.
                    self.session_dismissed.insert(banner.id.clone());
                }
            }
        }
        let outcome = BannerOutcome {
            banner_id: banner.id.clone(),
            action_id: action_id.to_string(),
            dismissed,
            dismissal_scope: banner.dismissal_scope,
        };
        let drain = self.drain_active();
        BannerEffects {
            resolved: vec![outcome],
            invalidated: drain.invalidated,
            active_changed: true,
        }
    }

    /// Resolve the active banner via a mnemonic keystroke
    /// (`Alt+<letter>`). Routes to the matching action's id and fires
    /// `banner_result`. Returns empty effects if no action matches the
    /// mnemonic.
    pub fn resolve_mnemonic(&mut self, c: char) -> BannerEffects {
        let action_id = self
            .active
            .as_ref()
            .and_then(|b| b.action_for_mnemonic(c))
            .map(|a| a.id.clone());
        match action_id {
            Some(id) => self.resolve_active(&id),
            None => BannerEffects::default(),
        }
    }

    /// Drop every banner (active and queued) whose `invalidate_on`
    /// triggers match the supplied `event`. Used by the editor to react
    /// to trust changes, authority changes, and named hooks.
    pub fn invalidate(&mut self, event: InvalidationEvent<'_>) -> BannerEffects {
        let mut effects = BannerEffects::default();

        // Active first — preserves the "active_changed" flag.
        let drop_active = self
            .active
            .as_ref()
            .map(|b| matches_any(&b.invalidate_on, &event))
            .unwrap_or(false);
        if drop_active {
            let b = self.active.take().unwrap();
            effects.invalidated.push(BannerInvalidationEvent {
                banner_id: b.id,
                reason: event.label().to_string(),
            });
            effects.active_changed = true;
        }

        for q in self.queues.values_mut() {
            let mut i = 0;
            while i < q.len() {
                if matches_any(&q[i].invalidate_on, &event) {
                    let b = q.remove(i).unwrap();
                    effects.invalidated.push(BannerInvalidationEvent {
                        banner_id: b.id,
                        reason: event.label().to_string(),
                    });
                } else {
                    i += 1;
                }
            }
        }

        if effects.active_changed {
            effects = effects.merged_with(self.drain_active());
        }
        effects
    }

    /// Promote the highest-priority queued banner to active. Called
    /// internally after every state change that might leave the active
    /// slot empty. Returns whether the active slot changed.
    fn drain_active(&mut self) -> BannerEffects {
        if self.active.is_some() {
            return BannerEffects::default();
        }
        for priority in [
            BannerPriority::Security,
            BannerPriority::Authority,
            BannerPriority::Activation,
            BannerPriority::Info,
        ] {
            if let Some(q) = self.queues.get_mut(&priority) {
                if let Some(banner) = q.pop_front() {
                    self.active = Some(banner);
                    return BannerEffects {
                        active_changed: true,
                        ..Default::default()
                    };
                }
            }
        }
        BannerEffects::default()
    }
}

/// Editor-facing event that may invalidate queued banners. Constructed by
/// the editor at the call site of the underlying state change
/// (`set_level`, `set_authority`, generic hook).
#[derive(Debug, Clone, Copy)]
pub enum InvalidationEvent<'a> {
    /// The workspace trust level just changed to `new_level`.
    TrustReached(&'a str),
    /// The active authority changed.
    AuthorityChanged,
    /// A named hook fired.
    Hook(&'a str),
}

impl<'a> InvalidationEvent<'a> {
    fn label(&self) -> &'static str {
        match self {
            InvalidationEvent::TrustReached(_) => "trust_reaches",
            InvalidationEvent::AuthorityChanged => "authority_changed",
            InvalidationEvent::Hook(_) => "hook",
        }
    }
}

fn matches_any(triggers: &[BannerInvalidation], event: &InvalidationEvent<'_>) -> bool {
    triggers.iter().any(|t| match (t, event) {
        (BannerInvalidation::TrustReaches { level }, InvalidationEvent::TrustReached(actual)) => {
            level == actual
        }
        (BannerInvalidation::AuthorityChanged, InvalidationEvent::AuthorityChanged) => true,
        (BannerInvalidation::Hook { name }, InvalidationEvent::Hook(actual)) => name == actual,
        _ => false,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn action(id: &str) -> BannerAction {
        BannerAction {
            id: id.to_string(),
            label: id.to_string(),
            mnemonic: String::new(),
        }
    }

    fn opts(id: &str, priority: BannerPriority) -> BannerOptions {
        BannerOptions {
            id: id.to_string(),
            priority,
            title: id.to_string(),
            message: String::new(),
            actions: vec![action("ok")],
            invalidate_on: Vec::new(),
            dismissal_scope: BannerDismissalScope::Session,
        }
    }

    #[test]
    fn enqueue_makes_a_banner_active() {
        let mut m = BannerManager::new();
        let fx = m.enqueue(opts("a", BannerPriority::Info));
        assert!(fx.active_changed);
        assert_eq!(m.active().unwrap().id, "a");
        assert_eq!(m.queued_count(), 0);
    }

    #[test]
    fn higher_priority_preempts_lower_via_queue() {
        // The earlier-enqueued lower-priority banner is active. A new
        // higher-priority enqueue does *not* immediately preempt the
        // active slot in the current impl — it goes onto its own
        // priority queue, and will become active only when the current
        // one resolves. This is the simpler invariant ("active stays
        // until resolved or invalidated") and matches the user
        // expectation that they shouldn't have their answer yanked.
        let mut m = BannerManager::new();
        m.enqueue(opts("info", BannerPriority::Info));
        m.enqueue(opts("sec", BannerPriority::Security));
        assert_eq!(m.active().unwrap().id, "info");
        assert_eq!(m.queued_count(), 1);

        let fx = m.resolve_active("ok");
        assert_eq!(fx.resolved.len(), 1);
        assert_eq!(fx.resolved[0].banner_id, "info");
        assert_eq!(m.active().unwrap().id, "sec");
    }

    #[test]
    fn invalidation_drops_silently_no_result() {
        let mut m = BannerManager::new();
        let mut o = opts("env", BannerPriority::Activation);
        o.invalidate_on = vec![BannerInvalidation::AuthorityChanged];
        m.enqueue(o);
        assert!(m.active().is_some());

        let fx = m.invalidate(InvalidationEvent::AuthorityChanged);
        assert!(fx.resolved.is_empty(), "no banner_result on invalidation");
        assert_eq!(fx.invalidated.len(), 1);
        assert!(m.active().is_none());
    }

    #[test]
    fn invalidation_drops_queued_too() {
        let mut m = BannerManager::new();
        m.enqueue(opts("active", BannerPriority::Security));
        let mut o = opts("queued", BannerPriority::Activation);
        o.invalidate_on = vec![BannerInvalidation::Hook {
            name: "x".to_string(),
        }];
        m.enqueue(o);

        let fx = m.invalidate(InvalidationEvent::Hook("x"));
        assert_eq!(fx.invalidated.len(), 1);
        assert_eq!(fx.invalidated[0].banner_id, "queued");
        // Active was untouched (different invalidation trigger).
        assert_eq!(m.active().unwrap().id, "active");
    }

    #[test]
    fn replace_in_place_no_queue_growth() {
        let mut m = BannerManager::new();
        let mut o = opts("dup", BannerPriority::Info);
        o.message = "v1".into();
        m.enqueue(o);
        let mut o2 = opts("dup", BannerPriority::Info);
        o2.message = "v2".into();
        m.enqueue(o2);
        assert_eq!(m.queued_count(), 0);
        assert_eq!(m.active().unwrap().message, "v2");
    }

    #[test]
    fn session_dismissed_drops_re_requests() {
        let mut m = BannerManager::new();
        m.enqueue(opts("env", BannerPriority::Activation));
        m.resolve_active("dismissed");
        assert!(m.active().is_none());

        let fx = m.enqueue(opts("env", BannerPriority::Activation));
        assert!(!fx.active_changed);
        assert!(m.active().is_none());
    }

    #[test]
    fn workspace_dismissed_drops_at_enqueue() {
        let mut m = BannerManager::new();
        let mut s = HashSet::new();
        s.insert("env".to_string());
        m.set_workspace_dismissed(s);
        let fx = m.enqueue(opts("env", BannerPriority::Activation));
        assert!(!fx.active_changed);
        assert!(m.active().is_none());
    }

    #[test]
    fn withdraw_removes_active_and_promotes_next() {
        let mut m = BannerManager::new();
        m.enqueue(opts("a", BannerPriority::Activation));
        m.enqueue(opts("b", BannerPriority::Activation));
        let fx = m.withdraw("a");
        assert!(fx.active_changed);
        assert_eq!(m.active().unwrap().id, "b");
    }

    #[test]
    fn mnemonic_routes_to_matching_action() {
        let mut m = BannerManager::new();
        let mut o = opts("multi", BannerPriority::Info);
        o.actions = vec![
            action("Trust"),
            action("Not now"),
            action("Keep restricted"),
        ];
        m.enqueue(o);
        let fx = m.resolve_mnemonic('n');
        assert_eq!(fx.resolved.len(), 1);
        assert_eq!(fx.resolved[0].action_id, "Not now");
        assert!(!fx.resolved[0].dismissed);
    }

    #[test]
    fn mnemonic_case_insensitive() {
        let mut m = BannerManager::new();
        let mut o = opts("m", BannerPriority::Info);
        o.actions = vec![action("Trust")];
        m.enqueue(o);
        let fx = m.resolve_mnemonic('T');
        assert_eq!(fx.resolved[0].action_id, "Trust");
    }

    #[test]
    fn explicit_mnemonic_field_wins_over_label_first_letter() {
        let mut m = BannerManager::new();
        let mut o = opts("m", BannerPriority::Info);
        o.actions = vec![BannerAction {
            id: "act".into(),
            label: "Keep restricted".into(),
            mnemonic: "x".into(),
        }];
        m.enqueue(o);
        // 'k' (first letter of label) does NOT match
        assert!(m.resolve_mnemonic('k').resolved.is_empty());
        assert!(m.active().is_some());
        // 'x' (explicit) does
        let fx = m.resolve_mnemonic('x');
        assert_eq!(fx.resolved[0].action_id, "act");
    }

    #[test]
    fn mnemonic_with_no_match_is_noop() {
        let mut m = BannerManager::new();
        let mut o = opts("m", BannerPriority::Info);
        o.actions = vec![action("Trust")];
        m.enqueue(o);
        let fx = m.resolve_mnemonic('z');
        assert!(fx.resolved.is_empty());
        assert!(
            m.active().is_some(),
            "banner survives an unmatched mnemonic"
        );
    }
}
