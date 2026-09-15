//! Owning handles to a Tokio runtime.
//!
//! The editor keeps a small pool for async I/O, and each remote session keeps
//! its own alongside its connection. Anything that wants to put work on one of
//! those runtimes has to say so in its type, and this module is how.
//!
//! This is the sanctioned wrapper around `tokio::runtime::Handle`, so it is
//! also the one module allowed to name one — see `clippy.toml`.
#![allow(clippy::disallowed_types, clippy::disallowed_methods)]

use std::future::Future;
use std::sync::Arc;
use tokio::runtime::Runtime;
use tokio::task::JoinHandle;

/// Sole owner of a [`Runtime`], with a drop that is safe from anywhere.
struct RuntimeOwner(Option<Runtime>);

impl RuntimeOwner {
    /// The runtime. Only `None` while this owner is being dropped, and nothing
    /// can observe it then: reaching here means a [`LiveRuntime`] clone still
    /// exists, so the owner has not started dropping.
    fn get(&self) -> &Runtime {
        self.0
            .as_ref()
            .expect("LiveRuntime outlived the runtime it owns")
    }
}

impl Drop for RuntimeOwner {
    fn drop(&mut self) {
        let Some(runtime) = self.0.take() else { return };
        // Dropping a `Runtime` joins its worker threads, and tokio forbids
        // blocking like that from inside a runtime context — it panics with
        // "Cannot drop a runtime in a context where blocking is not allowed".
        // A `LiveRuntime` travels into spawned tasks, so the last clone can
        // land on a worker thread; quitting the editor while an off-loop task
        // is still in flight does exactly that. Handing the shutdown off
        // rather than waiting for it keeps that drop panic-free, which is what
        // makes this type safe to hold from inside a task at all.
        if tokio::runtime::Handle::try_current().is_ok() {
            runtime.shutdown_background();
        }
        // Otherwise fall through to the ordinary drop, which waits for the
        // workers — the right thing to do from a plain thread.
    }
}

/// A Tokio runtime that is alive **because you are holding this**.
///
/// This type exists because [`tokio::runtime::Handle`] does not carry that
/// guarantee: a `Handle` is `Clone + 'static` and keeps nothing alive, so
/// "only usable while that runtime is up" is invisible to the compiler and
/// gets reported at runtime instead — loudly, by `Handle::block_on` polling a
/// timer on a runtime mid-shutdown (#3299), or quietly, by `Handle::spawn`
/// dropping the task on the floor and taking the work with it.
///
/// Holding a `LiveRuntime` rules out both, because the runtime cannot have
/// shut down while you have one. Clones share ownership and the runtime shuts
/// down when the last clone drops, from whatever thread that turns out to be.
///
/// Prefer this to a `Handle` in every field and every struct. `Handle` is
/// still the right parameter type for something whose own lifetime is already
/// bounded by a `LiveRuntime` held by the caller — a task already running on
/// that runtime, say — and [`LiveRuntime::handle`] is there for that case.
#[derive(Clone)]
pub struct LiveRuntime(Arc<RuntimeOwner>);

impl LiveRuntime {
    /// Build a multi-threaded runtime with every driver enabled.
    pub fn multi_thread(thread_name: &str, worker_threads: usize) -> std::io::Result<Self> {
        Ok(Self::new(
            tokio::runtime::Builder::new_multi_thread()
                .worker_threads(worker_threads)
                .thread_name(thread_name)
                .enable_all()
                .build()?,
        ))
    }

    /// Build a single-threaded runtime, driven only while [`Self::block_on`]
    /// is running — the shape `#[tokio::test]` uses by default.
    pub fn current_thread() -> std::io::Result<Self> {
        Ok(Self::new(
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()?,
        ))
    }

    /// Take ownership of an already-built runtime.
    pub fn new(runtime: Runtime) -> Self {
        Self(Arc::new(RuntimeOwner(Some(runtime))))
    }

    /// Spawn a task. It cannot be silently dropped for want of a runtime: the
    /// runtime is up as long as `self` exists, and the task's own clone of a
    /// `LiveRuntime`, if it carries one, keeps it up for as long as it runs.
    pub fn spawn<F>(&self, future: F) -> JoinHandle<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        self.0.get().spawn(future)
    }

    /// Run a blocking closure on the runtime's blocking pool.
    pub fn spawn_blocking<F, R>(&self, f: F) -> JoinHandle<R>
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send + 'static,
    {
        self.0.get().spawn_blocking(f)
    }

    /// Drive `future` to completion on this runtime, blocking the caller.
    ///
    /// Panics if the calling thread is already inside *any* runtime — tokio
    /// forbids blocking a thread that is driving tasks. That is a property of
    /// the call site, which the caller can check, and not of the runtime's
    /// state, which it cannot; `LiveRuntime` is what removes the second half.
    pub fn block_on<F: Future>(&self, future: F) -> F::Output {
        self.0.get().block_on(future)
    }

    /// Enter the runtime's context for the lifetime of the returned guard, so
    /// that `tokio::spawn` and the timer work on the current thread.
    pub fn enter(&self) -> tokio::runtime::EnterGuard<'_> {
        self.0.get().enter()
    }

    /// How many clones of this `LiveRuntime` exist, this one included.
    ///
    /// Test-only, and only meaningful to a test that controls every clone:
    /// it is what lets `plugin_offloop` assert that off-loop work does not
    /// hold an owning reference to the editor's runtime.
    #[cfg(test)]
    pub(crate) fn live_clones(&self) -> usize {
        Arc::strong_count(&self.0)
    }

    /// A borrowed `Handle`, for the APIs that insist on one.
    ///
    /// The `Handle` keeps nothing alive, so only hand one to something whose
    /// own lifetime is already bounded by this `LiveRuntime`. Anything that
    /// *stores* it wants a `LiveRuntime` instead.
    pub fn handle(&self) -> &tokio::runtime::Handle {
        self.0.get().handle()
    }
}

impl std::fmt::Debug for LiveRuntime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("LiveRuntime")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clones_keep_the_runtime_up() {
        let rt = LiveRuntime::multi_thread("test-live", 1).expect("build runtime");
        let clone = rt.clone();
        drop(rt);
        // The original is gone; the clone must still be able to run work.
        assert_eq!(clone.block_on(async { 1 + 1 }), 2);
    }

    #[test]
    fn dropping_the_last_clone_from_inside_a_task_does_not_panic() {
        // The contract that lets a `LiveRuntime` be held from anywhere: a
        // task outlives every other reference, so the *final* drop lands on a
        // worker thread. A plain `Arc<Runtime>` panics there with "Cannot drop
        // a runtime in a context where blocking is not allowed", which is why
        // callers used to have to make do with a bare `Handle`.
        let rt = LiveRuntime::multi_thread("test-live-drop", 1).expect("build runtime");
        let (started_tx, started_rx) = std::sync::mpsc::channel();
        let (go_tx, go_rx) = std::sync::mpsc::channel::<()>();
        let (done_tx, done_rx) = std::sync::mpsc::channel();

        let moved = rt.clone();
        rt.spawn(async move {
            let inner = moved;
            started_tx.send(()).expect("test is listening");
            // Park the worker until the test has let go of its own clone, so
            // the drop below is the one that shuts the runtime down.
            go_rx.recv().expect("test signalled");
            drop(inner);
            // Unreachable if that drop panicked: the task unwinds, `done_tx`
            // drops, and the recv below fails the test.
            done_tx.send(()).expect("test is listening");
        });

        started_rx.recv().expect("task started");
        drop(rt);
        go_tx.send(()).expect("task is listening");
        done_rx
            .recv()
            .expect("dropping the last LiveRuntime inside a task must not panic");
    }
}
