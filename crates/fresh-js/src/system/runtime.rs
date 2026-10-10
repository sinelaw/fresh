//! `Runtime` and `Context`: the owning handles.

use super::{ffi, Ctx, Error, Result, Value};
use std::cell::RefCell;
use std::ffi::c_void;
use std::rc::Rc;

/// Evaluation options, as rquickjs's (strict global script by default, and
/// non-exhaustive, so code builds them from `Default` on either backend).
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct EvalOptions {
    pub global: bool,
    pub strict: bool,
    pub backtrace_barrier: bool,
    pub promise: bool,
    pub filename: Option<String>,
}

impl Default for EvalOptions {
    fn default() -> Self {
        EvalOptions {
            global: true,
            strict: true,
            backtrace_barrier: false,
            promise: false,
            filename: None,
        }
    }
}

impl EvalOptions {
    pub(crate) fn to_flag(&self) -> i32 {
        let mut flag = if self.global {
            ffi::JS_EVAL_TYPE_GLOBAL
        } else {
            ffi::JS_EVAL_TYPE_MODULE
        };
        if self.strict {
            flag |= ffi::JS_EVAL_FLAG_STRICT;
        }
        if self.backtrace_barrier {
            flag |= ffi::JS_EVAL_FLAG_BACKTRACE_BARRIER;
        }
        if self.promise {
            flag |= ffi::JS_EVAL_FLAG_ASYNC;
        }
        flag as i32
    }
}

/// Interrupt callback: return `true` to abort the running script.
pub type InterruptHandler = Box<dyn FnMut() -> bool + 'static>;

/// Called when a promise is rejected with no handler (`is_handled == false`),
/// or when a handler is attached later (`true`).
pub type RejectionTracker = Box<dyn for<'a> Fn(Ctx<'a>, Value<'a>, Value<'a>, bool) + 'static>;

pub(crate) struct RuntimeInner {
    pub(crate) rt: *mut ffi::JSRuntime,
    // Boxed so the engine's opaque pointers to them stay put.
    interrupt: Box<RefCell<Option<InterruptHandler>>>,
    rejection_tracker: Box<RefCell<Option<RejectionTracker>>>,
}

impl Drop for RuntimeInner {
    fn drop(&mut self) {
        unsafe { ffi::JS_FreeRuntime(self.rt) }
    }
}

unsafe extern "C" fn rejection_trampoline(
    ctx: *mut ffi::JSContext,
    promise: ffi::JSValue,
    reason: ffi::JSValue,
    is_handled: i32,
    opaque: *mut c_void,
) {
    let cell = &*(opaque as *const RefCell<Option<RejectionTracker>>);
    let Ok(tracker) = cell.try_borrow() else {
        return;
    };
    let Some(tracker) = tracker.as_ref() else {
        return;
    };
    let ctx = Ctx::from_borrowed(ctx);
    let promise = Value::from_borrowed(ctx.clone(), promise);
    let reason = Value::from_borrowed(ctx.clone(), reason);
    super::function::catch_panic(|| tracker(ctx, promise, reason, is_handled != 0));
}

unsafe extern "C" fn interrupt_trampoline(_rt: *mut ffi::JSRuntime, opaque: *mut c_void) -> i32 {
    let cell = &*(opaque as *const RefCell<Option<InterruptHandler>>);
    match cell.try_borrow_mut() {
        Ok(mut handler) => match handler.as_mut() {
            Some(h) => h() as i32,
            None => 0,
        },
        Err(_) => 0,
    }
}

/// A QuickJS runtime: the heap and job queue shared by its contexts.
#[derive(Clone)]
pub struct Runtime {
    pub(crate) inner: Rc<RuntimeInner>,
}

/// A pending job raised an exception; [`Ctx::catch`] on the context
/// retrieves it.
#[derive(Clone)]
pub struct JobException(pub Context);

impl std::fmt::Debug for JobException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("JobException(..)")
    }
}

impl std::fmt::Display for JobException {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Job raised an exception")
    }
}

impl Runtime {
    pub fn new() -> Result<Self> {
        let rt = unsafe { ffi::JS_NewRuntime() };
        if rt.is_null() {
            return Err(Error::Allocation);
        }
        let inner = Rc::new(RuntimeInner {
            rt,
            interrupt: Box::new(RefCell::new(None)),
            rejection_tracker: Box::new(RefCell::new(None)),
        });
        unsafe {
            ffi::JS_SetInterruptHandler(
                rt,
                Some(interrupt_trampoline),
                &*inner.interrupt as *const RefCell<Option<InterruptHandler>> as *mut c_void,
            )
        };
        Ok(Runtime { inner })
    }

    pub fn set_interrupt_handler(&self, handler: Option<InterruptHandler>) {
        *self.inner.interrupt.borrow_mut() = handler;
    }

    pub fn set_host_promise_rejection_tracker(&self, tracker: Option<RejectionTracker>) {
        let enable = tracker.is_some();
        *self.inner.rejection_tracker.borrow_mut() = tracker;
        unsafe {
            ffi::JS_SetHostPromiseRejectionTracker(
                self.inner.rt,
                if enable {
                    Some(rejection_trampoline)
                } else {
                    None
                },
                &*self.inner.rejection_tracker as *const RefCell<Option<RejectionTracker>>
                    as *mut c_void,
            )
        };
    }

    pub fn set_memory_limit(&self, limit: usize) {
        unsafe { ffi::JS_SetMemoryLimit(self.inner.rt, limit as _) }
    }

    pub fn set_max_stack_size(&self, limit: usize) {
        unsafe { ffi::JS_SetMaxStackSize(self.inner.rt, limit as _) }
    }

    pub fn set_gc_threshold(&self, threshold: usize) {
        unsafe { ffi::JS_SetGCThreshold(self.inner.rt, threshold as _) }
    }

    pub fn run_gc(&self) {
        unsafe { ffi::JS_RunGC(self.inner.rt) }
    }

    pub fn is_job_pending(&self) -> bool {
        unsafe { ffi::JS_IsJobPending(self.inner.rt) != 0 }
    }

    /// Run one pending job. `Ok(true)` if one ran, `Ok(false)` if none were
    /// queued, `Err` if it raised an exception.
    pub fn execute_pending_job(&self) -> std::result::Result<bool, JobException> {
        unsafe { ffi::JS_UpdateStackTop(self.inner.rt) };
        let mut job_ctx: *mut ffi::JSContext = std::ptr::null_mut();
        let r = unsafe { ffi::JS_ExecutePendingJob(self.inner.rt, &mut job_ctx) };
        match r {
            0 => Ok(false),
            r if r > 0 => Ok(true),
            _ => {
                let ctx = unsafe { Ctx::from_borrowed(job_ctx) };
                Err(JobException(Context {
                    ctx: ctx.as_ptr(),
                    owned: ctx,
                    rt: self.clone(),
                }))
            }
        }
    }
}

/// An owning handle to a JS realm (global object and intrinsics).
#[derive(Clone)]
pub struct Context {
    ctx: *mut ffi::JSContext,
    // Holds the context's reference; declared before `rt` so the context is
    // freed before the runtime that owns it.
    owned: Ctx<'static>,
    rt: Runtime,
}

impl Context {
    /// A context with all standard intrinsics.
    pub fn full(runtime: &Runtime) -> Result<Self> {
        let ctx = unsafe { ffi::JS_NewContext(runtime.inner.rt) };
        if ctx.is_null() {
            return Err(Error::Allocation);
        }
        // JS_NewContext hands us one reference; `from_borrowed` takes a second
        // for `owned`, so drop the first.
        let owned = unsafe { Ctx::from_borrowed(ctx) };
        unsafe { ffi::JS_FreeContext(ctx) };
        Ok(Context {
            ctx,
            owned,
            rt: runtime.clone(),
        })
    }

    /// The same as [`Context::full`]: Fresh only uses full contexts.
    pub fn base(runtime: &Runtime) -> Result<Self> {
        Self::full(runtime)
    }

    pub fn runtime(&self) -> &Runtime {
        &self.rt
    }

    /// Run `f` with a [`Ctx`] for this context.
    pub fn with<F, R>(&self, f: F) -> R
    where
        F: for<'js> FnOnce(Ctx<'js>) -> R,
    {
        let _ = &self.owned;
        unsafe { ffi::JS_UpdateStackTop(self.rt.inner.rt) };
        let ctx = unsafe { Ctx::from_borrowed(self.ctx) };
        f(ctx)
    }
}
