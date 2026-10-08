//! A small safe wrapper over the system QuickJS (`fresh-quickjs-sys`).
//!
//! This is the spike described in `docs/internal/debian-quickjs-spike.md`: it
//! covers the shape of what the plugin runtime needs from rquickjs (a runtime,
//! contexts, values, native functions, JSON conversion and the pending-job
//! loop) on top of Debian's QuickJS, without reimplementing rquickjs's
//! lifetime-branded API.
//!
//! Ownership model: a [`Value`] owns one QuickJS reference and keeps its
//! [`Context`] alive, which keeps the [`Runtime`] alive, so nothing can be
//! freed out from under a value. Everything here is single-threaded (`!Send`),
//! like the plugin thread that would use it.
//!
//! Native functions must not capture [`Value`]s: the closure is owned by the JS
//! heap, so a captured value would form a reference cycle through the
//! context and leak the whole runtime. Capture plain Rust state instead and
//! reach JS through the [`Context`] passed to the call.

use fresh_quickjs_sys as sys;
use std::cell::Cell;
use std::ffi::{c_char, c_int, c_void, CString};
use std::fmt;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::rc::Rc;
use std::sync::OnceLock;
use std::time::Instant;

/// An error raised by JS, or by the wrapper itself.
#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    /// A JS exception, read out of the engine.
    Exception {
        name: String,
        message: String,
        stack: Option<String>,
    },
    /// The engine failed to allocate a runtime or context.
    Allocation(&'static str),
    /// A Rust string passed to JS contained an interior NUL byte.
    InteriorNul,
    /// Anything else, with a message (also what a native function throws).
    Other(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Exception { name, message, .. } if name.is_empty() => write!(f, "{message}"),
            Error::Exception { name, message, .. } => write!(f, "{name}: {message}"),
            Error::Allocation(what) => write!(f, "QuickJS could not allocate a {what}"),
            Error::InteriorNul => write!(f, "string contains an interior NUL byte"),
            Error::Other(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

/// JS value type, from the value's tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Undefined,
    Null,
    Bool,
    Int,
    Float,
    BigInt,
    String,
    Symbol,
    Object,
    Exception,
    Other(i32),
}

struct RuntimeInner {
    rt: *mut sys::JSRuntime,
    // Boxed so the interrupt handler's opaque pointer stays valid.
    deadline: Box<Cell<Option<Instant>>>,
}

impl Drop for RuntimeInner {
    fn drop(&mut self) {
        // QuickJS asserts here that every object has been freed, which makes a
        // leaked reference fail loudly instead of silently.
        unsafe { sys::JS_FreeRuntime(self.rt) }
    }
}

/// A QuickJS runtime: the heap and the job queue shared by its contexts.
#[derive(Clone)]
pub struct Runtime {
    inner: Rc<RuntimeInner>,
}

impl Runtime {
    pub fn new() -> Result<Self> {
        let rt = unsafe { sys::JS_NewRuntime() };
        if rt.is_null() {
            return Err(Error::Allocation("runtime"));
        }
        let inner = Rc::new(RuntimeInner {
            rt,
            deadline: Box::new(Cell::new(None)),
        });
        register_closure_class(rt)?;
        unsafe {
            sys::JS_SetInterruptHandler(
                rt,
                Some(interrupt_handler),
                &*inner.deadline as *const Cell<Option<Instant>> as *mut c_void,
            )
        };
        Ok(Runtime { inner })
    }

    /// Abort any JS still running at `deadline` with an uncatchable
    /// "interrupted" error (how a hung plugin would be stopped). `None` clears it.
    pub fn set_deadline(&self, deadline: Option<Instant>) {
        self.inner.deadline.set(deadline);
    }

    pub fn set_memory_limit(&self, bytes: usize) {
        unsafe { sys::JS_SetMemoryLimit(self.inner.rt, bytes as _) }
    }

    pub fn set_max_stack_size(&self, bytes: usize) {
        unsafe { sys::JS_SetMaxStackSize(self.inner.rt, bytes as _) }
    }

    pub fn is_job_pending(&self) -> bool {
        unsafe { sys::JS_IsJobPending(self.inner.rt) != 0 }
    }

    /// Run queued promise jobs until none are left. Returns how many ran, or
    /// the first exception a job threw (later jobs stay queued).
    pub fn execute_pending_jobs(&self) -> Result<usize> {
        let mut ran = 0;
        loop {
            let mut job_ctx: *mut sys::JSContext = std::ptr::null_mut();
            let r = unsafe { sys::JS_ExecutePendingJob(self.inner.rt, &mut job_ctx) };
            match r {
                0 => return Ok(ran),
                r if r < 0 => {
                    let ctx = unsafe { Context::from_raw(job_ctx) };
                    return Err(ctx.take_exception());
                }
                _ => ran += 1,
            }
        }
    }
}

unsafe extern "C" fn interrupt_handler(_rt: *mut sys::JSRuntime, opaque: *mut c_void) -> c_int {
    let deadline = &*(opaque as *const Cell<Option<Instant>>);
    matches!(deadline.get(), Some(d) if Instant::now() >= d) as c_int
}

struct ContextInner {
    ctx: *mut sys::JSContext,
    // Declared after `ctx` so the runtime outlives the context it owns.
    _rt: Rc<RuntimeInner>,
}

impl Drop for ContextInner {
    fn drop(&mut self) {
        unsafe { sys::JS_FreeContext(self.ctx) }
    }
}

/// A JS realm (global object and intrinsics) within a [`Runtime`].
#[derive(Clone)]
pub struct Context {
    inner: Rc<ContextInner>,
}

impl Context {
    pub fn new(runtime: &Runtime) -> Result<Self> {
        let ctx = unsafe { sys::JS_NewContext(runtime.inner.rt) };
        if ctx.is_null() {
            return Err(Error::Allocation("context"));
        }
        let inner = Rc::new(ContextInner {
            ctx,
            _rt: runtime.inner.clone(),
        });
        // Non-owning back-pointer, so native-function calls can recover the
        // `Context` from the raw `JSContext*` the engine hands them.
        unsafe { sys::JS_SetContextOpaque(ctx, Rc::as_ptr(&inner) as *mut c_void) };
        Ok(Context { inner })
    }

    /// Recover the `Context` that owns a live `JSContext*`.
    ///
    /// # Safety
    /// `ctx` must have been created by [`Context::new`] and still be alive.
    unsafe fn from_raw(ctx: *mut sys::JSContext) -> Context {
        let ptr = sys::JS_GetContextOpaque(ctx) as *const ContextInner;
        Rc::increment_strong_count(ptr);
        Context {
            inner: Rc::from_raw(ptr),
        }
    }

    fn raw(&self) -> *mut sys::JSContext {
        self.inner.ctx
    }

    /// Wrap a raw value this context now owns (one reference).
    fn own(&self, raw: sys::JSValue) -> Value {
        Value {
            ctx: self.clone(),
            raw,
        }
    }

    /// Wrap a raw value, taking a new reference to it.
    fn dup(&self, raw: sys::JSValue) -> Value {
        self.own(unsafe { sys::fqjs_dup_value(self.raw(), raw) })
    }

    /// Turn an engine return value into a `Result`, reading the pending
    /// exception if it signalled one.
    fn check(&self, raw: sys::JSValue) -> Result<Value> {
        if unsafe { sys::fqjs_tag(raw) } == sys::JS_TAG_EXCEPTION as c_int {
            Err(self.take_exception())
        } else {
            Ok(self.own(raw))
        }
    }

    fn take_exception(&self) -> Error {
        let exc = self.own(unsafe { sys::JS_GetException(self.raw()) });
        if exc.is_object() {
            let read = |key: &str| {
                exc.get(key)
                    .ok()
                    .filter(|v| !v.is_undefined())
                    .and_then(|v| v.to_string().ok())
            };
            Error::Exception {
                name: read("name").unwrap_or_default(),
                message: read("message").unwrap_or_default(),
                stack: read("stack"),
            }
        } else {
            Error::Exception {
                name: String::new(),
                message: exc.to_string().unwrap_or_default(),
                stack: None,
            }
        }
    }

    /// Throw `err` into JS and return the exception marker for a native
    /// function to return.
    fn throw(&self, err: Error) -> sys::JSValue {
        unsafe {
            let obj = sys::JS_NewError(self.raw());
            let (name, message) = match &err {
                Error::Exception { name, message, .. } => (name.clone(), message.clone()),
                other => (String::new(), other.to_string()),
            };
            let obj_v = self.dup(obj);
            let _ = obj_v.set("message", self.string(&message));
            if !name.is_empty() {
                let _ = obj_v.set("name", self.string(&name));
            }
            drop(obj_v);
            sys::JS_Throw(self.raw(), obj)
        }
    }

    /// Evaluate `source` as global (script) code.
    pub fn eval(&self, source: &str, filename: &str) -> Result<Value> {
        self.eval_flags(source, filename, sys::JS_EVAL_TYPE_GLOBAL)
    }

    /// Parse and compile `source` without running it.
    pub fn compile_check(&self, source: &str, filename: &str) -> Result<()> {
        self.eval_flags(
            source,
            filename,
            sys::JS_EVAL_TYPE_GLOBAL | sys::JS_EVAL_FLAG_COMPILE_ONLY,
        )
        .map(drop)
    }

    fn eval_flags(&self, source: &str, filename: &str, flags: u32) -> Result<Value> {
        // JS_Eval requires input[len] == '\0'.
        let src = CString::new(source).map_err(|_| Error::InteriorNul)?;
        let name = CString::new(filename).map_err(|_| Error::InteriorNul)?;
        let raw = unsafe {
            sys::JS_Eval(
                self.raw(),
                src.as_ptr(),
                source.len() as _,
                name.as_ptr(),
                flags as c_int,
            )
        };
        self.check(raw)
    }

    pub fn global(&self) -> Value {
        self.own(unsafe { sys::JS_GetGlobalObject(self.raw()) })
    }

    pub fn undefined(&self) -> Value {
        self.own(unsafe { sys::fqjs_undefined() })
    }

    pub fn null(&self) -> Value {
        self.own(unsafe { sys::fqjs_null() })
    }

    pub fn bool(&self, b: bool) -> Value {
        self.own(unsafe { sys::fqjs_new_bool(self.raw(), b as c_int) })
    }

    pub fn int(&self, n: i64) -> Value {
        self.own(unsafe { sys::fqjs_new_int64(self.raw(), n) })
    }

    pub fn float(&self, n: f64) -> Value {
        self.own(unsafe { sys::fqjs_new_float64(self.raw(), n) })
    }

    pub fn string(&self, s: &str) -> Value {
        // JS_NewStringLen takes a length, so interior NULs are fine here.
        let raw =
            unsafe { sys::JS_NewStringLen(self.raw(), s.as_ptr() as *const c_char, s.len() as _) };
        self.check(raw)
            .unwrap_or_else(|_| self.own(unsafe { sys::fqjs_undefined() }))
    }

    pub fn object(&self) -> Result<Value> {
        self.check(unsafe { sys::JS_NewObject(self.raw()) })
    }

    pub fn array(&self) -> Result<Value> {
        self.check(unsafe { sys::JS_NewArray(self.raw()) })
    }

    /// Parse JSON text into a JS value.
    pub fn parse_json(&self, text: &str) -> Result<Value> {
        let buf = CString::new(text).map_err(|_| Error::InteriorNul)?;
        let raw = unsafe {
            sys::JS_ParseJSON(
                self.raw(),
                buf.as_ptr(),
                text.len() as _,
                c"<json>".as_ptr(),
            )
        };
        self.check(raw)
    }

    /// Convert a `serde_json::Value` into a JS value (via JSON text, which is
    /// what a serde bridge would replace rquickjs-serde with).
    pub fn from_json(&self, value: &serde_json::Value) -> Result<Value> {
        self.parse_json(&value.to_string())
    }

    /// Create a JS function backed by a Rust closure.
    pub fn function<F>(&self, name: &str, length: u32, f: F) -> Result<Value>
    where
        F: Fn(&Context, &Value, &[Value]) -> Result<Value> + 'static,
    {
        let class_id = closure_class_id();
        unsafe {
            let holder = self.check(sys::JS_NewObjectClass(self.raw(), class_id as c_int))?;
            let boxed: Box<NativeFn> = Box::new(Box::new(f));
            sys::fqjs_set_opaque(holder.raw, Box::into_raw(boxed) as *mut c_void);
            let mut data = [holder.raw];
            let func = self.check(sys::JS_NewCFunctionData(
                self.raw(),
                Some(trampoline),
                length as c_int,
                0,
                1,
                data.as_mut_ptr(),
            ))?;
            // The function holds its own reference to `holder` now.
            drop(holder);
            // `name` is non-writable on functions, so define it rather than
            // assign it.
            if sys::JS_DefinePropertyValueStr(
                self.raw(),
                func.raw,
                c"name".as_ptr(),
                self.string(name).into_raw(),
                sys::JS_PROP_CONFIGURABLE as c_int,
            ) < 0
            {
                return Err(self.take_exception());
            }
            Ok(func)
        }
    }
}

type NativeFn = Box<dyn Fn(&Context, &Value, &[Value]) -> Result<Value>>;

static CLOSURE_CLASS_ID: OnceLock<u32> = OnceLock::new();

fn closure_class_id() -> u32 {
    *CLOSURE_CLASS_ID.get().expect("closure class registered")
}

fn register_closure_class(rt: *mut sys::JSRuntime) -> Result<()> {
    let id = *CLOSURE_CLASS_ID.get_or_init(|| {
        let mut id: sys::JSClassID = 0;
        unsafe { sys::fqjs_new_class_id(rt, &mut id) };
        id
    });
    let def = sys::JSClassDef {
        class_name: c"FreshNativeFunction".as_ptr(),
        finalizer: Some(closure_finalizer),
        // The rest (gc_mark, call, exotic) default to none.
        ..unsafe { std::mem::zeroed() }
    };
    if unsafe { sys::JS_NewClass(rt, id, &def) } != 0 {
        return Err(Error::Allocation("native function class"));
    }
    Ok(())
}

unsafe extern "C" fn closure_finalizer(_rt: *mut sys::JSRuntime, val: sys::JSValue) {
    let p = sys::JS_GetOpaque(val, closure_class_id()) as *mut NativeFn;
    if !p.is_null() {
        drop(Box::from_raw(p));
    }
}

unsafe extern "C" fn trampoline(
    ctx: *mut sys::JSContext,
    this: sys::JSValue,
    argc: c_int,
    argv: *mut sys::JSValue,
    _magic: c_int,
    data: *mut sys::JSValue,
) -> sys::JSValue {
    let context = Context::from_raw(ctx);
    let f = &*(sys::JS_GetOpaque(*data, closure_class_id()) as *const NativeFn);
    let this = context.dup(this);
    let args: Vec<Value> = (0..argc.max(0) as usize)
        .map(|i| context.dup(*argv.add(i)))
        .collect();
    match catch_unwind(AssertUnwindSafe(|| f(&context, &this, &args))) {
        Ok(Ok(v)) => v.into_raw(),
        Ok(Err(e)) => context.throw(e),
        Err(_) => context.throw(Error::Other("panic in native function".into())),
    }
}

/// One owned reference to a JS value.
pub struct Value {
    ctx: Context,
    raw: sys::JSValue,
}

impl Clone for Value {
    fn clone(&self) -> Self {
        self.ctx.dup(self.raw)
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        unsafe { sys::fqjs_free_value(self.ctx.raw(), self.raw) }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({:?})", self.type_of())
    }
}

impl Value {
    /// Give up ownership of the value's reference (for returning to the
    /// engine). The `Context` handle is still released: forgetting it would
    /// leak the context, and with it the runtime.
    fn into_raw(self) -> sys::JSValue {
        let this = std::mem::ManuallyDrop::new(self);
        let raw = this.raw;
        drop(unsafe { std::ptr::read(&this.ctx) });
        raw
    }

    pub fn context(&self) -> &Context {
        &self.ctx
    }

    pub fn type_of(&self) -> Type {
        let tag = unsafe { sys::fqjs_tag(self.raw) };
        match tag {
            t if t == sys::JS_TAG_UNDEFINED as c_int => Type::Undefined,
            t if t == sys::JS_TAG_NULL as c_int => Type::Null,
            t if t == sys::JS_TAG_BOOL as c_int => Type::Bool,
            t if t == sys::JS_TAG_INT as c_int => Type::Int,
            t if t == sys::JS_TAG_FLOAT64 as c_int => Type::Float,
            t if t == sys::JS_TAG_BIG_INT as c_int || t == sys::JS_TAG_SHORT_BIG_INT as c_int => {
                Type::BigInt
            }
            // Bellard's QuickJS can hand out rope strings (JS_TAG_STRING_ROPE);
            // anything string-like converts through JS_ToCStringLen alike.
            t if t == sys::JS_TAG_STRING as c_int || is_rope_tag(t) => Type::String,
            t if t == sys::JS_TAG_SYMBOL as c_int => Type::Symbol,
            t if t == sys::JS_TAG_OBJECT as c_int => Type::Object,
            t if t == sys::JS_TAG_EXCEPTION as c_int => Type::Exception,
            t => Type::Other(t),
        }
    }

    pub fn is_undefined(&self) -> bool {
        self.type_of() == Type::Undefined
    }

    pub fn is_null(&self) -> bool {
        self.type_of() == Type::Null
    }

    pub fn is_object(&self) -> bool {
        self.type_of() == Type::Object
    }

    pub fn is_string(&self) -> bool {
        self.type_of() == Type::String
    }

    pub fn is_function(&self) -> bool {
        unsafe { sys::JS_IsFunction(self.ctx.raw(), self.raw) != 0 }
    }

    pub fn is_array(&self) -> bool {
        unsafe { sys::fqjs_is_array(self.ctx.raw(), self.raw) > 0 }
    }

    pub fn as_bool(&self) -> Option<bool> {
        (self.type_of() == Type::Bool).then(|| unsafe { sys::fqjs_get_bool(self.raw) != 0 })
    }

    /// The value as an `f64`, for either number representation.
    pub fn as_f64(&self) -> Option<f64> {
        match self.type_of() {
            Type::Int => Some(unsafe { sys::fqjs_get_int(self.raw) } as f64),
            Type::Float => Some(unsafe { sys::fqjs_get_float64(self.raw) }),
            _ => None,
        }
    }

    /// `String(value)`, as JS would convert it.
    pub fn to_string(&self) -> Result<String> {
        let mut len: usize = 0;
        let ptr = unsafe {
            sys::fqjs_to_cstring_len(self.ctx.raw(), &mut len as *mut usize as _, self.raw)
        };
        if ptr.is_null() {
            return Err(self.ctx.take_exception());
        }
        let bytes = unsafe { std::slice::from_raw_parts(ptr as *const u8, len) };
        // QuickJS emits WTF-8 for lone surrogates; replace those like JSON would.
        let s = String::from_utf8_lossy(bytes).into_owned();
        unsafe { sys::JS_FreeCString(self.ctx.raw(), ptr) };
        Ok(s)
    }

    pub fn get(&self, key: &str) -> Result<Value> {
        let k = CString::new(key).map_err(|_| Error::InteriorNul)?;
        self.ctx
            .check(unsafe { sys::JS_GetPropertyStr(self.ctx.raw(), self.raw, k.as_ptr()) })
    }

    pub fn set(&self, key: &str, value: Value) -> Result<()> {
        let k = CString::new(key).map_err(|_| Error::InteriorNul)?;
        // JS_SetPropertyStr consumes the value's reference.
        let r = unsafe {
            sys::JS_SetPropertyStr(self.ctx.raw(), self.raw, k.as_ptr(), value.into_raw())
        };
        if r < 0 {
            Err(self.ctx.take_exception())
        } else {
            Ok(())
        }
    }

    pub fn get_index(&self, index: u32) -> Result<Value> {
        self.ctx
            .check(unsafe { sys::JS_GetPropertyUint32(self.ctx.raw(), self.raw, index) })
    }

    /// Call this value as a function.
    pub fn call(&self, this: &Value, args: &[Value]) -> Result<Value> {
        // JS_Call borrows its arguments.
        let mut raw_args: Vec<sys::JSValue> = args.iter().map(|a| a.raw).collect();
        self.ctx.check(unsafe {
            sys::JS_Call(
                self.ctx.raw(),
                self.raw,
                this.raw,
                raw_args.len() as c_int,
                raw_args.as_mut_ptr(),
            )
        })
    }

    /// `JSON.stringify(value)`; `None` where JSON has no representation
    /// (undefined, functions, symbols).
    pub fn to_json_string(&self) -> Result<Option<String>> {
        let undef = self.ctx.undefined();
        let s = self.ctx.check(unsafe {
            sys::JS_JSONStringify(self.ctx.raw(), self.raw, undef.raw, undef.raw)
        })?;
        if s.is_undefined() {
            Ok(None)
        } else {
            s.to_string().map(Some)
        }
    }

    /// Convert to a `serde_json::Value` through `JSON.stringify`.
    pub fn to_json(&self) -> Result<serde_json::Value> {
        match self.to_json_string()? {
            None => Ok(serde_json::Value::Null),
            Some(s) => serde_json::from_str(&s).map_err(|e| Error::Other(e.to_string())),
        }
    }
}

fn is_rope_tag(tag: c_int) -> bool {
    // JS_TAG_STRING_ROPE only exists in Bellard's QuickJS (2025+); it sits in
    // the gap at -6 that quickjs-ng leaves unused.
    tag == -6
}
