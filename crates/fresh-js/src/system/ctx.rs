//! `Ctx<'js>`: a counted reference to a `JSContext`, valid inside one
//! `Context::with` call.

use super::runtime::EvalOptions;
use super::{ffi, Error, FromJs, Invariant, Object, Result, Value};
use std::ffi::CString;
use std::marker::PhantomData;
use std::ptr::NonNull;

pub struct Ctx<'js> {
    ctx: NonNull<ffi::JSContext>,
    _marker: Invariant<'js>,
}

impl Clone for Ctx<'_> {
    fn clone(&self) -> Self {
        unsafe { ffi::JS_DupContext(self.ctx.as_ptr()) };
        Ctx {
            ctx: self.ctx,
            _marker: PhantomData,
        }
    }
}

impl Drop for Ctx<'_> {
    fn drop(&mut self) {
        unsafe { ffi::JS_FreeContext(self.ctx.as_ptr()) }
    }
}

impl std::fmt::Debug for Ctx<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Ctx(..)")
    }
}

impl<'js> Ctx<'js> {
    /// Take a new reference to a live context.
    ///
    /// # Safety
    /// `ctx` must be a live `JSContext`.
    pub(crate) unsafe fn from_borrowed(ctx: *mut ffi::JSContext) -> Self {
        ffi::JS_DupContext(ctx);
        Ctx {
            ctx: NonNull::new_unchecked(ctx),
            _marker: PhantomData,
        }
    }

    pub(crate) fn as_ptr(&self) -> *mut ffi::JSContext {
        self.ctx.as_ptr()
    }

    pub(crate) fn runtime_ptr(&self) -> *mut ffi::JSRuntime {
        unsafe { ffi::JS_GetRuntime(self.as_ptr()) }
    }

    /// Turn an engine return value into a `Result`, leaving a raised
    /// exception pending for [`Ctx::catch`].
    pub(crate) fn handle_exception(&self, raw: ffi::JSValue) -> Result<ffi::JSValue> {
        if unsafe { ffi::fqjs_tag(raw) } == ffi::JS_TAG_EXCEPTION as i32 {
            super::function::resume_pending_panic();
            Err(Error::Exception)
        } else {
            Ok(raw)
        }
    }

    /// Wrap an owned raw value, or report the pending exception.
    pub(crate) fn wrap(&self, raw: ffi::JSValue) -> Result<Value<'js>> {
        self.handle_exception(raw)
            .map(|raw| unsafe { Value::from_raw(self.clone(), raw) })
    }

    /// Turn a `0`/`1`/`-1` engine status into a `Result`.
    pub(crate) fn check_status(&self, status: i32) -> Result<i32> {
        if status < 0 {
            Err(Error::Exception)
        } else {
            Ok(status)
        }
    }

    /// The global object.
    pub fn globals(&self) -> Object<'js> {
        let raw = unsafe { ffi::JS_GetGlobalObject(self.as_ptr()) };
        Object(unsafe { Value::from_raw(self.clone(), raw) })
    }

    /// Evaluate a script in global (strict) context.
    pub fn eval<V: FromJs<'js>, S: Into<Vec<u8>>>(&self, source: S) -> Result<V> {
        self.eval_with_options(source, EvalOptions::default())
    }

    /// Evaluate a script with the given options.
    pub fn eval_with_options<V: FromJs<'js>, S: Into<Vec<u8>>>(
        &self,
        source: S,
        options: EvalOptions,
    ) -> Result<V> {
        let file_name = CString::new(
            options
                .filename
                .clone()
                .unwrap_or_else(|| "eval_script".to_string()),
        )?;
        let mut src: Vec<u8> = source.into();
        let len = src.len();
        // JS_Eval requires input[len] == '\0'.
        src.push(0);
        let raw = unsafe {
            ffi::JS_Eval(
                self.as_ptr(),
                src.as_ptr() as *const _,
                len as _,
                file_name.as_ptr(),
                options.to_flag(),
            )
        };
        let value = self.wrap(raw)?;
        V::from_js(self, value)
    }

    /// Take the pending exception (or `undefined`/uninitialized when there is
    /// none).
    pub fn catch(&self) -> Value<'js> {
        let raw = unsafe { ffi::JS_GetException(self.as_ptr()) };
        unsafe { Value::from_raw(self.clone(), raw) }
    }

    /// Raise `value` as an exception.
    pub fn throw(&self, value: Value<'js>) -> Error {
        unsafe { ffi::JS_Throw(self.as_ptr(), value.into_raw()) };
        Error::Exception
    }

    /// Run one pending job; `true` if one ran (an exception it raised stays
    /// pending in its context).
    pub fn execute_pending_job(&self) -> bool {
        let mut job_ctx: *mut ffi::JSContext = std::ptr::null_mut();
        let r = unsafe { ffi::JS_ExecutePendingJob(self.runtime_ptr(), &mut job_ctx) };
        r != 0
    }

    /// Run the garbage collector.
    pub fn run_gc(&self) {
        unsafe { ffi::JS_RunGC(self.runtime_ptr()) }
    }

    /// Parse JSON text.
    pub fn json_parse<S: Into<Vec<u8>>>(&self, json: S) -> Result<Value<'js>> {
        let mut src: Vec<u8> = json.into();
        let len = src.len();
        src.push(0);
        let raw = unsafe {
            ffi::JS_ParseJSON(
                self.as_ptr(),
                src.as_ptr() as *const _,
                len as _,
                c"<input>".as_ptr(),
            )
        };
        self.wrap(raw)
    }

    /// `JSON.stringify(value)`; `None` where JSON has no representation.
    pub fn json_stringify(&self, value: &Value<'js>) -> Result<Option<std::string::String>> {
        let undefined = unsafe { ffi::fqjs_undefined() };
        let raw =
            unsafe { ffi::JS_JSONStringify(self.as_ptr(), value.as_raw(), undefined, undefined) };
        let s = self.wrap(raw)?;
        if s.is_undefined() {
            Ok(None)
        } else {
            s.to_js_string().map(Some)
        }
    }
}
