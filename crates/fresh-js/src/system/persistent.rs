//! `Persistent<T>`: a JS value kept outside any `Context::with` call.
//!
//! Like rquickjs's, it holds a reference against the runtime rather than a
//! context, and must be dropped before its runtime is.

use super::{
    ffi, Array, Ctx, Error, Exception, Function, JsLifetime, Object, Result, String, Value,
};
use std::marker::PhantomData;

/// A value type that can be stored in a [`Persistent`].
pub trait JsHandle<'js>: Sized {
    fn into_raw_value(self) -> ffi::JSValue;
    /// # Safety
    /// `value` must be a live value of `ctx`'s runtime with one reference
    /// handed over.
    unsafe fn from_raw_value(ctx: Ctx<'js>, value: ffi::JSValue) -> Self;
}

impl<'js> JsHandle<'js> for Value<'js> {
    fn into_raw_value(self) -> ffi::JSValue {
        self.into_raw()
    }
    unsafe fn from_raw_value(ctx: Ctx<'js>, value: ffi::JSValue) -> Self {
        Value::from_raw(ctx, value)
    }
}

macro_rules! handle {
    ($($ty:ident),*) => {$(
        impl<'js> JsHandle<'js> for $ty<'js> {
            fn into_raw_value(self) -> ffi::JSValue {
                self.0.into_raw()
            }
            unsafe fn from_raw_value(ctx: Ctx<'js>, value: ffi::JSValue) -> Self {
                $ty(Value::from_raw(ctx, value))
            }
        }
    )*};
}
handle!(Object, Array, Function, String, Exception);

pub struct Persistent<T> {
    rt: *mut ffi::JSRuntime,
    value: ffi::JSValue,
    _marker: PhantomData<T>,
}

impl<T> Clone for Persistent<T> {
    fn clone(&self) -> Self {
        let value = unsafe { ffi::fqjs_dup_value_rt(self.rt, self.value) };
        Persistent {
            rt: self.rt,
            value,
            _marker: PhantomData,
        }
    }
}

impl<T> Drop for Persistent<T> {
    fn drop(&mut self) {
        unsafe { ffi::fqjs_free_value_rt(self.rt, self.value) }
    }
}

impl<T> std::fmt::Debug for Persistent<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Persistent(..)")
    }
}

impl<T> Persistent<T> {
    /// Keep `value` beyond the current `with` call.
    pub fn save<'js>(ctx: &Ctx<'js>, value: T) -> Persistent<T::Changed<'static>>
    where
        T: JsLifetime<'js> + JsHandle<'js>,
    {
        Persistent {
            rt: ctx.runtime_ptr(),
            value: value.into_raw_value(),
            _marker: PhantomData,
        }
    }

    /// Get the value back in a context of the same runtime.
    pub fn restore<'js>(self, ctx: &Ctx<'js>) -> Result<T::Changed<'js>>
    where
        T: JsLifetime<'static>,
        T::Changed<'js>: JsHandle<'js>,
    {
        if ctx.runtime_ptr() != self.rt {
            return Err(Error::UnrelatedRuntime);
        }
        let value = unsafe { ffi::fqjs_dup_value(ctx.as_ptr(), self.value) };
        Ok(unsafe { <T::Changed<'js> as JsHandle<'js>>::from_raw_value(ctx.clone(), value) })
    }
}
