//! `Value<'js>` and its typed views.

use super::{ffi, Array, Ctx, Error, FromJs, Function, Object, Result};
use std::fmt;

/// One owned reference to a JS value.
pub struct Value<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) value: ffi::JSValue,
}

impl Clone for Value<'_> {
    fn clone(&self) -> Self {
        let raw = unsafe { ffi::fqjs_dup_value(self.ctx.as_ptr(), self.value) };
        Value {
            ctx: self.ctx.clone(),
            value: raw,
        }
    }
}

impl Drop for Value<'_> {
    fn drop(&mut self) {
        unsafe { ffi::fqjs_free_value(self.ctx.as_ptr(), self.value) }
    }
}

impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { ffi::JS_StrictEq(self.ctx.as_ptr(), self.value, other.value) != 0 }
    }
}

/// The JS type of a value, with rquickjs's names (used in conversion errors).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Type {
    Uninitialized,
    Undefined,
    Null,
    Bool,
    Int,
    Float,
    String,
    Symbol,
    Array,
    Constructor,
    Function,
    Promise,
    Exception,
    Proxy,
    Object,
    Module,
    BigInt,
    Unknown,
}

impl Type {
    /// `undefined`, `null` or uninitialized.
    pub const fn is_void(self) -> bool {
        matches!(self, Type::Uninitialized | Type::Undefined | Type::Null)
    }

    /// Whether a value of this type can be used as `other`.
    pub const fn interpretable_as(self, other: Self) -> bool {
        use Type::*;
        if self as u8 == other as u8 {
            return true;
        }
        match other {
            Float => matches!(self, Int),
            Object => matches!(
                self,
                Array | Function | Constructor | Exception | Promise | Proxy
            ),
            Function => matches!(self, Constructor),
            _ => false,
        }
    }

    pub const fn as_str(self) -> &'static str {
        match self {
            Type::Uninitialized => "uninitialized",
            Type::Undefined => "undefined",
            Type::Null => "null",
            Type::Bool => "bool",
            Type::Int => "int",
            Type::Float => "float",
            Type::String => "string",
            Type::Symbol => "symbol",
            Type::Array => "array",
            Type::Constructor => "constructor",
            Type::Function => "function",
            Type::Promise => "promise",
            Type::Exception => "exception",
            Type::Proxy => "proxy",
            Type::Object => "object",
            Type::Module => "module",
            Type::BigInt => "big_int",
            Type::Unknown => "Unknown type",
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

// JS_TAG_STRING_ROPE: Bellard's QuickJS (2025+) hands out rope strings for
// long concatenations. quickjs-ng has no such tag.
const TAG_STRING_ROPE: i32 = -6;

macro_rules! typed_view {
    ($(#[$meta:meta])* $name:ident, $ty:ident, $to:literal) => {
        $(#[$meta])*
        #[repr(transparent)]
        #[derive(Clone, PartialEq)]
        pub struct $name<'js>(pub(crate) Value<'js>);

        impl<'js> std::ops::Deref for $name<'js> {
            type Target = Value<'js>;
            fn deref(&self) -> &Value<'js> {
                &self.0
            }
        }

        impl<'js> AsRef<Value<'js>> for $name<'js> {
            fn as_ref(&self) -> &Value<'js> {
                &self.0
            }
        }

        impl<'js> $name<'js> {
            pub fn into_value(self) -> Value<'js> {
                self.0
            }

            pub fn as_value(&self) -> &Value<'js> {
                &self.0
            }

            /// Reinterpret a value as this type, if it is one.
            pub fn from_value(value: Value<'js>) -> Result<Self> {
                let ty = value.type_of();
                if ty.interpretable_as(Type::$ty) {
                    Ok($name(value))
                } else {
                    Err(Error::new_from_js(ty.as_str(), $to))
                }
            }
        }

        impl<'js> FromJs<'js> for $name<'js> {
            fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
                Self::from_value(value)
            }
        }

        impl<'js> super::IntoJs<'js> for $name<'js> {
            fn into_js(self, _ctx: &Ctx<'js>) -> Result<Value<'js>> {
                Ok(self.0)
            }
        }

        impl fmt::Debug for $name<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }
    };
}

typed_view!(
    /// A JS string.
    String,
    String,
    "string"
);
typed_view!(
    /// A JS `Error` object.
    Exception,
    Exception,
    "exception"
);
typed_view!(
    /// A JS BigInt.
    BigInt,
    BigInt,
    "big_int"
);

impl<'js> BigInt<'js> {
    // Takes `self` like rquickjs's.
    #[allow(clippy::wrong_self_convention)]
    pub fn to_i64(self) -> Result<i64> {
        let mut out: i64 = 0;
        let r = unsafe { ffi::JS_ToBigInt64(self.0.ctx.as_ptr(), &mut out, self.0.value) };
        self.0.ctx.check_status(r).map(|_| out)
    }
}

impl<'js> Value<'js> {
    /// Wrap a raw value this `Value` now owns.
    ///
    /// # Safety
    /// `value` must be a live value of `ctx`'s runtime carrying one reference
    /// that is handed over.
    pub(crate) unsafe fn from_raw(ctx: Ctx<'js>, value: ffi::JSValue) -> Self {
        Value { ctx, value }
    }

    /// Wrap a borrowed raw value, taking a new reference.
    ///
    /// # Safety
    /// `value` must be a live value of `ctx`'s runtime.
    pub(crate) unsafe fn from_borrowed(ctx: Ctx<'js>, value: ffi::JSValue) -> Self {
        let value = ffi::fqjs_dup_value(ctx.as_ptr(), value);
        Value { ctx, value }
    }

    pub(crate) fn as_raw(&self) -> ffi::JSValue {
        self.value
    }

    /// Give up ownership of the value's reference (the `Ctx` reference is
    /// still released).
    pub(crate) fn into_raw(self) -> ffi::JSValue {
        let this = std::mem::ManuallyDrop::new(self);
        let value = this.value;
        // Skip `Value::drop` (which would free `value`) but not the context
        // reference the `Ctx` field holds.
        drop(unsafe { std::ptr::read(&this.ctx) });
        value
    }

    pub fn ctx(&self) -> &Ctx<'js> {
        &self.ctx
    }

    fn tag(&self) -> i32 {
        unsafe { ffi::fqjs_tag(self.value) }
    }

    pub fn new_undefined(ctx: Ctx<'js>) -> Self {
        let raw = unsafe { ffi::fqjs_undefined() };
        unsafe { Value::from_raw(ctx, raw) }
    }

    pub fn new_null(ctx: Ctx<'js>) -> Self {
        let raw = unsafe { ffi::fqjs_null() };
        unsafe { Value::from_raw(ctx, raw) }
    }

    pub fn new_bool(ctx: Ctx<'js>, value: bool) -> Self {
        let raw = unsafe { ffi::fqjs_new_bool(ctx.as_ptr(), value as i32) };
        unsafe { Value::from_raw(ctx, raw) }
    }

    pub fn new_int(ctx: Ctx<'js>, value: i32) -> Self {
        let raw = unsafe { ffi::fqjs_new_int32(ctx.as_ptr(), value) };
        unsafe { Value::from_raw(ctx, raw) }
    }

    pub fn new_float(ctx: Ctx<'js>, value: f64) -> Self {
        let raw = unsafe { ffi::fqjs_new_float64(ctx.as_ptr(), value) };
        unsafe { Value::from_raw(ctx, raw) }
    }

    /// A number: an int when it is integral and fits, else a float.
    pub fn new_number(ctx: Ctx<'js>, value: f64) -> Self {
        let as_int = value as i32;
        if as_int as f64 == value && !(value == 0.0 && value.is_sign_negative()) {
            Self::new_int(ctx, as_int)
        } else {
            Self::new_float(ctx, value)
        }
    }

    /// The value's JS type, with rquickjs's classification of objects
    /// (array, constructor, function, promise, error, plain object).
    pub fn type_of(&self) -> Type {
        let tag = self.tag();
        let ctx = self.ctx.as_ptr();
        match tag {
            t if t == ffi::JS_TAG_UNINITIALIZED as i32 => Type::Uninitialized,
            t if t == ffi::JS_TAG_UNDEFINED as i32 => Type::Undefined,
            t if t == ffi::JS_TAG_NULL as i32 => Type::Null,
            t if t == ffi::JS_TAG_BOOL as i32 => Type::Bool,
            t if t == ffi::JS_TAG_INT as i32 => Type::Int,
            t if t == ffi::JS_TAG_FLOAT64 as i32 => Type::Float,
            t if t == ffi::JS_TAG_STRING as i32 || t == TAG_STRING_ROPE => Type::String,
            t if t == ffi::JS_TAG_SYMBOL as i32 => Type::Symbol,
            t if t == ffi::JS_TAG_BIG_INT as i32 || t == ffi::JS_TAG_SHORT_BIG_INT as i32 => {
                Type::BigInt
            }
            t if t == ffi::JS_TAG_MODULE as i32 => Type::Module,
            t if t == ffi::JS_TAG_OBJECT as i32 => unsafe {
                if ffi::fqjs_is_array(ctx, self.value) > 0 {
                    Type::Array
                } else if ffi::JS_IsConstructor(ctx, self.value) != 0 {
                    Type::Constructor
                } else if ffi::JS_IsFunction(ctx, self.value) != 0 {
                    Type::Function
                } else if (ffi::JS_PromiseState(ctx, self.value) as i32) >= 0 {
                    Type::Promise
                } else if ffi::JS_IsError(ctx, self.value) != 0 {
                    Type::Exception
                } else {
                    Type::Object
                }
            },
            _ => Type::Unknown,
        }
    }

    pub fn type_name(&self) -> &'static str {
        self.type_of().as_str()
    }

    pub fn is_undefined(&self) -> bool {
        self.tag() == ffi::JS_TAG_UNDEFINED as i32
    }

    pub fn is_null(&self) -> bool {
        self.tag() == ffi::JS_TAG_NULL as i32
    }

    pub fn is_bool(&self) -> bool {
        self.tag() == ffi::JS_TAG_BOOL as i32
    }

    pub fn is_int(&self) -> bool {
        self.tag() == ffi::JS_TAG_INT as i32
    }

    pub fn is_float(&self) -> bool {
        self.tag() == ffi::JS_TAG_FLOAT64 as i32
    }

    pub fn is_number(&self) -> bool {
        self.is_int() || self.is_float()
    }

    pub fn is_string(&self) -> bool {
        let t = self.tag();
        t == ffi::JS_TAG_STRING as i32 || t == TAG_STRING_ROPE
    }

    pub fn is_symbol(&self) -> bool {
        self.tag() == ffi::JS_TAG_SYMBOL as i32
    }

    pub fn is_object(&self) -> bool {
        self.tag() == ffi::JS_TAG_OBJECT as i32
    }

    pub fn is_array(&self) -> bool {
        self.is_object() && unsafe { ffi::fqjs_is_array(self.ctx.as_ptr(), self.value) > 0 }
    }

    pub fn is_function(&self) -> bool {
        self.is_object() && unsafe { ffi::JS_IsFunction(self.ctx.as_ptr(), self.value) != 0 }
    }

    pub fn is_constructor(&self) -> bool {
        self.is_object() && unsafe { ffi::JS_IsConstructor(self.ctx.as_ptr(), self.value) != 0 }
    }

    pub fn is_promise(&self) -> bool {
        self.type_of() == Type::Promise
    }

    /// Whether this is QuickJS's exception *marker* (`JS_TAG_EXCEPTION`), as
    /// rquickjs defines it — not whether it is an `Error` object. A value you
    /// can hold (e.g. from [`Ctx::catch`]) is never the marker; use
    /// [`Value::as_exception`] or `type_of() == Type::Exception` for errors.
    pub fn is_exception(&self) -> bool {
        self.tag() == ffi::JS_TAG_EXCEPTION as i32
    }

    pub fn is_error(&self) -> bool {
        self.is_object() && unsafe { ffi::JS_IsError(self.ctx.as_ptr(), self.value) != 0 }
    }

    pub fn as_bool(&self) -> Option<bool> {
        self.is_bool()
            .then(|| unsafe { ffi::fqjs_get_bool(self.value) != 0 })
    }

    pub fn as_int(&self) -> Option<i32> {
        self.is_int()
            .then(|| unsafe { ffi::fqjs_get_int(self.value) })
    }

    pub fn as_float(&self) -> Option<f64> {
        self.is_float()
            .then(|| unsafe { ffi::fqjs_get_float64(self.value) })
    }

    /// The value as an `f64`, for either number representation.
    pub fn as_number(&self) -> Option<f64> {
        self.as_int().map(|i| i as f64).or_else(|| self.as_float())
    }

    pub(crate) unsafe fn get_int_unchecked(&self) -> i32 {
        ffi::fqjs_get_int(self.value)
    }

    pub(crate) unsafe fn get_float_unchecked(&self) -> f64 {
        ffi::fqjs_get_float64(self.value)
    }

    pub fn as_string(&self) -> Option<&String<'js>> {
        self.is_string()
            .then(|| unsafe { &*(self as *const Value<'js> as *const String<'js>) })
    }

    pub fn as_object(&self) -> Option<&Object<'js>> {
        self.is_object()
            .then(|| unsafe { &*(self as *const Value<'js> as *const Object<'js>) })
    }

    pub fn as_array(&self) -> Option<&Array<'js>> {
        self.is_array()
            .then(|| unsafe { &*(self as *const Value<'js> as *const Array<'js>) })
    }

    pub fn as_function(&self) -> Option<&Function<'js>> {
        self.is_function()
            .then(|| unsafe { &*(self as *const Value<'js> as *const Function<'js>) })
    }

    pub fn as_exception(&self) -> Option<&Exception<'js>> {
        (self.type_of() == Type::Exception)
            .then(|| unsafe { &*(self as *const Value<'js> as *const Exception<'js>) })
    }

    pub fn as_big_int(&self) -> Option<&BigInt<'js>> {
        (self.type_of() == Type::BigInt)
            .then(|| unsafe { &*(self as *const Value<'js> as *const BigInt<'js>) })
    }

    pub fn from_object(object: Object<'js>) -> Self {
        object.0
    }

    pub fn into_string(self) -> Option<String<'js>> {
        self.is_string().then(|| String(self))
    }

    pub fn into_object(self) -> Option<Object<'js>> {
        self.is_object().then(|| Object(self))
    }

    pub fn into_array(self) -> Option<Array<'js>> {
        self.is_array().then(|| Array(self))
    }

    pub fn into_function(self) -> Option<Function<'js>> {
        self.is_function().then(|| Function(self))
    }

    pub fn into_exception(self) -> Option<Exception<'js>> {
        (self.type_of() == Type::Exception).then(|| Exception(self))
    }

    /// Convert into a Rust type.
    pub fn get<T: FromJs<'js>>(&self) -> Result<T> {
        T::from_js(&self.ctx, self.clone())
    }

    /// `String(value)`, as JS would convert it.
    pub(crate) fn to_js_string(&self) -> Result<std::string::String> {
        let mut len: usize = 0;
        let ptr = unsafe {
            ffi::fqjs_to_cstring_len(self.ctx.as_ptr(), &mut len as *mut usize as _, self.value)
        };
        if ptr.is_null() {
            return Err(Error::Exception);
        }
        let bytes = unsafe { std::slice::from_raw_parts(ptr as *const u8, len) };
        let s = match std::str::from_utf8(bytes) {
            Ok(s) => Ok(s.to_owned()),
            // Lone surrogates come out as WTF-8; replace them rather than fail.
            Err(_) => Ok(std::string::String::from_utf8_lossy(bytes).into_owned()),
        };
        unsafe { ffi::JS_FreeCString(self.ctx.as_ptr(), ptr) };
        s
    }
}

impl<'js> From<Object<'js>> for Value<'js> {
    fn from(object: Object<'js>) -> Self {
        object.0
    }
}

impl<'js> From<Array<'js>> for Value<'js> {
    fn from(array: Array<'js>) -> Self {
        array.0
    }
}

impl<'js> From<Function<'js>> for Value<'js> {
    fn from(function: Function<'js>) -> Self {
        function.0
    }
}

impl<'js> From<String<'js>> for Value<'js> {
    fn from(string: String<'js>) -> Self {
        string.0
    }
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty = self.type_of();
        match ty {
            Type::Uninitialized | Type::Undefined | Type::Null => write!(f, "{ty:?}"),
            Type::Bool | Type::Int | Type::Float | Type::String => match self.to_js_string() {
                Ok(s) => write!(f, "{ty:?}({s:?})"),
                Err(_) => write!(f, "{ty:?}"),
            },
            Type::Exception => {
                let e = Exception(self.clone());
                write!(
                    f,
                    "Exception({:?}, {:?})",
                    e.message().unwrap_or_default(),
                    e.stack().unwrap_or_default()
                )
            }
            _ => write!(f, "{ty:?}(..)"),
        }
    }
}

impl<'js> String<'js> {
    pub fn from_str(ctx: Ctx<'js>, s: &str) -> Result<Self> {
        let raw =
            unsafe { ffi::JS_NewStringLen(ctx.as_ptr(), s.as_ptr() as *const _, s.len() as _) };
        ctx.wrap(raw).map(String)
    }

    pub fn to_string(&self) -> Result<std::string::String> {
        self.0.to_js_string()
    }
}

impl<'js> Exception<'js> {
    fn prop(&self, key: &str) -> Option<std::string::String> {
        let obj = Object(self.0.clone());
        obj.get::<_, Option<std::string::String>>(key)
            .ok()
            .flatten()
    }

    pub fn message(&self) -> Option<std::string::String> {
        self.prop("message")
    }

    pub fn stack(&self) -> Option<std::string::String> {
        self.prop("stack")
    }

    pub fn into_object(self) -> Object<'js> {
        Object(self.0)
    }

    pub fn as_object(&self) -> &Object<'js> {
        unsafe { &*(self as *const Exception<'js> as *const Object<'js>) }
    }

    /// A new `Error` with `message`.
    pub fn from_message(ctx: Ctx<'js>, message: &str) -> Result<Self> {
        let raw = unsafe { ffi::JS_NewError(ctx.as_ptr()) };
        let obj = Object(ctx.wrap(raw)?);
        obj.set("message", message)?;
        Ok(Exception(obj.0))
    }

    /// Raise an `Error` with `message`.
    pub fn throw_message(ctx: &Ctx<'js>, message: &str) -> Error {
        match Self::from_message(ctx.clone(), message) {
            Ok(e) => ctx.throw(e.0),
            Err(e) => e,
        }
    }

    /// Raise a `TypeError` with `message`.
    pub fn throw_type(ctx: &Ctx<'js>, message: &str) -> Error {
        let msg = std::ffi::CString::new(message.replace('\0', "\\0")).unwrap_or_default();
        unsafe { ffi::JS_ThrowTypeError(ctx.as_ptr(), c"%s".as_ptr(), msg.as_ptr()) };
        Error::Exception
    }

    /// Raise this exception.
    pub fn throw(self) -> Error {
        let ctx = self.0.ctx.clone();
        ctx.throw(self.0)
    }
}
