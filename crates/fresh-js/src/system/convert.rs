//! Conversions between JS values and Rust types, and native-call parameters.
//!
//! The rules (which JS types each Rust type accepts, range checks, the error
//! raised) follow rquickjs's, because plugins and tests depend on them.

use super::value::Type;
use super::{ffi, Array, Ctx, Error, Object, Result, Value};
use std::collections::{BTreeMap, HashMap};
use std::hash::{BuildHasher, Hash};

/// Conversion from a JS value.
pub trait FromJs<'js>: Sized {
    fn from_js(ctx: &Ctx<'js>, value: Value<'js>) -> Result<Self>;
}

/// Conversion into a JS value.
pub trait IntoJs<'js> {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>>;
}

// ── FromJs ────────────────────────────────────────────────────────────────

impl<'js> FromJs<'js> for Value<'js> {
    fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        Ok(value)
    }
}

impl<'js> FromJs<'js> for () {
    fn from_js(_: &Ctx<'js>, _: Value<'js>) -> Result<Self> {
        Ok(())
    }
}

impl<'js> FromJs<'js> for String {
    fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        super::String::from_value(value)?.to_string()
    }
}

impl<'js> FromJs<'js> for bool {
    fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        value
            .as_bool()
            .ok_or_else(|| Error::new_from_js(value.type_name(), "bool"))
    }
}

macro_rules! from_js_number {
    ($($ty:ty),*) => {$(
        impl<'js> FromJs<'js> for $ty {
            fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
                match value.type_of() {
                    Type::Int => Ok(unsafe { value.get_int_unchecked() } as $ty),
                    Type::Float => Ok(unsafe { value.get_float_unchecked() } as $ty),
                    ty => Err(Error::new_from_js(ty.as_str(), stringify!($ty))),
                }
            }
        }
    )*};
}
from_js_number!(i32, f64);

impl<'js> FromJs<'js> for f32 {
    fn from_js(ctx: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        f64::from_js(ctx, value).map(|n| n as f32)
    }
}

fn number_match_range<T: PartialOrd>(
    val: T,
    min: T,
    max: T,
    from: &'static str,
    to: &'static str,
) -> Result<()> {
    if val < min {
        Err(Error::new_from_js_message(from, to, "Underflow"))
    } else if val > max {
        Err(Error::new_from_js_message(from, to, "Overflow"))
    } else {
        Ok(())
    }
}

macro_rules! from_js_ranged {
    ($base:ty: $($ty:ident)*) => {$(
        impl<'js> FromJs<'js> for $ty {
            fn from_js(ctx: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
                let num = <$base>::from_js(ctx, value)?;
                number_match_range(num, $ty::MIN as $base, $ty::MAX as $base, stringify!($base), stringify!($ty))?;
                Ok(num as $ty)
            }
        }
    )*};
}
from_js_ranged!(i32: i8 u8 i16 u16);
from_js_ranged!(f64: u32 u64 i64 usize isize);

impl<'js, T: FromJs<'js>> FromJs<'js> for Option<T> {
    fn from_js(ctx: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        if value.type_of().is_void() {
            Ok(None)
        } else {
            T::from_js(ctx, value).map(Some)
        }
    }
}

impl<'js, T: FromJs<'js>> FromJs<'js> for Box<T> {
    fn from_js(ctx: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        T::from_js(ctx, value).map(Box::new)
    }
}

impl<'js, T: FromJs<'js>> FromJs<'js> for Vec<T> {
    fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        let array = Array::from_value(value)?;
        array.iter().collect()
    }
}

impl<'js, V: FromJs<'js>, S: BuildHasher + Default> FromJs<'js> for HashMap<String, V, S> {
    fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        let object = Object::from_value(value)?;
        object.props::<String, V>().collect()
    }
}

impl<'js, V: FromJs<'js>> FromJs<'js> for BTreeMap<String, V> {
    fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        let object = Object::from_value(value)?;
        object.props::<String, V>().collect()
    }
}

macro_rules! from_js_tuple {
    ($($t:ident $i:tt),*) => {
        impl<'js, $($t: FromJs<'js>),*> FromJs<'js> for ($($t,)*) {
            fn from_js(_: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
                let array = Array::from_value(value)?;
                let expected = [$($i),*].len();
                let actual = array.len();
                if actual != expected {
                    return Err(Error::new_from_js_message(
                        "array",
                        "tuple",
                        if actual < expected { "Not enough values" } else { "Too many values" },
                    ));
                }
                Ok(($(array.get::<$t>($i)?,)*))
            }
        }
    };
}
from_js_tuple!(A 0);
from_js_tuple!(A 0, B 1);
from_js_tuple!(A 0, B 1, C 2);
from_js_tuple!(A 0, B 1, C 2, D 3);

// ── IntoJs ────────────────────────────────────────────────────────────────

impl<'js> IntoJs<'js> for Value<'js> {
    fn into_js(self, _: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(self)
    }
}

impl<'js> IntoJs<'js> for &Value<'js> {
    fn into_js(self, _: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(self.clone())
    }
}

impl<'js> IntoJs<'js> for () {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(Value::new_undefined(ctx.clone()))
    }
}

impl<'js> IntoJs<'js> for bool {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(Value::new_bool(ctx.clone(), self))
    }
}

impl<'js> IntoJs<'js> for &str {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        super::String::from_str(ctx.clone(), self).map(|s| s.0)
    }
}

impl<'js> IntoJs<'js> for String {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        self.as_str().into_js(ctx)
    }
}

impl<'js> IntoJs<'js> for &String {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        self.as_str().into_js(ctx)
    }
}

impl<'js> IntoJs<'js> for char {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        self.to_string().into_js(ctx)
    }
}

macro_rules! into_js_int {
    ($($ty:ty),*) => {$(
        impl<'js> IntoJs<'js> for $ty {
            fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
                Ok(match i32::try_from(self) {
                    Ok(n) => Value::new_int(ctx.clone(), n),
                    Err(_) => Value::new_float(ctx.clone(), self as f64),
                })
            }
        }
    )*};
}
into_js_int!(i8, u8, i16, u16, i32, u32, i64, u64, isize, usize);

impl<'js> IntoJs<'js> for f64 {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(Value::new_float(ctx.clone(), self))
    }
}

impl<'js> IntoJs<'js> for f32 {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(Value::new_float(ctx.clone(), self as f64))
    }
}

impl<'js, T: IntoJs<'js>> IntoJs<'js> for Option<T> {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        match self {
            Some(v) => v.into_js(ctx),
            None => Ok(Value::new_undefined(ctx.clone())),
        }
    }
}

impl<'js, T: IntoJs<'js>> IntoJs<'js> for Result<T> {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        self.and_then(|v| v.into_js(ctx))
    }
}

impl<'js, T: IntoJs<'js>> IntoJs<'js> for Vec<T> {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        let array = Array::new(ctx.clone())?;
        for (i, item) in self.into_iter().enumerate() {
            array.set(i, item)?;
        }
        Ok(array.0)
    }
}

impl<'js, T: IntoJs<'js> + Clone> IntoJs<'js> for &[T] {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        self.to_vec().into_js(ctx)
    }
}

impl<'js, K, V, S> IntoJs<'js> for HashMap<K, V, S>
where
    K: AsRef<str> + Eq + Hash,
    V: IntoJs<'js>,
{
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        let object = Object::new(ctx.clone())?;
        for (k, v) in self {
            object.set(k.as_ref(), v)?;
        }
        Ok(object.0)
    }
}

impl<'js, K: AsRef<str>, V: IntoJs<'js>> IntoJs<'js> for BTreeMap<K, V> {
    fn into_js(self, ctx: &Ctx<'js>) -> Result<Value<'js>> {
        let object = Object::new(ctx.clone())?;
        for (k, v) in self {
            object.set(k.as_ref(), v)?;
        }
        Ok(object.0)
    }
}

// ── Native-call parameters ────────────────────────────────────────────────

/// An optional trailing argument: `None` when the caller passed fewer
/// arguments.
pub struct Opt<T>(pub Option<T>);

/// All remaining arguments.
pub struct Rest<T>(pub Vec<T>);

impl<T> Opt<T> {
    pub fn into_inner(self) -> Option<T> {
        self.0
    }
}

impl<T> Rest<T> {
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }
}

impl<T> std::ops::Deref for Opt<T> {
    type Target = Option<T>;
    fn deref(&self) -> &Option<T> {
        &self.0
    }
}

impl<T> std::ops::DerefMut for Opt<T> {
    fn deref_mut(&mut self) -> &mut Option<T> {
        &mut self.0
    }
}

impl<T> std::ops::Deref for Rest<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Vec<T> {
        &self.0
    }
}

impl<T> std::ops::DerefMut for Rest<T> {
    fn deref_mut(&mut self) -> &mut Vec<T> {
        &mut self.0
    }
}

impl<T> From<Opt<T>> for Option<T> {
    fn from(o: Opt<T>) -> Self {
        o.0
    }
}

impl<T> From<Rest<T>> for Vec<T> {
    fn from(r: Rest<T>) -> Self {
        r.0
    }
}

/// How many arguments a parameter list needs.
#[derive(Debug, Clone, Copy)]
pub struct ParamRequirement {
    min: usize,
    max: usize,
    exhaustive: bool,
}

impl ParamRequirement {
    /// Consumes no argument (e.g. an injected `Ctx`).
    pub const fn none() -> Self {
        ParamRequirement {
            min: 0,
            max: 0,
            exhaustive: false,
        }
    }

    /// One required argument.
    pub const fn single() -> Self {
        ParamRequirement {
            min: 1,
            max: 1,
            exhaustive: false,
        }
    }

    /// One optional argument.
    pub const fn optional() -> Self {
        ParamRequirement {
            min: 0,
            max: 1,
            exhaustive: false,
        }
    }

    /// Any number of arguments.
    pub const fn any() -> Self {
        ParamRequirement {
            min: 0,
            max: usize::MAX,
            exhaustive: false,
        }
    }

    pub const fn combine(self, other: Self) -> Self {
        ParamRequirement {
            min: self.min.saturating_add(other.min),
            max: self.max.saturating_add(other.max),
            exhaustive: self.exhaustive || other.exhaustive,
        }
    }

    pub fn min(&self) -> usize {
        self.min
    }
}

/// The arguments of one native call.
pub struct Params<'a, 'js> {
    ctx: Ctx<'js>,
    this: ffi::JSValue,
    args: &'a [ffi::JSValue],
}

impl<'a, 'js> Params<'a, 'js> {
    /// # Safety
    /// `this` and `args` must be live values of `ctx` for the call's duration.
    pub(crate) unsafe fn new(ctx: Ctx<'js>, this: ffi::JSValue, args: &'a [ffi::JSValue]) -> Self {
        Params { ctx, this, args }
    }

    pub fn ctx(&self) -> &Ctx<'js> {
        &self.ctx
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    /// The `this` value.
    pub fn this(&self) -> Value<'js> {
        unsafe { Value::from_borrowed(self.ctx.clone(), self.this) }
    }

    pub(crate) fn this_raw(&self) -> ffi::JSValue {
        self.this
    }

    /// Check the argument count against `req`, as rquickjs does.
    pub fn check(&self, req: ParamRequirement) -> Result<()> {
        if self.args.len() < req.min {
            return Err(Error::MissingArgs {
                expected: req.min,
                given: self.args.len(),
            });
        }
        if req.exhaustive && self.args.len() > req.max {
            return Err(Error::TooManyArgs {
                expected: req.max,
                given: self.args.len(),
            });
        }
        Ok(())
    }

    pub fn access(&self) -> ParamsAccessor<'_, 'a, 'js> {
        ParamsAccessor {
            params: self,
            offset: 0,
        }
    }
}

/// Hands out a call's arguments in order.
pub struct ParamsAccessor<'p, 'a, 'js> {
    params: &'p Params<'a, 'js>,
    offset: usize,
}

impl<'js> ParamsAccessor<'_, '_, 'js> {
    pub fn ctx(&self) -> &Ctx<'js> {
        &self.params.ctx
    }

    /// Arguments not yet taken.
    pub fn len(&self) -> usize {
        self.params.args.len() - self.offset
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Take the next argument (`undefined` past the end).
    pub fn arg(&mut self) -> Value<'js> {
        let ctx = self.params.ctx.clone();
        match self.params.args.get(self.offset) {
            Some(raw) => {
                self.offset += 1;
                unsafe { Value::from_borrowed(ctx, *raw) }
            }
            None => Value::new_undefined(ctx),
        }
    }
}

/// A type that can appear as a native function parameter.
pub trait FromParam<'js>: Sized {
    fn param_requirement() -> ParamRequirement;
    fn from_param(params: &mut ParamsAccessor<'_, '_, 'js>) -> Result<Self>;
}

impl<'js, T: FromJs<'js>> FromParam<'js> for T {
    fn param_requirement() -> ParamRequirement {
        ParamRequirement::single()
    }

    fn from_param(params: &mut ParamsAccessor<'_, '_, 'js>) -> Result<Self> {
        let ctx = params.ctx().clone();
        T::from_js(&ctx, params.arg())
    }
}

impl<'js> FromParam<'js> for Ctx<'js> {
    fn param_requirement() -> ParamRequirement {
        ParamRequirement::none()
    }

    fn from_param(params: &mut ParamsAccessor<'_, '_, 'js>) -> Result<Self> {
        Ok(params.ctx().clone())
    }
}

impl<'js, T: FromJs<'js>> FromParam<'js> for Opt<T> {
    fn param_requirement() -> ParamRequirement {
        ParamRequirement::optional()
    }

    fn from_param(params: &mut ParamsAccessor<'_, '_, 'js>) -> Result<Self> {
        if params.is_empty() {
            Ok(Opt(None))
        } else {
            let ctx = params.ctx().clone();
            T::from_js(&ctx, params.arg()).map(|v| Opt(Some(v)))
        }
    }
}

impl<'js, T: FromJs<'js>> FromParam<'js> for Rest<T> {
    fn param_requirement() -> ParamRequirement {
        ParamRequirement::any()
    }

    fn from_param(params: &mut ParamsAccessor<'_, '_, 'js>) -> Result<Self> {
        let ctx = params.ctx().clone();
        let mut out = Vec::with_capacity(params.len());
        while !params.is_empty() {
            out.push(T::from_js(&ctx, params.arg())?);
        }
        Ok(Rest(out))
    }
}

/// Arguments for calling a JS function from Rust.
pub trait IntoArgs<'js> {
    fn into_args(self, ctx: &Ctx<'js>) -> Result<Vec<Value<'js>>>;
}

impl<'js> IntoArgs<'js> for () {
    fn into_args(self, _: &Ctx<'js>) -> Result<Vec<Value<'js>>> {
        Ok(Vec::new())
    }
}

impl<'js, T: IntoJs<'js>> IntoArgs<'js> for Rest<T> {
    fn into_args(self, ctx: &Ctx<'js>) -> Result<Vec<Value<'js>>> {
        self.0.into_iter().map(|v| v.into_js(ctx)).collect()
    }
}

macro_rules! into_args_tuple {
    ($($t:ident),*) => {
        impl<'js, $($t: IntoJs<'js>),*> IntoArgs<'js> for ($($t,)*) {
            #[allow(non_snake_case)]
            fn into_args(self, ctx: &Ctx<'js>) -> Result<Vec<Value<'js>>> {
                let ($($t,)*) = self;
                Ok(vec![$($t.into_js(ctx)?),*])
            }
        }
    };
}
into_args_tuple!(A);
into_args_tuple!(A, B);
into_args_tuple!(A, B, C);
into_args_tuple!(A, B, C, D);
into_args_tuple!(A, B, C, D, E);
into_args_tuple!(A, B, C, D, E, F);
