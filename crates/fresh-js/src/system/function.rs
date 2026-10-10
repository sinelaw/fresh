//! `Function<'js>`: calling JS functions, and JS functions backed by Rust
//! closures.

use super::convert::{FromParam, IntoArgs, ParamRequirement, Params};
use super::value::Type;
use super::{ffi, Ctx, Error, FromJs, IntoJs, Object, Result, Value};
use std::any::Any;
use std::cell::RefCell;
use std::ffi::c_void;
use std::fmt;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::OnceLock;

/// A JS function.
#[repr(transparent)]
#[derive(Clone, PartialEq)]
pub struct Function<'js>(pub(crate) Value<'js>);

impl<'js> std::ops::Deref for Function<'js> {
    type Target = Value<'js>;
    fn deref(&self) -> &Value<'js> {
        &self.0
    }
}

impl<'js> AsRef<Value<'js>> for Function<'js> {
    fn as_ref(&self) -> &Value<'js> {
        &self.0
    }
}

impl fmt::Debug for Function<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'js> FromJs<'js> for Function<'js> {
    fn from_js(_ctx: &Ctx<'js>, value: Value<'js>) -> Result<Self> {
        Self::from_value(value)
    }
}

impl<'js> IntoJs<'js> for Function<'js> {
    fn into_js(self, _ctx: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(self.0)
    }
}

/// The type-erased Rust side of a native function.
pub(crate) type NativeFn<'js> = Box<dyn Fn(&Params<'_, 'js>) -> Result<Value<'js>> + 'js>;

/// A Rust closure usable as a JS function; implemented for closures whose
/// parameters are [`FromParam`] and whose result is [`IntoJs`].
pub trait IntoJsFunc<'js, P> {
    fn param_requirements() -> ParamRequirement;
    fn call(&self, params: &Params<'_, 'js>) -> Result<Value<'js>>;
}

macro_rules! into_js_func {
    ($($p:ident),*) => {
        impl<'js, Func, R, $($p),*> IntoJsFunc<'js, ($($p,)*)> for Func
        where
            Func: Fn($($p),*) -> R + 'js,
            R: IntoJs<'js>,
            $($p: FromParam<'js>,)*
        {
            fn param_requirements() -> ParamRequirement {
                ParamRequirement::none()$(.combine($p::param_requirement()))*
            }

            #[allow(non_snake_case, unused_mut, unused_variables)]
            fn call(&self, params: &Params<'_, 'js>) -> Result<Value<'js>> {
                params.check(Self::param_requirements())?;
                let mut access = params.access();
                $(let $p = $p::from_param(&mut access)?;)*
                (self)($($p),*).into_js(params.ctx())
            }
        }
    };
}
into_js_func!();
into_js_func!(A);
into_js_func!(A, B);
into_js_func!(A, B, C);
into_js_func!(A, B, C, D);
into_js_func!(A, B, C, D, E);
into_js_func!(A, B, C, D, E, F);

static CLOSURE_CLASS_ID: OnceLock<ffi::JSClassID> = OnceLock::new();

fn closure_class_id(rt: *mut ffi::JSRuntime) -> ffi::JSClassID {
    let id = *CLOSURE_CLASS_ID.get_or_init(|| {
        let mut id: ffi::JSClassID = 0;
        unsafe { ffi::fqjs_new_class_id(rt, &mut id) };
        id
    });
    unsafe {
        if ffi::JS_IsRegisteredClass(rt, id) == 0 {
            let def = ffi::JSClassDef {
                class_name: c"FreshNativeFunction".as_ptr(),
                finalizer: Some(closure_finalizer),
                ..std::mem::zeroed()
            };
            ffi::JS_NewClass(rt, id, &def);
        }
    }
    id
}

unsafe extern "C" fn closure_finalizer(_rt: *mut ffi::JSRuntime, val: ffi::JSValue) {
    if let Some(id) = CLOSURE_CLASS_ID.get() {
        let p = ffi::JS_GetOpaque(val, *id) as *mut NativeFn<'static>;
        if !p.is_null() {
            drop(Box::from_raw(p));
        }
    }
}

unsafe extern "C" fn closure_trampoline(
    ctx: *mut ffi::JSContext,
    this: ffi::JSValue,
    argc: i32,
    argv: *mut ffi::JSValue,
    _magic: i32,
    data: *mut ffi::JSValue,
) -> ffi::JSValue {
    let ctx = Ctx::from_borrowed(ctx);
    let id = *CLOSURE_CLASS_ID.get().expect("closure class registered");
    let f = &*(ffi::JS_GetOpaque(*data, id) as *const NativeFn<'_>);
    let args: &[ffi::JSValue] = if argc <= 0 || argv.is_null() {
        &[]
    } else {
        std::slice::from_raw_parts(argv, argc as usize)
    };
    let params = Params::new(ctx.clone(), this, args);
    invoke(&ctx, || f(&params))
}

thread_local! {
    /// A panic caught in a native call, resumed once control is back in Rust
    /// (as rquickjs does), so a panic is never unwound through QuickJS.
    static PENDING_PANIC: RefCell<Option<Box<dyn Any + Send>>> = const { RefCell::new(None) };
}

/// Resume a panic a native call raised, if any.
pub(crate) fn resume_pending_panic() {
    if let Some(panic) = PENDING_PANIC.with(|p| p.borrow_mut().take()) {
        std::panic::resume_unwind(panic);
    }
}

/// Run a callback from the engine, holding any panic for
/// [`resume_pending_panic`].
pub(crate) fn catch_panic(f: impl FnOnce()) {
    if let Err(panic) = catch_unwind(AssertUnwindSafe(f)) {
        PENDING_PANIC.with(|p| *p.borrow_mut() = Some(panic));
    }
}

/// Run a native call, turning its error into a JS exception. A panic is
/// stored and raised as an exception, then resumed in Rust by
/// [`resume_pending_panic`].
pub(crate) fn invoke<'js>(ctx: &Ctx<'js>, f: impl FnOnce() -> Result<Value<'js>>) -> ffi::JSValue {
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(Ok(v)) => v.into_raw(),
        Ok(Err(e)) => e.throw(ctx),
        Err(panic) => {
            PENDING_PANIC.with(|p| *p.borrow_mut() = Some(panic));
            Error::Unknown.throw(ctx)
        }
    }
}

impl<'js> Function<'js> {
    pub fn from_value(value: Value<'js>) -> Result<Self> {
        let ty = value.type_of();
        if ty.interpretable_as(Type::Function) {
            Ok(Function(value))
        } else {
            Err(Error::new_from_js(ty.as_str(), "function"))
        }
    }

    pub fn into_value(self) -> Value<'js> {
        self.0
    }

    pub fn as_object(&self) -> &Object<'js> {
        unsafe { &*(self as *const Function<'js> as *const Object<'js>) }
    }

    pub fn into_object(self) -> Object<'js> {
        Object(self.0)
    }

    /// A JS function backed by a Rust closure.
    pub fn new<P, F>(ctx: Ctx<'js>, f: F) -> Result<Self>
    where
        F: IntoJsFunc<'js, P> + 'js,
        P: 'js,
    {
        let length = F::param_requirements().min() as i32;
        let native: NativeFn<'js> = Box::new(move |params| f.call(params));
        Self::from_native(ctx, native, length)
    }

    pub(crate) fn from_native(ctx: Ctx<'js>, native: NativeFn<'js>, length: i32) -> Result<Self> {
        let class_id = closure_class_id(ctx.runtime_ptr());
        unsafe {
            let holder = ctx.wrap(ffi::JS_NewObjectClass(ctx.as_ptr(), class_id as i32))?;
            // The JS heap owns the closure from here; it is dropped by
            // `closure_finalizer`. Erasing 'js is what rquickjs does too: the
            // closure cannot outlive the runtime that holds it.
            let boxed: Box<NativeFn<'static>> = std::mem::transmute(Box::new(native));
            ffi::fqjs_set_opaque(holder.as_raw(), Box::into_raw(boxed) as *mut c_void);
            let mut data = [holder.as_raw()];
            let raw = ffi::JS_NewCFunctionData(
                ctx.as_ptr(),
                Some(closure_trampoline),
                length,
                0,
                1,
                data.as_mut_ptr(),
            );
            drop(holder);
            ctx.wrap(raw).map(Function)
        }
    }

    /// Call with `this` = `undefined`.
    pub fn call<A: IntoArgs<'js>, R: FromJs<'js>>(&self, args: A) -> Result<R> {
        let ctx = self.0.ctx.clone();
        let this = Value::new_undefined(ctx.clone());
        self.call_with_this(&this, args)
    }

    pub(crate) fn call_with_this<A: IntoArgs<'js>, R: FromJs<'js>>(
        &self,
        this: &Value<'js>,
        args: A,
    ) -> Result<R> {
        let ctx = self.0.ctx.clone();
        let args = args.into_args(&ctx)?;
        let mut raw: Vec<ffi::JSValue> = args.iter().map(|a| a.as_raw()).collect();
        let result = unsafe {
            ffi::JS_Call(
                ctx.as_ptr(),
                self.0.as_raw(),
                this.as_raw(),
                raw.len() as i32,
                raw.as_mut_ptr(),
            )
        };
        let value = ctx.wrap(result)?;
        R::from_js(&ctx, value)
    }

    pub fn set_name<S: AsRef<str>>(&self, name: S) -> Result<()> {
        self.define_configurable("name", name.as_ref())
    }

    pub fn with_name<S: AsRef<str>>(self, name: S) -> Result<Self> {
        self.set_name(name)?;
        Ok(self)
    }

    pub(crate) fn define_configurable(&self, key: &str, value: &str) -> Result<()> {
        let ctx = &self.0.ctx;
        let key = std::ffi::CString::new(key)?;
        let value = value.into_js(ctx)?;
        let r = unsafe {
            ffi::JS_DefinePropertyValueStr(
                ctx.as_ptr(),
                self.0.as_raw(),
                key.as_ptr(),
                value.into_raw(),
                ffi::JS_PROP_CONFIGURABLE as i32,
            )
        };
        ctx.check_status(r).map(drop)
    }
}
