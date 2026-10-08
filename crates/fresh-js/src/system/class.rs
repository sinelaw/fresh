//! Rust structs exposed to JS as class instances (`#[class]` + `#[methods]`).
//!
//! The struct lives in a `Box` owned by the JS object (freed by the class
//! finalizer); its methods sit on the class prototype, one native function
//! each, and borrow the struct from `this`.

use super::convert::Params;
use super::function::invoke;
use super::{ffi, Ctx, Error, IntoJs, Object, Result, Value};
use std::any::TypeId;
use std::collections::HashMap;
use std::ffi::{c_void, CString};
use std::marker::PhantomData;
use std::sync::Mutex;

/// A Rust type exposed as a JS class (implemented by `#[class]`).
pub trait JsClass: Sized + 'static {
    const NAME: &'static str;
}

/// A native method of a class.
pub type MethodFn<C> = for<'a, 'js> fn(&C, &Params<'a, 'js>) -> Result<Value<'js>>;

/// One entry of a class's method table.
pub struct MethodDef<C> {
    pub name: &'static str,
    pub length: i32,
    pub call: MethodFn<C>,
}

/// A class's methods (implemented by `#[methods]`).
pub trait JsMethods: JsClass {
    fn methods() -> &'static [MethodDef<Self>];
}

/// Garbage-collector tracing. Fresh's classes hold no JS values the
/// collector must see, so the derive generates an empty implementation.
pub trait Trace<'js> {
    fn trace<'a>(&self, tracer: Tracer<'a, 'js>);
}

/// Handed to [`Trace::trace`].
pub struct Tracer<'a, 'js>(PhantomData<(&'a (), super::Invariant<'js>)>);

/// Rebinds a type's `'js` lifetime (for [`super::Persistent`]).
///
/// # Safety
/// `Changed<'to>` must be the same type with `'js` replaced by `'to`.
pub unsafe trait JsLifetime<'js> {
    type Changed<'to>: 'to;
}

macro_rules! static_lifetime {
    ($($ty:ty),*) => {$(
        unsafe impl<'js> JsLifetime<'js> for $ty {
            type Changed<'to> = $ty;
        }
    )*};
}
static_lifetime!(
    (),
    bool,
    u8,
    u16,
    u32,
    u64,
    usize,
    i8,
    i16,
    i32,
    i64,
    isize,
    f32,
    f64,
    std::string::String
);

macro_rules! value_lifetime {
    ($($ty:ident),*) => {$(
        unsafe impl<'js> JsLifetime<'js> for super::$ty<'js> {
            type Changed<'to> = super::$ty<'to>;
        }
    )*};
}
value_lifetime!(Value, Object, Array, Function, String, Exception);

static CLASS_IDS: Mutex<Option<HashMap<TypeId, ffi::JSClassID>>> = Mutex::new(None);

fn class_id<C: JsClass>(rt: *mut ffi::JSRuntime) -> ffi::JSClassID {
    let id = {
        let mut ids = CLASS_IDS.lock().unwrap_or_else(|e| e.into_inner());
        *ids.get_or_insert_with(HashMap::new)
            .entry(TypeId::of::<C>())
            .or_insert_with(|| {
                let mut id: ffi::JSClassID = 0;
                unsafe { ffi::fqjs_new_class_id(rt, &mut id) };
                id
            })
    };
    unsafe {
        if ffi::JS_IsRegisteredClass(rt, id) == 0 {
            let name = CString::new(C::NAME).unwrap_or_default();
            // QuickJS keeps the class name as an atom, so a temporary is fine.
            let def = ffi::JSClassDef {
                class_name: name.as_ptr(),
                finalizer: Some(finalizer::<C>),
                ..std::mem::zeroed()
            };
            ffi::JS_NewClass(rt, id, &def);
        }
    }
    id
}

fn lookup_class_id<C: JsClass>() -> Option<ffi::JSClassID> {
    let ids = CLASS_IDS.lock().unwrap_or_else(|e| e.into_inner());
    ids.as_ref()
        .and_then(|m| m.get(&TypeId::of::<C>()).copied())
}

unsafe extern "C" fn finalizer<C: JsClass>(_rt: *mut ffi::JSRuntime, val: ffi::JSValue) {
    if let Some(id) = lookup_class_id::<C>() {
        let p = ffi::JS_GetOpaque(val, id) as *mut C;
        if !p.is_null() {
            drop(Box::from_raw(p));
        }
    }
}

unsafe extern "C" fn method_trampoline<C: JsMethods>(
    ctx: *mut ffi::JSContext,
    this: ffi::JSValue,
    argc: i32,
    argv: *mut ffi::JSValue,
    magic: i32,
    _data: *mut ffi::JSValue,
) -> ffi::JSValue {
    let ctx = Ctx::from_borrowed(ctx);
    let args: &[ffi::JSValue] = if argc <= 0 || argv.is_null() {
        &[]
    } else {
        std::slice::from_raw_parts(argv, argc as usize)
    };
    let params = Params::new(ctx.clone(), this, args);
    invoke(&ctx, || {
        let def = &C::methods()[magic as usize];
        let id = lookup_class_id::<C>().ok_or(Error::Unknown)?;
        let p = ffi::JS_GetOpaque(params.this_raw(), id) as *const C;
        if p.is_null() {
            return Err(Error::new_from_js(params.this().type_name(), C::NAME));
        }
        (def.call)(&*p, &params)
    })
}

/// The class prototype for `C` in `ctx`, created (with its methods) on first
/// use. QuickJS keeps one per context and frees it with the context.
fn ensure_prototype<C: JsMethods>(ctx: &Ctx<'_>, id: ffi::JSClassID) -> Result<()> {
    unsafe {
        let existing = Value::from_raw(ctx.clone(), ffi::JS_GetClassProto(ctx.as_ptr(), id));
        if existing.is_object() {
            return Ok(());
        }
    }
    let proto = Object::new(ctx.clone())?;
    for (i, def) in C::methods().iter().enumerate() {
        let name = CString::new(def.name)?;
        let raw = unsafe {
            ffi::JS_NewCFunctionData(
                ctx.as_ptr(),
                Some(method_trampoline::<C>),
                def.length,
                i as i32,
                0,
                std::ptr::null_mut(),
            )
        };
        let func = super::Function(ctx.wrap(raw)?);
        func.set_name(def.name)?;
        let r = unsafe {
            ffi::JS_DefinePropertyValueStr(
                ctx.as_ptr(),
                proto.as_raw(),
                name.as_ptr(),
                func.0.into_raw(),
                (ffi::JS_PROP_WRITABLE | ffi::JS_PROP_CONFIGURABLE) as i32,
            )
        };
        ctx.check_status(r)?;
    }
    // JS_SetClassProto takes the reference.
    unsafe { ffi::JS_SetClassProto(ctx.as_ptr(), id, proto.0.into_raw()) };
    Ok(())
}

/// A JS object that owns a `C`.
pub struct Class<'js, C>(Object<'js>, PhantomData<C>);

impl<'js, C> Clone for Class<'js, C> {
    fn clone(&self) -> Self {
        Class(self.0.clone(), PhantomData)
    }
}

impl<'js, C: JsMethods> Class<'js, C> {
    /// Create an instance owning `value`.
    pub fn instance(ctx: Ctx<'js>, value: C) -> Result<Self> {
        let id = class_id::<C>(ctx.runtime_ptr());
        ensure_prototype::<C>(&ctx, id)?;
        let raw = unsafe { ffi::JS_NewObjectClass(ctx.as_ptr(), id as i32) };
        let obj = Object(ctx.wrap(raw)?);
        unsafe {
            ffi::fqjs_set_opaque(obj.as_raw(), Box::into_raw(Box::new(value)) as *mut c_void)
        };
        Ok(Class(obj, PhantomData))
    }

    pub fn into_value(self) -> Value<'js> {
        self.0 .0
    }

    pub fn as_object(&self) -> &Object<'js> {
        &self.0
    }
}

impl<'js, C> IntoJs<'js> for Class<'js, C> {
    fn into_js(self, _ctx: &Ctx<'js>) -> Result<Value<'js>> {
        Ok(self.0 .0)
    }
}
