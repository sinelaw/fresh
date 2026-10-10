//! `Object<'js>` and `Array<'js>`.

use super::value::Type;
use super::{ffi, Ctx, Error, FromJs, IntoJs, Result, Value};
use std::fmt;
use std::marker::PhantomData;

const JS_PROP_THROW: i32 = 1 << 14;
const JS_GPN_STRING_MASK: i32 = 1 << 0;
const JS_GPN_ENUM_ONLY: i32 = 1 << 4;

/// A JS object (or anything that is one: arrays, functions, errors, …).
#[repr(transparent)]
#[derive(Clone, PartialEq)]
pub struct Object<'js>(pub(crate) Value<'js>);

/// A JS array.
#[repr(transparent)]
#[derive(Clone, PartialEq)]
pub struct Array<'js>(pub(crate) Value<'js>);

macro_rules! object_like {
    ($name:ident, $ty:ident, $to:literal) => {
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

        impl<'js> IntoJs<'js> for $name<'js> {
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

object_like!(Object, Object, "object");
object_like!(Array, Array, "array");

/// A property key: a name or an index.
pub trait IntoAtom<'js> {
    fn into_atom(self, ctx: &Ctx<'js>) -> Result<Atom<'js>>;
}

/// A property key read back as a Rust value.
pub trait FromAtom<'js>: Sized {
    fn from_atom(atom: Atom<'js>) -> Result<Self>;
}

/// An interned property key, freed on drop.
pub struct Atom<'js> {
    ctx: Ctx<'js>,
    atom: ffi::JSAtom,
}

impl Drop for Atom<'_> {
    fn drop(&mut self) {
        unsafe { ffi::JS_FreeAtom(self.ctx.as_ptr(), self.atom) }
    }
}

impl<'js> Atom<'js> {
    fn from_str(ctx: &Ctx<'js>, s: &str) -> Result<Self> {
        let atom =
            unsafe { ffi::JS_NewAtomLen(ctx.as_ptr(), s.as_ptr() as *const _, s.len() as _) };
        if atom == 0 {
            return Err(Error::Exception);
        }
        Ok(Atom {
            ctx: ctx.clone(),
            atom,
        })
    }

    fn from_u32(ctx: &Ctx<'js>, n: u32) -> Result<Self> {
        let atom = unsafe { ffi::JS_NewAtomUInt32(ctx.as_ptr(), n) };
        if atom == 0 {
            return Err(Error::Exception);
        }
        Ok(Atom {
            ctx: ctx.clone(),
            atom,
        })
    }

    pub fn to_string(&self) -> Result<std::string::String> {
        self.to_value()?.to_js_string()
    }

    pub fn to_value(&self) -> Result<Value<'js>> {
        let raw = unsafe { ffi::JS_AtomToString(self.ctx.as_ptr(), self.atom) };
        self.ctx.wrap(raw)
    }
}

impl<'js> IntoAtom<'js> for &str {
    fn into_atom(self, ctx: &Ctx<'js>) -> Result<Atom<'js>> {
        Atom::from_str(ctx, self)
    }
}

impl<'js> IntoAtom<'js> for &std::string::String {
    fn into_atom(self, ctx: &Ctx<'js>) -> Result<Atom<'js>> {
        Atom::from_str(ctx, self)
    }
}

impl<'js> IntoAtom<'js> for std::string::String {
    fn into_atom(self, ctx: &Ctx<'js>) -> Result<Atom<'js>> {
        Atom::from_str(ctx, &self)
    }
}

impl<'js> IntoAtom<'js> for u32 {
    fn into_atom(self, ctx: &Ctx<'js>) -> Result<Atom<'js>> {
        Atom::from_u32(ctx, self)
    }
}

impl<'js> IntoAtom<'js> for usize {
    fn into_atom(self, ctx: &Ctx<'js>) -> Result<Atom<'js>> {
        match u32::try_from(self) {
            Ok(n) => Atom::from_u32(ctx, n),
            Err(_) => Atom::from_str(ctx, &self.to_string()),
        }
    }
}

impl<'js> IntoAtom<'js> for i32 {
    fn into_atom(self, ctx: &Ctx<'js>) -> Result<Atom<'js>> {
        match u32::try_from(self) {
            Ok(n) => Atom::from_u32(ctx, n),
            Err(_) => Atom::from_str(ctx, &self.to_string()),
        }
    }
}

impl<'js> FromAtom<'js> for std::string::String {
    fn from_atom(atom: Atom<'js>) -> Result<Self> {
        atom.to_string()
    }
}

impl<'js> FromAtom<'js> for Value<'js> {
    fn from_atom(atom: Atom<'js>) -> Result<Self> {
        atom.to_value()
    }
}

impl<'js> Object<'js> {
    pub fn new(ctx: Ctx<'js>) -> Result<Self> {
        let raw = unsafe { ffi::JS_NewObject(ctx.as_ptr()) };
        ctx.wrap(raw).map(Object)
    }

    pub fn get<K: IntoAtom<'js>, V: FromJs<'js>>(&self, key: K) -> Result<V> {
        let ctx = &self.0.ctx;
        let atom = key.into_atom(ctx)?;
        let raw = unsafe {
            ffi::JS_GetPropertyInternal(ctx.as_ptr(), self.0.value, atom.atom, self.0.value, 0)
        };
        let value = ctx.wrap(raw)?;
        V::from_js(ctx, value)
    }

    pub fn set<K: IntoAtom<'js>, V: IntoJs<'js>>(&self, key: K, value: V) -> Result<()> {
        let ctx = &self.0.ctx;
        let atom = key.into_atom(ctx)?;
        let value = value.into_js(ctx)?;
        // JS_SetPropertyInternal consumes the value's reference.
        let r = unsafe {
            ffi::JS_SetPropertyInternal(
                ctx.as_ptr(),
                self.0.value,
                atom.atom,
                value.into_raw(),
                self.0.value,
                JS_PROP_THROW,
            )
        };
        ctx.check_status(r).map(drop)
    }

    pub fn contains_key<K: IntoAtom<'js>>(&self, key: K) -> Result<bool> {
        let ctx = &self.0.ctx;
        let atom = key.into_atom(ctx)?;
        let r = unsafe { ffi::JS_HasProperty(ctx.as_ptr(), self.0.value, atom.atom) };
        ctx.check_status(r).map(|r| r != 0)
    }

    pub fn remove<K: IntoAtom<'js>>(&self, key: K) -> Result<()> {
        let ctx = &self.0.ctx;
        let atom = key.into_atom(ctx)?;
        let r =
            unsafe { ffi::JS_DeleteProperty(ctx.as_ptr(), self.0.value, atom.atom, JS_PROP_THROW) };
        ctx.check_status(r).map(drop)
    }

    /// Own enumerable string keys, in property order.
    fn own_key_atoms(&self) -> Result<Vec<Atom<'js>>> {
        let ctx = &self.0.ctx;
        let mut tab: *mut ffi::JSPropertyEnum = std::ptr::null_mut();
        let mut len: u32 = 0;
        let r = unsafe {
            ffi::JS_GetOwnPropertyNames(
                ctx.as_ptr(),
                &mut tab,
                &mut len,
                self.0.value,
                JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY,
            )
        };
        ctx.check_status(r)?;
        let mut atoms = Vec::with_capacity(len as usize);
        for i in 0..len as usize {
            let atom = unsafe { (*tab.add(i)).atom };
            // Each entry owns its atom; `Atom` frees it.
            atoms.push(Atom {
                ctx: ctx.clone(),
                atom,
            });
        }
        unsafe { ffi::js_free(ctx.as_ptr(), tab as *mut _) };
        Ok(atoms)
    }

    pub fn len(&self) -> usize {
        self.own_key_atoms().map(|a| a.len()).unwrap_or(0)
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Own enumerable string keys.
    pub fn keys<K: FromAtom<'js>>(&self) -> ObjectKeysIter<'js, K> {
        ObjectKeysIter {
            atoms: self.own_key_atoms().map(|a| a.into_iter()),
            _marker: PhantomData,
        }
    }

    /// Own enumerable string-keyed properties.
    pub fn props<K: FromAtom<'js>, V: FromJs<'js>>(&self) -> ObjectIter<'js, K, V> {
        ObjectIter {
            object: self.clone(),
            atoms: self.own_key_atoms().map(|a| a.into_iter()),
            _marker: PhantomData,
        }
    }

    pub fn into_array(self) -> Option<Array<'js>> {
        self.0.into_array()
    }

    pub fn is_instance_of(&self, class: impl AsRef<Value<'js>>) -> bool {
        unsafe { ffi::JS_IsInstanceOf(self.0.ctx.as_ptr(), self.0.value, class.as_ref().value) > 0 }
    }
}

/// Iterator over an object's keys.
pub struct ObjectKeysIter<'js, K> {
    atoms: Result<std::vec::IntoIter<Atom<'js>>>,
    _marker: PhantomData<K>,
}

impl<'js, K: FromAtom<'js>> Iterator for ObjectKeysIter<'js, K> {
    type Item = Result<K>;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.atoms {
            Ok(it) => it.next().map(K::from_atom),
            Err(_) => match std::mem::replace(&mut self.atoms, Ok(Vec::new().into_iter())) {
                Err(e) => Some(Err(e)),
                Ok(_) => None,
            },
        }
    }
}

/// Iterator over an object's key/value pairs.
pub struct ObjectIter<'js, K, V> {
    object: Object<'js>,
    atoms: Result<std::vec::IntoIter<Atom<'js>>>,
    _marker: PhantomData<(K, V)>,
}

impl<'js, K: FromAtom<'js>, V: FromJs<'js>> Iterator for ObjectIter<'js, K, V> {
    type Item = Result<(K, V)>;
    fn next(&mut self) -> Option<Self::Item> {
        let atom = match &mut self.atoms {
            Ok(it) => it.next()?,
            Err(_) => {
                return match std::mem::replace(&mut self.atoms, Ok(Vec::new().into_iter())) {
                    Err(e) => Some(Err(e)),
                    Ok(_) => None,
                }
            }
        };
        let ctx = self.object.0.ctx.clone();
        let raw = unsafe {
            ffi::JS_GetPropertyInternal(
                ctx.as_ptr(),
                self.object.0.value,
                atom.atom,
                self.object.0.value,
                0,
            )
        };
        Some((|| {
            let value = ctx.wrap(raw)?;
            let v = V::from_js(&ctx, value)?;
            let k = K::from_atom(atom)?;
            Ok((k, v))
        })())
    }
}

impl<'js> Array<'js> {
    pub fn new(ctx: Ctx<'js>) -> Result<Self> {
        let raw = unsafe { ffi::JS_NewArray(ctx.as_ptr()) };
        ctx.wrap(raw).map(Array)
    }

    pub fn len(&self) -> usize {
        self.as_object()
            .get::<_, f64>("length")
            .map(|n| n as usize)
            .unwrap_or(0)
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get<V: FromJs<'js>>(&self, idx: usize) -> Result<V> {
        let ctx = &self.0.ctx;
        let raw = unsafe { ffi::JS_GetPropertyUint32(ctx.as_ptr(), self.0.value, idx as u32) };
        let value = ctx.wrap(raw)?;
        V::from_js(ctx, value)
    }

    pub fn set<V: IntoJs<'js>>(&self, idx: usize, value: V) -> Result<()> {
        let ctx = &self.0.ctx;
        let value = value.into_js(ctx)?;
        let r = unsafe {
            ffi::JS_SetPropertyUint32(ctx.as_ptr(), self.0.value, idx as u32, value.into_raw())
        };
        ctx.check_status(r).map(drop)
    }

    pub fn iter<T: FromJs<'js>>(&self) -> ArrayIter<'js, T> {
        ArrayIter {
            array: self.clone(),
            index: 0,
            count: self.len(),
            _marker: PhantomData,
        }
    }

    pub fn as_object(&self) -> &Object<'js> {
        unsafe { &*(self as *const Array<'js> as *const Object<'js>) }
    }

    pub fn into_object(self) -> Object<'js> {
        Object(self.0)
    }
}

/// Iterator over an array's elements.
pub struct ArrayIter<'js, T> {
    array: Array<'js>,
    index: usize,
    count: usize,
    _marker: PhantomData<T>,
}

impl<'js, T: FromJs<'js>> Iterator for ArrayIter<'js, T> {
    type Item = Result<T>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.count {
            return None;
        }
        let i = self.index;
        self.index += 1;
        Some(self.array.get(i))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.count - self.index;
        (n, Some(n))
    }
}
