//! Proc macros for fresh-js's system-QuickJS backend.
//!
//! They accept what rquickjs's macros accept, for the parts Fresh uses, so the
//! same source builds on either backend:
//!
//! - `#[class]` on a struct: implements `fresh_js::class::JsClass` and strips
//!   `#[qjs(...)]` field attributes.
//! - `#[methods(rename_all = "camelCase")]` on an impl: exports every method
//!   not marked `#[qjs(skip)]`, under its camelCase name or
//!   `#[qjs(rename = "...")]`, through a generated method table that converts
//!   arguments with `FromParam` and results with `IntoJs` — rquickjs's rules.
//! - `#[derive(Trace)]` / `#[derive(JsLifetime)]`: the trait impls a class
//!   needs (an empty trace: Fresh's classes hold no JS values).

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::visit_mut::VisitMut;
use syn::{
    parse_macro_input, Attribute, Data, DeriveInput, Fields, FnArg, ImplItem, ItemImpl, ItemStruct,
    Lifetime, LitStr, Meta, Pat, ReturnType, Type,
};

fn is_qjs(attr: &Attribute) -> bool {
    attr.path().is_ident("qjs")
}

/// `#[class]`: implement `JsClass` and drop `#[qjs]` field attributes.
#[proc_macro_attribute]
pub fn class(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item = parse_macro_input!(input as ItemStruct);
    if let Fields::Named(fields) = &mut item.fields {
        for f in fields.named.iter_mut() {
            f.attrs.retain(|a| !is_qjs(a));
        }
    }
    let name = &item.ident;
    let js_name = name.to_string();
    quote! {
        #item
        impl ::fresh_js::class::JsClass for #name {
            const NAME: &'static str = #js_name;
        }
    }
    .into()
}

/// `#[derive(Trace)]`: an empty trace (accepts `#[qjs(...)]` helpers).
#[proc_macro_derive(Trace, attributes(qjs))]
pub fn derive_trace(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let _ = matches!(input.data, Data::Struct(_));
    quote! {
        impl<'js> ::fresh_js::class::Trace<'js> for #name {
            fn trace<'a>(&self, _tracer: ::fresh_js::class::Tracer<'a, 'js>) {}
        }
    }
    .into()
}

/// `#[derive(JsLifetime)]` for a type with no lifetime parameters.
#[proc_macro_derive(JsLifetime, attributes(qjs))]
pub fn derive_js_lifetime(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    quote! {
        unsafe impl<'js> ::fresh_js::JsLifetime<'js> for #name {
            type Changed<'to> = #name;
        }
    }
    .into()
}

/// What `#[qjs(...)]` says about one method.
#[derive(Default)]
struct MethodConfig {
    skip: bool,
    rename: Option<String>,
}

fn method_config(attrs: &[Attribute]) -> syn::Result<MethodConfig> {
    let mut cfg = MethodConfig::default();
    for attr in attrs.iter().filter(|a| is_qjs(a)) {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("skip") {
                cfg.skip = true;
                Ok(())
            } else if meta.path.is_ident("rename") {
                let s: LitStr = meta.value()?.parse()?;
                cfg.rename = Some(s.value());
                Ok(())
            } else {
                Err(meta.error("fresh-js: unsupported #[qjs] option on a method"))
            }
        })?;
    }
    Ok(cfg)
}

/// rquickjs's `rename_all = "camelCase"` for snake_case Rust names.
fn camel_case(name: &str) -> String {
    let mut out = String::new();
    for (i, word) in name.split('_').filter(|w| !w.is_empty()).enumerate() {
        let lower = word.to_lowercase();
        if i == 0 {
            out.push_str(&lower);
        } else {
            let mut chars = lower.chars();
            if let Some(c) = chars.next() {
                out.extend(c.to_uppercase());
                out.push_str(chars.as_str());
            }
        }
    }
    out
}

/// Rewrites every lifetime in a parameter type to `'js` (the wrapper's), so
/// `Ctx<'_>`, `Value<'js>` and a method's own `<'a>` all line up.
struct ToJsLifetime;

impl VisitMut for ToJsLifetime {
    fn visit_lifetime_mut(&mut self, lt: &mut Lifetime) {
        if lt.ident != "static" {
            *lt = Lifetime::new("'js", Span::call_site());
        }
    }
}

fn is_ctx(ty: &Type) -> bool {
    match ty {
        Type::Path(p) => p.path.segments.last().is_some_and(|s| s.ident == "Ctx"),
        _ => false,
    }
}

/// `#[methods]`: export the impl's methods through `JsMethods`.
#[proc_macro_attribute]
pub fn methods(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item = parse_macro_input!(input as ItemImpl);
    match expand_methods(args.into(), &mut item) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand_methods(args: TokenStream2, item: &mut ItemImpl) -> syn::Result<TokenStream2> {
    let mut camel = false;
    if !args.is_empty() {
        let meta: Meta = syn::parse2(args)?;
        if let Meta::NameValue(nv) = &meta {
            if nv.path.is_ident("rename_all") {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = &nv.value
                {
                    camel = s.value() == "camelCase";
                }
            }
        }
    }

    let self_ty = item.self_ty.clone();
    let mut wrappers = Vec::new();
    let mut defs = Vec::new();

    for impl_item in item.items.iter_mut() {
        let ImplItem::Fn(f) = impl_item else { continue };
        let cfg = method_config(&f.attrs)?;
        f.attrs.retain(|a| !is_qjs(a));
        if cfg.skip {
            continue;
        }
        let rust_name = f.sig.ident.clone();
        let js_name = cfg.rename.unwrap_or_else(|| {
            let n = rust_name.to_string();
            if camel {
                camel_case(&n)
            } else {
                n
            }
        });

        let mut has_self = false;
        let mut arg_types: Vec<Type> = Vec::new();
        for input in &f.sig.inputs {
            match input {
                FnArg::Receiver(r) => {
                    if r.reference.is_none() || r.mutability.is_some() {
                        return Err(syn::Error::new_spanned(
                            r,
                            "fresh-js: exported methods take &self",
                        ));
                    }
                    has_self = true;
                }
                FnArg::Typed(pt) => {
                    if !matches!(&*pt.pat, Pat::Ident(_) | Pat::Wild(_)) {
                        return Err(syn::Error::new_spanned(
                            &pt.pat,
                            "fresh-js: unsupported parameter pattern",
                        ));
                    }
                    let mut ty = (*pt.ty).clone();
                    ToJsLifetime.visit_type_mut(&mut ty);
                    arg_types.push(ty);
                }
            }
        }
        if !has_self {
            return Err(syn::Error::new_spanned(
                &f.sig,
                "fresh-js: exported methods take &self (mark static helpers #[qjs(skip)])",
            ));
        }

        let wrapper = format_ident!("__fresh_js_method_{}", rust_name);
        let args: Vec<_> = (0..arg_types.len())
            .map(|i| format_ident!("__arg{}", i))
            .collect();
        let length = arg_types.iter().filter(|t| !is_ctx(t)).count() as i32;
        let returns_unit = matches!(f.sig.output, ReturnType::Default);
        let call = quote! { this.#rust_name(#(#args),*) };
        let result = if returns_unit {
            quote! {{ #call; ::fresh_js::IntoJs::into_js((), params.ctx()) }}
        } else {
            quote! { ::fresh_js::IntoJs::into_js(#call, params.ctx()) }
        };

        wrappers.push(quote! {
            #[doc(hidden)]
            #[allow(non_snake_case, clippy::needless_lifetimes)]
            fn #wrapper<'a, 'js>(
                this: &#self_ty,
                params: &::fresh_js::__private::Params<'a, 'js>,
            ) -> ::fresh_js::Result<::fresh_js::Value<'js>> {
                let requirement = ::fresh_js::__private::ParamRequirement::none()
                    #(.combine(<#arg_types as ::fresh_js::__private::FromParam<'js>>::param_requirement()))*;
                params.check(requirement)?;
                let mut access = params.access();
                #(
                    let #args: #arg_types =
                        <#arg_types as ::fresh_js::__private::FromParam<'js>>::from_param(&mut access)?;
                )*
                #result
            }
        });
        defs.push(quote! {
            ::fresh_js::__private::MethodDef {
                name: #js_name,
                length: #length,
                call: <#self_ty>::#wrapper,
            }
        });
    }

    Ok(quote! {
        #item

        impl #self_ty {
            #(#wrappers)*
        }

        impl ::fresh_js::class::JsMethods for #self_ty {
            fn methods() -> &'static [::fresh_js::__private::MethodDef<Self>] {
                static METHODS: &[::fresh_js::__private::MethodDef<#self_ty>] = &[#(#defs),*];
                METHODS
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::camel_case;

    #[test]
    fn camel_case_matches_rquickjs_for_fresh_names() {
        assert_eq!(camel_case("api_version"), "apiVersion");
        assert_eq!(camel_case("utf8_byte_length"), "utf8ByteLength");
        assert_eq!(camel_case("get_active_buffer_id"), "getActiveBufferId");
        assert_eq!(camel_case("debug"), "debug");
    }
}
