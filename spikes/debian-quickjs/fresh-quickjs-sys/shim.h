/* Real (linkable) functions for the parts of the QuickJS API that quickjs.h
 * only provides as static inline functions or macros.
 *
 * Rust never encodes the JSValue layout itself: every tag read, value
 * construction and refcount operation goes through these, compiled against the
 * same header the library was built with. That keeps the bindings correct on
 * the 32-bit targets, where QuickJS NaN-boxes JSValue into a uint64_t.
 *
 * The few calls whose signature differs between Bellard's QuickJS and the
 * quickjs-ng fork (JS_NewClassID, JS_SetOpaque, JS_IsArray) are wrapped here
 * too, so the Rust side has one signature for either. */
#ifndef FRESH_QUICKJS_SHIM_H
#define FRESH_QUICKJS_SHIM_H

#include <stddef.h>
#include <stdint.h>
#include <quickjs.h>

JSValue fqjs_undefined(void);
JSValue fqjs_null(void);
JSValue fqjs_exception(void);
JSValue fqjs_new_bool(JSContext *ctx, int val);
JSValue fqjs_new_int32(JSContext *ctx, int32_t val);
JSValue fqjs_new_int64(JSContext *ctx, int64_t val);
JSValue fqjs_new_float64(JSContext *ctx, double val);

int fqjs_tag(JSValue v);
int32_t fqjs_get_int(JSValue v);
int fqjs_get_bool(JSValue v);
double fqjs_get_float64(JSValue v);

void fqjs_free_value(JSContext *ctx, JSValue v);
void fqjs_free_value_rt(JSRuntime *rt, JSValue v);
JSValue fqjs_dup_value(JSContext *ctx, JSValue v);

const char *fqjs_to_cstring_len(JSContext *ctx, size_t *plen, JSValue v);
int fqjs_is_array(JSContext *ctx, JSValue v);

JSClassID fqjs_new_class_id(JSRuntime *rt, JSClassID *pclass_id);
void fqjs_set_opaque(JSValue obj, void *opaque);

#endif
