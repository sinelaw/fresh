#include "shim.h"

JSValue fqjs_undefined(void) { return JS_UNDEFINED; }
JSValue fqjs_null(void) { return JS_NULL; }
JSValue fqjs_exception(void) { return JS_EXCEPTION; }
JSValue fqjs_new_bool(JSContext *ctx, int val) { return JS_NewBool(ctx, val); }
JSValue fqjs_new_int32(JSContext *ctx, int32_t val) { return JS_NewInt32(ctx, val); }
JSValue fqjs_new_int64(JSContext *ctx, int64_t val) { return JS_NewInt64(ctx, val); }
JSValue fqjs_new_float64(JSContext *ctx, double val) { return JS_NewFloat64(ctx, val); }

int fqjs_tag(JSValue v) { return JS_VALUE_GET_NORM_TAG(v); }
int32_t fqjs_get_int(JSValue v) { return JS_VALUE_GET_INT(v); }
int fqjs_get_bool(JSValue v) { return JS_VALUE_GET_BOOL(v); }
double fqjs_get_float64(JSValue v) { return JS_VALUE_GET_FLOAT64(v); }

void fqjs_free_value(JSContext *ctx, JSValue v) { JS_FreeValue(ctx, v); }
void fqjs_free_value_rt(JSRuntime *rt, JSValue v) { JS_FreeValueRT(rt, v); }
JSValue fqjs_dup_value(JSContext *ctx, JSValue v) { return JS_DupValue(ctx, v); }
JSValue fqjs_dup_value_rt(JSRuntime *rt, JSValue v) { return JS_DupValueRT(rt, v); }

const char *fqjs_to_cstring_len(JSContext *ctx, size_t *plen, JSValue v)
{
    return JS_ToCStringLen(ctx, plen, v);
}

/* quickjs-ng defines QJS_VERSION_MAJOR; Bellard's QuickJS does not. */
int fqjs_is_array(JSContext *ctx, JSValue v)
{
#ifdef QJS_VERSION_MAJOR
    (void)ctx;
    return JS_IsArray(v);
#else
    return JS_IsArray(ctx, v);
#endif
}

JSClassID fqjs_new_class_id(JSRuntime *rt, JSClassID *pclass_id)
{
#ifdef QJS_VERSION_MAJOR
    return JS_NewClassID(rt, pclass_id);
#else
    (void)rt;
    return JS_NewClassID(pclass_id);
#endif
}

void fqjs_set_opaque(JSValue obj, void *opaque) { JS_SetOpaque(obj, opaque); }
