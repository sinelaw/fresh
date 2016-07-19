# Fresh, a programming language

** This document is a work in progress **

## Introduction

The Fresh programming language is designed for low-level, systems programming. It should be useful where C would be used. As such, it has the following properties:

* Very low runtime overhead
* Easy integration with existing C code
* Strong, static type system, with type inference

Which lead to the following choices:

* Manual memory management (with some static safety)
* Generate human-readable C code (to allow additional manual or automatic verification of compilation output)

## Type System

Fresh's type system includes:

* Type inference
* Generics (including rank-n types)
* Type classes
* Polymorphic sum and product types

# Type System Reference

## Fresh is based on HMF

The Fresh type system is based on HMF, with added type classes and row polymorphism (for both sum and product types). HMF was chosen as the basis for Fresh, for supporting rank-n types while being relatively easy to understand & simple to implement.

## Type Inference

HMF, and hence Fresh, can infer types completely, as long as they don't require rank-n polymorphism. Wherever rank-n polymorphism is used, an annotation is required. The simpleset way to ensure succesful inference is to place type annotations on all rank-n function arguments.

## Polymorphic Sum and Product Types ##

*Sum types* are like tagged unions: each value must be one of a several specific forms. For example, `Maybe a` is a sum type, and all values must be either `Nothing` or have the form `Just x` (where `x` has type `a`). `Nothing` and `Just` are called *data constructors*. The type `Maybe a` has two data constructors: `Nothing` and `Just`.

Some sum types have no names, and are described by their structure. For example, the type: `+{ FileNotFound | AccessDenied }` has exactly two constructors. Notice the `+{ .. }` notation: `+` stands for "sum type".

*Polymorphic sum types* are types where the set of data constructors is polymorphic. For example, the type `+{ FileNotFound | AccessDenied | a }` has *at least* two constructors, `FileNotFound` and `AccessDenied`, but is compatible with types that have more than just those two. This pattern is very useful for partial error handling:

```
errorHandler err =
    case err of
        FileNotFound -> createFile ...
        AccessDenied -> assertionFailed "Not supposed to happen!"
        x -> customHandler x -- The function 'customHandler' must accept all other constructors not handled here.
```

# Example
