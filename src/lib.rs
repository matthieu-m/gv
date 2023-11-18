//! An exploration of Generic Variadics.
//!
//! #   Goals
//!
//! This exploration sets out to understand:
//!
//! -   What is the best minimal set of built-in operations the compiler should provide.
//! -   What kind of ergonomic API could be provided to users.
//!
//!
//! #   Cons-Lists
//!
//! Since Variadics are NOT available yet, this repository will, instead, use a cons-list representation of tuples:
//!
//! -   The empty tuple will be represented as `()`.
//! -   A non-empty tuple will be represented as `(T, <tuple>)`.
//!
//! This representation is inefficient to work with, but will allow exploring the API to be.

#![cfg_attr(not(test), no_std)]
//  Laundry list of features...
#![feature(never_type)]

mod builtin;
