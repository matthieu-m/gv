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
//
//  Laundry list of features...
//
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![feature(never_type)]
#![feature(non_lifetime_binders)]
#![feature(specialization)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
//
//  Lints
//
#![allow(incomplete_features)]

pub mod builtin;
pub mod peano;
pub mod polymorphic;
pub mod tuple_iterator;
