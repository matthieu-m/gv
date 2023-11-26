//! Abstractions over polymorphic functions and predicates.
//!
//! Placeholders for bounds of the form: `F: ...FnMut(T) -> R`.

/// A function over a generic argument.
#[const_trait]
pub trait PolymorphicFnStatic {
    /// Output type of `Self::call(...)` for a given argument `T`.
    type Output<T: ?Sized>;

    /// Result of calling the function with T.
    const RESULT<T>: Self::Output<T>;
}

/// A function over a generic argument.
#[const_trait]
pub trait PolymorphicFn {
    /// Output type of `self.call(...)` for a given argument `T`.
    type Output<T: ?Sized>;

    /// Calls `self` with `value`.
    fn call<T>(&self, value: T) -> Self::Output<T>;
}

/// A function over a generic argument.
#[const_trait]
pub trait PolymorphicFnMut {
    /// Output type of `self.call(...)` for a given argument `T`.
    type Output<T: ?Sized>;

    /// Calls `self` with `value`.
    fn call_mut<T>(&mut self, value: T) -> Self::Output<T>;
}

/// A function over a generic argument.
#[const_trait]
pub trait PolymorphicFnOnce {
    /// Output type of `self.call(...)` for a given argument `T`.
    type Output<T: ?Sized>;

    /// Calls `self` with `value`.
    fn call_once<T>(self, value: T) -> Self::Output<T>;
}

/// A function over a generic argument.
#[const_trait]
pub trait Polymorphic2Fn<A> {
    /// Output type of `self.call(...)` for a given argument `T`.
    type Output<T: ?Sized>;

    /// Calls `self` with `value`.
    fn call<T>(&self, argument: A, value: T) -> Self::Output<T>;
}

/// A function over a generic argument.
#[const_trait]
pub trait Polymorphic2FnMut<A> {
    /// Output type of `self.call(...)` for a given argument `T`.
    type Output<T: ?Sized>;

    /// Calls `self` with `value`.
    fn call_mut<T>(&mut self, argument: A, value: T) -> Self::Output<T>;
}

/// A function over a generic argument.
#[const_trait]
pub trait Polymorphic2FnOnce<A> {
    /// Output type of `self.call(...)` for a given argument `T`.
    type Output<T: ?Sized>;

    /// Calls `self` with `value`.
    fn call_once<T>(self, argument: A, value: T) -> Self::Output<T>;
}
