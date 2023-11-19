//! Abstractions over generic functions and predicates.

/// A function evaluating to `R` for any value.
#[const_trait]
pub trait GenericFn<R> {
    /// Calls the function with the given value, returning `R`.
    fn call<T>(&self, value: T) -> R;
}

/// A function evaluating to `R` for any value.
#[const_trait]
pub trait GenericFnMut<R> {
    /// Calls the function with the given value, returning `R`.
    fn call_mut<T>(&mut self, value: T) -> R;
}

/// A function evaluating to `R` for any value.
#[const_trait]
pub trait GenericFnOnce<R> {
    /// Calls the function with the given value, returning `R`.
    fn call_once<T>(self, value: T) -> R;
}

/// A function evaluating to `R` for any value.
#[const_trait]
pub trait Generic2Fn<S, R> {
    /// Calls the function with the given values, returning `R`.
    fn call<T>(&self, supp: S, value: T) -> R;
}

/// A function evaluating to `R` for any value.
#[const_trait]
pub trait Generic2FnMut<S, R> {
    /// Calls the function with the given value, returning `R`.
    fn call_mut<T>(&mut self, supp: S, value: T) -> R;
}

/// A function evaluating to `R` for any value.
#[const_trait]
pub trait Generic2FnOnce<S, R> {
    /// Calls the function with the given value, returning `R`.
    fn call_once<T>(self, supp: S, value: T) -> R;
}

/// A flexible function whose resulting type depends on the given input.
#[const_trait]
pub trait FlexibleFn {
    /// The result of applying this function to `T`.
    type Output<T>;

    /// Calls the function with the given value, returning `Self::Output<T>`.
    fn call<T>(&self, value: T) -> Self::Output<T>;
}

/// A flexible function whose resulting type depends on the given input.
#[const_trait]
pub trait FlexibleFnMut {
    /// The result of applying this function to `T`.
    type Output<T>;

    /// Calls the function with the given value, returning `Self::Output<T>`.
    fn call_mut<T>(&mut self, value: T) -> Self::Output<T>;
}

/// A flexible function whose resulting type depends on the given input.
#[const_trait]
pub trait FlexibleFnOnce {
    /// The result of applying this function to `T`.
    type Output<T>;

    /// Calls the function with the given value, returning `Self::Output<T>`.
    fn call_once<T>(self, value: T) -> Self::Output<T>;
}
