//! A definition of natural numbers represented by tuples.
//!
//! The arity of the tuple defines the number, and its elements are all empty tuples.
//!
//! While the final design is better of using `const N: usize`, there are significant limitations on what can be done
//! with const generics today, making it awkward to manipulate those `N`.

/// Zero.
pub type N0 = ();

/// One.
pub type N1 = Add1<N0>;

/// Two.
pub type N2 = Add2<N0>;

/// Three.
pub type N3 = Add1<N2>;

/// Four.
pub type N4 = Add2<N2>;

/// Five.
pub type N5 = Add5<N0>;

/// Ten.
pub type N10 = Add10<N0>;

/// Twenty.
pub type N20 = Add10<N10>;

/// Fifty.
pub type N50 = Add10<Add10<Add10<N20>>>;

/// Adds one to a peano number.
pub type Add1<T> = ((), T);

/// Adds two to a peano number.
pub type Add2<T> = ((), ((), T));

/// Adds five to a peano number.
pub type Add5<T> = ((), ((), ((), ((), ((), T)))));

/// Adds ten to a peano number.
pub type Add10<T> = Add5<Add5<T>>;
