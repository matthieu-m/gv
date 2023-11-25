//! The built-in API.
//!
//! The strict minimum set of operations to operate on a const-list are popping the head and pushing the head.
//!
//! However, attempting to build join/split atop those primitives result in O(N) algorithmic complexity for those, which
//! means a lot of compile-time spent for... nothing.
//!
//! Instead, providing join/split as builtins make them essentially O(1) operations, on top of which `push` can easily
//! be built.

pub use last_popper::{LastPopper, LastPopperAccumulator};
pub use splitter::{Splitter, SplitterAccumulator};

/// A macro to simplify the creation of cons-tuples.
///
/// That is, `t!(1, 2, 3, 4)` expands as a cons-tuple semantically equivalent to `(1, 2, 3, 4)`.
#[macro_export]
macro_rules! t {
    () => ();
    ($i:expr) => { ($i, ()) };
    ($i:expr, $j:expr) => { ($i, ($j, ())) };
    ($i:expr, $j:expr, $k:expr) => { ($i, ($j, ($k, ()))) };
    ($i:expr, $j:expr, $k:expr, $($t:expr),*) => { ($i, ($j, ($k, t!($($t),*)))) };
}

#[allow(unused_imports)]
pub(crate) use t;

/// The tuple trait, only implementable for tuples.
pub trait Tuple: sealed::TupleSealed + Sized {
    /// The number of elements in the tuple.
    const ARITY: usize;

    /// The type returned by the `self.as_ref()` operation.
    type AsRef<'a>: Tuple
    where
        Self: 'a;

    /// The type returned by the `self.as_mut()` operation.
    type AsMut<'a>: Tuple
    where
        Self: 'a;

    /// The type returned by the `self.join(<tuple>)` operation.
    type Join<O>: Tuple
    where
        O: Tuple;

    /// The head type returned by the `self.pop_first()` operation.
    type PopFirstHead;

    /// The tail type returned by the `self.pop_first()` operation.
    type PopFirstTail: Tuple;

    /// The head type returned by the `self.pop_last()` operation.
    type PopLastHead: Tuple
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    /// The tail type returned by the `self.pop_last()` operation.
    type PopLastTail
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    /// The head type returned by the `self.split::<N>()` operation.
    type SplitHead<N>: Tuple
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    /// The tail type returned by the `self.split::<N>()` operation.
    type SplitTail<N>: Tuple
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    /// Returns a tuple of references to the elements of `self`.
    fn as_ref(&self) -> Self::AsRef<'_>;

    /// Returns a tuple of mutable references to the elements of `self`.
    fn as_mut(&mut self) -> Self::AsMut<'_>;

    /// Joins `self` and `other`, appending `other` at the end of `self`.
    fn join<O>(self, other: O) -> Self::Join<O>
    where
        O: Tuple;

    /// Pops the first element of this tuple, and returns it and a tuple of the other elements.
    fn pop_first(self) -> (Self::PopFirstHead, Self::PopFirstTail);

    /// Pops the last element of this tuple, and returns it and a tuple of the other elements.
    fn pop_last(self) -> (Self::PopLastHead, Self::PopLastTail)
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    /// Splits the tuple at the N-th element, returning a pair of the N first elements (head) and the rest of the
    /// elements (tail).
    fn split<N>(self) -> (Self::SplitHead<N>, Self::SplitTail<N>)
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;
}

impl Tuple for ! {
    const ARITY: usize = 0;

    type AsRef<'a> = ! where Self: 'a;

    type AsMut<'a> = ! where Self: 'a;

    type Join<O> = ! where O: Tuple;

    type PopFirstHead = !;

    type PopFirstTail = !;

    type PopLastHead = !
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    type PopLastTail = !
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    type SplitHead<N> = !
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    type SplitTail<N> = !
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    fn as_ref(&self) -> Self::AsRef<'_> {
        unreachable!("`as_ref` called on never type !")
    }

    fn as_mut(&mut self) -> Self::AsMut<'_> {
        unreachable!("`as_mut` called on never type !")
    }

    fn join<O>(self, _other: O) -> Self::Join<O>
    where
        O: Tuple,
    {
        unreachable!("`join` called on never type !")
    }

    fn pop_first(self) -> (Self::PopFirstHead, Self::PopFirstTail) {
        unreachable!("`pop_first` called on never type !")
    }

    fn pop_last(self) -> (Self::PopLastHead, Self::PopLastTail)
    where
        LastPopperAccumulator<(), Self>: LastPopper,
    {
        unreachable!("`pop_last` called on never type !")
    }

    fn split<N>(self) -> (Self::SplitHead<N>, Self::SplitTail<N>)
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter,
    {
        unreachable!("`split` called on never type !")
    }
}

impl Tuple for () {
    const ARITY: usize = 0;

    type AsRef<'a> = () where Self: 'a;

    type AsMut<'a> = () where Self: 'a;

    type Join<O> = O where O: Tuple;

    type PopFirstHead = !;

    type PopFirstTail = !;

    type PopLastHead = !
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    type PopLastTail = !
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    type SplitHead<N> = ()
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    type SplitTail<N> = ()
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    fn as_ref(&self) -> Self::AsRef<'_> {}

    fn as_mut(&mut self) -> Self::AsMut<'_> {}

    fn join<O>(self, other: O) -> Self::Join<O>
    where
        O: Tuple,
    {
        other
    }

    fn pop_first(self) -> (Self::PopFirstHead, Self::PopFirstTail) {
        unreachable!("`pop_first` called on empty tuple")
    }

    fn pop_last(self) -> (Self::PopLastHead, Self::PopLastTail)
    where
        LastPopperAccumulator<(), Self>: LastPopper,
    {
        unreachable!("`pop_last` called on empty tuple")
    }

    fn split<N>(self) -> (Self::SplitHead<N>, Self::SplitTail<N>)
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter,
    {
        ((), ())
    }
}

impl<H, Tail> Tuple for (H, Tail)
where
    Tail: Tuple,
{
    const ARITY: usize = 1 + Tail::ARITY;

    type AsRef<'a> = (&'a H, Tail::AsRef<'a>) where Self: 'a;

    type AsMut<'a> = (&'a mut H, Tail::AsMut<'a>) where Self: 'a;

    type Join<O> = (H, Tail::Join<O>) where O: Tuple;

    type PopFirstHead = H;

    type PopFirstTail = Tail;

    type PopLastHead = <LastPopperAccumulator<(), Self> as LastPopper>::Head
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    type PopLastTail = <LastPopperAccumulator<(), Self> as LastPopper>::Tail
    where
        LastPopperAccumulator<(), Self>: LastPopper;

    type SplitHead<N> = <SplitterAccumulator<N, (), Self> as Splitter>::Head
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    type SplitTail<N> = <SplitterAccumulator<N, (), Self> as Splitter>::Tail
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter;

    fn as_ref(&self) -> Self::AsRef<'_> {
        (&self.0, self.1.as_ref())
    }

    fn as_mut(&mut self) -> Self::AsMut<'_> {
        (&mut self.0, self.1.as_mut())
    }

    fn join<O>(self, other: O) -> Self::Join<O>
    where
        O: Tuple,
    {
        (self.0, self.1.join(other))
    }

    fn pop_first(self) -> (Self::PopFirstHead, Self::PopFirstTail) {
        self
    }

    fn pop_last(self) -> (Self::PopLastHead, Self::PopLastTail)
    where
        LastPopperAccumulator<(), Self>: LastPopper,
    {
        LastPopperAccumulator((), self).last()
    }

    fn split<N>(self) -> (Self::SplitHead<N>, Self::SplitTail<N>)
    where
        N: Tuple,
        SplitterAccumulator<N, (), Self>: Splitter,
    {
        let accumulator: SplitterAccumulator<N, _, _> = SplitterAccumulator::new((), self);

        accumulator.split()
    }
}

mod sealed {
    //  A sealed trait to prevent implementing the Tuple trait for non-tuples.
    #[doc(hidden)]
    pub trait TupleSealed {}

    impl TupleSealed for ! {}

    impl TupleSealed for () {}

    impl<H, Tail> TupleSealed for (H, Tail) where Tail: TupleSealed {}
} // mod sealed

#[doc(hidden)]
mod last_popper {
    use super::Tuple;

    pub struct LastPopperAccumulator<Head, Tail>(pub(super) Head, pub(super) Tail);

    pub trait LastPopper {
        type Head: Tuple;
        type Tail;

        fn last(self) -> (Self::Head, Self::Tail);
    }

    //  FIXME: make the compiler understand that since `H::Join<(TH, ())>` implements `Tuple` and `TT` does so too, then
    //         `LastPopperAccumulator<Self::Head, TT>` must be implementing it too.
    //
    // impl<H, TH, TT> LastPopper for LastPopperAccumulator<H, (TH, TT)>
    // where
    //     H: Tuple,
    //     TT: Tuple,
    // {
    //     default type Head = H::Join<(TH, ())>;
    //     default type Tail = <LastPopperAccumulator<Self::Head, TT> as LastPopper>::Tail;
    //
    //     default fn last(self) -> (Self::Head, Self::Tail) {
    //         let new_head = self.0.join(self.1.0);
    //
    //         LastPopperAccumulator(new_head, self.1.1).last()
    //     }
    // }

    impl<H, T> LastPopper for LastPopperAccumulator<H, T>
    where
        H: Tuple,
        T: Tuple,
    {
        type Head = !;
        type Tail = !;

        fn last(self) -> (Self::Head, Self::Tail) {
            todo!("Implement proper recursion support for `last`")
        }
    }
} // mod last_popper

#[doc(hidden)]
mod splitter {
    use core::marker::PhantomData;

    use super::Tuple;

    pub struct SplitterAccumulator<Index, Head, Tail>(Head, Tail, PhantomData<Index>);

    impl<Index, Head, Tail> SplitterAccumulator<Index, Head, Tail> {
        #[doc(hidden)]
        pub fn new(head: Head, tail: Tail) -> Self {
            Self(head, tail, PhantomData)
        }
    }

    pub trait Splitter {
        type Head: Tuple;
        type Tail: Tuple;

        fn split(self) -> (Self::Head, Self::Tail);
    }

    impl Splitter for SplitterAccumulator<(), (), ()> {
        type Head = ();
        type Tail = ();

        fn split(self) -> (Self::Head, Self::Tail) {
            ((), ())
        }
    }

    impl<IndexTail> Splitter for SplitterAccumulator<((), IndexTail), (), ()> {
        type Head = ();
        type Tail = ();

        fn split(self) -> (Self::Head, Self::Tail) {
            ((), ())
        }
    }

    impl<H, T> Splitter for SplitterAccumulator<(), (H, T), ()>
    where
        T: Tuple,
    {
        type Head = (H, T);
        type Tail = ();

        fn split(self) -> (Self::Head, Self::Tail) {
            (self.0, self.1)
        }
    }

    impl<H, T> Splitter for SplitterAccumulator<(), (), (H, T)>
    where
        T: Tuple,
    {
        type Head = ();
        type Tail = (H, T);

        fn split(self) -> (Self::Head, Self::Tail) {
            (self.0, self.1)
        }
    }

    impl<HH, HT, TH, TT> Splitter for SplitterAccumulator<(), (HH, HT), (TH, TT)>
    where
        HT: Tuple,
        TT: Tuple,
    {
        type Head = (HH, HT);
        type Tail = (TH, TT);

        fn split(self) -> (Self::Head, Self::Tail) {
            (self.0, self.1)
        }
    }

    impl<IndexTail, HH, HT> Splitter for SplitterAccumulator<((), IndexTail), (HH, HT), ()>
    where
        HT: Tuple,
    {
        type Head = (HH, HT);
        type Tail = ();

        fn split(self) -> (Self::Head, Self::Tail) {
            (self.0, self.1)
        }
    }

    impl<IndexTail, H, TH, TT> Splitter for SplitterAccumulator<((), IndexTail), H, (TH, TT)>
    where
        H: Tuple,
        TT: Tuple,
        SplitterAccumulator<IndexTail, (TH, ()), TT>: Splitter,
    {
        type Head = H::Join<<SplitterAccumulator<IndexTail, (TH, ()), TT> as Splitter>::Head>;
        type Tail = <SplitterAccumulator<IndexTail, (TH, ()), TT> as Splitter>::Tail;

        fn split(self) -> (Self::Head, Self::Tail) {
            let (th, tt) = self.1;

            let accumulator: SplitterAccumulator<IndexTail, (TH, ()), TT> = SplitterAccumulator::new((th, ()), tt);

            let (head, tail) = accumulator.split();

            (self.0.join(head), tail)
        }
    }
} // splitter

#[cfg(test)]
mod tests {
    #![allow(clippy::unit_cmp)]

    use crate::peano;

    use super::*;

    const NIL: () = ();

    macro_rules! s {
        ($i:literal) => {
            String::from($i)
        };
    }

    #[test]
    fn join() {
        assert_eq!(NIL, NIL.join(NIL));
        assert_eq!(t!(s!("Hello")), t!(s!("Hello")).join(NIL));
        assert_eq!(t!(s!("Hello")), NIL.join(t!(s!("Hello"))));
        assert_eq!(t!(s!("Hello"), s!("World")), t!(s!("Hello"), s!("World")).join(NIL));
        assert_eq!(t!(s!("Hello"), s!("World")), t!(s!("Hello")).join(t!(s!("World"))));
        assert_eq!(t!(s!("Hello"), s!("World")), NIL.join(t!(s!("Hello"), s!("World"))));
    }

    #[test]
    fn pop_first() {
        assert_eq!((s!("Hello"), NIL), t!(s!("Hello")).pop_first());
        assert_eq!((s!("Hello"), t!(s!("World"))), t!(s!("Hello"), s!("World")).pop_first());
        assert_eq!(
            (s!("Hello"), t!(s!("World"), "!")),
            t!(s!("Hello"), s!("World"), "!").pop_first()
        );
    }

    #[test]
    fn split() {
        assert_eq!((NIL, NIL), NIL.split::<peano::N0>());
        assert_eq!((NIL, NIL), NIL.split::<peano::N2>());

        assert_eq!((NIL, t!(s!("0"))), t!(s!("0")).split::<peano::N0>());
        assert_eq!((t!(s!("0")), NIL), t!(s!("0")).split::<peano::N1>());
        assert_eq!((t!(s!("0")), NIL), t!(s!("0")).split::<peano::N2>());

        assert_eq!((NIL, t!(s!("0"), s!("1"))), t!(s!("0"), s!("1")).split::<peano::N0>());
        assert_eq!((t!(s!("0")), t!(s!("1"))), t!(s!("0"), s!("1")).split::<peano::N1>());
        assert_eq!((t!(s!("0"), s!("1")), NIL), t!(s!("0"), s!("1")).split::<peano::N2>());
        assert_eq!((t!(s!("0"), s!("1")), NIL), t!(s!("0"), s!("1")).split::<peano::N3>());
    }

    #[test]
    fn splitter() {
        fn acc<N, H, T>(h: H, t: T) -> SplitterAccumulator<N, H, T>
        where
            N: Tuple,
        {
            SplitterAccumulator::new(h, t)
        }

        assert_eq!((NIL, NIL), acc::<peano::N0, _, _>(NIL, NIL).split());
        assert_eq!((NIL, NIL), acc::<peano::N2, _, _>(NIL, NIL).split());

        assert_eq!((NIL, t!(s!("0"))), acc::<peano::N0, _, _>(NIL, t!(s!("0"))).split());
        assert_eq!(
            (t!(s!("0")), t!(s!("1"))),
            acc::<peano::N0, _, _>(t!(s!("0")), t!(s!("1"))).split()
        );

        assert_eq!(
            (t!(s!("0")), t!(s!("1"))),
            acc::<peano::N1, _, _>(NIL, t!(s!("0"), s!("1"))).split()
        );
    }
} // mod tests
