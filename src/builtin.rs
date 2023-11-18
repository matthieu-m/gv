//! The built-in API.
//!
//! The strict minimum set of operations to operate on a const-list are popping the head and pushing the head.
//!
//! However, attempting to build join/split atop those primitives result in O(N) algorithmic complexity for those, which
//! means a lot of compile-time spent for... nothing.
//!
//! Instead, providing join/split as builtins make them essentially O(1) operations, on top of which `push` can easily
//! be built.

use splitter::{Splitter, SplitterAccumulator};

/// The tuple trait, only implementable for tuples.
pub trait Tuple: sealed::TupleSealed + Sized {
    /// The number of elements in the tuple.
    const ARITY: usize;

    /// The type returned by the `self.join(<tuple>)` operation.
    type Join<O>: Tuple
    where
        O: Tuple;

    /// The head type returned by the `self.pop()` operation.
    type PopHead;

    /// The tail type returned by the `self.pop()` operation.
    type PopTail;

    /// The head type returned by the `self.split::<N>()` operation.
    type SplitHead<const N: usize>: Tuple
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter;

    /// The tail type returned by the `self.split::<N>()` operation.
    type SplitTail<const N: usize>: Tuple
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter;

    /// Joins `self` and `other`, appending `other` at the end of `self`.
    fn join<O>(self, other: O) -> Self::Join<O>
    where
        O: Tuple;

    /// Pops the head of this tuple, that is returns its first element, and the rest of them.
    fn pop(self) -> (Self::PopHead, Self::PopTail);

    /// Splits the tuple at the N-th element, returning a pair of the N first elements (head) and the rest of the
    /// elements (tail).
    fn split<const N: usize>(self) -> (Self::SplitHead<N>, Self::SplitTail<N>)
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter;
}

impl Tuple for () {
    const ARITY: usize = 0;

    type Join<O> = O where O: Tuple;

    type PopHead = !;

    type PopTail = !;

    type SplitHead<const N: usize> = ()
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter;

    type SplitTail<const N: usize> = ()
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter;

    fn join<O>(self, other: O) -> Self::Join<O>
    where
        O: Tuple,
    {
        other
    }

    fn pop(self) -> (Self::PopHead, Self::PopTail) {
        panic!("`pop` called on empty tuple")
    }

    fn split<const N: usize>(self) -> (Self::SplitHead<N>, Self::SplitTail<N>)
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter,
    {
        ((), ())
    }
}

impl<H, Tail> Tuple for (H, Tail)
where
    Tail: Tuple,
{
    const ARITY: usize = 1 + Tail::ARITY;

    type Join<O> = (H, Tail::Join<O>) where O: Tuple;

    type PopHead = H;

    type PopTail = Tail;

    type SplitHead<const N: usize> = <SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self> as Splitter>::Head
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter;

    type SplitTail<const N: usize> = <SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self> as Splitter>::Tail
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter;

    fn join<O>(self, other: O) -> Self::Join<O>
    where
        O: Tuple,
    {
        (self.0, self.1.join(other))
    }

    fn pop(self) -> (Self::PopHead, Self::PopTail) {
        self
    }

    fn split<const N: usize>(self) -> (Self::SplitHead<N>, Self::SplitTail<N>)
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), Self>: Splitter,
    {
        let accumulator: SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, _, _> =
            SplitterAccumulator::new((), self);

        accumulator.split()
    }
}

//  Manipulating -- or specializing -- on a const parameter is painful, at the moment, so internally we use a helper
//  method to represent a const parameter as a type. Oh well...
#[doc(hidden)]
pub trait ConsNumber {
    type AsTuple;
}

#[doc(hidden)]
pub struct Peano<const N: usize>;

mod sealed {
    //  A sealed trait to prevent implementing the Tuple trait for non-tuples.
    #[doc(hidden)]
    pub trait TupleSealed {}

    impl TupleSealed for () {}

    impl<H, Tail> TupleSealed for (H, Tail) where Tail: TupleSealed {}
} // mod sealed

mod splitter {
    use core::marker::PhantomData;

    use super::Tuple;

    #[doc(hidden)]
    pub struct SplitterAccumulator<Index, Head, Tail>(Head, Tail, PhantomData<Index>);

    impl<Index, Head, Tail> SplitterAccumulator<Index, Head, Tail> {
        #[doc(hidden)]
        pub fn new(head: Head, tail: Tail) -> Self {
            Self(head, tail, PhantomData)
        }
    }

    #[doc(hidden)]
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

#[rustfmt::skip]
mod peano {
    use super::{ConsNumber, Peano};

    impl ConsNumber for Peano<0> { type AsTuple = (); }
    impl ConsNumber for Peano<1> { type AsTuple = ((), <Peano<0> as ConsNumber>::AsTuple); }
    impl ConsNumber for Peano<2> { type AsTuple = ((), <Peano<1> as ConsNumber>::AsTuple); }
    impl ConsNumber for Peano<3> { type AsTuple = ((), <Peano<2> as ConsNumber>::AsTuple); }
    impl ConsNumber for Peano<4> { type AsTuple = ((), <Peano<3> as ConsNumber>::AsTuple); }
    impl ConsNumber for Peano<5> { type AsTuple = ((), <Peano<4> as ConsNumber>::AsTuple); }
    impl ConsNumber for Peano<6> { type AsTuple = ((), <Peano<5> as ConsNumber>::AsTuple); }
    impl ConsNumber for Peano<7> { type AsTuple = ((), <Peano<6> as ConsNumber>::AsTuple); }
} // mod peano

#[cfg(test)]
mod tests {
    #![allow(clippy::unit_cmp)]

    use super::*;

    const NIL: () = ();

    macro_rules! s {
        ($i:literal) => {
            String::from($i)
        };
    }

    macro_rules! t {
        () => ();
        ($i:literal) => { (s!($i), ()) };
        ($i:literal, $j:literal) => { (s!($i), (s!($j), ())) };
        ($i:literal, $j:literal, $k:literal) => { (s!($i), (s!($j), (s!($k), ()))) };
        ($i:literal, $j:literal, $k:literal, $($t:literal),*) => { (s!($i), (s!($j), (s!($k), t!($($t),*)))) };
    }

    #[test]
    fn join() {
        assert_eq!(NIL, NIL.join(NIL));
        assert_eq!(t!("Hello"), t!("Hello").join(NIL));
        assert_eq!(t!("Hello"), NIL.join(t!("Hello")));
        assert_eq!(t!("Hello", "World"), t!("Hello", "World").join(NIL));
        assert_eq!(t!("Hello", "World"), t!("Hello").join(t!("World")));
        assert_eq!(t!("Hello", "World"), NIL.join(t!("Hello", "World")));
    }

    #[test]
    fn pop() {
        assert_eq!((s!("Hello"), NIL), t!("Hello").pop());
        assert_eq!((s!("Hello"), t!("World")), t!("Hello", "World").pop());
        assert_eq!((s!("Hello"), t!("World", "!")), t!("Hello", "World", "!").pop());
    }

    #[test]
    fn split() {
        assert_eq!((NIL, NIL), NIL.split::<0>());
        assert_eq!((NIL, NIL), NIL.split::<2>());

        assert_eq!((NIL, t!("0")), t!("0").split::<0>());
        assert_eq!((t!("0"), NIL), t!("0").split::<1>());
        assert_eq!((t!("0"), NIL), t!("0").split::<2>());

        assert_eq!((NIL, t!("0", "1")), t!("0", "1").split::<0>());
        assert_eq!((t!("0"), t!("1")), t!("0", "1").split::<1>());
        assert_eq!((t!("0", "1"), NIL), t!("0", "1").split::<2>());
        assert_eq!((t!("0", "1"), NIL), t!("0", "1").split::<3>());
    }

    #[test]
    fn splitter() {
        fn acc<const N: usize, H, T>(h: H, t: T) -> SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, H, T>
        where
            Peano<N>: ConsNumber,
        {
            SplitterAccumulator::new(h, t)
        }

        assert_eq!((NIL, NIL), acc::<0, _, _>(NIL, NIL).split());
        assert_eq!((NIL, NIL), acc::<2, _, _>(NIL, NIL).split());

        assert_eq!((NIL, t!("0")), acc::<0, _, _>(NIL, t!("0")).split());
        assert_eq!((t!("0"), t!("1")), acc::<0, _, _>(t!("0"), t!("1")).split());

        assert_eq!((t!("0"), t!("1")), acc::<1, _, _>(NIL, t!("0", "1")).split());
    }
} // mod tests
