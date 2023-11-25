//! Transforming tuples, one element at a time.

use core::ops::Try;

use crate::{
    builtin::{ConsNumber, LastPopper, LastPopperAccumulator, Peano, Splitter, SplitterAccumulator, Tuple},
    polymorphic::{Polymorphic2FnMut, PolymorphicFnMut},
};

/// Iterator over tuple elements.
#[derive(Clone, Copy, Debug)]
pub struct TupleIterator<T>(T);

//
//  Constants
//

impl<T> TupleIterator<T>
where
    T: Tuple,
{
    /// Number of elements of the iterator.
    pub const ARITY: usize = T::ARITY;
}

//
//  Construction
//

impl<T> TupleIterator<T> {
    /// Creates a new instance from a tuple.
    ///
    /// Consider using `Tuple::tuple_into_iter` instead.
    pub fn from_tuple(tuple: T) -> Self {
        Self(tuple)
    }
}

impl<T> TupleIterator<T>
where
    T: Tuple,
{
    /// Creates a new instance from a tuple.
    ///
    /// Consider using `Tuple::tuple_iter` instead.
    pub fn from_tuple_ref(tuple: &T) -> TupleIterator<T::AsRef<'_>> {
        TupleIterator::from_tuple(tuple.as_ref())
    }

    /// Creates a new instance from a tuple.
    ///
    /// Consider using `Tuple::tuple_iter_mut` instead.
    pub fn from_tuple_mut(tuple: &mut T) -> TupleIterator<T::AsMut<'_>> {
        TupleIterator::from_tuple(tuple.as_mut())
    }

    /// Returns an iterator over references to the elements of the current iterator.
    pub fn as_ref(&self) -> TupleIterator<T::AsRef<'_>> {
        TupleIterator::from_tuple(self.0.as_ref())
    }

    /// Returns an iterator over mutable references to the elements of the current iterator.
    pub fn as_mut(&mut self) -> TupleIterator<T::AsMut<'_>> {
        TupleIterator::from_tuple(self.0.as_mut())
    }
}

//
//  Terminal operations.
//
/// These operations do not return an iterator.
//

impl<T> TupleIterator<T> {
    /// Collects the iterator into a tuple.
    pub fn collect(self) -> T {
        self.0
    }
}

impl<T> TupleIterator<T>
where
    T: Tuple,
{
    /// Returns the number of elements.
    pub fn count(self) -> usize {
        Self::ARITY
    }

    /// Returns the first element.
    ///
    /// #   Panics
    ///
    /// If there are no such elements.
    pub fn first(self) -> T::PopFirstHead
    where
        [(); Self::ARITY - 1]: Sized,
    {
        self.0.pop_first().0
    }

    /// Returns the last element
    pub fn last(self) -> T::PopLastTail
    where
        LastPopperAccumulator<(), Self>: LastPopper,
    {
        self.0.pop_last().1
    }

    /// Returns the n-th element.
    pub fn nth<const N: usize>(self) -> <T::SplitTail<N> as Tuple>::PopFirstHead
    where
        [(); Self::ARITY + 1 - N]: Sized,
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), T>: Splitter,
    {
        self.0.split::<N>().1.pop_first().0
    }

    /// Returns whether any element matches the predicate.
    ///
    /// The predicate is applied in order, until it returns true or there are no elements left.
    pub fn any<F>(self, fun: F) -> bool
    where
        F: for<T> PolymorphicFnMut<Output<T> = bool>,
    {
        struct Any<F>(F);

        impl<F> Polymorphic2FnMut<bool> for Any<F>
        where
            F: for<T> PolymorphicFnMut<Output<T> = bool>,
        {
            type Output<T: ?Sized> = bool;

            fn call_mut<T>(&mut self, accumulator: bool, value: T) -> bool {
                accumulator || self.0.call_mut(value)
            }
        }

        self.fold(false, Any(fun))
    }

    /// Returns whether all elements match the predicate.
    ///
    /// The predicate is applied in order, until it returns false or there are no elements left.
    pub fn all<F>(self, fun: F) -> bool
    where
        F: for<T> PolymorphicFnMut<Output<T> = bool>,
    {
        struct All<F>(F);

        impl<F> Polymorphic2FnMut<bool> for All<F>
        where
            F: for<T> PolymorphicFnMut<Output<T> = bool>,
        {
            type Output<T: ?Sized> = bool;

            fn call_mut<T>(&mut self, accumulator: bool, value: T) -> bool {
                accumulator && self.0.call_mut(value)
            }
        }

        self.fold(true, All(fun))
    }

    /// Returns whether no element match the predicate.
    ///
    /// The predicate is applied in order, until it returns true or there are no elements left.
    pub fn none<F>(self, fun: F) -> bool
    where
        F: for<T> PolymorphicFnMut<Output<T> = bool>,
    {
        !self.any(fun)
    }

    /// Applies an operation to each element of the iterator, consuming it.
    pub fn for_each<F>(self, fun: F)
    where
        F: for<T> PolymorphicFnMut<Output<T> = ()>,
    {
        self.fold((), Foldable(fun));
    }

    /// Applies an operation to each element of the iterator, consuming it, until a failure occurs.
    ///
    /// Returns the failure value, if any.
    pub fn try_for_each<R, F>(self, fun: F) -> R
    where
        F: for<T> PolymorphicFnMut<Output<T> = R>,
        R: Try<Output = ()>,
    {
        self.try_fold((), Foldable(fun))
    }

    /// Applies an operation to each element of the iterator, producing a single final value.
    pub fn fold<B, F>(self, initial: B, mut fun: F) -> B
    where
        F: for<T> Polymorphic2FnMut<B, Output<T> = B>,
    {
        if T::ARITY == 0 {
            initial
        } else {
            let (head, tail) = self.0.pop_first();

            let folded = fun.call_mut(initial, head);

            TupleIterator::from_tuple(tail).fold(folded, fun)
        }
    }

    /// Applies an operation to each element of the iterator, consuming it, until a failure occurs or a single final
    /// value is produced.
    ///
    /// Returns either the final value or the failure, whichever occurs.
    pub fn try_fold<R, B, F>(self, initial: B, mut fun: F) -> R
    where
        F: for<T> Polymorphic2FnMut<B, Output<T> = R>,
        R: Try<Output = B>,
    {
        if T::ARITY == 0 {
            try { initial }
        } else {
            let (head, tail) = self.0.pop_first();

            let folded = fun.call_mut(initial, head)?;

            TupleIterator::from_tuple(tail).try_fold(folded, fun)
        }
    }
}

//
//  Implementation helpers for Terminal operations.
//

struct Foldable<F>(F);

impl<F, R> Polymorphic2FnMut<()> for Foldable<F>
where
    F: for<T> PolymorphicFnMut<Output<T> = R>,
{
    type Output<T: ?Sized> = R;

    fn call_mut<T>(&mut self, _argument: (), value: T) -> R {
        self.0.call_mut(value)
    }
}

//
//  Type-based Terminal Operations
//

//  TODO: find, try_find, position.

//
//  Homogeneous operations.
//
//  That is, element-wise operations which return the iterator itself, with its values modified.
//

impl<T> TupleIterator<T>
where
    T: Tuple,
{
    //  What kind of operation do we get here?
}

//
//  Heterogeneous operations.
//
//  That is, operations which may alter the shape of the iterator.
//

//  TODO: rev.

impl<T> TupleIterator<T>
where
    T: Tuple,
{
    /// Maps each element of the iterator according to the `map` function.
    pub fn map<F>(self, fun: F) -> TupleIterator<<MapperAccumulator<F, (), T> as Mapper<F>>::Head>
    where
        F: PolymorphicFnMut,
        MapperAccumulator<F, (), T>: Mapper<F>,
    {
        let (_, head, _) = MapperAccumulator(fun, (), self.0).map();

        TupleIterator::from_tuple(head)
    }

    /// Returns an iterator over the elements after the first N of `self`.
    pub fn skip<const N: usize>(self) -> TupleIterator<<T as Tuple>::SplitTail<N>>
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), T>: Splitter,
    {
        let (_, tail) = self.0.split::<N>();

        TupleIterator::from_tuple(tail)
    }

    /// Returns an iterator over the first N elements, or fewer if there are less in `self`.
    pub fn take<const N: usize>(self) -> TupleIterator<<T as Tuple>::SplitHead<N>>
    where
        Peano<N>: ConsNumber,
        SplitterAccumulator<<Peano<N> as ConsNumber>::AsTuple, (), T>: Splitter,
    {
        let (head, _) = self.0.split::<N>();

        TupleIterator::from_tuple(head)
    }
}

//
//  Implementation helpers for Heterogeneous operations.
//

use mapper::{Mapper, MapperAccumulator};

#[doc(hidden)]
mod mapper {
    use crate::builtin::Tuple;
    use crate::polymorphic::PolymorphicFnMut;

    pub struct MapperAccumulator<F, H, T>(pub(super) F, pub(super) H, pub(super) T);

    pub trait Mapper<F> {
        type Head: Tuple;
        type Tail: Tuple;

        fn map(self) -> (F, Self::Head, Self::Tail);
    }

    impl<F, H> Mapper<F> for MapperAccumulator<F, H, ()>
    where
        H: Tuple,
    {
        type Head = H;
        type Tail = ();

        fn map(self) -> (F, Self::Head, Self::Tail) {
            (self.0, self.1, self.2)
        }
    }

    impl<F, H, TH, TT> Mapper<F> for MapperAccumulator<F, H, (TH, TT)>
    where
        F: PolymorphicFnMut,
        H: Tuple,
        TT: Tuple,
    {
        type Head = <H as Tuple>::Join<(<F as PolymorphicFnMut>::Output<TH>, ())>;
        type Tail = TT;

        fn map(self) -> (F, Self::Head, Self::Tail) {
            let (mut fun, head, tail) = (self.0, self.1, self.2);

            let mapped = fun.call_mut(tail.0);

            (fun, head.join((mapped, ())), tail.1)
        }
    }
} // mod mapper

//
//  Type-based Heterogeneous Operations
//

//  TODO: filter, partition, skip_while, take_while.
