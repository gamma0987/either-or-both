//! Iterators for [`Either`]

use core::iter::FusedIterator;

use crate::Either;

/// TODO: DOCS
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IntoIterEither<T>(Item<T>);

/// TODO: DOCS
#[derive(Debug, Clone, PartialEq, Eq)]
struct Item<T>(Option<Either<T>>);

/// TODO: DOCS
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IterEither<'a, T>(Item<&'a T>);

/// TODO: DOCS
#[derive(Debug, Default, PartialEq, Eq)]
pub struct IterMutEither<'a, T>(Item<&'a mut T>);

/// TODO: DOCS
#[derive(Debug, Clone)]
pub struct InnerIterEither<T>(Either<T>);

/// TODO: DOCS
#[derive(Debug, Clone)]
pub struct SwapIterEither<L, R>(Either<L, R>);

impl<T> InnerIterEither<T> {
    pub(crate) fn new(either: Either<T, T>) -> Self {
        Self(either)
    }
}

impl<T> DoubleEndedIterator for InnerIterEither<T>
where
    T: DoubleEndedIterator,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.as_mut().reduce_map(DoubleEndedIterator::next_back)
    }
}

impl<T> ExactSizeIterator for InnerIterEither<T> where T: ExactSizeIterator {}

impl<T> FusedIterator for InnerIterEither<T> where T: FusedIterator {}

impl<T> Iterator for InnerIterEither<T>
where
    T: Iterator,
{
    type Item = T::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.as_mut().reduce_map(Iterator::next)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.as_ref().reduce_map(Iterator::size_hint)
    }
}

impl<T> IntoIterEither<T> {
    pub(crate) fn new(either: Either<T>) -> Self {
        Self(either.into())
    }
}

impl<T> DoubleEndedIterator for IntoIterEither<T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IntoIterEither<T> {}
impl<T> FusedIterator for IntoIterEither<T> {}

impl<T> Iterator for IntoIterEither<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> From<Either<T>> for Item<T> {
    fn from(value: Either<T>) -> Self {
        Self(Some(value))
    }
}

impl<T> Default for Item<T> {
    fn default() -> Self {
        Self(None)
    }
}

impl<T> DoubleEndedIterator for Item<T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

impl<T> ExactSizeIterator for Item<T> {}
impl<T> FusedIterator for Item<T> {}

impl<T> Iterator for Item<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.take().map(Either::reduce)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.0 {
            Some(_) => (1, Some(1)),
            None => (0, Some(0)),
        }
    }
}

impl<'a, T> IterEither<'a, T> {
    pub(crate) fn new(either: &'a Either<T>) -> Self {
        Self(either.as_ref().into())
    }
}

impl<T> DoubleEndedIterator for IterEither<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IterEither<'_, T> {}
impl<T> FusedIterator for IterEither<'_, T> {}

impl<'a, T> Iterator for IterEither<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> IterMutEither<'a, T> {
    pub(crate) fn new(either: &'a mut Either<T>) -> Self {
        Self(either.as_mut().into())
    }
}

impl<T> DoubleEndedIterator for IterMutEither<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IterMutEither<'_, T> {}
impl<T> FusedIterator for IterMutEither<'_, T> {}

impl<'a, T> Iterator for IterMutEither<'a, T> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<L, R> SwapIterEither<L, R> {
    pub(crate) fn new(either: Either<L, R>) -> Self {
        Self(either)
    }
}

impl<L, R> DoubleEndedIterator for SwapIterEither<L, R>
where
    L: DoubleEndedIterator,
    R: DoubleEndedIterator,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.0.as_mut() {
            Either::Left(left) => left.next_back().map(Either::Left),
            Either::Right(right) => right.next_back().map(Either::Right),
        }
    }
}

impl<L, R> ExactSizeIterator for SwapIterEither<L, R>
where
    L: ExactSizeIterator,
    R: ExactSizeIterator,
{
}

impl<L, R> FusedIterator for SwapIterEither<L, R>
where
    L: FusedIterator,
    R: FusedIterator,
{
}

impl<L, R> Iterator for SwapIterEither<L, R>
where
    L: Iterator,
    R: Iterator,
{
    type Item = Either<L::Item, R::Item>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.as_mut() {
            Either::Left(left) => left.next().map(Either::Left),
            Either::Right(right) => right.next().map(Either::Right),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0
            .as_ref()
            .bireduce(Iterator::size_hint, Iterator::size_hint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn item_default() {
        assert_eq!(Item::default(), Item::<u8>(None));
    }
}
