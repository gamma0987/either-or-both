//! The iterators for `Either`

use core::iter::FusedIterator;

use crate::Either;

/// TODO: DOCS
#[derive(Debug, Clone)]
pub struct IterEither<T>(Either<T>);

impl<T> IterEither<T> {
    pub(crate) fn new(either: Either<T, T>) -> Self {
        Self(either)
    }
}

impl<T> Default for IterEither<T>
where
    T: Default,
{
    fn default() -> Self {
        Self(Either::Left(T::default()))
    }
}

impl<T> DoubleEndedIterator for IterEither<T>
where
    T: DoubleEndedIterator,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.as_mut().map(DoubleEndedIterator::next_back).reduce()
    }
}

impl<T> ExactSizeIterator for IterEither<T> where T: ExactSizeIterator {}
impl<T> FusedIterator for IterEither<T> where T: FusedIterator {}

impl<T> Iterator for IterEither<T>
where
    T: Iterator,
{
    type Item = T::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.as_mut().map(Iterator::next).reduce()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.as_ref().map(Iterator::size_hint).reduce()
    }
}

/// TODO: DOCS
#[derive(Debug, Clone)]
pub struct SwapIterEither<L, R>(Either<L, R>);

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
            .bimap(Iterator::size_hint, Iterator::size_hint)
            .reduce()
    }
}
