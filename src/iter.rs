//! Implementations of iterators for `EitherOrBoth`

use std::iter::FusedIterator;

use crate::either_or_both::EitherOrBoth;

/// TODO: DOCS
pub struct ChainedIterEitherOrBoth<T>(EitherOrBoth<T, T>);

/// TODO: DOCS
pub struct SwapIterEitherOrBoth<L, R>(EitherOrBoth<L, R>);

impl<T> ChainedIterEitherOrBoth<T> {
    pub(crate) fn new(either_or_both: EitherOrBoth<T, T>) -> Self {
        Self(either_or_both)
    }
}

impl<T> Default for ChainedIterEitherOrBoth<T>
where
    T: Default,
{
    fn default() -> Self {
        Self(EitherOrBoth::Left(T::default()))
    }
}

impl<T> DoubleEndedIterator for ChainedIterEitherOrBoth<T>
where
    T: DoubleEndedIterator,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.0.as_mut() {
            EitherOrBoth::Both(left, right) => right.next_back().or_else(|| left.next_back()),
            EitherOrBoth::Left(side) | EitherOrBoth::Right(side) => side.next_back(),
        }
    }
}

impl<T> FusedIterator for ChainedIterEitherOrBoth<T> where T: FusedIterator {}

impl<T> Iterator for ChainedIterEitherOrBoth<T>
where
    T: Iterator,
{
    type Item = T::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.as_mut() {
            EitherOrBoth::Both(left, right) => left.next().or_else(|| right.next()),
            EitherOrBoth::Left(side) | EitherOrBoth::Right(side) => side.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0
            .as_ref()
            .map(Iterator::size_hint)
            .reduce(|(l_lower, l_upper), (r_lower, r_upper)| {
                (
                    l_lower.saturating_add(r_lower),
                    l_upper.and_then(|l| r_upper.and_then(|r| l.checked_add(r))),
                )
            })
    }
}

impl<L, R> SwapIterEitherOrBoth<L, R> {
    pub(crate) fn new(either_or_both: EitherOrBoth<L, R>) -> Self {
        Self(either_or_both)
    }
}

impl<L, R> DoubleEndedIterator for SwapIterEitherOrBoth<L, R>
where
    L: DoubleEndedIterator,
    R: DoubleEndedIterator,
{
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.0.as_mut() {
            EitherOrBoth::Both(left, right) => {
                (left.next_back(), right.next_back()).try_into().ok()
            }
            EitherOrBoth::Left(left) => left.next_back().map(EitherOrBoth::Left),
            EitherOrBoth::Right(right) => right.next_back().map(EitherOrBoth::Right),
        }
    }
}

impl<L, R> ExactSizeIterator for SwapIterEitherOrBoth<L, R>
where
    L: ExactSizeIterator,
    R: ExactSizeIterator,
{
}

impl<L, R> FusedIterator for SwapIterEitherOrBoth<L, R>
where
    L: FusedIterator,
    R: FusedIterator,
{
}

impl<L, R> Iterator for SwapIterEitherOrBoth<L, R>
where
    L: Iterator,
    R: Iterator,
{
    type Item = EitherOrBoth<L::Item, R::Item>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.as_mut() {
            EitherOrBoth::Both(left, right) => (left.next(), right.next()).try_into().ok(),
            EitherOrBoth::Left(left) => left.next().map(EitherOrBoth::Left),
            EitherOrBoth::Right(right) => right.next().map(EitherOrBoth::Right),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0
            .as_ref()
            .map_both(Iterator::size_hint, Iterator::size_hint)
            .reduce(|(l_lower, l_upper), (r_lower, r_upper)| {
                (
                    // TODO: min or max?
                    l_lower.min(r_lower),
                    // is `None` if the left or right upper bound is `None`
                    l_upper.and_then(|l| r_upper.map(|r| l.max(r))),
                )
            })
    }
}
