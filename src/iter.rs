//! Implementations of iterators for `EitherOrBoth`
// TODO: Improve Performance. Use #[inline] where possible

use std::collections::VecDeque;
use std::iter::FusedIterator;

use crate::either_or_both::EitherOrBoth;

/// TODO: DOCS
#[derive(Debug, Clone)]
pub struct ChainedIterEitherOrBoth<T>(EitherOrBoth<T, T>);

/// TODO: DOCS
#[derive(Debug, Clone, Default)]
pub struct IntoIterEitherOrBoth<T>(Items<T>);

/// TODO: DOCS
#[derive(Debug, Clone)]
pub struct Items<T>(VecDeque<T>);

/// TODO: DOCS
#[derive(Debug, Clone, Default)]
pub struct IterEitherOrBoth<'a, T: 'a>(Items<&'a T>);

/// TODO: DOCS
#[derive(Debug, Default)]
pub struct IterMutEitherOrBoth<'a, T: 'a>(Items<&'a mut T>);

/// TODO: DOCS
#[derive(Debug, Clone)]
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

impl<T> IntoIterEitherOrBoth<T> {
    pub(crate) fn new(either_or_both: EitherOrBoth<T, T>) -> Self {
        Self(either_or_both.into())
    }
}

impl<T> DoubleEndedIterator for IntoIterEitherOrBoth<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IntoIterEitherOrBoth<T> {}
impl<T> FusedIterator for IntoIterEitherOrBoth<T> {}

impl<T> Iterator for IntoIterEitherOrBoth<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> From<EitherOrBoth<T>> for Items<T> {
    fn from(value: EitherOrBoth<T>) -> Self {
        match value {
            EitherOrBoth::Both(left, right) => Self(VecDeque::from([left, right])),
            EitherOrBoth::Left(side) | EitherOrBoth::Right(side) => Self(VecDeque::from([side])),
        }
    }
}

impl<T> Default for Items<T> {
    fn default() -> Self {
        Self(VecDeque::default())
    }
}

impl<T> DoubleEndedIterator for Items<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.pop_back()
    }
}

impl<T> ExactSizeIterator for Items<T> {
    fn len(&self) -> usize {
        let (lower, upper) = self.size_hint();
        // Note: This assertion is overly defensive, but it checks the invariant
        // guaranteed by the trait. If this trait were rust-internal,
        // we could use debug_assert!; assert_eq! will check all Rust user
        // implementations too.
        std::assert_eq!(upper, Some(lower));
        lower
    }
}
impl<T> FusedIterator for Items<T> {}

impl<T> Iterator for Items<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop_front()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.iter().size_hint()
    }
}

impl<'a, T> IterEitherOrBoth<'a, T> {
    pub(crate) fn new(either_or_both: &'a EitherOrBoth<T>) -> Self {
        Self(either_or_both.as_ref().into())
    }
}

impl<T> DoubleEndedIterator for IterEitherOrBoth<'_, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IterEitherOrBoth<'_, T> {}
impl<T> FusedIterator for IterEitherOrBoth<'_, T> {}

impl<'a, T> Iterator for IterEitherOrBoth<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> IterMutEitherOrBoth<'a, T> {
    pub(crate) fn new(either_or_both: &'a mut EitherOrBoth<T>) -> Self {
        Self(either_or_both.as_mut().into())
    }
}

impl<T> DoubleEndedIterator for IterMutEitherOrBoth<'_, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IterMutEitherOrBoth<'_, T> {}
impl<T> FusedIterator for IterMutEitherOrBoth<'_, T> {}

impl<'a, T> Iterator for IterMutEitherOrBoth<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
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
            EitherOrBoth::Both(left, right) => (left.next_back(), right.next_back()).try_into().ok(),
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
            .bimap(Iterator::size_hint, Iterator::size_hint)
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
