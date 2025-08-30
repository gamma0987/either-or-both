//! Iterators for [`EitherOrBoth`]

use core::iter::FusedIterator;

use crate::EitherOrBoth;

/// A iterator over the contained iterators of a uniform type chaining both iterators from
/// left to right
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChainedIterEitherOrBoth<T>(EitherOrBoth<T, T>);

/// An iterator over the contained values of a uniform type
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IntoIterEitherOrBoth<T>(Items<T>);

/// The `Items` used to iterate over an `EitherOrBoth`
#[derive(Debug, Clone, PartialEq, Eq)]
struct Items<T> {
    inner: EitherOrBoth<Option<T>>,
}

/// An iterator over the references to the contained values of a uniform type
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IterEitherOrBoth<'a, T>(Items<&'a T>);

/// An iterator over the mutable references to the contained values of a uniform type
#[derive(Debug, Default, PartialEq, Eq)]
pub struct IterMutEitherOrBoth<'a, T>(Items<&'a mut T>);

/// An iterator over `EitherOrBoths`
#[derive(Debug, Clone, PartialEq, Eq)]
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
            EitherOrBoth::Left(iter) | EitherOrBoth::Right(iter) => iter.next(),
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
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IntoIterEitherOrBoth<T> {}
impl<T> FusedIterator for IntoIterEitherOrBoth<T> {}

impl<T> Iterator for IntoIterEitherOrBoth<T> {
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

impl<T> From<EitherOrBoth<T>> for Items<T> {
    fn from(value: EitherOrBoth<T>) -> Self {
        Self {
            inner: value.map(Some),
        }
    }
}

impl<T> Default for Items<T> {
    fn default() -> Self {
        Self {
            inner: EitherOrBoth::Left(None),
        }
    }
}

impl<T> DoubleEndedIterator for Items<T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.inner.as_mut() {
            EitherOrBoth::Both(_, right) | EitherOrBoth::Right(right) if right.is_some() => {
                right.take()
            }
            EitherOrBoth::Both(left, None) | EitherOrBoth::Left(left) if left.is_some() => {
                left.take()
            }
            _ => None,
        }
    }
}

impl<T> ExactSizeIterator for Items<T> {}
impl<T> FusedIterator for Items<T> {}

impl<T> Iterator for Items<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.as_mut() {
            EitherOrBoth::Both(left, _) | EitherOrBoth::Left(left) if left.is_some() => left.take(),
            EitherOrBoth::Both(None, right) | EitherOrBoth::Right(right) if right.is_some() => {
                right.take()
            }
            _ => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match &self.inner {
            EitherOrBoth::Both(Some(_), Some(_)) => (2, Some(2)),
            EitherOrBoth::Left(Some(_))
            | EitherOrBoth::Right(Some(_))
            | EitherOrBoth::Both(None, Some(_))
            | EitherOrBoth::Both(Some(_), None) => (1, Some(1)),
            _ => (0, Some(0)),
        }
    }
}

impl<'a, T> IterEitherOrBoth<'a, T> {
    pub(crate) fn new(either_or_both: &'a EitherOrBoth<T>) -> Self {
        Self(either_or_both.as_ref().into())
    }
}

impl<T> DoubleEndedIterator for IterEitherOrBoth<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IterEitherOrBoth<'_, T> {}
impl<T> FusedIterator for IterEitherOrBoth<'_, T> {}

impl<'a, T> Iterator for IterEitherOrBoth<'a, T> {
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

impl<'a, T> IterMutEitherOrBoth<'a, T> {
    pub(crate) fn new(either_or_both: &'a mut EitherOrBoth<T>) -> Self {
        Self(either_or_both.as_mut().into())
    }
}

impl<T> DoubleEndedIterator for IterMutEitherOrBoth<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T> ExactSizeIterator for IterMutEitherOrBoth<'_, T> {}
impl<T> FusedIterator for IterMutEitherOrBoth<'_, T> {}

impl<'a, T> Iterator for IterMutEitherOrBoth<'a, T> {
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
                    l_lower.max(r_lower),
                    // is `None` if the left or right upper bound is `None`
                    l_upper.and_then(|l| r_upper.map(|r| l.max(r))),
                )
            })
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use EitherOrBoth::*;

    use super::*;

    #[rstest]
    #[case::both(Both(1, 2), [Some(1), Some(2), None])]
    #[case::left(Left(1), [Some(1), None, None])]
    #[case::right(Right(2), [Some(2), None, None])]
    fn items_next(#[case] either_or_both: EitherOrBoth<i32>, #[case] expected: [Option<i32>; 3]) {
        let mut expected_iter = expected.into_iter();
        let mut items = Items::from(either_or_both);
        assert_eq!(items.next(), expected_iter.next().unwrap());
        assert_eq!(items.next(), expected_iter.next().unwrap());
        assert_eq!(items.next(), expected_iter.next().unwrap());
    }

    #[rstest]
    #[case::both(Both(1, 2), [Some(2), Some(1), None])]
    #[case::left(Left(1), [Some(1), None, None])]
    #[case::right(Right(2), [Some(2), None, None])]
    fn items_next_back(
        #[case] either_or_both: EitherOrBoth<i32>,
        #[case] expected: [Option<i32>; 3],
    ) {
        let mut expected_iter = expected.into_iter();
        let mut items = Items::from(either_or_both);
        assert_eq!(items.next_back(), expected_iter.next().unwrap());
        assert_eq!(items.next_back(), expected_iter.next().unwrap());
        assert_eq!(items.next_back(), expected_iter.next().unwrap());
    }

    #[rstest]
    #[case::both(Both(1, 2), [(2, Some(2)), (1, Some(1)), (0, Some(0))])]
    #[case::left(Left(1), [(1, Some(1)), (0, Some(0)), (0, Some(0))])]
    #[case::right(Right(2), [(1, Some(1)), (0, Some(0)), (0, Some(0))])]
    fn items_size_hint(
        #[case] either_or_both: EitherOrBoth<i32>,
        #[case] expected: [(usize, Option<usize>); 3],
    ) {
        let mut expected_iter = expected.into_iter();
        let mut iter = Items::from(either_or_both);
        assert_eq!(iter.size_hint(), expected_iter.next().unwrap());
        iter.next();
        assert_eq!(iter.size_hint(), expected_iter.next().unwrap());
        iter.next();
        assert_eq!(iter.size_hint(), expected_iter.next().unwrap());
    }

    #[test]
    fn items_default() {
        assert_eq!(Items { inner: Left(None) }, Items::<i32>::default());
    }

    #[test]
    fn chained_iter_default() {
        assert_eq!(
            ChainedIterEitherOrBoth(Left('\0')),
            ChainedIterEitherOrBoth::<char>::default()
        );
    }
}
