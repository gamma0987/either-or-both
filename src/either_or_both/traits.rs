//! Trait implementations for `EitherOrBoth`

#[cfg(feature = "std")]
use std::vec;
#[cfg(feature = "std")]
use std::vec::Vec;

use crate::iter::{IntoIterEitherOrBoth, IterEitherOrBoth, IterMutEitherOrBoth};
#[cfg(feature = "either")]
use crate::Either;
use crate::{EitherOrBoth, TryFromOptionsError};

impl<T> IntoIterator for EitherOrBoth<T, T> {
    type Item = T;
    type IntoIter = IntoIterEitherOrBoth<T>;

    /// Returns a consuming iterator over the contained values of a uniform type
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To reverse the
    /// order use [`flip`].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Both('c', 'a');
    /// let mut iter = value.into_iter();
    /// assert_eq!(iter.next(), Some('c'));
    /// assert_eq!(iter.next(), Some('a'));
    /// assert_eq!(iter.next(), None);
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Left('c');
    /// let mut iter = value.into_iter();
    /// assert_eq!(iter.next(), Some('c'));
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// [`Both`]: EitherOrBoth::Both
    /// [`flip`]: EitherOrBoth::flip
    fn into_iter(self) -> Self::IntoIter {
        IntoIterEitherOrBoth::new(self)
    }
}

impl<'a, T> IntoIterator for &'a EitherOrBoth<T, T> {
    type Item = &'a T;
    type IntoIter = IterEitherOrBoth<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        IterEitherOrBoth::new(self)
    }
}

impl<'a, T> IntoIterator for &'a mut EitherOrBoth<T, T> {
    type Item = &'a mut T;
    type IntoIter = IterMutEitherOrBoth<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        IterMutEitherOrBoth::new(self)
    }
}

#[cfg(feature = "std")]
impl<L, R> FromIterator<EitherOrBoth<L, R>> for EitherOrBoth<Vec<L>, Vec<R>> {
    /// Takes each element in the [`Iterator`] collecting the left and right values into separate
    /// iterators
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let items: Vec<EitherOrBoth<u8, char>> =
    ///     vec![EitherOrBoth::Both(1, 'c'), EitherOrBoth::Left(2)];
    ///
    /// let collected: EitherOrBoth<_, _> = items
    ///     .iter()
    ///     .map(|e| e.bimap(|l| l + 1, |r| r.max('a')))
    ///     .collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Both(vec![2, 3], vec!['c']));
    /// ```
    fn from_iter<T: IntoIterator<Item = EitherOrBoth<L, R>>>(iter: T) -> Self {
        let (left, right) =
            iter.into_iter()
                .fold((vec![], vec![]), |(mut left, mut right), either_or_both| {
                    either_or_both.biapply(|l| left.push(l), |r| right.push(r));
                    (left, right)
                });

        match (left.is_empty(), right.is_empty()) {
            (true, true) | (false, false) => Self::Both(left, right),
            (true, false) => Self::Right(right),
            (false, true) => Self::Left(left),
        }
    }
}

/// TODO: TEST
#[cfg(all(feature = "std", feature = "either"))]
impl<L, R> FromIterator<Either<L, R>> for EitherOrBoth<Vec<L>, Vec<R>> {
    /// Takes each element in the [`Iterator`] collecting the left and right values into separate
    /// iterators yielding an [`EitherOrBoth`]
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let items: Vec<Either<u8, char>> = vec![Either::Left(1), Either::Right('c')];
    ///
    /// let collected: EitherOrBoth<_, _> = items
    ///     .iter()
    ///     .map(|e| e.bimap(|l| l + 1, |r| r.max('a')))
    ///     .collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Both(vec![2], vec!['c']));
    /// ```
    fn from_iter<T: IntoIterator<Item = Either<L, R>>>(iter: T) -> Self {
        let (left, right) =
            iter.into_iter()
                .fold((vec![], vec![]), |(mut left, mut right), either| {
                    either.biapply(|l| left.push(l), |r| right.push(r));
                    (left, right)
                });

        match (left.is_empty(), right.is_empty()) {
            (true, true) | (false, false) => Self::Both(left, right),
            (true, false) => Self::Right(right),
            (false, true) => Self::Left(left),
        }
    }
}

#[cfg(feature = "either")]
impl<L, R> From<Either<L, R>> for EitherOrBoth<L, R> {
    fn from(value: Either<L, R>) -> Self {
        match value {
            Either::Left(left) => Self::Left(left),
            Either::Right(right) => Self::Right(right),
        }
    }
}

impl<L, R> From<(L, R)> for EitherOrBoth<L, R> {
    fn from((left, right): (L, R)) -> Self {
        Self::Both(left, right)
    }
}

impl<L, R> From<(Option<L>, R)> for EitherOrBoth<L, R> {
    fn from((left, right): (Option<L>, R)) -> Self {
        match left {
            Some(left) => Self::Both(left, right),
            None => Self::Right(right),
        }
    }
}

impl<L, R> From<(L, Option<R>)> for EitherOrBoth<L, R> {
    fn from((left, right): (L, Option<R>)) -> Self {
        match right {
            Some(right) => Self::Both(left, right),
            None => Self::Left(left),
        }
    }
}

impl<L, R> TryFrom<(Option<L>, Option<R>)> for EitherOrBoth<L, R> {
    type Error = TryFromOptionsError;

    fn try_from(value: (Option<L>, Option<R>)) -> Result<Self, Self::Error> {
        match value {
            (None, None) => Err(TryFromOptionsError),
            (None, Some(right)) => Ok(Self::Right(right)),
            (Some(left), None) => Ok(Self::Left(left)),
            (Some(left), Some(right)) => Ok(Self::Both(left, right)),
        }
    }
}
