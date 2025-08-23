//! The trait implementations if feature is `std`

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
