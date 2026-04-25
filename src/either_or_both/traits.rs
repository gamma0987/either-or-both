//! Trait implementations for `EitherOrBoth`

#[cfg(any(
    feature = "std",
    feature = "indexmap",
    all(feature = "std", feature = "either"),
    all(feature = "indexmap", feature = "either")
))]
macro_rules! collect {
    (
        $iter:expr;
        $cap:ident =>
        $new_left:expr,
        $new_right:expr; fold => |
        ($left:ident, $right:ident),
        $either:ident |
        $body:expr
    ) => {{
        let into_iter = $iter.into_iter();
        let $cap = match into_iter.size_hint() {
            (_, Some(upper)) => upper,
            (lower, None) => lower,
        };

        let (left, right) = into_iter.fold(
            ($new_left, $new_right),
            |(mut $left, mut $right), $either| $body,
        );

        match (left.is_empty(), right.is_empty()) {
            (true, true) | (false, false) => Self::Both(left, right),
            (true, false) => Self::Right(right),
            (false, true) => Self::Left(left),
        }
    }};
}

use core::cmp::Ordering;
use core::fmt::Display;
#[cfg(feature = "std")]
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    vec::Vec,
};

#[cfg(feature = "indexmap")]
use indexmap::{IndexMap, IndexSet};

use crate::iter::{IntoIterEitherOrBoth, IterEitherOrBoth, IterMutEitherOrBoth};
#[cfg(feature = "either")]
use crate::Either;
use crate::{EitherOrBoth, TryFromOptionsError};

impl<T> IntoIterator for EitherOrBoth<T, T> {
    type Item = T;
    type IntoIter = IntoIterEitherOrBoth<T>;

    /// Returns a consuming iterator over the contained values of a uniform type
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
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

////////////////////////////////////////////////////////////////////////////////
// FromIterator implementations for EitherOrBoth
////////////////////////////////////////////////////////////////////////////////

#[cfg(feature = "std")]
impl<L, R> FromIterator<EitherOrBoth<L, R>> for EitherOrBoth<Vec<L>, Vec<R>> {
    /// Consumes an [`Iterator`] of [`EitherOrBoth`] items, collecting all left and right
    /// values into separate [vectors].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`Vec`].
    /// - If only right values are present, returns [`Right`] with the right [`Vec`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`Vec`]s.
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::EitherOrBoth;
    ///
    /// let items: Vec<EitherOrBoth<u8, char>> =
    ///     vec![EitherOrBoth::Both(1, 'c'), EitherOrBoth::Left(2)];
    ///
    /// let collected: EitherOrBoth<Vec<u8>, Vec<char>> = items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Both(vec![1, 2], vec!['c']));
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::EitherOrBoth;
    ///
    /// let items: Vec<EitherOrBoth<u8, char>> = vec![EitherOrBoth::Left(2)];
    /// let collected: EitherOrBoth<Vec<u8>, Vec<char>> = items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Left(vec![2]));
    /// ```
    ///
    /// [vectors]: Vec
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T: IntoIterator<Item = EitherOrBoth<L, R>>>(iter: T) -> Self {
        collect!(
            iter;
            cap => Vec::with_capacity(cap), Vec::with_capacity(cap);
            fold => |(left, right), either| {
                either.biapply(|l| left.push(l), |r| right.push(r));
                (left, right)
        })
    }
}

#[cfg(feature = "std")]
impl<K1, K2, V1, V2, S1, S2> FromIterator<EitherOrBoth<(K1, V1), (K2, V2)>>
    for EitherOrBoth<HashMap<K1, V1, S1>, HashMap<K2, V2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`EitherOrBoth`] items, collecting all left and right
    /// values into separate [`HashMaps`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`HashMap`].
    /// - If only right values are present, returns [`Right`] with the right [`HashMap`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`HashMaps`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashMap;
    ///
    /// use either_or_both::EitherOrBoth;
    ///
    /// let items: Vec<EitherOrBoth<(&str, u8), (&str, char)>> = vec![
    ///     EitherOrBoth::Both(("both_l", 1), ("both_r", 'c')),
    ///     EitherOrBoth::Left(("left", 2)),
    /// ];
    ///
    /// let collected: EitherOrBoth<HashMap<&str, u8>, HashMap<&str, char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(
    ///         HashMap::from([("both_l", 1), ("left", 2)]),
    ///         HashMap::from([("both_r", 'c')])
    ///     )
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashMap;
    ///
    /// use either_or_both::EitherOrBoth;
    ///
    /// let items: Vec<EitherOrBoth<(&str, u8), (&str, char)>> =
    ///     vec![EitherOrBoth::Left(("left", 2))];
    ///
    /// let collected: EitherOrBoth<HashMap<&str, u8>, HashMap<&str, char>> =
    ///     items.into_iter(). collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Left(HashMap::from([("left", 2)]),));
    /// ```
    ///
    /// [`HashMaps`]: std::collections::HashMap
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = EitherOrBoth<(K1, V1), (K2, V2)>>,
    {
        collect!(
            iter;
            cap => HashMap::with_capacity_and_hasher(cap, S1::default()),
                   HashMap::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |(k1, v1)| {
                        left.insert(k1, v1);
                    },
                    |(k2, v2)| {
                        right.insert(k2, v2);
                    },
                );
                (left, right)
        })
    }
}

#[cfg(feature = "std")]
impl<K1, K2, S1, S2> FromIterator<EitherOrBoth<K1, K2>>
    for EitherOrBoth<HashSet<K1, S1>, HashSet<K2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`EitherOrBoth`] items, collecting all left and right
    /// values into separate [`HashSets`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`HashSet`].
    /// - If only right values are present, returns [`Right`] with the right [`HashSet`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`HashSets`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashSet;
    ///
    /// use either_or_both::EitherOrBoth;
    ///
    /// let items: Vec<EitherOrBoth<u8, char>> =
    ///     vec![EitherOrBoth::Both(1, 'c'), EitherOrBoth::Left(2)];
    ///
    /// let collected: EitherOrBoth<HashSet<u8>, HashSet<char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(HashSet::from([1, 2]), HashSet::from(['c']))
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashSet;
    ///
    /// use either_or_both::EitherOrBoth;
    ///
    /// let items: Vec<EitherOrBoth<u8, char>> = vec![EitherOrBoth::Left(2)];
    /// let collected: EitherOrBoth<HashSet<u8>, HashSet<char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Left(HashSet::from([2])));
    /// ```
    ///
    /// [`HashSets`]: std::collections::HashSet
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = EitherOrBoth<K1, K2>>,
    {
        collect!(
            iter;
            cap => HashSet::with_capacity_and_hasher(cap, S1::default()),
                   HashSet::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |k1| {
                        left.insert(k1);
                    },
                    |k2| {
                        right.insert(k2);
                    },
                );
                (left, right)
        })
    }
}

#[cfg(feature = "indexmap")]
impl<K1, K2, V1, V2, S1, S2> FromIterator<EitherOrBoth<(K1, V1), (K2, V2)>>
    for EitherOrBoth<IndexMap<K1, V1, S1>, IndexMap<K2, V2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`EitherOrBoth`] items, collecting all left and right
    /// values into separate [`IndexMaps`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`IndexMap`].
    /// - If only right values are present, returns [`Right`] with the right [`IndexMap`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`IndexMaps`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::EitherOrBoth;
    /// use indexmap::IndexMap;
    ///
    /// let items: Vec<EitherOrBoth<(&str, u8), (&str, char)>> = vec![
    ///     EitherOrBoth::Both(("both_l", 1), ("both_r", 'c')),
    ///     EitherOrBoth::Left(("left", 2)),
    /// ];
    ///
    /// let collected: EitherOrBoth<IndexMap<&str, u8>, IndexMap<&str, char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(
    ///         IndexMap::from([("both_l", 1), ("left", 2)]),
    ///         IndexMap::from([("both_r", 'c')])
    ///     )
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::EitherOrBoth;
    /// use indexmap::IndexMap;
    ///
    /// let items: Vec<EitherOrBoth<(&str, u8), (&str, char)>> =
    ///     vec![EitherOrBoth::Left(("left", 2))];
    /// let collected: EitherOrBoth<IndexMap<&str, u8>, IndexMap<&str, char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Left(IndexMap::from([("left", 2)]),)
    /// );
    /// ```
    ///
    /// [`IndexMaps`]: indexmap::IndexMap
    /// [`IndexMap`]: indexmap::IndexMap
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = EitherOrBoth<(K1, V1), (K2, V2)>>,
    {
        collect!(
            iter;
            cap => IndexMap::with_capacity_and_hasher(cap, S1::default()),
                   IndexMap::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |(k1, v1)| {
                        left.insert(k1, v1);
                    },
                    |(k2, v2)| {
                        right.insert(k2, v2);
                    },
                );
                (left, right)
        })
    }
}

#[cfg(feature = "indexmap")]
impl<K1, K2, S1, S2> FromIterator<EitherOrBoth<K1, K2>>
    for EitherOrBoth<IndexSet<K1, S1>, IndexSet<K2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`EitherOrBoth`] items, collecting all left and right
    /// values into separate [`IndexSets`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`IndexSet`].
    /// - If only right values are present, returns [`Right`] with the right [`IndexSet`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`IndexSets`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::EitherOrBoth;
    /// use indexmap::IndexSet;
    ///
    /// let items: Vec<EitherOrBoth<u8, char>> =
    ///     vec![EitherOrBoth::Both(1, 'c'), EitherOrBoth::Left(2)];
    ///
    /// let collected: EitherOrBoth<IndexSet<u8>, IndexSet<char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(IndexSet::from([1, 2]), IndexSet::from(['c']))
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::EitherOrBoth;
    /// use indexmap::IndexSet;
    ///
    /// let items: Vec<EitherOrBoth<u8, char>> = vec![EitherOrBoth::Left(2)];
    /// let collected: EitherOrBoth<IndexSet<u8>, IndexSet<char>> =
    ///    items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Left(IndexSet::from([2])));
    /// ```
    ///
    /// [`IndexSets`]: indexmap::IndexSet
    /// [`IndexSet`]: indexmap::IndexSet
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = EitherOrBoth<K1, K2>>,
    {
        collect!(
            iter;
            cap => indexmap::IndexSet::with_capacity_and_hasher(cap, S1::default()),
                   indexmap::IndexSet::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |k1| {
                        left.insert(k1);
                    },
                    |k2| {
                        right.insert(k2);
                    },
                );
                (left, right)
        })
    }
}

////////////////////////////////////////////////////////////////////////////////
// FromIterator implementations for Either
////////////////////////////////////////////////////////////////////////////////

#[cfg(all(feature = "either", feature = "std"))]
impl<L, R> FromIterator<Either<L, R>> for EitherOrBoth<Vec<L>, Vec<R>> {
    /// Consumes an [`Iterator`] of [`Either`] items, collecting all left and right values
    /// into separate [vectors].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`Vec`].
    /// - If only right values are present, returns [`Right`] with the right [`Vec`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`Vec`]s.
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let items: Vec<Either<u8, char>> = vec![Either::Left(1), Either::Right('c')];
    /// let collected: EitherOrBoth<Vec<u8>, Vec<char>> = items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Both(vec![1], vec!['c']));
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let items: Vec<Either<u8, char>> = vec![Either::Left(1), Either::Right('c')];
    /// let collected: EitherOrBoth<Vec<u8>, Vec<char>> = items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Both(vec![1], vec!['c']));
    /// ```
    ///
    /// [vectors]: Vec
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T: IntoIterator<Item = Either<L, R>>>(iter: T) -> Self {
        collect!(
            iter;
            cap => Vec::with_capacity(cap), Vec::with_capacity(cap);
            fold => |(left, right), either| {
                either.biapply(|l| left.push(l), |r| right.push(r));
                (left, right)
        })
    }
}

#[cfg(all(feature = "either", feature = "std"))]
impl<K1, K2, V1, V2, S1, S2> FromIterator<Either<(K1, V1), (K2, V2)>>
    for EitherOrBoth<HashMap<K1, V1, S1>, HashMap<K2, V2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`Either`] items, collecting all left and right values
    /// into separate [`HashMaps`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`HashMap`].
    /// - If only right values are present, returns [`Right`] with the right [`HashMap`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`HashMaps`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashMap;
    ///
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let items: Vec<Either<(&str, u8), (&str, char)>> =
    ///     vec![Either::Right(("right", 'c')), Either::Left(("left", 2))];
    ///
    /// let collected: EitherOrBoth<HashMap<&str, u8>, HashMap<&str, char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(
    ///         HashMap::from([("left", 2)]),
    ///         HashMap::from([("right", 'c')])
    ///     )
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashMap;
    ///
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let items: Vec<Either<(&str, u8), (&str, char)>> =
    ///     vec![Either::Left(("left", 2))];
    /// let collected: EitherOrBoth<HashMap<&str, u8>, HashMap<&str, char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Left(HashMap::from([("left", 2)]),));
    /// ```
    ///
    /// [`HashMaps`]: std::collections::HashMap
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Either<(K1, V1), (K2, V2)>>,
    {
        collect!(
            iter;
            cap => HashMap::with_capacity_and_hasher(cap, S1::default()),
                   HashMap::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |(k1, v1)| {
                        left.insert(k1, v1);
                    },
                    |(k2, v2)| {
                        right.insert(k2, v2);
                    },
                );
                (left, right)
        })
    }
}

#[cfg(all(feature = "either", feature = "std"))]
impl<K1, K2, S1, S2> FromIterator<Either<K1, K2>> for EitherOrBoth<HashSet<K1, S1>, HashSet<K2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`Either`] items, collecting all left and right values
    /// into separate [`HashSets`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`HashSet`].
    /// - If only right values are present, returns [`Right`] with the right [`HashSet`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`HashSets`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashSet;
    ///
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let items: Vec<Either<u8, char>> = vec![Either::Right('c'), Either::Left(2)];
    ///
    /// let collected: EitherOrBoth<HashSet<u8>, HashSet<char>> =
    ///    items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(HashSet::from([2]), HashSet::from(['c']))
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use std::collections::HashSet;
    ///
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let items: Vec<Either<u8, char>> = vec![Either::Left(2)];
    /// let collected: EitherOrBoth<HashSet<u8>, HashSet<char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Left(HashSet::from([2])));
    /// ```
    ///
    /// [`HashSets`]: std::collections::HashSet
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Either<K1, K2>>,
    {
        collect!(
            iter;
            cap => HashSet::with_capacity_and_hasher(cap, S1::default()),
                   HashSet::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |k1| {
                        left.insert(k1);
                    },
                    |k2| {
                        right.insert(k2);
                    },
                );
                (left, right)
        })
    }
}

#[cfg(all(feature = "either", feature = "indexmap"))]
impl<K1, K2, V1, V2, S1, S2> FromIterator<Either<(K1, V1), (K2, V2)>>
    for EitherOrBoth<IndexMap<K1, V1, S1>, IndexMap<K2, V2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`Either`] items, collecting all left and right values
    /// into separate [`IndexMaps`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`IndexMap`].
    /// - If only right values are present, returns [`Right`] with the right [`IndexMap`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`IndexMaps`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::{Either, EitherOrBoth};
    /// use indexmap::IndexMap;
    ///
    /// let items: Vec<Either<(&str, u8), (&str, char)>> =
    ///     vec![Either::Right(("right", 'c')), Either::Left(("left", 2))];
    ///
    /// let collected: EitherOrBoth<IndexMap<&str, u8>, IndexMap<&str, char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(
    ///         IndexMap::from([("left", 2)]),
    ///         IndexMap::from([("right", 'c')])
    ///     )
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::{Either, EitherOrBoth};
    /// use indexmap::IndexMap;
    ///
    /// let items: Vec<Either<(&str, u8), (&str, char)>> =
    ///     vec![Either::Left(("left", 2))];
    /// let collected: EitherOrBoth<IndexMap<&str, u8>, IndexMap<&str, char>> =
    ///     items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Left(IndexMap::from([("left", 2)]),)
    /// );
    /// ```
    ///
    /// [`IndexMaps`]: indexmap::IndexMap
    /// [`IndexMap`]: indexmap::IndexMap
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Either<(K1, V1), (K2, V2)>>,
    {
        collect!(
            iter;
            cap => indexmap::IndexMap::with_capacity_and_hasher(cap, S1::default()),
                   indexmap::IndexMap::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |(k1, v1)| {
                        left.insert(k1, v1);
                    },
                    |(k2, v2)| {
                        right.insert(k2, v2);
                    },
                );
                (left, right)
        })
    }
}

#[cfg(all(feature = "either", feature = "indexmap"))]
impl<K1, K2, S1, S2> FromIterator<Either<K1, K2>>
    for EitherOrBoth<IndexSet<K1, S1>, IndexSet<K2, S2>>
where
    K1: core::hash::Hash + Eq,
    K2: core::hash::Hash + Eq,
    S1: core::hash::BuildHasher + Default,
    S2: core::hash::BuildHasher + Default,
{
    /// Consumes an [`Iterator`] of [`Either`] items, collecting all left and right values
    /// into separate [`IndexSets`].
    ///
    /// - If only left values are present, returns [`Left`] with the left [`IndexSet`].
    /// - If only right values are present, returns [`Right`] with the right [`IndexSet`].
    /// - If none or both left and right values are present, returns [`Both`] with both
    ///   [`IndexSets`].
    ///
    /// # Examples
    ///
    /// This example collects into a [`Both`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::{Either, EitherOrBoth};
    /// use indexmap::IndexSet;
    ///
    /// let items: Vec<Either<u8, char>> = vec![Either::Right('c'), Either::Left(2)];
    /// let collected: EitherOrBoth<IndexSet<u8>, IndexSet<char>> =
    ///    items.into_iter().collect();
    ///
    /// assert_eq!(
    ///     collected,
    ///     EitherOrBoth::Both(IndexSet::from([2]), IndexSet::from(['c']))
    /// );
    /// ```
    ///
    /// This example collects into a [`Left`] variant:
    #[cfg_attr(feature = "std", doc = "```rust")]
    #[cfg_attr(not(feature = "std"), doc = "```rust,ignore")]
    /// use either_or_both::{Either, EitherOrBoth};
    /// use indexmap::IndexSet;
    ///
    /// let items: Vec<Either<u8, char>> = vec![Either::Left(2)];
    /// let collected: EitherOrBoth<IndexSet<u8>, IndexSet<char>> =
    ///    items.into_iter().collect();
    ///
    /// assert_eq!(collected, EitherOrBoth::Left(IndexSet::from([2])));
    /// ```
    ///
    /// [`IndexSets`]: indexmap::IndexSet
    /// [`IndexSet`]: indexmap::IndexSet
    /// [`Left`]: EitherOrBoth::Left
    /// [`Right`]: EitherOrBoth::Right
    /// [`Both`]: EitherOrBoth::Both
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Either<K1, K2>>,
    {
        collect!(
            iter;
            cap => IndexSet::with_capacity_and_hasher(cap, S1::default()),
                   IndexSet::with_capacity_and_hasher(cap, S2::default());
            fold => |(left, right), either| {
                either.biapply(
                    |k1| {
                        left.insert(k1);
                    },
                    |k2| {
                        right.insert(k2);
                    },
                );
                (left, right)
        })
    }
}

////////////////////////////////////////////////////////////////////////////////
// From/TryFrom/... implementations
////////////////////////////////////////////////////////////////////////////////

#[cfg(feature = "std")]
impl<L, R> EitherOrBoth<L, R>
where
    L: Error,
    R: Error,
{
    /// Returns both error sources as a pair, complementing [`Error::source`]
    ///
    /// Since [`Error::source`] returns only the left source from the `Both` variant this
    /// method can be used to retrieve both sources, if any.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::fmt::{Display, Formatter, Result};
    /// use std::error::Error;
    ///
    /// use either_or_both::EitherOrBoth;
    ///
    /// #[derive(Debug)]
    /// struct SuperError {};
    /// impl Error for SuperError {};
    /// impl Display for SuperError {
    ///     fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    ///         f.write_str("super error")
    ///     }
    /// }
    ///
    /// #[derive(Debug)]
    /// struct ErrLeft {
    ///     source: Option<SuperError>,
    /// };
    /// impl Display for ErrLeft {
    ///     fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    ///         f.write_str("left")
    ///     }
    /// }
    /// impl Error for ErrLeft {
    ///     fn source(&self) -> Option<&(dyn Error + 'static)> {
    ///         self.source.as_ref().map(|e| e as &dyn Error)
    ///     }
    /// }
    ///
    /// #[derive(Debug)]
    /// struct ErrRight {
    ///     source: Option<SuperError>,
    /// };
    /// impl Display for ErrRight {
    ///     fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    ///         f.write_str("right")
    ///     }
    /// }
    /// impl Error for ErrRight {
    ///     fn source(&self) -> Option<&(dyn Error + 'static)> {
    ///         self.source.as_ref().map(|e| e as &dyn Error)
    ///     }
    /// }
    ///
    /// let left: EitherOrBoth<ErrLeft, ErrRight> = EitherOrBoth::Left(ErrLeft { source: None });
    /// assert!(matches!(left.error_sources(), (None, None)));
    ///
    /// let right: EitherOrBoth<ErrLeft, ErrRight> = EitherOrBoth::Right(ErrRight {
    ///     source: Some(SuperError {}),
    /// });
    /// assert!(matches!(right.error_sources(), (None, Some(_))));
    ///
    /// let both: EitherOrBoth<ErrLeft, ErrRight> = EitherOrBoth::Both(
    ///     ErrLeft { source: None },
    ///     ErrRight {
    ///         source: Some(SuperError {}),
    ///     },
    /// );
    /// assert!(matches!(both.error_sources(), (None, Some(_))));
    /// ```
    ///
    /// [`Error::source`]: std::error::Error::source
    pub fn error_sources(
        &self,
    ) -> (
        Option<&(dyn Error + 'static)>,
        Option<&(dyn Error + 'static)>,
    ) {
        match self {
            Self::Left(l) => (l.source(), None),
            Self::Right(r) => (None, r.source()),
            Self::Both(l, r) => (l.source(), r.source()),
        }
    }
}

impl<L, R> Display for EitherOrBoth<L, R>
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Both(l, r) => write!(f, "{l}\n{r}"),
            Self::Left(l) => l.fmt(f),
            Self::Right(r) => r.fmt(f),
        }
    }
}

#[cfg(feature = "std")]
impl<L, R> Error for EitherOrBoth<L, R>
where
    L: Error,
    R: Error,
{
    /// Returns the lower-level source of this `EitherOrBoth` error, if any
    ///
    /// In case there are two error sources from the `Both` variant, the left source takes
    /// precedence. If you need both error sources use [`EitherOrBoth::error_sources`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::fmt::{Display, Formatter, Result};
    /// use std::error::Error;
    ///
    /// use either_or_both::EitherOrBoth;
    ///
    /// #[derive(Debug)]
    /// struct SuperError {}
    /// impl Error for SuperError {}
    /// impl Display for SuperError {
    ///     fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    ///         f.write_str("super error")
    ///     }
    /// }
    ///
    /// #[derive(Debug)]
    /// struct SomeError {
    ///     source: Option<SuperError>,
    /// };
    /// impl Display for SomeError {
    ///     fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    ///         f.write_str("some error")
    ///     }
    /// }
    /// impl Error for SomeError {
    ///     fn source(&self) -> Option<&(dyn Error + 'static)> {
    ///         self.source.as_ref().map(|e| e as &dyn Error)
    ///     }
    /// }
    ///
    /// let left: EitherOrBoth<SomeError> = EitherOrBoth::Left(SomeError { source: None });
    /// assert!(matches!(left.source(), None));
    ///
    /// let right: EitherOrBoth<SomeError> = EitherOrBoth::Right(SomeError {
    ///     source: Some(SuperError {}),
    /// });
    /// assert!(matches!(right.source(), Some(_)));
    ///
    /// // Since left has no source, this method returns None even if the right error has a source
    /// let both: EitherOrBoth<SomeError> = EitherOrBoth::Both(
    ///     SomeError { source: None },
    ///     SomeError {
    ///         source: Some(SuperError {}),
    ///     },
    /// );
    /// assert!(matches!(both.source(), None));
    /// ```
    ///
    /// [`Error::source`]: std::error::Error::source
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Right(r) => r.source(),
            Self::Left(l) | Self::Both(l, _) => l.source(),
        }
    }
}

impl<L, R> Ord for EitherOrBoth<L, R>
where
    L: Ord,
    R: Ord,
{
    /// Compares two [`EitherOrBoth`] values using the ordering `Left < Both < Right`.
    ///
    /// This differs from a derived `Ord` implementation, which would order by discriminant
    /// (`Both < Left < Right`). When both values are the same variant, the inner values
    /// are compared directly. For the [`Both`] variant, the left values are compared
    /// first, and only if equal are the right values compared.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// // Left is less than Both and Right
    /// assert!(EitherOrBoth::<u8>::Left(5) < EitherOrBoth::Both(1, 1));
    /// assert!(EitherOrBoth::<u8>::Left(5) < EitherOrBoth::Right(1));
    ///
    /// // Both is less than Right
    /// assert!(EitherOrBoth::Both(1, 1) < EitherOrBoth::<u8, u8>::Right(1));
    ///
    /// // Same variants compare inner values
    /// assert!(EitherOrBoth::<u8>::Left(1) < EitherOrBoth::Left(2));
    /// assert!(EitherOrBoth::<u8>::Right(1) < EitherOrBoth::Right(2));
    /// assert!(EitherOrBoth::Both(1, 1) < EitherOrBoth::Both(2, 1));
    /// assert!(EitherOrBoth::Both(1, 1) < EitherOrBoth::Both(1, 2));
    /// ```
    ///
    /// [`Both`]: EitherOrBoth::Both
    fn cmp(&self, other: &Self) -> Ordering {
        // The implementation of Left < Both < Right
        match (self, other) {
            (Self::Left(a), Self::Left(b)) => a.cmp(b),
            (Self::Left(_), _) | (Self::Both(_, _), Self::Right(_)) => Ordering::Less,
            (Self::Both(l1, r1), Self::Both(l2, r2)) => l1.cmp(l2).then(r1.cmp(r2)),
            (Self::Right(a), Self::Right(b)) => a.cmp(b),
            (Self::Both(_, _), Self::Left(_)) | (Self::Right(_), _) => Ordering::Greater,
        }
    }
}

impl<L, R> PartialOrd for EitherOrBoth<L, R>
where
    L: Ord,
    R: Ord,
{
    /// Compares two [`EitherOrBoth`] values using the same ordering as [`Ord::cmp`].
    ///
    /// [`EitherOrBoth`] always has a total ordering, so this always returns `Some`.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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
