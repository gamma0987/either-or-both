//! `EitherOrBoth` with different types

macro_rules! map_each {
    ($src:expr; $left:pat, $right:pat => $left_expr:expr, $right_expr:expr; $( $rest:tt )*) => {
        match $src {
            Self::Both($left, $right) => EitherOrBoth::Both($left_expr, $right_expr),
            Self::Left($left) => EitherOrBoth::Left($left_expr),
            Self::Right($right) => EitherOrBoth::Right($right_expr),
        } $( $rest )*
    };
    ($src:expr; $left:pat, $right:pat => $left_expr:expr, $right_expr:expr) => {
        match $src {
            Self::Both($left, $right) => EitherOrBoth::Both($left_expr, $right_expr),
            Self::Left($left) => EitherOrBoth::Left($left_expr),
            Self::Right($right) => EitherOrBoth::Right($right_expr),
        }
    };
    ($src:expr) => {
        match $src {
            Self::Both(left, right) => EitherOrBoth::Both(left, right),
            Self::Left(left) => EitherOrBoth::Left(left),
            Self::Right(right) => EitherOrBoth::Right(right),
        }
    };
}

pub mod iter;
pub mod traits;

use core::ops::{Deref, DerefMut};
use core::pin::Pin;
use core::{mem, ptr};

use iter::{ChainedIterEitherOrBoth, IterEitherOrBoth, IterMutEitherOrBoth, SwapIterEitherOrBoth};

#[cfg(feature = "either")]
use crate::either::Either;
use crate::unwrap_failed;

/// Either left or right or both can be present
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "c_repr", repr(C))]
#[cfg_attr(feature = "serde", allow(clippy::unsafe_derive_deserialize))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum EitherOrBoth<L, R = L> {
    /// Both values are present
    Both(L, R),
    /// The left value
    Left(L),
    /// The right value
    Right(R),
}

impl<L, R> EitherOrBoth<L, R> {
    /// Returns true if `Left` or `Both`
    pub fn has_left(&self) -> bool {
        match self {
            Self::Left(_) | Self::Both(_, _) => true,
            Self::Right(_) => false,
        }
    }

    /// Returns true if `Left` or `Both` and the left value matches a predicate
    pub fn has_left_and<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Left(left) | Self::Both(left, _) => f(left),
            Self::Right(_) => false,
        }
    }

    /// Returns true if `Left` or `Both` or the `Right` value matches a predicate
    pub fn has_left_or<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Left(_) | Self::Both(_, _) => true,
            Self::Right(right) => f(right),
        }
    }

    /// Returns true if `Right` or `Both`
    pub fn has_right(&self) -> bool {
        match self {
            Self::Right(_) | Self::Both(_, _) => true,
            Self::Left(_) => false,
        }
    }

    /// Returns true if `Right` or `Both` and the right value matches a predicate
    pub fn has_right_and<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Right(right) | Self::Both(_, right) => f(right),
            Self::Left(_) => false,
        }
    }

    /// Returns true if `Right` or `Both` or the `Left` value matches a predicate
    pub fn has_right_or<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Right(_) | Self::Both(_, _) => true,
            Self::Left(left) => f(left),
        }
    }

    /// Returns true if `Both`
    pub fn is_both(&self) -> bool {
        matches!(self, Self::Both(_, _))
    }

    /// Returns true if `Both` and both values match their respective predicate
    pub fn is_both_and<F, G>(self, f: F, g: G) -> bool
    where
        F: FnOnce(L) -> bool,
        G: FnOnce(R) -> bool,
    {
        match self {
            Self::Both(left, right) => f(left) && g(right),
            _ => false,
        }
    }

    /// Returns true if `Both` or if `Left` or `Right` match their respective predicate
    pub fn is_both_or<F, G>(self, f: F, g: G) -> bool
    where
        F: FnOnce(L) -> bool,
        G: FnOnce(R) -> bool,
    {
        match self {
            Self::Both(_, _) => true,
            Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }
    }

    /// Returns true if `Left` (but not `Both`)
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left(_))
    }

    /// Returns true if `Left` (but not `Both`) and it matches a predicate
    pub fn is_left_and<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Left(left) => f(left),
            _ => false,
        }
    }

    /// Returns true if `Left` (but not `Both`) or the right value matches a predicate
    pub fn is_left_or<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Left(_) => true,
            Self::Both(_, right) | Self::Right(right) => f(right),
        }
    }

    /// Returns true if `Right` (but not `Both`)
    pub fn is_right(&self) -> bool {
        matches!(self, Self::Right(_))
    }

    /// Returns true if `right` (but not `Both`) and it matches a predicate
    pub fn is_right_and<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Right(right) => f(right),
            _ => false,
        }
    }

    /// Returns true if `right` (but not `Both`) or the right value matches a predicate
    pub fn is_right_or<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Right(_) => true,
            Self::Both(left, _) | Self::Left(left) => f(left),
        }
    }

    /// Converts from `&EitherOrBoth<L, R>` to `EitherOrBoth<&L, &R>`
    pub fn as_ref(&self) -> EitherOrBoth<&L, &R> {
        map_each!(self)
    }

    /// Converts from `&mut EitherOrBoth<L, R>` to `EitherOrBoth<&mut L, &mut R>`
    pub fn as_mut(&mut self) -> EitherOrBoth<&mut L, &mut R> {
        map_each!(self)
    }

    /// Converts from `EitherOrBoth<L, R>` to `EitherOrBoth<&L::Target, &R::Target>`.
    ///
    /// This method leaves the original `EitherOrBoth` unchanged, creating a new one with a
    /// reference to the original one, additionally coercing the contents via Deref.
    pub fn as_deref(&self) -> EitherOrBoth<&<L as Deref>::Target, &<R as Deref>::Target>
    where
        L: Deref,
        R: Deref,
    {
        map_each!(self; l, r => &**l, &**r)
    }

    /// Converts from `EitherOrBoth<L, R>` to `EitherOrBoth<&mut L::Target, &mut R::Target>`.
    ///
    /// This method leaves the original `EitherOrBoth` unchanged, creating a new one containing a
    /// mutable reference to the inner type’s `Deref::Target` type.
    pub fn as_deref_mut(
        &mut self,
    ) -> EitherOrBoth<&mut <L as Deref>::Target, &mut <R as Deref>::Target>
    where
        L: DerefMut,
        R: DerefMut,
    {
        map_each!(self; l, r => &mut **l, &mut **r)
    }

    /// Converts from `Pin<&EitherOrBoth<L, R>>` to `EitherOrBoth<Pin<&L>, Pin<&R>>`.
    pub fn as_pin_ref(self: Pin<&Self>) -> EitherOrBoth<Pin<&L>, Pin<&R>> {
        // SAFETY: `x` is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            map_each!(Pin::get_ref(self); l, r => Pin::new_unchecked(l), Pin::new_unchecked(r))
        }
    }

    /// Converts from `Pin<&mut EitherOrBoth<L, R>>` to `EitherOrBoth<Pin<&mut L>, Pin<&mut R>>`.
    pub fn as_pin_mut(self: Pin<&mut Self>) -> EitherOrBoth<Pin<&mut L>, Pin<&mut R>> {
        // SAFETY: `get_unchecked_mut` is never used to move the `EitherOrBoth` inside `self`. `x`
        // is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            map_each!(
                Pin::get_unchecked_mut(self); l, r => Pin::new_unchecked(l), Pin::new_unchecked(r)
            )
        }
    }

    /// Returns the contained `Both` values consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if only a `Left` or `Right` value is present with a custom panic message provided by
    /// `msg`
    pub fn expect_both(self, msg: &str) -> (L, R) {
        match self {
            Self::Both(left, right) => (left, right),
            _ => unwrap_failed(msg),
        }
    }

    /// Returns the contained left value if `Left` or `Both` consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if there is no left value present with a custom panic message provided by `msg`
    pub fn expect_left(self, msg: &str) -> L {
        match self {
            Self::Both(left, _) | Self::Left(left) => left,
            Self::Right(_) => unwrap_failed(msg),
        }
    }

    /// Returns the contained `Left` value consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is not `Left` with a custom panic message provided by `msg`
    pub fn expect_only_left(self, msg: &str) -> L {
        match self {
            Self::Left(left) => left,
            _ => unwrap_failed(msg),
        }
    }

    /// Returns the contained right value if `Right` or `Both` consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if there is no right value present with a custom panic message provided by `msg`
    pub fn expect_right(self, msg: &str) -> R {
        match self {
            Self::Both(_, right) | Self::Right(right) => right,
            Self::Left(_) => unwrap_failed(msg),
        }
    }

    /// Returns the contained `Right` value consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is not `Right` with a custom panic message provided by `msg`
    pub fn expect_only_right(self, msg: &str) -> R {
        match self {
            Self::Right(right) => right,
            _ => unwrap_failed(msg),
        }
    }

    /// Returns the contained `Both` value as tuple consuming `self`
    pub fn unwrap_both(self) -> (L, R) {
        self.expect_both("Called `EitherOrBoth::unwrap_both` on a `Left` or `Right` value")
    }

    /// TODO: DOCS
    ///
    /// # SAFETY
    pub unsafe fn unwrap_both_unchecked(self) -> (L, R) {
        match self {
            Self::Both(left, right) => (left, right),
            // SAFETY: the safety contract must be upheld by the caller.
            // cov:excl-start
            _ => unsafe { core::hint::unreachable_unchecked() },
            // cov:excl-stop
        }
    }

    /// Returns the contained left value consuming `self`
    pub fn unwrap_left(self) -> L {
        self.expect_left("Called `EitherOrBoth::unwrap_left` on a `Right` value")
    }

    /// TODO: DOCS
    ///
    /// # SAFETY
    pub unsafe fn unwrap_left_unchecked(self) -> L {
        match self {
            Self::Both(left, _) | Self::Left(left) => left,
            // SAFETY: the safety contract must be upheld by the caller.
            // cov:excl-start
            Self::Right(_) => unsafe { core::hint::unreachable_unchecked() },
            // cov:excl-stop
        }
    }

    /// Returns the contained `Left` value consuming `self`
    pub fn unwrap_only_left(self) -> L {
        self.expect_only_left("Called `EitherOrBoth::unwrap_only_left` on a `Both` or `Right` value")
    }

    /// TODO: DOCS
    ///
    /// # SAFETY
    pub unsafe fn unwrap_only_left_unchecked(self) -> L {
        match self {
            Self::Left(left) => left,
            // SAFETY: the safety contract must be upheld by the caller.
            // cov:excl-start
            _ => unsafe { core::hint::unreachable_unchecked() },
            // cov:excl-stop
        }
    }

    /// Returns the contained right value consuming `self`
    pub fn unwrap_right(self) -> R {
        self.expect_right("Called EitherOrBoth::unwrap_right` on a `Left` value")
    }

    /// TODO: DOCS
    ///
    /// # SAFETY
    pub unsafe fn unwrap_right_unchecked(self) -> R {
        match self {
            Self::Both(_, right) | Self::Right(right) => right,
            // SAFETY: the safety contract must be upheld by the caller.
            // cov:excl-start
            Self::Left(_) => unsafe { core::hint::unreachable_unchecked() },
            // cov:excl-stop
        }
    }

    /// Returns the contained `Right` value consuming `self`
    pub fn unwrap_only_right(self) -> R {
        self.expect_only_right("Called EitherOrBoth::unwrap_only_right` on a `Both` or `Left` value")
    }

    /// TODO: DOCS
    ///
    /// # SAFETY
    pub unsafe fn unwrap_only_right_unchecked(self) -> R {
        match self {
            Self::Right(right) => right,
            // SAFETY: the safety contract must be upheld by the caller.
            // cov:excl-start
            _ => unsafe { core::hint::unreachable_unchecked() },
            // cov:excl-stop
        }
    }

    /// If both values are present, return `Some` containing the values otherwise return `None`
    pub fn both(self) -> Option<(L, R)> {
        match self {
            Self::Both(left, right) => Some((left, right)),
            _ => None,
        }
    }

    /// If a left value is present, return `Some` containing the value otherwise return `None`
    pub fn left(self) -> Option<L> {
        match self {
            Self::Left(left) | Self::Both(left, _) => Some(left),
            Self::Right(_) => None,
        }
    }

    /// Returns `Right` otherwise returns `other`
    pub fn left_and<T>(self, other: EitherOrBoth<T, R>) -> EitherOrBoth<T, R> {
        match self {
            Self::Left(_) | Self::Both(_, _) => other,
            Self::Right(right) => EitherOrBoth::Right(right),
        }
    }

    /// Returns `Right` otherwise calls `f` with the left value and returns the result
    pub fn left_and_then<F, T>(self, f: F) -> EitherOrBoth<T, R>
    where
        F: FnOnce(L) -> EitherOrBoth<T, R>,
    {
        match self {
            Self::Left(left) | Self::Both(left, _) => f(left),
            Self::Right(right) => EitherOrBoth::Right(right),
        }
    }

    /// Returns the `Left` value in a `Some` if present otherwise `None`
    pub fn only_left(self) -> Option<L> {
        match self {
            Self::Left(left) => Some(left),
            _ => None,
        }
    }

    /// If a right value is present, return `Some` containing the value otherwise return `None`
    pub fn right(self) -> Option<R> {
        match self {
            Self::Right(right) | Self::Both(_, right) => Some(right),
            Self::Left(_) => None,
        }
    }

    /// Returns `Left` otherwise returns `other`
    pub fn right_and<T>(self, other: EitherOrBoth<L, T>) -> EitherOrBoth<L, T> {
        match self {
            Self::Right(_) | Self::Both(_, _) => other,
            Self::Left(left) => EitherOrBoth::Left(left),
        }
    }

    /// Returns `Left` otherwise calls `f` with the right value and returns the result
    pub fn right_and_then<F, T>(self, f: F) -> EitherOrBoth<L, T>
    where
        F: FnOnce(R) -> EitherOrBoth<L, T>,
    {
        match self {
            Self::Right(right) | Self::Both(_, right) => f(right),
            Self::Left(left) => EitherOrBoth::Left(left),
        }
    }

    /// Returns the `Right` value in a `Some` if present otherwise `None`
    pub fn only_right(self) -> Option<R> {
        match self {
            Self::Right(right) => Some(right),
            _ => None,
        }
    }

    /// Returns both values if present, otherwise returns `Some` with the `Left` or `Right` value
    pub fn unzip(self) -> (Option<L>, Option<R>) {
        match self {
            Self::Both(left, right) => (Some(left), Some(right)),
            Self::Left(left) => (Some(left), None),
            Self::Right(right) => (None, Some(right)),
        }
    }

    /// Converts `EitherOrBoth<L, R>` to `EitherOrBoth<R, L>`.
    pub fn flip(self) -> EitherOrBoth<R, L> {
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(right, left),
            Self::Left(left) => EitherOrBoth::Right(left),
            Self::Right(right) => EitherOrBoth::Left(right),
        }
    }

    /// TODO: DOCS
    pub fn into_iter_swap(
        self,
    ) -> SwapIterEitherOrBoth<<L as IntoIterator>::IntoIter, <R as IntoIterator>::IntoIter>
    where
        L: IntoIterator,
        R: IntoIterator,
    {
        SwapIterEitherOrBoth::new(self.bimap(IntoIterator::into_iter, IntoIterator::into_iter))
    }

    /// TODO: DOCS
    pub fn iter_swap(
        &self,
    ) -> SwapIterEitherOrBoth<<&L as IntoIterator>::IntoIter, <&R as IntoIterator>::IntoIter>
    where
        for<'a> &'a L: IntoIterator,
        for<'a> &'a R: IntoIterator,
    {
        SwapIterEitherOrBoth::new(
            self.as_ref()
                .bimap(IntoIterator::into_iter, IntoIterator::into_iter),
        )
    }

    /// TODO: DOCS
    pub fn iter_swap_mut(
        &mut self,
    ) -> SwapIterEitherOrBoth<<&mut L as IntoIterator>::IntoIter, <&mut R as IntoIterator>::IntoIter>
    where
        for<'a> &'a mut L: IntoIterator,
        for<'a> &'a mut R: IntoIterator,
    {
        SwapIterEitherOrBoth::new(
            self.as_mut()
                .bimap(IntoIterator::into_iter, IntoIterator::into_iter),
        )
    }

    /// Applies mapping functions to the left and right values returning the result
    pub fn bimap<F, G, T, U>(self, f: F, g: G) -> EitherOrBoth<T, U>
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> U,
    {
        map_each!(self; l, r => f(l), g(r))
    }

    /// Applies a mapping function to the left value in `Both` or `Left` and returns the result
    pub fn map_left<F, T>(self, f: F) -> EitherOrBoth<T, R>
    where
        F: FnOnce(L) -> T,
    {
        map_each!(self; l, r => f(l), r)
    }

    /// TODO: DOCS
    pub fn map_left_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(L) -> U,
    {
        self.map_left_or_else(|| default, f)
    }

    /// TODO: DOCS
    pub fn map_left_or_default<T, F>(self, f: F) -> T
    where
        T: Default,
        F: FnOnce(L) -> T,
    {
        self.map_left_or_else(T::default, f)
    }

    /// TODO: DOCS
    pub fn map_left_or_else<U, D, F>(self, default: D, f: F) -> U
    where
        D: FnOnce() -> U,
        F: FnOnce(L) -> U,
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => default(),
        }
    }

    /// Applies a mapping function to the right value in `Both` or `Right` and returns the result
    pub fn map_right<T>(self, f: fn(R) -> T) -> EitherOrBoth<L, T> {
        map_each!(self; l, r => l, f(r))
    }

    /// TODO: DOCS
    pub fn map_right_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(R) -> U,
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => default,
        }
    }

    /// TODO: DOCS
    pub fn map_right_or_default<T, F>(self, f: F) -> T
    where
        T: Default,
        F: FnOnce(R) -> T,
    {
        self.map_right_or_else(T::default, f)
    }

    /// TODO: DOCS
    pub fn map_right_or_else<U, D, F>(self, default: D, f: F) -> U
    where
        D: FnOnce() -> U,
        F: FnOnce(R) -> U,
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => default(),
        }
    }

    /// TODO: DOCS
    pub fn biinspect<F, G>(self, f: F, g: G) -> Self
    where
        for<'a> F: FnOnce(&'a L),
        for<'a> G: FnOnce(&'a R),
    {
        match &self {
            Self::Both(left, right) => {
                f(left);
                g(right);
            }
            Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }

        self
    }

    /// TODO: DOCS
    pub fn inspect_left<F>(self, f: F) -> Self
    where
        for<'a> F: FnOnce(&'a L),
    {
        match &self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => {}
        }

        self
    }

    /// TODO: DOCS
    pub fn inspect_right<F>(self, f: F) -> Self
    where
        for<'a> F: FnOnce(&'a R),
    {
        match &self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => {}
        }

        self
    }

    /// TODO: DOCS
    pub fn biapply<F, G>(self, f: F, g: G)
    where
        F: FnOnce(L),
        G: FnOnce(R),
    {
        match self {
            Self::Both(left, right) => {
                f(left);
                g(right);
            }
            Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }
    }

    /// TODO: DOCS
    pub fn apply_left<F>(self, f: F)
    where
        F: FnOnce(L),
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => {}
        }
    }

    /// TODO: DOCS
    pub fn apply_right<F>(self, f: F)
    where
        F: FnOnce(R),
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => {}
        }
    }

    // TODO: In Either these methods are named `reduce` not `fold`
    /// TODO: DOCS
    pub fn bifold_left<F, G, T>(self, f: F, g: G) -> T
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> T,
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }
    }

    /// TODO: DOCS
    pub fn bifold_right<F, G, T>(self, f: F, g: G) -> T
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> T,
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => g(right),
            Self::Left(left) => f(left),
        }
    }

    /// TODO: DOCS
    pub fn ok(self) -> Result<R, L> {
        match self {
            Self::Both(left, _) | Self::Left(left) => Err(left),
            Self::Right(right) => Ok(right),
        }
    }

    /// TODO: DOCS
    pub fn ok_or(self, error: L) -> Result<R, L> {
        self.ok_or_else(|| error)
    }

    /// TODO: DOCS
    pub fn ok_or_else<F>(self, error: F) -> Result<R, L>
    where
        F: FnOnce() -> L,
    {
        match self {
            Self::Both(_, _) | Self::Left(_) => Err(error()),
            Self::Right(ok) => Ok(ok),
        }
    }

    /// Returns `Both` if present otherwise the missing value supplied by `left` or `right`
    pub fn or(self, left: L, right: R) -> (L, R) {
        match self {
            Self::Both(left, right) => (left, right),
            Self::Left(left) => (left, right),
            Self::Right(right) => (left, right),
        }
    }

    /// Returns `Both` if present otherwise the missing default value
    pub fn or_default(self) -> (L, R)
    where
        L: Default,
        R: Default,
    {
        match self {
            Self::Both(left, right) => (left, right),
            Self::Left(left) => (left, R::default()),
            Self::Right(right) => (L::default(), right),
        }
    }

    /// Returns `Both` if present otherwise computes the missing value
    pub fn or_else<F, G>(self, f: F, g: G) -> (L, R)
    where
        F: FnOnce() -> L,
        G: FnOnce() -> R,
    {
        match self {
            Self::Both(left, right) => (left, right),
            Self::Left(left) => (left, g()),
            Self::Right(right) => (f(), right),
        }
    }

    /// TODO: DOCS
    pub fn insert_left(&mut self, left: L) -> &mut L {
        match self {
            Self::Both(old_left, _) | Self::Left(old_left) => {
                *old_left = left;
                old_left
            }
            Self::Right(right) => {
                // SAFETY: The pointers are valid for reading and writing since they (right and self)
                // comes from a reference. See other comments below.
                unsafe {
                    // This bitwise copy is safe since we're about to overwrite all of self without
                    // using `right` again.
                    let right = ptr::read(right);
                    ptr::write(self, Self::Both(left, right));
                    // This is safe since we just filled the `left` value
                    self.as_mut().left().unwrap_unchecked()
                }
            }
        }
    }

    /// TODO: DOCS
    pub fn insert_right(&mut self, right: R) -> &mut R {
        match self {
            Self::Both(_, old_right) | Self::Right(old_right) => {
                *old_right = right;
                old_right
            }
            Self::Left(left) => {
                // SAFETY: The pointers are valid for reading and writing since they (left and self)
                // comes from a reference. See other comments below.
                unsafe {
                    // This bitwise copy is safe since we're about to overwrite all of self without
                    // using `left` again.
                    let left = ptr::read(left);
                    ptr::write(self, Self::Both(left, right));
                    // This is safe since we just filled the right value
                    self.as_mut().right().unwrap_unchecked()
                }
            }
        }
    }

    /// TODO: DOCS
    pub fn left_or_insert(&mut self, value: L) -> &mut L {
        self.left_or_insert_with(|| value)
    }

    /// TODO: DOCS
    pub fn left_or_insert_default(&mut self) -> &mut L
    where
        L: Default,
    {
        self.left_or_insert_with(L::default)
    }

    /// TODO: DOCS
    pub fn left_or_insert_with<F>(&mut self, f: F) -> &mut L
    where
        F: FnOnce() -> L,
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => left,
            Self::Right(_) => self.insert_left(f()),
        }
    }

    /// TODO: DOCS
    pub fn right_or_insert(&mut self, value: R) -> &mut R {
        self.right_or_insert_with(|| value)
    }

    /// TODO: DOCS
    pub fn right_or_insert_default(&mut self) -> &mut R
    where
        R: Default,
    {
        self.right_or_insert_with(R::default)
    }

    /// TODO: DOCS
    pub fn right_or_insert_with<F>(&mut self, f: F) -> &mut R
    where
        F: FnOnce() -> R,
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => right,
            Self::Left(_) => self.insert_right(f()),
        }
    }

    /// TODO: DOCS
    pub fn replace_any(&mut self, left: L, right: R) -> Self {
        map_each!(self; l, r => mem::replace(l, left), mem::replace(r, right))
    }

    /// TODO: DOCS
    pub fn replace_left(&mut self, value: L) -> Option<L> {
        match self {
            Self::Both(left, _) | Self::Left(left) => Some(mem::replace(left, value)),
            Self::Right(_) => None,
        }
    }

    /// TODO: DOCS
    pub fn replace_right(&mut self, value: R) -> Option<R> {
        match self {
            Self::Both(_, right) | Self::Right(right) => Some(mem::replace(right, value)),
            Self::Left(_) => None,
        }
    }

    /// TODO: DOCS
    pub fn into_left<F>(self, f: F) -> Self
    where
        F: FnOnce(R) -> L,
    {
        match self {
            Self::Both(left, _) => Self::Left(left),
            Self::Left(_) => self,
            Self::Right(right) => Self::Left(f(right)),
        }
    }

    /// TODO: DOCS
    pub fn into_right<F>(self, f: F) -> Self
    where
        F: FnOnce(L) -> R,
    {
        match self {
            Self::Both(_, right) => Self::Right(right),
            Self::Left(left) => Self::Right(f(left)),
            Self::Right(_) => self,
        }
    }

    /// TODO: DOCS
    #[cfg(feature = "either")]
    pub fn either<F>(self, f: F) -> Either<L, R>
    where
        F: FnOnce(L, R) -> Either<L, R>,
    {
        match self {
            Self::Both(left, right) => f(left, right),
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => Either::Right(right),
        }
    }

    /// TODO: DOCS
    #[cfg(feature = "either")]
    pub fn either_or_else<F>(self, f: F) -> Either<L, R>
    where
        F: FnOnce() -> Either<L, R>,
    {
        match self {
            Self::Both(_, _) => f(),
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => Either::Right(right),
        }
    }
}

impl<T> EitherOrBoth<T, T> {
    /// TODO: DOCS
    pub fn apply<F>(self, mut f: F)
    where
        F: FnMut(T),
    {
        match self {
            Self::Both(left, right) => {
                f(left);
                f(right);
            }
            Self::Left(left) => f(left),
            Self::Right(right) => f(right),
        }
    }

    /// TODO: DOCS
    pub fn inspect<F>(self, f: F) -> Self
    where
        for<'a> F: Fn(&'a T),
    {
        match &self {
            Self::Both(left, right) => {
                f(left);
                f(right);
            }
            Self::Left(left) => f(left),
            Self::Right(right) => f(right),
        }
        self
    }

    /// TODO: DOCS
    pub fn map<F, U>(self, f: F) -> EitherOrBoth<U, U>
    where
        F: Fn(T) -> U,
    {
        map_each!(self; l, r => f(l), f(r))
    }

    /// TODO: DOCS
    pub fn reduce<F>(self, f: F) -> T
    where
        F: FnOnce(T, T) -> T,
    {
        match self {
            Self::Both(left, right) => f(left, right),
            Self::Left(left) => left,
            Self::Right(right) => right,
        }
    }

    /// TODO: DOCS
    pub fn iter(&self) -> IterEitherOrBoth<'_, T> {
        IterEitherOrBoth::new(self)
    }

    /// TODO: DOCS
    pub fn iter_mut(&mut self) -> IterMutEitherOrBoth<'_, T> {
        IterMutEitherOrBoth::new(self)
    }

    /// TODO: DOCS
    pub fn into_iter_chain(self) -> ChainedIterEitherOrBoth<<T as IntoIterator>::IntoIter>
    where
        T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.map(IntoIterator::into_iter))
    }

    /// TODO: DOCS
    pub fn iter_chain(&self) -> ChainedIterEitherOrBoth<<&T as IntoIterator>::IntoIter>
    where
        for<'a> &'a T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.as_ref().map(IntoIterator::into_iter))
    }

    /// TODO: DOCS
    pub fn iter_chain_mut(&mut self) -> ChainedIterEitherOrBoth<<&mut T as IntoIterator>::IntoIter>
    where
        for<'a> &'a mut T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.as_mut().map(IntoIterator::into_iter))
    }
}

impl<L, R> EitherOrBoth<&L, &R> {
    /// TODO: DOCS
    pub fn cloned(self) -> EitherOrBoth<L, R>
    where
        L: Clone,
        R: Clone,
    {
        map_each!(self; l, r => l.clone(), r.clone())
    }

    /// TODO: DOCS
    pub fn copied(self) -> EitherOrBoth<L, R>
    where
        L: Copy,
        R: Copy,
    {
        map_each!(self; l, r => *l, *r)
    }
}

impl<L, R> EitherOrBoth<&mut L, &mut R> {
    /// TODO: DOCS
    pub fn cloned(self) -> EitherOrBoth<L, R>
    where
        L: Clone,
        R: Clone,
    {
        map_each!(self; l, r => l.clone(), r.clone())
    }

    /// TODO: DOCS
    pub fn copied(self) -> EitherOrBoth<L, R>
    where
        L: Copy,
        R: Copy,
    {
        map_each!(self; l, r => *l, *r)
    }
}

impl<L1, L2, R1, R2> EitherOrBoth<(L1, R1), (L2, R2)> {
    /// TODO: DOCS
    pub fn transpose(self) -> (EitherOrBoth<L1, L2>, EitherOrBoth<R1, R2>) {
        match self {
            Self::Both((l1, r1), (l2, r2)) => {
                (EitherOrBoth::Both(l1, l2), EitherOrBoth::Both(r1, r2))
            }
            Self::Left((l1, r1)) => (EitherOrBoth::Left(l1), EitherOrBoth::Left(r1)),
            Self::Right((l2, r2)) => (EitherOrBoth::Right(l2), EitherOrBoth::Right(r2)),
        }
    }
}

impl<L, R1, R2> EitherOrBoth<(L, R1), (L, R2)> {
    /// TODO: DOCS
    pub fn transpose_left<F>(self, f: F) -> (L, EitherOrBoth<R1, R2>)
    where
        F: FnOnce(L, L) -> L,
    {
        match self {
            Self::Both((l1, r1), (l2, r2)) => (f(l1, l2), EitherOrBoth::Both(r1, r2)),
            Self::Left((l, r1)) => (l, EitherOrBoth::Left(r1)),
            Self::Right((l, r2)) => (l, EitherOrBoth::Right(r2)),
        }
    }
}

impl<L1, L2, R> EitherOrBoth<(L1, R), (L2, R)> {
    /// TODO: DOCS
    pub fn transpose_right<F>(self, f: F) -> (EitherOrBoth<L1, L2>, R)
    where
        F: FnOnce(R, R) -> R,
    {
        match self {
            Self::Both((l1, r1), (l2, r2)) => (EitherOrBoth::Both(l1, l2), f(r1, r2)),
            Self::Left((l1, r)) => (EitherOrBoth::Left(l1), r),
            Self::Right((l2, r)) => (EitherOrBoth::Right(l2), r),
        }
    }
}

impl<L, R> EitherOrBoth<Option<L>, Option<R>> {
    /// TODO: DOCS
    pub fn transpose(self) -> Option<EitherOrBoth<L, R>> {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Some(left), Some(right)) => Some(EitherOrBoth::Both(left, right)),
                _ => None,
            },
            Self::Left(left) => left.map(EitherOrBoth::Left),
            Self::Right(right) => right.map(EitherOrBoth::Right),
        }
    }
}

impl<L, R, E1, E2> EitherOrBoth<Result<L, E1>, Result<R, E2>> {
    /// TODO: DOCS
    pub fn transpose(self) -> Result<EitherOrBoth<L, R>, EitherOrBoth<E1, E2>> {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Ok(left), Ok(right)) => Ok(EitherOrBoth::Both(left, right)),
                (Ok(_), Err(err)) => Err(EitherOrBoth::Right(err)),
                (Err(err), Ok(_)) => Err(EitherOrBoth::Left(err)),
                (Err(err_left), Err(err_right)) => Err(EitherOrBoth::Both(err_left, err_right)),
            },
            Self::Left(left) => match left {
                Ok(ok) => Ok(EitherOrBoth::Left(ok)),
                Err(err) => Err(EitherOrBoth::Left(err)),
            },
            Self::Right(right) => match right {
                Ok(ok) => Ok(EitherOrBoth::Right(ok)),
                Err(err) => Err(EitherOrBoth::Right(err)),
            },
        }
    }
}

impl<L, R, E> EitherOrBoth<Result<L, E>, Result<R, E>> {
    /// TODO: DOCS
    pub fn transpose_err<F>(self, f: F) -> Result<EitherOrBoth<L, R>, E>
    where
        F: FnOnce(E, E) -> E,
    {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Ok(left), Ok(right)) => Ok(EitherOrBoth::Both(left, right)),
                (Ok(_), Err(err)) | (Err(err), Ok(_)) => Err(err),
                (Err(err_left), Err(err_right)) => Err(f(err_left, err_right)),
            },
            Self::Left(left) => left.map(EitherOrBoth::Left),
            Self::Right(right) => right.map(EitherOrBoth::Right),
        }
    }
}

impl<T, E1, E2> EitherOrBoth<Result<T, E1>, Result<T, E2>> {
    /// TODO: DOCS
    pub fn transpose_ok<F>(self, f: F) -> Result<T, EitherOrBoth<E1, E2>>
    where
        F: FnOnce(T, T) -> T,
    {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Ok(left), Ok(right)) => Ok(f(left, right)),
                (Ok(_), Err(err)) => Err(EitherOrBoth::Right(err)),
                (Err(err), Ok(_)) => Err(EitherOrBoth::Left(err)),
                (Err(err_left), Err(err_right)) => Err(EitherOrBoth::Both(err_left, err_right)),
            },
            Self::Left(left) => left.map_err(EitherOrBoth::Left),
            Self::Right(right) => right.map_err(EitherOrBoth::Right),
        }
    }
}
