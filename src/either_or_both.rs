//! `EitherOrBoth` with different types

use core::ops::{Deref, DerefMut};
use core::pin::Pin;
use core::{mem, ptr};
#[cfg(feature = "std")]
use std::vec::Vec;

use crate::either::Either;
use crate::error::Error;
use crate::iter::{
    ChainedIterEitherOrBoth, IntoIterEitherOrBoth, IterEitherOrBoth, IterMutEitherOrBoth,
    SwapIterEitherOrBoth,
};

/// Either left or right or both can be present
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "c_repr", repr(C))]
pub enum EitherOrBoth<L, R = L> {
    /// Both values are present
    Both(L, R),
    /// The left value
    Left(L),
    /// The right value
    Right(R),
}

// TODO: Double check that all left (or right) methods have an equivalent right (or left) method
// TODO: Eagerly versus lazily
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
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(left, right),
            Self::Left(left) => EitherOrBoth::Left(left),
            Self::Right(right) => EitherOrBoth::Right(right),
        }
    }

    /// Converts from `&mut EitherOrBoth<L, R>` to `EitherOrBoth<&mut L, &mut R>`
    pub fn as_mut(&mut self) -> EitherOrBoth<&mut L, &mut R> {
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(left, right),
            Self::Left(left) => EitherOrBoth::Left(left),
            Self::Right(right) => EitherOrBoth::Right(right),
        }
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
        self.as_ref().bimap(|l| &**l, |r| &**r)
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
        self.as_mut().bimap(|l| &mut **l, |r| &mut **r)
    }

    /// Converts from `Pin<&EitherOrBoth<L, R>>` to `EitherOrBoth<Pin<&L>, Pin<&R>>`.
    pub fn as_pin_ref(self: Pin<&Self>) -> EitherOrBoth<Pin<&L>, Pin<&R>> {
        // SAFETY: `x` is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            match Pin::get_ref(self) {
                Self::Both(left, right) => {
                    EitherOrBoth::Both(Pin::new_unchecked(left), Pin::new_unchecked(right))
                }
                Self::Left(left) => EitherOrBoth::Left(Pin::new_unchecked(left)),
                Self::Right(right) => EitherOrBoth::Right(Pin::new_unchecked(right)),
            }
        }
    }

    /// Converts from `Pin<&mut EitherOrBoth<L, R>>` to `EitherOrBoth<Pin<&mut L>, Pin<&mut R>>`.
    pub fn as_pin_mut(self: Pin<&mut Self>) -> EitherOrBoth<Pin<&mut L>, Pin<&mut R>> {
        // SAFETY: `get_unchecked_mut` is never used to move the `EitherOrBoth` inside `self`. `x`
        // is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            match Pin::get_unchecked_mut(self) {
                Self::Both(left, right) => {
                    EitherOrBoth::Both(Pin::new_unchecked(left), Pin::new_unchecked(right))
                }
                Self::Left(left) => EitherOrBoth::Left(Pin::new_unchecked(left)),
                Self::Right(right) => EitherOrBoth::Right(Pin::new_unchecked(right)),
            }
        }
    }

    /// Returns the contained `Both` values consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if only a `Left` or `Right` value is present with a custom panic message provided by
    /// `msg`
    pub fn expect_both(self, msg: &str) -> (L, R) {
        self.both().expect(msg)
    }

    /// Returns the contained left value if `Left` or `Both` consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if there is no left value present with a custom panic message provided by `msg`
    pub fn expect_left(self, msg: &str) -> L {
        self.left().expect(msg)
    }

    /// Returns the contained `Left` value consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is not `Left` with a custom panic message provided by `msg`
    pub fn expect_only_left(self, msg: &str) -> L {
        self.only_left().expect(msg)
    }

    /// Returns the contained right value if `Right` or `Both` consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if there is no right value present with a custom panic message provided by `msg`
    pub fn expect_right(self, msg: &str) -> R {
        self.right().expect(msg)
    }

    /// Returns the contained `Right` value consuming `self`
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is not `Right` with a custom panic message provided by `msg`
    pub fn expect_only_right(self, msg: &str) -> R {
        self.only_right().expect(msg)
    }

    /// Returns the contained `Both` value as tuple consuming `self`
    pub fn unwrap_both(self) -> (L, R) {
        self.both().unwrap()
    }

    /// Returns the contained left value consuming `self`
    pub fn unwrap_left(self) -> L {
        self.left().unwrap()
    }

    /// Returns the contained `Left` value consuming `self`
    pub fn unwrap_only_left(self) -> L {
        self.only_left().unwrap()
    }

    /// Returns the contained right value consuming `self`
    pub fn unwrap_right(self) -> R {
        self.right().unwrap()
    }

    /// Returns the contained `Right` value consuming `self`
    pub fn unwrap_only_right(self) -> R {
        self.only_right().unwrap()
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

    // TODO: More like `or`?
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

    // TODO: Check this and the `left_and` methods
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
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(f(left), g(right)),
            Self::Left(left) => EitherOrBoth::Left(f(left)),
            Self::Right(right) => EitherOrBoth::Right(g(right)),
        }
    }

    /// Applies a mapping function to the left value in `Both` or `Left` and returns the result
    pub fn map_left<F, T>(self, f: F) -> EitherOrBoth<T, R>
    where
        F: FnOnce(L) -> T,
    {
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(f(left), right),
            Self::Left(left) => EitherOrBoth::Left(f(left)),
            Self::Right(right) => EitherOrBoth::Right(right),
        }
    }

    /// TODO: DOCS
    pub fn map_left_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(L) -> U,
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => default,
        }
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
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(left, f(right)),
            Self::Left(left) => EitherOrBoth::Left(left),
            Self::Right(right) => EitherOrBoth::Right(f(right)),
        }
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
        F: FnOnce(&L),
        G: FnOnce(&R),
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
        F: FnOnce(&L),
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
        F: FnOnce(&R),
    {
        match &self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => {}
        }

        self
    }

    /// TODO: DOCS
    pub fn biconsume<F, G>(self, f: F, g: G)
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
    pub fn consume_left<F>(self, f: F)
    where
        F: FnOnce(L),
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => {}
        }
    }

    /// TODO: DOCS
    pub fn consume_right<F>(self, f: F)
    where
        F: FnOnce(R),
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => {}
        }
    }

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
    ///
    /// TODO: Eagerly versus lazily
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
    ///
    /// TODO: Eagerly versus lazily
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
        match self {
            Self::Both(old_left, old_right) => {
                let old_left = mem::replace(old_left, left);
                let old_right = mem::replace(old_right, right);
                Self::Both(old_left, old_right)
            }
            Self::Left(old_left) => Self::Left(mem::replace(old_left, left)),
            Self::Right(old_right) => Self::Right(mem::replace(old_right, right)),
        }
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
}

impl<T> EitherOrBoth<T, T> {
    /// TODO: DOCS
    pub fn consume<F>(self, f: F)
    where
        F: Fn(T),
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
        F: Fn(&T),
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
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(f(left), f(right)),
            Self::Left(left) => EitherOrBoth::Left(f(left)),
            Self::Right(right) => EitherOrBoth::Right(f(right)),
        }
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
    pub fn iter_chain<'iter>(
        &'iter self,
    ) -> ChainedIterEitherOrBoth<<&'iter T as IntoIterator>::IntoIter>
    where
        &'iter T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.as_ref().map(IntoIterator::into_iter))
    }

    /// TODO: DOCS
    pub fn iter_chain_mut<'iter>(
        &'iter mut self,
    ) -> ChainedIterEitherOrBoth<<&'iter mut T as IntoIterator>::IntoIter>
    where
        &'iter mut T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.as_mut().map(IntoIterator::into_iter))
    }
}

#[cfg(feature = "std")]
impl<L, R> FromIterator<EitherOrBoth<L, R>> for EitherOrBoth<Vec<L>, Vec<R>> {
    fn from_iter<T: IntoIterator<Item = EitherOrBoth<L, R>>>(iter: T) -> Self {
        use std::vec;

        let mut left_vec: Vec<L> = vec![];
        let mut right_vec: Vec<R> = vec![];
        for item in iter {
            item.biconsume(|l| left_vec.push(l), |r| right_vec.push(r));
        }

        match (left_vec.is_empty(), right_vec.is_empty()) {
            (true, true) | (false, false) => Self::Both(left_vec, right_vec),
            (true, false) => Self::Right(right_vec),
            (false, true) => Self::Left(left_vec),
        }
    }
}

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

impl<L, R> EitherOrBoth<&L, &R> {
    /// TODO: DOCS
    pub fn cloned(self) -> EitherOrBoth<L, R>
    where
        L: Clone,
        R: Clone,
    {
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(left.clone(), right.clone()),
            Self::Left(left) => EitherOrBoth::Left(left.clone()),
            Self::Right(right) => EitherOrBoth::Right(right.clone()),
        }
    }

    /// TODO: DOCS
    pub fn copied(self) -> EitherOrBoth<L, R>
    where
        L: Copy,
        R: Copy,
    {
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(*left, *right),
            Self::Left(left) => EitherOrBoth::Left(*left),
            Self::Right(right) => EitherOrBoth::Right(*right),
        }
    }
}

impl<L, R> EitherOrBoth<&mut L, &mut R> {
    /// TODO: DOCS
    pub fn cloned(self) -> EitherOrBoth<L, R>
    where
        L: Clone,
        R: Clone,
    {
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(left.clone(), right.clone()),
            Self::Left(left) => EitherOrBoth::Left(left.clone()),
            Self::Right(right) => EitherOrBoth::Right(right.clone()),
        }
    }

    /// TODO: DOCS
    pub fn copied(self) -> EitherOrBoth<L, R>
    where
        L: Copy,
        R: Copy,
    {
        match self {
            Self::Both(left, right) => EitherOrBoth::Both(*left, *right),
            Self::Left(left) => EitherOrBoth::Left(*left),
            Self::Right(right) => EitherOrBoth::Right(*right),
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
    pub fn bimap_err<F, G, X1, X2>(self, f: F, g: G) -> EitherOrBoth<Result<L, X1>, Result<R, X2>>
    where
        F: FnOnce(E1) -> X1,
        G: FnOnce(E2) -> X2,
    {
        self.bimap(|l| l.map_err(f), |r| r.map_err(g))
    }
}

impl<L, R, E> EitherOrBoth<Result<L, E>, Result<R, E>> {
    /// TODO: DOCS
    pub fn transpose<F>(self, f: F) -> Result<EitherOrBoth<L, R>, E>
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

    /// TODO: DOCS
    pub fn map_err<F, X>(self, f: F) -> EitherOrBoth<Result<L, X>, Result<R, X>>
    where
        F: Fn(E) -> X,
    {
        self.bimap(|l| l.map_err(&f), |r| r.map_err(&f))
    }
}

/// TODO: feature either and then make this dependent on the feature
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
    type Error = Error;

    fn try_from(value: (Option<L>, Option<R>)) -> Result<Self, Self::Error> {
        match value {
            (None, None) => Err(Error::TryFromOptions),
            (None, Some(right)) => Ok(Self::Right(right)),
            (Some(left), None) => Ok(Self::Left(left)),
            (Some(left), Some(right)) => Ok(Self::Both(left, right)),
        }
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    #[test]
    fn test_map() {
        // struct NotClone;
        // let either = EitherOrBoth::<NotClone, NotClone>::Left(NotClone);
        // let either = EitherOrBoth::Right(NotClone);

        // let either = EitherOrBoth::Both("a", "b");
        // let new = either.map(ToOwned::to_owned);

        // let either = EitherOrBoth::Both("a", "b".to_owned());
        // let new = either.map(ToOwned::to_owned);
    }

    #[rstest]
    #[case::left(EitherOrBoth::Left(1), 2, Some(1))]
    #[case::right(EitherOrBoth::Right(1), 2, None)]
    #[case::both(EitherOrBoth::Both(1, 2), 3, Some(1))]
    fn test_replace_left(
        #[case] mut either: EitherOrBoth<u64, u64>,
        #[case] value: u64,
        #[case] expected: Option<u64>,
    ) {
        let old = either.replace_left(value);
        assert_eq!(old, expected);
    }

    #[test]
    fn test_transpose() {
        // let either: EitherOrBoth<Option<i32>, Option<i32>> = EitherOrBoth::Both(Some(0), None);
        // let new = either.transpose();
    }
}
