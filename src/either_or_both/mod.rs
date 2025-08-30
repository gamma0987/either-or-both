//! `EitherOrBoth` with different types

macro_rules! map_each {
    ($src:expr; $left:pat, $right:pat => $left_expr:expr, $right_expr:expr; $( $rest:tt )*) => {
        match $src {
            Self::Both($left, $right) => Both($left_expr, $right_expr),
            Self::Left($left) => Left($left_expr),
            Self::Right($right) => Right($right_expr),
        } $( $rest )*
    };
    ($src:expr; $left:pat, $right:pat => $left_expr:expr, $right_expr:expr) => {
        match $src {
            Self::Both($left, $right) => Both($left_expr, $right_expr),
            Self::Left($left) => Left($left_expr),
            Self::Right($right) => Right($right_expr),
        }
    };
    ($src:expr) => {
        match $src {
            Self::Both(left, right) => Both(left, right),
            Self::Left(left) => Left(left),
            Self::Right(right) => Right(right),
        }
    };
}

pub mod iter;
pub mod traits;

use core::hint::unreachable_unchecked;
use core::ops::{Deref, DerefMut};
use core::pin::Pin;
use core::{mem, ptr};

use iter::{ChainedIterEitherOrBoth, IterEitherOrBoth, IterMutEitherOrBoth, SwapIterEitherOrBoth};
use EitherOrBoth::*;

use crate::unwrap_failed;

/// Represent values that have either a `Left` or `Right` value or `Both` values
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", allow(clippy::unsafe_derive_deserialize))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum EitherOrBoth<L, R = L> {
    /// Represents a value from both sides
    Both(L, R),
    /// Represents a value from the left side
    Left(L),
    /// Represents a value from the right side
    Right(R),
}

impl<L, R> EitherOrBoth<L, R> {
    ////////////////////////////////////////////////////////////////////////////////
    // Boolish
    ////////////////////////////////////////////////////////////////////////////////

    /// Returns `true` if `EitherOrBoth` is a [`Left`] or [`Both`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.has_left(), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.has_left(), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.has_left(), false);
    /// ```
    pub const fn has_left(&self) -> bool {
        match self {
            Self::Left(_) | Self::Both(_, _) => true,
            Self::Right(_) => false,
        }
    }

    /// Returns `true` if this is a [`Left`] or [`Both`] and the left value satisfies a
    /// predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.has_left_and(|l| l == 1), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.has_left_and(|l| l == 1), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.has_left_and(|l| l == 1), false);
    /// ```
    pub fn has_left_and<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Left(left) | Self::Both(left, _) => f(left),
            Self::Right(_) => false,
        }
    }

    /// Returns `true` if this is a [`Left`] or [`Both`] or the [`Right`] value satisfies
    /// a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.has_left_or(|r| r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.has_left_or(|r| r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.has_left_or(|r| r != 'c'), false);
    /// ```
    pub fn has_left_or<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Left(_) | Self::Both(_, _) => true,
            Self::Right(right) => f(right),
        }
    }

    /// Returns `true` if `EitherOrBoth` is a [`Right`] or [`Both`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.has_right(), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.has_right(), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.has_right(), true);
    /// ```
    pub const fn has_right(&self) -> bool {
        match self {
            Self::Right(_) | Self::Both(_, _) => true,
            Self::Left(_) => false,
        }
    }

    /// Returns `true` if this is a [`Right`] or [`Both`] and the right value satisfies a
    /// predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.has_right_and(|r| r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.has_right_and(|r| r == 'm'), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.has_right_and(|r| r == 'c'), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.has_right_and(|r| r == 'c'), true);
    /// ```
    pub fn has_right_and<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Right(right) | Self::Both(_, right) => f(right),
            Self::Left(_) => false,
        }
    }

    /// Returns `true` if this is a [`Right`] or [`Both`] or the [`Left`] value satisfies
    /// a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.has_right_or(|l| l == 1), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.has_right_or(|l| l == 1), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.has_right_or(|l| l == 2), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.has_right_or(|l| l == 2), true);
    /// ```
    pub fn has_right_or<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Right(_) | Self::Both(_, _) => true,
            Self::Left(left) => f(left),
        }
    }

    /// Returns `true` if `EitherOrBoth` is a [`Both`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_both(), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_both(), false);
    /// ```
    pub const fn is_both(&self) -> bool {
        matches!(self, Self::Both(_, _))
    }

    /// Returns `true` if this is a [`Both`] and both values satisfy a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_both_and(|l, r| l == 1 && r == 'c'), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_both_and(|l, r| l == 1 && r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_both_and(|l, _| l == 2), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_both_and(|_, r| r == 'c'), false);
    /// ```
    pub fn is_both_and<F>(self, f: F) -> bool
    where
        F: FnOnce(L, R) -> bool,
    {
        match self {
            Self::Both(left, right) => f(left, right),
            _ => false,
        }
    }

    /// Returns `true` if this is a [`Both`] or if [`Left`] or [`Right`] match their
    /// respective predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_both_or(|l| l == 2, |r| r == 'm'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_both_or(|l| l == 1, |r| r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_both_or(|l| l == 2, |r| r == 'c'), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_both_or(|l| l == 1, |r| r == 'm'), false);
    /// ```
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

    /// Returns `true` if `EitherOrBoth` is a [`Left`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_left(), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_left(), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_left(), false);
    /// ```
    pub const fn is_left(&self) -> bool {
        matches!(self, Self::Left(_))
    }

    /// Returns `true` if this is a [`Left`] and the inner value satisfies a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_left_and(|l| l == 1), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_left_and(|l| l == 1), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_left_and(|l| l == 1), false);
    /// ```
    pub fn is_left_and<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Left(left) => f(left),
            _ => false,
        }
    }

    /// Returns `true` if this is a [`Left`] or the right value satisfies a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_left_or(|r| r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_left_or(|r| r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_left_or(|r| r == 'm'), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_left_or(|r| r == 'c'), true);
    /// ```
    pub fn is_left_or<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Left(_) => true,
            Self::Both(_, right) | Self::Right(right) => f(right),
        }
    }

    /// Returns `true` `EitherOrBoth` is a [`Right`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_right(), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_right(), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_right(), false);
    pub const fn is_right(&self) -> bool {
        matches!(self, Self::Right(_))
    }

    /// Returns `true` if this is a [`Right`] and the inner value satisfies a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_right_and(|r| r == 'c'), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_right_and(|r| r == 'm'), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_right_and(|r| r == 'c'), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_right_and(|r| r == 'c'), false);
    /// ```
    pub fn is_right_and<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Right(right) => f(right),
            _ => false,
        }
    }

    /// Returns `true` if this is a [`Right`] or the left value satisfies a predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.is_right_or(|l| l == 1), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_right_or(|l| l == 1), true);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.is_right_or(|l| l == 2), false);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.is_right_or(|l| l == 1), true);
    /// ```
    pub fn is_right_or<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Right(_) => true,
            Self::Both(left, _) | Self::Left(left) => f(left),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // As reference conversions
    ////////////////////////////////////////////////////////////////////////////////

    /// Converts from `&EitherOrBoth<L, R>` to `EitherOrBoth<&L, &R>`.
    ///
    /// # Examples
    ///
    /// Calculate the length of the [strings] without moving the [`Strings`].
    ///
    /// [strings]: String
    /// [`Strings`]: String
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let text: EitherOrBoth<String> = EitherOrBoth::Left("left value".to_owned());
    ///
    /// // Cast `EitherOrBoth<String>` to `EitherOrBoth<&String>` and then apply a map consuming the
    /// // result of `as_ref` instead of `text` itself
    /// let length: EitherOrBoth<usize> = text.as_ref().map(String::len);
    /// assert_eq!(length, EitherOrBoth::Left(10));
    ///
    /// println!("`text` has not been moved: {:?}", &text);
    /// ```
    pub const fn as_ref(&self) -> EitherOrBoth<&L, &R> {
        map_each!(self)
    }

    /// Converts from `&mut EitherOrBoth<L, R>` to `EitherOrBoth<&mut L, &mut R>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut both: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    ///
    /// match both.as_mut() {
    ///     EitherOrBoth::Both(left, right) => {
    ///         *left = 2;
    ///         *right = 'm';
    ///     }
    ///     EitherOrBoth::Left(left) => *left = 3,
    ///     EitherOrBoth::Right(right) => *right = 'x',
    /// }
    ///
    /// assert_eq!(both, EitherOrBoth::Both(2, 'm'))
    /// ```
    pub fn as_mut(&mut self) -> EitherOrBoth<&mut L, &mut R> {
        map_each!(self)
    }

    /// Converts from `EitherOrBoth<L, R>` to `EitherOrBoth<&L::Target, &R::Target>`.
    ///
    /// This method keeps the original `EitherOrBoth` unchanged, while creating a new
    /// instance that holds a reference to the original. It also coerces the inner
    /// values through the `Deref` trait.
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let values: EitherOrBoth<String> = EitherOrBoth::Both("left".to_owned(), "right".to_owned());
    /// let deref: EitherOrBoth<&str> = values.as_deref();
    /// assert_eq!(deref.both(), Some(("left", "right")));
    ///
    /// let values: EitherOrBoth<String> = EitherOrBoth::Left("left".to_owned());
    /// let deref: EitherOrBoth<&str> = values.as_deref();
    /// assert_eq!(deref.left(), Some("left"));
    /// ```
    pub fn as_deref(&self) -> EitherOrBoth<&<L as Deref>::Target, &<R as Deref>::Target>
    where
        L: Deref,
        R: Deref,
    {
        map_each!(self; l, r => &**l, &**r)
    }

    /// Converts from `EitherOrBoth<L, R>` to `EitherOrBoth<&mut L::Target, &mut
    /// R::Target>`.
    ///
    /// This method keeps the original `EitherOrBoth` unchanged, while creating a new
    /// instance that holds a mutable reference to the inner type's [`Deref::Target`]
    /// type.
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<String> = EitherOrBoth::Both("left".to_owned(), "right".to_owned());
    /// let upper_case: EitherOrBoth<&mut str> = value.as_deref_mut().map(|v| {
    ///     v.make_ascii_uppercase();
    ///     v
    /// });
    ///
    /// assert_eq!(
    ///     upper_case.both(),
    ///     Some((
    ///         "LEFT".to_owned().as_mut_str(),
    ///         "RIGHT".to_owned().as_mut_str()
    ///     ))
    /// );
    /// ```
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
    #[must_use]
    pub fn as_pin_ref(self: Pin<&Self>) -> EitherOrBoth<Pin<&L>, Pin<&R>> {
        // SAFETY: `x` is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            map_each!(Pin::get_ref(self); l, r => Pin::new_unchecked(l), Pin::new_unchecked(r))
        }
    }

    /// Converts from `Pin<&mut EitherOrBoth<L, R>>` to `EitherOrBoth<Pin<&mut L>,
    /// Pin<&mut R>>`.
    #[must_use]
    pub fn as_pin_mut(self: Pin<&mut Self>) -> EitherOrBoth<Pin<&mut L>, Pin<&mut R>> {
        // SAFETY: `get_unchecked_mut` is never used to move the `EitherOrBoth` inside `self`. `x`
        // is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            map_each!(
                Pin::get_unchecked_mut(self); l, r => Pin::new_unchecked(l), Pin::new_unchecked(r)
            )
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Unwrapping the contained values
    ////////////////////////////////////////////////////////////////////////////////

    /// Returns the contained [`Both`] values as tuple consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics with a custom panic message provided by `msg` if only a [`Left`] or
    /// [`Right`] value is present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value = EitherOrBoth::Both("left", "right");
    /// assert_eq!(value.expect_both("should be both"), ("left", "right"));
    /// ```
    ///
    /// This example panics with the message `should be both`
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// value.expect_both("should be both");
    /// ```
    #[track_caller]
    pub fn expect_both(self, msg: &str) -> (L, R) {
        match self {
            Self::Both(left, right) => (left, right),
            _ => unwrap_failed(msg),
        }
    }

    /// Returns the contained left value if [`Left`] or [`Both`] consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics with a custom panic message provided by `msg` if there is no left value
    /// present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(value.expect_left("should be left"), "left");
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// assert_eq!(value.expect_left("should be left"), "left");
    /// ```
    ///
    /// This example panics with the message `should be left`
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// value.expect_left("should be left");
    /// ```
    #[track_caller]
    pub fn expect_left(self, msg: &str) -> L {
        match self {
            Self::Both(left, _) | Self::Left(left) => left,
            Self::Right(_) => unwrap_failed(msg),
        }
    }

    /// Returns the contained [`Left`] value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics with a custom panic message provided by `msg` if `EitherOrBoth` is not
    /// [`Left`]
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(value.expect_only_left("should be left"), "left");
    /// ```
    ///
    /// The following examples panic with the message `should be left`
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// value.expect_only_left("should be left");
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// value.expect_only_left("should be left"); // panics with the message `should be left`
    /// ```
    #[track_caller]
    pub fn expect_only_left(self, msg: &str) -> L {
        match self {
            Self::Left(left) => left,
            _ => unwrap_failed(msg),
        }
    }

    /// Returns the contained right value if [`Right`] or [`Both`] consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics with a custom panic message provided by `msg` if there is no right value
    /// present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(value.expect_right("should be right"), "right");
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// assert_eq!(value.expect_right("should be right"), "right");
    /// ```
    ///
    /// The following example panics with the message `should be right`
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// value.expect_right("should be right");
    /// ```
    #[track_caller]
    pub fn expect_right(self, msg: &str) -> R {
        match self {
            Self::Both(_, right) | Self::Right(right) => right,
            Self::Left(_) => unwrap_failed(msg),
        }
    }

    /// Returns the contained [`Right`] value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics with a custom panic message provided by `msg` if there is no [`Right`]
    /// value present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(value.expect_only_right("should be right"), "right");
    /// ```
    ///
    /// The following examples panic with the message `should be right`
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// value.expect_only_right("should be right");
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// value.expect_only_right("should be right"); // panics with the message `should be right`
    /// ```
    #[track_caller]
    pub fn expect_only_right(self, msg: &str) -> R {
        match self {
            Self::Right(right) => right,
            _ => unwrap_failed(msg),
        }
    }

    /// Returns the contained [`Both`] values as tuple consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics if only a [`Left`] or [`Right`] value is present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value = EitherOrBoth::Both("left", "right");
    /// assert_eq!(value.unwrap_both(), ("left", "right"));
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// value.unwrap_both(); // panics
    /// ```
    #[track_caller]
    pub fn unwrap_both(self) -> (L, R) {
        self.expect_both("Called `EitherOrBoth::unwrap_both` on a `Left` or `Right` value")
    }

    /// Returns the contained [`Both`] values as tuple consuming `self`, without checking
    /// that the value is not [`Both`].
    ///
    /// # SAFETY
    ///
    /// Calling this method on a [`Right`] or [`Left`] variant is *[undefined behavior]*.
    ///
    /// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value = EitherOrBoth::Both("left", "right");
    /// assert_eq!(unsafe { value.unwrap_both_unchecked() }, ("left", "right"));
    /// ```
    ///
    /// The following example introduces *[undefined behavior]*
    ///
    /// ```no_run
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value = EitherOrBoth::Left("left");
    /// assert_eq!(unsafe { value.unwrap_both_unchecked() }, ("left", "right"));
    /// ```
    #[track_caller]
    pub unsafe fn unwrap_both_unchecked(self) -> (L, R) {
        match self {
            Self::Both(left, right) => (left, right),
            // SAFETY: the safety contract must be upheld by the caller.
            _ => core::hint::unreachable_unchecked(), // cov:excl-line
        }
    }

    /// Returns the contained left value of a [`Left`] or [`Both`] variant consuming
    /// `self`.
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is a [`Right`] variant
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(value.unwrap_left(), "left");
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// assert_eq!(value.unwrap_left(), "left");
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// value.unwrap_left(); // panics
    /// ```
    #[track_caller]
    pub fn unwrap_left(self) -> L {
        self.expect_left("Called `EitherOrBoth::unwrap_left` on a `Right` value")
    }

    /// Returns the contained left value of a [`Left`] or [`Both`] variant consuming
    /// `self`, without checking that the value is not [`Left`] or [`Both`].
    ///
    /// # SAFETY
    ///
    /// Calling this method on a [`Right`] variant is *[undefined behavior]*.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value = EitherOrBoth::Both("left", "right");
    /// assert_eq!(unsafe { value.unwrap_left_unchecked() }, "left");
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(unsafe { value.unwrap_left_unchecked() }, "left");
    /// ```
    ///
    /// The following example introduces *[undefined behavior]*
    ///
    /// ```no_run
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(unsafe { value.unwrap_left_unchecked() }, "left");
    /// ```
    ///
    /// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
    #[track_caller]
    pub unsafe fn unwrap_left_unchecked(self) -> L {
        match self {
            Self::Both(left, _) | Self::Left(left) => left,
            // SAFETY: the safety contract must be upheld by the caller.
            Self::Right(_) => core::hint::unreachable_unchecked(), // cov:excl-line
        }
    }

    /// Returns the contained [`Left`] value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is a [`Right`] or [`Both`] variant
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(value.unwrap_only_left(), "left");
    /// ```
    ///
    /// The following examples panic
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// value.unwrap_only_left(); // panics
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// value.unwrap_only_left(); // panics
    /// ```
    #[track_caller]
    pub fn unwrap_only_left(self) -> L {
        self.expect_only_left("Called `EitherOrBoth::unwrap_only_left` on a `Both` or `Right` value")
    }

    /// Returns the contained [`Left`] value consuming `self`, without checking that the
    /// value is not [`Left`].
    ///
    /// # SAFETY
    ///
    /// Calling this method on a [`Right`] or [`Both`] variant is *[undefined behavior]*.
    ///
    /// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(unsafe { value.unwrap_only_left_unchecked() }, "left");
    /// ```
    ///
    /// The following example introduces *[undefined behavior]*
    ///
    /// ```no_run
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(unsafe { value.unwrap_only_left_unchecked() }, "left");
    /// ```
    #[track_caller]
    pub unsafe fn unwrap_only_left_unchecked(self) -> L {
        match self {
            Self::Left(left) => left,
            // SAFETY: the safety contract must be upheld by the caller.
            _ => core::hint::unreachable_unchecked(), // cov:excl-line
        }
    }

    /// Returns the contained right value of a [`Right`] or [`Both`] variant consuming
    /// `self`.
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is a [`Left`] variant
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(value.unwrap_right(), "right");
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// assert_eq!(value.unwrap_right(), "right");
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// value.unwrap_right(); // panics
    /// ```
    #[track_caller]
    pub fn unwrap_right(self) -> R {
        self.expect_right("Called EitherOrBoth::unwrap_right` on a `Left` value")
    }

    /// Returns the contained right value of a [`Right`] or [`Both`] variant consuming
    /// `self`, without checking that the value is not [`Right`] or [`Both`].
    ///
    /// # SAFETY
    ///
    /// Calling this method on a [`Left`] variant is *[undefined behavior]*.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value = EitherOrBoth::Both("left", "right");
    /// assert_eq!(unsafe { value.unwrap_right_unchecked() }, "right");
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(unsafe { value.unwrap_right_unchecked() }, "right");
    /// ```
    ///
    /// The following example introduces *[undefined behavior]*
    ///
    /// ```no_run
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(unsafe { value.unwrap_right_unchecked() }, "right");
    /// ```
    ///
    /// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
    #[track_caller]
    pub unsafe fn unwrap_right_unchecked(self) -> R {
        match self {
            Self::Both(_, right) | Self::Right(right) => right,
            // SAFETY: the safety contract must be upheld by the caller.
            Self::Left(_) => core::hint::unreachable_unchecked(), // cov:excl-line
        }
    }

    /// Returns the contained [`Right`] value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics if `EitherOrBoth` is a [`Left`] or [`Both`] variant
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(value.unwrap_only_right(), "right");
    /// ```
    ///
    /// The following examples panic
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Both("left", "right");
    /// value.unwrap_only_right(); // panics
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// value.unwrap_only_right(); // panics
    /// ```
    #[track_caller]
    pub fn unwrap_only_right(self) -> R {
        self.expect_only_right("Called EitherOrBoth::unwrap_only_right` on a `Both` or `Left` value")
    }

    /// Returns the contained [`Right`] value consuming `self`, without checking that the
    /// value is not [`Right`].
    ///
    /// # SAFETY
    ///
    /// Calling this method on a [`Left`] or [`Both`] variant is *[undefined behavior]*.
    ///
    /// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Right("right");
    /// assert_eq!(unsafe { value.unwrap_only_right_unchecked() }, "right");
    /// ```
    ///
    /// The following example introduces *[undefined behavior]*
    ///
    /// ```no_run
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str> = EitherOrBoth::Left("left");
    /// assert_eq!(unsafe { value.unwrap_only_right_unchecked() }, "right");
    /// ```
    #[track_caller]
    pub unsafe fn unwrap_only_right_unchecked(self) -> R {
        match self {
            Self::Right(right) => right,
            // SAFETY: the safety contract must be upheld by the caller.
            _ => core::hint::unreachable_unchecked(), // cov:excl-line
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Getting the contained values safely
    ////////////////////////////////////////////////////////////////////////////////

    /// If both values are present, return `Some` containing the values otherwise return
    /// `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.both(), Some((1, 'c')));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.both(), None);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.both(), None);
    /// ```
    pub fn both(self) -> Option<(L, R)> {
        match self {
            Self::Both(left, right) => Some((left, right)),
            _ => None,
        }
    }

    /// Returns [`Right`] or [`Left`] if the `EitherOrBoth` is not [`Both`] otherwise
    /// returns `other`.
    ///
    /// The `both_and` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`both_and_then`] instead. It evaluates the function lazily.
    ///
    /// [`both_and_then`]: EitherOrBoth::both_and_then
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let y: EitherOrBoth<u8, char> = EitherOrBoth::Left(2);
    /// assert_eq!(x.both_and(y), EitherOrBoth::Left(2));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let y: EitherOrBoth<u8, char> = EitherOrBoth::Left(2);
    /// assert_eq!(x.both_and(y), EitherOrBoth::Left(1));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let y: EitherOrBoth<u8, char> = EitherOrBoth::Left(2);
    /// assert_eq!(x.both_and(y), EitherOrBoth::Right('c'));
    /// ```
    pub fn both_and(self, other: Self) -> Self {
        match self {
            Self::Both(_, _) => other,
            Self::Left(_) | Self::Right(_) => self,
        }
    }

    /// Returns [`Right`] or [`Left`] if the `EitherOrBoth` is not [`Both`] otherwise
    /// calls `f` with both values and returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// fn apply_to_both(left: u8, right: char) -> EitherOrBoth<u8, char> {
    ///     EitherOrBoth::Both(left + 1, right.max('m'))
    /// }
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(x.both_and_then(apply_to_both), EitherOrBoth::Both(2, 'm'));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(x.both_and_then(apply_to_both), EitherOrBoth::Left(1));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(x.both_and_then(apply_to_both), EitherOrBoth::Right('c'));
    /// ```
    pub fn both_and_then<F>(self, f: F) -> Self
    where
        F: FnOnce(L, R) -> Self,
    {
        match self {
            Self::Both(left, right) => f(left, right),
            Self::Left(_) | Self::Right(_) => self,
        }
    }

    /// If a left value is present, return `Some` containing the value otherwise return
    /// `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.left(), Some(1));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.left(), Some(1));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.left(), None);
    /// ```
    pub fn left(self) -> Option<L> {
        match self {
            Self::Left(left) | Self::Both(left, _) => Some(left),
            Self::Right(_) => None,
        }
    }

    /// Returns [`Right`] if the `EitherOrBoth` is [`Right`] otherwise returns `other`.
    ///
    /// The `left_and` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`left_and_then`] instead. It evaluates the function lazily.
    ///
    /// [`left_and_then`]: EitherOrBoth::left_and_then
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let y: EitherOrBoth<&str, char> = EitherOrBoth::Left("left");
    /// assert_eq!(x.left_and(y), EitherOrBoth::Left("left"));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let y: EitherOrBoth<&str, char> = EitherOrBoth::Left("left");
    /// assert_eq!(x.left_and(y), EitherOrBoth::Left("left"));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let y: EitherOrBoth<&str, char> = EitherOrBoth::Left("left");
    /// assert_eq!(x.left_and(y), EitherOrBoth::Right('c'));
    /// ```
    pub fn left_and<T>(self, other: EitherOrBoth<T, R>) -> EitherOrBoth<T, R> {
        match self {
            Self::Left(_) | Self::Both(_, _) => other,
            Self::Right(right) => Right(right),
        }
    }

    /// Returns [`Right`] otherwise calls `f` with the left value and returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// fn left_to_string(x: u8) -> EitherOrBoth<String, char> {
    ///     EitherOrBoth::Left(x.to_string())
    /// }
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     x.left_and_then(left_to_string),
    ///     EitherOrBoth::Left(1.to_string())
    /// );
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(
    ///     x.left_and_then(left_to_string),
    ///     EitherOrBoth::Left(1.to_string())
    /// );
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(x.left_and_then(left_to_string), EitherOrBoth::Right('c'));
    /// ```
    pub fn left_and_then<F, T>(self, f: F) -> EitherOrBoth<T, R>
    where
        F: FnOnce(L) -> EitherOrBoth<T, R>,
    {
        match self {
            Self::Left(left) | Self::Both(left, _) => f(left),
            Self::Right(right) => Right(right),
        }
    }

    /// Returns `Some` with the [`Left`] value if present otherwise `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.only_left(), Some(1));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.only_left(), None);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.only_left(), None);
    /// ```
    pub fn only_left(self) -> Option<L> {
        match self {
            Self::Left(left) => Some(left),
            _ => None,
        }
    }

    /// If a left value is present, return `Some` containing the value otherwise return
    /// `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.right(), Some('c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.right(), Some('c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.right(), None);
    /// ```
    pub fn right(self) -> Option<R> {
        match self {
            Self::Right(right) | Self::Both(_, right) => Some(right),
            Self::Left(_) => None,
        }
    }

    /// Returns [`Left`] if the `EitherOrBoth` is [`Left`] otherwise returns `other`.
    ///
    /// The `right_and` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`right_and_then`] instead. It evaluates the function lazily.
    ///
    /// [`right_and_then`]: EitherOrBoth::right_and_then
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let y: EitherOrBoth<u8, &str> = EitherOrBoth::Right("right");
    /// assert_eq!(x.right_and(y), EitherOrBoth::Right("right"));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let y: EitherOrBoth<u8, &str> = EitherOrBoth::Right("right");
    /// assert_eq!(x.right_and(y), EitherOrBoth::Right("right"));
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let y: EitherOrBoth<u8, &str> = EitherOrBoth::Right("right");
    /// assert_eq!(x.right_and(y), EitherOrBoth::Left(1));
    /// ```
    pub fn right_and<T>(self, other: EitherOrBoth<L, T>) -> EitherOrBoth<L, T> {
        match self {
            Self::Right(_) | Self::Both(_, _) => other,
            Self::Left(left) => Left(left),
        }
    }

    /// Returns [`Left`] otherwise calls `f` with the right value and returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// fn right_to_string(x: char) -> EitherOrBoth<u8, String> {
    ///     EitherOrBoth::Right(x.to_string())
    /// }
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     x.right_and_then(right_to_string),
    ///     EitherOrBoth::Right('c'.to_string())
    /// );
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(
    ///     x.right_and_then(right_to_string),
    ///     EitherOrBoth::Right('c'.to_string())
    /// );
    ///
    /// let x: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(x.right_and_then(right_to_string), EitherOrBoth::Left(1));
    /// ```
    pub fn right_and_then<F, T>(self, f: F) -> EitherOrBoth<L, T>
    where
        F: FnOnce(R) -> EitherOrBoth<L, T>,
    {
        match self {
            Self::Right(right) | Self::Both(_, right) => f(right),
            Self::Left(left) => Left(left),
        }
    }

    /// Returns `Some` with the [`Right`] value if present otherwise `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.only_right(), Some('c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.only_right(), None);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.only_right(), None);
    /// ```
    pub fn only_right(self) -> Option<R> {
        match self {
            Self::Right(right) => Some(right),
            _ => None,
        }
    }

    /// Unzips an `EitherOrBoth` into a tuple of [`Options`][Option].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.unzip(), (Some(1), Some('c')));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.unzip(), (Some(1), None));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.unzip(), (None, Some('c')));
    /// ```
    pub fn unzip(self) -> (Option<L>, Option<R>) {
        match self {
            Self::Both(left, right) => (Some(left), Some(right)),
            Self::Left(left) => (Some(left), None),
            Self::Right(right) => (None, Some(right)),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Iterators
    ////////////////////////////////////////////////////////////////////////////////

    /// Consumes the inner iterators and returns an iterator that yields items of type
    /// `EitherOrBoth<L, R>`, combining the iterators.
    ///
    ///
    /// This iterator allows traversing inner iterators with different types
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let x = EitherOrBoth::Both(vec![1], vec!['c', 'm']);
    /// let mut iter = x.into_iter_swap(); // moves `x` and the iterators
    ///
    /// assert_eq!(iter.next(), Some(EitherOrBoth::Both(1, 'c')));
    /// assert_eq!(iter.next(), Some(EitherOrBoth::Right('m')));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn into_iter_swap(
        self,
    ) -> SwapIterEitherOrBoth<<L as IntoIterator>::IntoIter, <R as IntoIterator>::IntoIter>
    where
        L: IntoIterator,
        R: IntoIterator,
    {
        SwapIterEitherOrBoth::new(self.bimap(IntoIterator::into_iter, IntoIterator::into_iter))
    }

    /// Borrow the inner iterators and returns an iterator that yields items of type
    /// `EitherOrBoth<&L, &R>`, combining the iterators.
    ///
    /// This iterator allows traversing inner iterators with different types
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let x = EitherOrBoth::Both(vec![1], vec!['c', 'm']);
    /// let mut iter = x.iter_swap();
    ///
    /// assert_eq!(iter.next(), Some(EitherOrBoth::Both(&1, &'c')));
    /// assert_eq!(iter.next(), Some(EitherOrBoth::Right(&'m')));
    /// assert_eq!(iter.next(), None);
    ///
    /// println!("{x:?}"); // still can access `x`
    /// ```
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

    /// Mutably borrows the inner iterators returning an iterator that yields items of
    /// type `EitherOrBoth<&mut L, &mut R>`, combining the iterators.
    ///
    /// This iterator allows traversing inner iterators with different types
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut x = EitherOrBoth::Both(vec![1], vec!['c', 'm']);
    /// let mut iter = x.iter_swap_mut();
    ///
    /// assert_eq!(iter.next(), Some(EitherOrBoth::Both(&mut 1, &mut 'c')));
    /// assert_eq!(iter.next(), Some(EitherOrBoth::Right(&mut 'm')));
    /// assert_eq!(iter.next(), None);
    ///
    /// println!("{x:?}"); // still can access `x`
    /// ```
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

    ////////////////////////////////////////////////////////////////////////////////
    // Conversions
    ////////////////////////////////////////////////////////////////////////////////

    /// Converts `EitherOrBoth<L, R>` to `EitherOrBoth<R, L>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.flip(), EitherOrBoth::Both('c', 1));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.flip(), EitherOrBoth::Right(1));
    /// ```
    #[must_use]
    pub fn flip(self) -> EitherOrBoth<R, L> {
        match self {
            Self::Both(left, right) => Both(right, left),
            Self::Left(left) => Right(left),
            Self::Right(right) => Left(right),
        }
    }

    /// Applies mapping functions to the left and right values returning an
    /// `EitherOrBoth<T, U>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_left = |left: u8| left.to_string();
    /// let map_right = |right: char| right as i32;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.bimap(map_left, map_right),
    ///     EitherOrBoth::Both("1".to_owned(), 99i32)
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(
    ///     value.bimap(map_left, map_right),
    ///     EitherOrBoth::Left("1".to_owned())
    /// );
    /// ```
    pub fn bimap<F, G, T, U>(self, f: F, g: G) -> EitherOrBoth<T, U>
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> U,
    {
        map_each!(self; l, r => f(l), g(r))
    }

    /// Applies a mapping function to the left value of [`Both`] or [`Left`] returning an
    /// `EitherOrBoth<T, R>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_left = |left: u8| left.to_string();
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.map_left(map_left),
    ///     EitherOrBoth::Both("1".to_owned(), 'c')
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.map_left(map_left), EitherOrBoth::Left("1".to_owned()));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.map_left(map_left), EitherOrBoth::Right('c'));
    /// ```
    pub fn map_left<F, T>(self, f: F) -> EitherOrBoth<T, R>
    where
        F: FnOnce(L) -> T,
    {
        map_each!(self; l, r => f(l), r)
    }

    /// Returns the provided default value if this is a [`Right`] or applies a mapping
    /// function to the contained left value.
    ///
    /// The `map_left_or` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function calls,
    /// use [`map_left_or_else`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_left = |left: u8| left as u64 + 1;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.map_left_or(42, map_left), 42u64);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.map_left_or(42, map_left), 2u64);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.map_left_or(42, map_left), 2u64);
    /// ```
    ///
    /// [`map_left_or_else`]: EitherOrBoth::map_left_or_else
    pub fn map_left_or<F, T>(self, default: T, f: F) -> T
    where
        F: FnOnce(L) -> T,
    {
        self.map_left_or_else(|| default, f)
    }

    /// Applies the given function to the left value of [`Left`] or [`Both`], mapping `L`
    /// to `T` otherwise returns the [default value] for type `T`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_left = |left: u8| left as u64 + 1;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.map_left_or_default(map_left), 0u64);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.map_left_or_default(map_left), 2u64);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.map_left_or_default(map_left), 2u64);
    /// ```
    ///
    /// [default value]: Default::default
    pub fn map_left_or_default<T, F>(self, f: F) -> T
    where
        T: Default,
        F: FnOnce(L) -> T,
    {
        self.map_left_or_else(T::default, f)
    }

    /// Applies the given function to the left value of [`Left`] or [`Both`], mapping `L`
    /// to `T` otherwise applies a different function.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_left = |left: u8| left.to_string();
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(
    ///     value.map_left_or_else(|| String::from("left"), map_left),
    ///     String::from("left")
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.map_left_or_else(|| String::from("left"), map_left),
    ///     1.to_string()
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(
    ///     value.map_left_or_else(|| String::from("left"), map_left),
    ///     1.to_string()
    /// );
    /// ```
    pub fn map_left_or_else<D, F, T>(self, default: D, f: F) -> T
    where
        D: FnOnce() -> T,
        F: FnOnce(L) -> T,
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => default(),
        }
    }

    /// Applies a mapping function to the right value of [`Both`] or [`Right`] returning
    /// an `EitherOrBoth<L, T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_right = |right: char| right.to_string();
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.map_right(map_right),
    ///     EitherOrBoth::Both(1, "c".to_owned())
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(
    ///     value.map_right(map_right),
    ///     EitherOrBoth::Right("c".to_owned())
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.map_right(map_right), EitherOrBoth::Left(1));
    /// ```
    pub fn map_right<T>(self, f: fn(R) -> T) -> EitherOrBoth<L, T> {
        map_each!(self; l, r => l, f(r))
    }

    /// Returns the provided default value if this is a [`Left`] or applies a mapping
    /// function to the contained right value.
    ///
    /// The `map_right_or` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function calls,
    /// use [`map_right_or_else`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_right = |right: char| right as i32;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.map_right_or(42, map_right), 99i32);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.map_right_or(42, map_right), 42i32);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.map_right_or(42, map_right), 99i32);
    /// ```
    ///
    /// [`map_right_or_else`]: EitherOrBoth::map_right_or_else
    pub fn map_right_or<U, F>(self, default: U, f: F) -> U
    where
        F: FnOnce(R) -> U,
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => default,
        }
    }

    /// Applies the given function to the right value of [`Right`] or [`Both`], mapping
    /// `R` to `T` otherwise returns the [default value] for type `T`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_right = |right: char| right as u64 + 1;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.map_right_or_default(map_right), 100u64);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.map_right_or_default(map_right), 0u64);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.map_right_or_default(map_right), 100u64);
    /// ```
    ///
    /// [default value]: Default::default
    pub fn map_right_or_default<T, F>(self, f: F) -> T
    where
        T: Default,
        F: FnOnce(R) -> T,
    {
        self.map_right_or_else(T::default, f)
    }

    /// Applies the given function to the right value of [`Right`] or [`Both`], mapping
    /// `R` to `T` otherwise applies a different function.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let map_right = |right: char| right.to_string();
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(
    ///     value.map_right_or_else(|| String::from("right"), map_right),
    ///     "c".to_owned()
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.map_right_or_else(|| String::from("right"), map_right),
    ///     "c".to_owned()
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(
    ///     value.map_right_or_else(|| String::from("right"), map_right),
    ///     String::from("right")
    /// );
    /// ```
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

    /// Calls functions with a reference to the contained values returning the original
    /// `EitherOrBoth`.
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// // Prints a single line with "Left is: 1"
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let left = value
    ///     .biinspect(|l| println!("Left is: {l}"), |r| println!("Right is: {r}"))
    ///     .expect_left("should be a left value");
    /// assert_eq!(left, 1);
    ///
    /// // Prints two lines:
    /// // Left is: 1
    /// // Right is: c
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let both = value
    ///     .biinspect(|l| println!("Left is: {l}"), |r| println!("Right is: {r}"))
    ///     .expect_both("both values should be present");
    ///
    /// assert_eq!(both, (1, 'c'));
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    pub fn biinspect<F, G>(self, f: F, g: G) -> Self
    where
        for<'a> F: Fn(&'a L),
        for<'a> G: Fn(&'a R),
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

    /// Calls a function with a reference to the contained left value if this is a
    /// [`Left`] or [`Both`] returning the original `EitherOrBoth`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// // Prints a single line with "Left is: 1"
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let left = value
    ///     .inspect_left(|l| println!("Left is: {l}"))
    ///     .expect_left("should be a left value");
    /// assert_eq!(left, 1);
    ///
    /// // Prints nothing
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let right = value
    ///     .inspect_left(|l| println!("Left is: {l}"))
    ///     .expect_right("should be a right value");
    ///
    /// assert_eq!(right, 'c');
    /// ```
    pub fn inspect_left<F>(self, f: F) -> Self
    where
        for<'a> F: Fn(&'a L),
    {
        match &self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => {}
        }

        self
    }

    /// Calls a function with a reference to the contained right value if this is a
    /// [`Right`] or [`Both`] returning the original `EitherOrBoth`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// // Prints a single line with "Right is: c"
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let right = value
    ///     .inspect_right(|r| println!("Right is: {r}"))
    ///     .expect_right("should be a right value");
    /// assert_eq!(right, 'c');
    ///
    /// // Prints nothing
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let left = value
    ///     .inspect_right(|r| println!("Right is: {r}"))
    ///     .expect_left("should be a left value");
    ///
    /// assert_eq!(left, 1);
    /// ```
    pub fn inspect_right<F>(self, f: F) -> Self
    where
        for<'a> F: Fn(&'a R),
    {
        match &self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => {}
        }

        self
    }

    /// Consumes this `EitherOrBoth` applying functions to the contained values taking
    /// mutable references to capture variables.
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut left = vec![];
    /// let mut right = vec![];
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// value.biapply(|l| left.push(l), |r| right.push(r)); // moves `value`
    ///
    /// assert_eq!(left, vec![1]);
    /// assert_eq!(right, vec!['c']);
    /// ```
    ///
    /// The following example will not compile with the error: "cannot borrow `both` as
    /// mutable more than once at a time".
    ///
    /// If you need to apply a function that requires mutable access to both elements
    /// simultaneously, consider using [`biapply_with`] instead.
    ///
    /// ```compile_fail
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut both = vec![];
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// value.biapply(|l| both.push(l), |r| both.push(r as u8));
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    /// [`biapply_with`]: EitherOrBoth::biapply_with
    pub fn biapply<F, G>(self, mut f: F, mut g: G)
    where
        F: FnMut(L),
        G: FnMut(R),
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

    /// Consumes this `EitherOrBoth` applying functions to the contained values and a
    /// given accumulator.
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut both = vec![];
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// value.biapply_with(&mut both, |acc, l| acc.push(l), |acc, r| acc.push(r as u8));
    ///
    /// assert_eq!(both, vec![1, 99]);
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    pub fn biapply_with<F, G, Acc>(self, acc: &mut Acc, mut f: F, mut g: G)
    where
        for<'a> F: FnMut(&'a mut Acc, L),
        for<'a> G: FnMut(&'a mut Acc, R),
    {
        match self {
            Self::Both(left, right) => {
                f(acc, left);
                g(acc, right);
            }
            Self::Left(left) => f(acc, left),
            Self::Right(right) => g(acc, right),
        }
    }

    /// Consumes this `EitherOrBoth` applying a function to the contained left value if
    /// this is a [`Left`] or [`Both`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut left = vec![];
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// value.apply_left(|l| left.push(l));
    /// assert_eq!(left, vec![1]);
    ///
    /// let mut left = vec![];
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// value.apply_left(|l| left.push(l));
    /// assert_eq!(left.is_empty(), true);
    /// ```
    pub fn apply_left<F>(self, mut f: F)
    where
        F: FnMut(L),
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(_) => {}
        }
    }

    /// Consumes this `EitherOrBoth` applying a function to the contained right value if
    /// this is a [`Right`] or [`Both`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut right = vec![];
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// value.apply_right(|r| right.push(r));
    /// assert_eq!(right, vec!['c']);
    ///
    /// let mut right = vec![];
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// value.apply_right(|r| right.push(r));
    /// assert_eq!(right.is_empty(), true);
    /// ```
    pub fn apply_right<F>(self, mut f: F)
    where
        F: FnMut(R),
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => f(right),
            Self::Left(_) => {}
        }
    }

    /// Returns the left value otherwise applies a function to a [`Right`] variant,
    /// converting it into an `L` value.
    ///
    /// # Example
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.reduce_left(|r| r as u8), 1);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.reduce_left(|r| r as u8), 99);
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.reduce_left(|r| r as u8), 1);
    /// ```
    pub fn reduce_left<F>(self, f: F) -> L
    where
        F: FnOnce(R) -> L,
    {
        self.reduce_map_left(|l| l, f)
    }

    /// Like [`reduce_left`] but with mapping functions which convert the `L` and `R`
    /// values to a uniform type.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.reduce_map_left(|l| l.to_string(), |r| r.to_string()),
    ///     "1".to_owned()
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(
    ///     value.reduce_map_left(|l| l.to_string(), |r| r.to_string()),
    ///     "1".to_owned()
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(
    ///     value.reduce_map_left(|l| l.to_string(), |r| r.to_string()),
    ///     "c".to_owned()
    /// );
    /// ```
    ///
    /// [`bimap`]: EitherOrBoth::bimap
    /// [`reduce_left`]: EitherOrBoth::reduce_left
    pub fn reduce_map_left<F, G, T>(self, f: F, g: G) -> T
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> T,
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }
    }

    /// Returns the right value otherwise applies a function to a [`Left`] variant,
    /// converting it into an `R` value.
    ///
    /// # Example
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(0, 'c');
    /// assert_eq!(value.reduce_right(|l| l as char), 'c');
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.reduce_right(|l| l as char), 'c');
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(0);
    /// assert_eq!(value.reduce_right(|l| l as char), '\0');
    /// ```
    pub fn reduce_right<F>(self, f: F) -> R
    where
        F: FnOnce(L) -> R,
    {
        self.reduce_map_right(f, |r| r)
    }

    /// Like [`reduce_right`] but with mapping functions which convert the `L` and `R`
    /// values to a uniform type.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.reduce_map_right(|l| l.to_string(), |r| r.to_string()),
    ///     "c".to_owned()
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(
    ///     value.reduce_map_right(|l| l.to_string(), |r| r.to_string()),
    ///     "c".to_owned()
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(
    ///     value.reduce_map_right(|l| l.to_string(), |r| r.to_string()),
    ///     "1".to_owned()
    /// );
    /// ```
    ///
    /// [`bimap`]: EitherOrBoth::bimap
    /// [`reduce_right`]: EitherOrBoth::reduce_right
    pub fn reduce_map_right<F, G, T>(self, f: F, g: G) -> T
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> T,
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => g(right),
            Self::Left(left) => f(left),
        }
    }

    /// Transforms the `EitherOrBoth<L, R>` into a `Result<R, L>`.
    ///
    /// Following the [convention], the left value represents an error and a right value
    /// represents a correct value. Also, the evaluation of error values takes
    /// precedence if [`Both`] values are present.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<&str, u8> = EitherOrBoth::Right(1);
    /// assert_eq!(value.ok(), Ok(1));
    ///
    /// let value: EitherOrBoth<&str, u8> = EitherOrBoth::Both("this is an error", 1);
    /// assert_eq!(value.ok(), Err("this is an error"));
    ///
    /// let value: EitherOrBoth<&str, u8> = EitherOrBoth::Left("this is an error");
    /// assert_eq!(value.ok(), Err("this is an error"));
    /// ```
    ///
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    pub fn ok(self) -> Result<R, L> {
        match self {
            Self::Both(left, _) | Self::Left(left) => Err(left),
            Self::Right(right) => Ok(right),
        }
    }

    /// Transforms the `EitherOrBoth<L, R>` into a `Result<R, L>` using the provided
    /// `error` as error value.
    ///
    /// Following the [convention], the left value represents an error and a right value
    /// represents a correct value. Also, the evaluation of the `error` value takes
    /// precedence if [`Both`] values are present.
    ///
    /// The `ok_or` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`ok_or_else`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.ok_or("error message"), Ok('c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.ok_or("error message"), Err("error message"));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.ok_or("error message"), Err("error message"));
    /// ```
    ///
    /// [`ok_or_else`]: EitherOrBoth::ok_or_else
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    pub fn ok_or<E>(self, error: E) -> Result<R, E> {
        self.ok_or_else(|| error)
    }

    /// Transforms the `EitherOrBoth<L, R>` into a `Result<R, L>` using the result of an
    /// `error` function as error value.
    ///
    /// Following the [convention], the left value represents an error and a right value
    /// represents a correct value. Also, the evaluation of the `error` value takes
    /// precedence if [`Both`] values are present.
    ///
    /// The `ok_or` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function calls,
    /// use [`ok_or_else`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.ok_or_else(|| String::from("error message")), Ok('c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(
    ///     value.ok_or_else(|| String::from("error message")),
    ///     Err(String::from("error message"))
    /// );
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(
    ///     value.ok_or_else(|| String::from("error message")),
    ///     Err(String::from("error message"))
    /// );
    /// ```
    ///
    /// [`ok_or_else`]: EitherOrBoth::ok_or_else
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    pub fn ok_or_else<F, E>(self, error: F) -> Result<R, E>
    where
        F: FnOnce() -> E,
    {
        match self {
            Self::Both(_, _) | Self::Left(_) => Err(error()),
            Self::Right(ok) => Ok(ok),
        }
    }

    /// Returns a tuple (L, R) with the provided values filling in any missing left or
    /// right value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.or(2, 'm'), (1, 'c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.or(2, 'm'), (1, 'm'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.or(2, 'm'), (2, 'c'));
    /// ```
    pub fn or(self, left: L, right: R) -> (L, R) {
        match self {
            Self::Both(left, right) => (left, right),
            Self::Left(left) => (left, right),
            Self::Right(right) => (left, right),
        }
    }

    /// Returns a tuple (L, R) where any missing left or right value is replaced with its
    /// respective default value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.or_default(), (1, 'c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.or_default(), (1, '\0'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.or_default(), (0, 'c'));
    /// ```
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

    /// Returns a tuple (L, R) where any missing left or right value is computed with the
    /// given functions.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.or_else(|| 1 + 1, || char::from(100)), (1, 'c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.or_else(|| 1 + 1, || char::from(100)), (1, 'd'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.or_else(|| 1 + 1, || char::from(100)), (2, 'c'));
    /// ```
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

    /// Converts into a [`Left`] variant, using the contained left value or applying a
    /// mapping function to the [`Right`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// assert_eq!(value.into_left(|r| r as u8), EitherOrBoth::Left(1));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// assert_eq!(value.into_left(|r| r as u8), EitherOrBoth::Left(1));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.into_left(|r| r as u8), EitherOrBoth::Left(99));
    /// ```
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

    /// Converts into a [`Right`] variant, using the contained right value or applying a
    /// mapping function to the [`Left`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(0, 'c');
    /// assert_eq!(value.into_right(|l| l as char), EitherOrBoth::Right('c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// assert_eq!(value.into_right(|l| l as char), EitherOrBoth::Right('c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(0);
    /// assert_eq!(value.into_right(|l| l as char), EitherOrBoth::Right('\0'));
    /// ```
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

    ////////////////////////////////////////////////////////////////////////////////
    // Replacing and inserting values
    ////////////////////////////////////////////////////////////////////////////////

    /// Inserts `left` into the `EitherOrBoth`, then returns a mutable reference to it
    ///
    /// The [`Right`] variant transforms into a [`Both`] variant.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x = value.insert_left(2);
    /// assert_eq!(*x, 2);
    /// assert_eq!(value, EitherOrBoth::Both(2, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x = value.insert_left(2);
    /// assert_eq!(*x, 2);
    /// assert_eq!(value, EitherOrBoth::Both(2, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x = value.insert_left(2);
    /// assert_eq!(*x, 2);
    /// assert_eq!(value, EitherOrBoth::Left(2));
    /// ```
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
                    // This is safe since we just filled the `Both` value
                    match self {
                        Self::Both(left, _) => left,
                        _ => unreachable_unchecked(), // cov:excl-line
                    }
                }
            }
        }
    }

    /// Inserts `right` into the `EitherOrBoth`, then returns a mutable reference to it
    ///
    /// The [`Left`] variant transforms into a [`Both`] variant.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x = value.insert_right('m');
    /// assert_eq!(*x, 'm');
    /// assert_eq!(value, EitherOrBoth::Both(1, 'm'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x = value.insert_right('m');
    /// assert_eq!(*x, 'm');
    /// assert_eq!(value, EitherOrBoth::Both(1, 'm'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x = value.insert_right('m');
    /// assert_eq!(*x, 'm');
    /// assert_eq!(value, EitherOrBoth::Right('m'));
    /// ```
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
                    // This is safe since we just filled the `Both` value
                    match self {
                        Self::Both(_, right) => right,
                        _ => unreachable_unchecked(), // cov:excl-line
                    }
                }
            }
        }
    }

    /// Returns a mutable reference to the contained left value if present; otherwise,
    /// inserts `value` into the [`Right`] variant and returns a mutable reference to
    /// it.
    ///
    /// The `left_or_insert` combinator eagerly evaluates its arguments, which can result
    /// in unnecessary computations. When chaining operations that involve function
    /// calls, use [`left_or_insert_with`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x: &mut u8 = value.left_or_insert(2);
    /// assert_eq!(*x, 1);
    /// assert_eq!(value, EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x: &mut u8 = value.left_or_insert(2);
    /// assert_eq!(*x, 1);
    /// assert_eq!(value, EitherOrBoth::Left(1));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x: &mut u8 = value.left_or_insert(2);
    /// assert_eq!(*x, 2);
    /// assert_eq!(value, EitherOrBoth::Both(2, 'c'));
    /// ```
    ///
    /// [`left_or_insert_with`]: EitherOrBoth::left_or_insert_with
    pub fn left_or_insert(&mut self, value: L) -> &mut L {
        self.left_or_insert_with(|| value)
    }

    /// Like [`left_or_insert`] but inserts the default `L` value into the [`Right`]
    /// variant and returns a mutable reference to it.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x: &mut u8 = value.left_or_insert_default();
    /// assert_eq!(*x, 1);
    /// assert_eq!(value, EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x: &mut u8 = value.left_or_insert_default();
    /// assert_eq!(*x, 1);
    /// assert_eq!(value, EitherOrBoth::Left(1));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x: &mut u8 = value.left_or_insert_default();
    /// assert_eq!(*x, 0);
    /// assert_eq!(value, EitherOrBoth::Both(0, 'c'));
    /// ```
    ///
    /// [`left_or_insert`]: EitherOrBoth::left_or_insert
    pub fn left_or_insert_default(&mut self) -> &mut L
    where
        L: Default,
    {
        self.left_or_insert_with(L::default)
    }

    /// Like [`left_or_insert`] but computes a left value from a given function inserting
    /// into the [`Right`] variant and returning a mutable reference to it.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x: &mut u8 = value.left_or_insert_with(|| 1 + 1);
    /// assert_eq!(*x, 1);
    /// assert_eq!(value, EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x: &mut u8 = value.left_or_insert_with(|| 1 + 1);
    /// assert_eq!(*x, 1);
    /// assert_eq!(value, EitherOrBoth::Left(1));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x: &mut u8 = value.left_or_insert_with(|| 1 + 1);
    /// assert_eq!(*x, 2);
    /// assert_eq!(value, EitherOrBoth::Both(2, 'c'));
    /// ```
    ///
    /// [`left_or_insert`]: EitherOrBoth::left_or_insert
    pub fn left_or_insert_with<F>(&mut self, f: F) -> &mut L
    where
        F: FnOnce() -> L,
    {
        match self {
            Self::Both(left, _) | Self::Left(left) => left,
            Self::Right(_) => self.insert_left(f()),
        }
    }

    /// Returns a mutable reference to the contained right value if present; otherwise,
    /// inserts `value` into the [`Left`] variant and returns a mutable reference to
    /// it.
    ///
    /// The `right_or_insert` combinator eagerly evaluates its arguments, which can result
    /// in unnecessary computations. When chaining operations that involve function
    /// calls, use [`right_or_insert_with`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x: &mut char = value.right_or_insert('m');
    /// assert_eq!(*x, 'c');
    /// assert_eq!(value, EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x: &mut char = value.right_or_insert('m');
    /// assert_eq!(*x, 'm');
    /// assert_eq!(value, EitherOrBoth::Both(1, 'm'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x: &mut char = value.right_or_insert('m');
    /// assert_eq!(*x, 'c');
    /// assert_eq!(value, EitherOrBoth::Right('c'));
    /// ```
    ///
    /// [`right_or_insert_with`]: EitherOrBoth::right_or_insert_with
    pub fn right_or_insert(&mut self, value: R) -> &mut R {
        self.right_or_insert_with(|| value)
    }

    /// Like [`right_or_insert`] but inserts the default `R` value into the [`Left`]
    /// variant and returns a mutable reference to it.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x: &mut char = value.right_or_insert_default();
    /// assert_eq!(*x, 'c');
    /// assert_eq!(value, EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x: &mut char = value.right_or_insert_default();
    /// assert_eq!(*x, '\0');
    /// assert_eq!(value, EitherOrBoth::Both(1, '\0'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x: &mut char = value.right_or_insert_default();
    /// assert_eq!(*x, 'c');
    /// assert_eq!(value, EitherOrBoth::Right('c'));
    /// ```
    ///
    /// [`right_or_insert`]: EitherOrBoth::right_or_insert
    pub fn right_or_insert_default(&mut self) -> &mut R
    where
        R: Default,
    {
        self.right_or_insert_with(R::default)
    }

    /// Like [`right_or_insert`] but computes a right value from a given function
    /// inserting into the [`Left`] variant and returning a mutable reference to it.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let x: &mut char = value.right_or_insert_with(|| char::from(109));
    /// assert_eq!(*x, 'c');
    /// assert_eq!(value, EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let x: &mut char = value.right_or_insert_with(|| char::from(109));
    /// assert_eq!(*x, 'm');
    /// assert_eq!(value, EitherOrBoth::Both(1, 'm'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let x: &mut char = value.right_or_insert_with(|| char::from(109));
    /// assert_eq!(*x, 'c');
    /// assert_eq!(value, EitherOrBoth::Right('c'));
    /// ```
    ///
    /// [`right_or_insert`]: EitherOrBoth::right_or_insert
    pub fn right_or_insert_with<F>(&mut self, f: F) -> &mut R
    where
        F: FnOnce() -> R,
    {
        match self {
            Self::Both(_, right) | Self::Right(right) => right,
            Self::Left(_) => self.insert_right(f()),
        }
    }

    /// Replaces any `left` and `right` values returning the original `EitherOrBoth`
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let old = value.replace_any(2, 'm');
    /// assert_eq!(old, EitherOrBoth::Both(1, 'c'));
    /// assert_eq!(value, EitherOrBoth::Both(2, 'm'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let old = value.replace_any(2, 'm');
    /// assert_eq!(old, EitherOrBoth::Left(1));
    /// assert_eq!(value, EitherOrBoth::Left(2));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let old = value.replace_any(2, 'm');
    /// assert_eq!(old, EitherOrBoth::Right('c'));
    /// assert_eq!(value, EitherOrBoth::Right('m'));
    /// ```
    pub fn replace_any(&mut self, left: L, right: R) -> Self {
        map_each!(self; l, r => mem::replace(l, left), mem::replace(r, right))
    }

    /// Replaces the `left` value returning the old value if present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let old = value.replace_left(2);
    /// assert_eq!(old, Some(1));
    /// assert_eq!(value, EitherOrBoth::Both(2, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let old = value.replace_left(2);
    /// assert_eq!(old, Some(1));
    /// assert_eq!(value, EitherOrBoth::Left(2));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let old = value.replace_left(2);
    /// assert_eq!(old, None);
    /// assert_eq!(value, EitherOrBoth::Right('c'));
    /// ```
    pub fn replace_left(&mut self, value: L) -> Option<L> {
        match self {
            Self::Both(left, _) | Self::Left(left) => Some(mem::replace(left, value)),
            Self::Right(_) => None,
        }
    }

    /// Replaces the `right` value returning the old value if present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let old = value.replace_right('m');
    /// assert_eq!(old, Some('c'));
    /// assert_eq!(value, EitherOrBoth::Both(1, 'm'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Right('c');
    /// let old = value.replace_right('m');
    /// assert_eq!(old, Some('c'));
    /// assert_eq!(value, EitherOrBoth::Right('m'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let old = value.replace_right('m');
    /// assert_eq!(old, None);
    /// assert_eq!(value, EitherOrBoth::Left(1));
    /// ```
    pub fn replace_right(&mut self, value: R) -> Option<R> {
        match self {
            Self::Both(_, right) | Self::Right(right) => Some(mem::replace(right, value)),
            Self::Left(_) => None,
        }
    }
}

impl<T> EitherOrBoth<T, T> {
    /// Consumes this `EitherOrBoth` applying a function to the contained values (of a
    /// uniform type) taking mutable references to capture variables.
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut both = vec![];
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Both('c', 'a');
    /// value.apply(|c| both.push(c));
    ///
    /// assert_eq!(both, vec!['c', 'a']);
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
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

    /// Calls a function with a reference to the contained values (of a uniform type)
    /// returning the original `EitherOrBoth`.
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// // Prints a single line with "The value is: c"
    /// let value: EitherOrBoth<char> = EitherOrBoth::Left('c');
    /// let left = value
    ///     .inspect(|c| println!("The value is: {c}"))
    ///     .expect_left("should be a left value");
    /// assert_eq!(left, 'c');
    ///
    /// // Prints two lines:
    /// // The value is: c
    /// // The value is: a
    /// let value: EitherOrBoth<char> = EitherOrBoth::Both('c', 'a');
    /// let both = value
    ///     .inspect(|c| println!("The value is: {c}"))
    ///     .expect_both("both values should be present");
    ///
    /// assert_eq!(both, ('c', 'a'));
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
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

    /// Applies a mapping function to the left and right values (of a uniform type)
    /// returning an `EitherOrBoth<U, U>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Both('c', 'a');
    /// assert_eq!(
    ///     value.map(|c| c.to_string()),
    ///     EitherOrBoth::Both("c".to_owned(), "a".to_owned())
    /// );
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Left('c');
    /// assert_eq!(
    ///     value.map(|c| c.to_string()),
    ///     EitherOrBoth::Left("c".to_owned())
    /// );
    /// ```
    pub fn map<F, U>(self, f: F) -> EitherOrBoth<U, U>
    where
        F: Fn(T) -> U,
    {
        map_each!(self; l, r => f(l), f(r))
    }

    /// Returns the contained [`Left`] or [`Right`] value otherwise applies a function,
    /// converting [`Both`] into an single value.
    ///
    /// # Example
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8> = EitherOrBoth::Both(3, 1);
    /// assert_eq!(value.reduce(|l, r| l + r), 4);
    ///
    /// let value: EitherOrBoth<u8> = EitherOrBoth::Right(2);
    /// assert_eq!(value.reduce(|l, r| l + r), 2);
    ///
    /// let value: EitherOrBoth<u8> = EitherOrBoth::Left(3);
    /// assert_eq!(value.reduce(|l, r| l + r), 3);
    /// ```
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

    //////////////////////////////////////////////////////////////////////////////////////////
    // Iterators
    //////////////////////////////////////////////////////////////////////////////////////////

    /// Returns an iterator over the contained values of a uniform type
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
    /// let mut iter = value.iter();
    /// assert_eq!(iter.next(), Some(&'c'));
    /// assert_eq!(iter.next(), Some(&'a'));
    /// assert_eq!(iter.next(), None);
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Left('c');
    /// let mut iter = value.iter();
    /// assert_eq!(iter.next(), Some(&'c'));
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    pub fn iter(&self) -> IterEitherOrBoth<'_, T> {
        IterEitherOrBoth::new(self)
    }

    /// Returns an iterator over the contained mutable values of a uniform type
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<char> = EitherOrBoth::Both('c', 'a');
    /// let mut iter = value.iter_mut();
    /// assert_eq!(iter.next(), Some(&mut 'c'));
    /// assert_eq!(iter.next(), Some(&mut 'a'));
    /// assert_eq!(iter.next(), None);
    ///
    /// let mut value: EitherOrBoth<char> = EitherOrBoth::Left('c');
    /// let mut iter = value.iter_mut();
    /// assert_eq!(iter.next(), Some(&mut 'c'));
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    pub fn iter_mut(&mut self) -> IterMutEitherOrBoth<'_, T> {
        IterMutEitherOrBoth::new(self)
    }

    /// Consumes the `EitherOrBoth`, returning a chained iterator over the contained
    /// iterators of a uniform type
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// For iteration over contained iterators with non-uniform types, you can use
    /// [`into_iter_swap`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<Vec<char>> = EitherOrBoth::Both(vec!['c', 'a'], vec!['b']);
    /// let collected: Vec<char> = value.into_iter_chain().collect();
    /// assert_eq!(collected, vec!['c', 'a', 'b']);
    ///
    /// let value: EitherOrBoth<Vec<char>> = EitherOrBoth::Left(vec!['c', 'a']);
    /// let collected: Vec<char> = value.into_iter_chain().collect();
    /// assert_eq!(collected, vec!['c', 'a']);
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    /// [`into_iter_swap`]: EitherOrBoth::into_iter_swap
    pub fn into_iter_chain(self) -> ChainedIterEitherOrBoth<<T as IntoIterator>::IntoIter>
    where
        T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.map(IntoIterator::into_iter))
    }

    /// Returns a chained iterator over the contained iterators of a uniform type
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// For iteration over contained iterators with non-uniform types, you can use
    /// [`iter_swap`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<Vec<char>> = EitherOrBoth::Both(vec!['c', 'a'], vec!['b']);
    /// let collected: Vec<&char> = value.iter_chain().collect();
    /// assert_eq!(collected, vec![&'c', &'a', &'b']);
    ///
    /// let value: EitherOrBoth<Vec<char>> = EitherOrBoth::Left(vec!['c', 'a']);
    /// let collected: Vec<&char> = value.iter_chain().collect();
    /// assert_eq!(collected, vec![&'c', &'a']);
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    /// [`iter_swap`]: EitherOrBoth::iter_swap
    pub fn iter_chain(&self) -> ChainedIterEitherOrBoth<<&T as IntoIterator>::IntoIter>
    where
        for<'a> &'a T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.as_ref().map(IntoIterator::into_iter))
    }

    /// Returns a chained iterator over the mutable values of the contained iterators of a
    /// uniform type
    ///
    /// The evaluation order is from left to right if this is a [`Both`] variant. To
    /// reverse the order use [`flip`].
    ///
    /// For iteration over contained iterators with non-uniform types, you can use
    /// [`iter_swap_mut`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<Vec<char>> = EitherOrBoth::Both(vec!['c', 'a'], vec!['b']);
    /// let collected: Vec<&mut char> = value.iter_chain_mut().collect();
    /// assert_eq!(collected, vec![&mut 'c', &mut 'a', &mut 'b']);
    ///
    /// let mut value: EitherOrBoth<Vec<char>> = EitherOrBoth::Left(vec!['c', 'a']);
    /// let collected: Vec<&mut char> = value.iter_chain_mut().collect();
    /// assert_eq!(collected, vec![&mut 'c', &mut 'a']);
    /// ```
    ///
    /// [`flip`]: EitherOrBoth::flip
    /// [`iter_swap_mut`]: EitherOrBoth::iter_swap_mut
    pub fn iter_chain_mut(&mut self) -> ChainedIterEitherOrBoth<<&mut T as IntoIterator>::IntoIter>
    where
        for<'a> &'a mut T: IntoIterator,
    {
        ChainedIterEitherOrBoth::new(self.as_mut().map(IntoIterator::into_iter))
    }
}

impl<L, R> EitherOrBoth<&L, &R> {
    /// Converts an `EitherOrBoth<&L, &R>` into an `EitherOrBoth<L, R>` by cloning its
    /// contents
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let refs: EitherOrBoth<&u8, &char> = value.as_ref();
    /// assert_eq!(refs.cloned(), EitherOrBoth::Both(1, 'c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let refs: EitherOrBoth<&u8, &char> = value.as_ref();
    /// assert_eq!(refs.cloned(), EitherOrBoth::Left(1));
    /// ```
    #[must_use]
    pub fn cloned(self) -> EitherOrBoth<L, R>
    where
        L: Clone,
        R: Clone,
    {
        map_each!(self; l, r => l.clone(), r.clone())
    }

    /// Converts an `EitherOrBoth<&L, &R>` into an `EitherOrBoth<L, R>` by copying its
    /// contents
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let refs: EitherOrBoth<&u8, &char> = value.as_ref();
    /// assert_eq!(refs.copied(), EitherOrBoth::Both(1, 'c'));
    ///
    /// let value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let refs: EitherOrBoth<&u8, &char> = value.as_ref();
    /// assert_eq!(refs.copied(), EitherOrBoth::Left(1));
    /// ```
    #[must_use]
    pub const fn copied(self) -> EitherOrBoth<L, R>
    where
        L: Copy,
        R: Copy,
    {
        map_each!(self; l, r => *l, *r)
    }
}

impl<L, R> EitherOrBoth<&mut L, &mut R> {
    /// Converts an `EitherOrBoth<&mut L, &mut R>` into an `EitherOrBoth<L, R>` by cloning
    /// its contents
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let refs: EitherOrBoth<&mut u8, &mut char> = value.as_mut();
    /// assert_eq!(refs.cloned(), EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let refs: EitherOrBoth<&mut u8, &mut char> = value.as_mut();
    /// assert_eq!(refs.cloned(), EitherOrBoth::Left(1));
    /// ```
    #[must_use]
    pub fn cloned(self) -> EitherOrBoth<L, R>
    where
        L: Clone,
        R: Clone,
    {
        map_each!(self; l, r => l.clone(), r.clone())
    }

    /// Converts an `EitherOrBoth<&mut L, &mut R>` into an `EitherOrBoth<L, R>` by copying
    /// its contents
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Both(1, 'c');
    /// let refs: EitherOrBoth<&mut u8, &mut char> = value.as_mut();
    /// assert_eq!(refs.copied(), EitherOrBoth::Both(1, 'c'));
    ///
    /// let mut value: EitherOrBoth<u8, char> = EitherOrBoth::Left(1);
    /// let refs: EitherOrBoth<&mut u8, &mut char> = value.as_mut();
    /// assert_eq!(refs.copied(), EitherOrBoth::Left(1));
    /// ```
    #[must_use]
    pub fn copied(self) -> EitherOrBoth<L, R>
    where
        L: Copy,
        R: Copy,
    {
        map_each!(self; l, r => *l, *r)
    }
}

impl<L1, L2, R1, R2> EitherOrBoth<(L1, R1), (L2, R2)> {
    /// Transposes a `EitherOrBoth` of tuples to a tuple of `EitherOrBoths`
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<(u8, char), (i32, u64)> = EitherOrBoth::Both((1, 'c'), (-2, 10));
    /// assert_eq!(
    ///     value.transpose(),
    ///     (EitherOrBoth::Both(1, -2), EitherOrBoth::Both('c', 10))
    /// );
    ///
    /// let value: EitherOrBoth<(u8, char), (i32, u64)> = EitherOrBoth::Left((1, 'c'));
    /// assert_eq!(
    ///     value.transpose(),
    ///     (EitherOrBoth::Left(1), EitherOrBoth::Left('c'))
    /// );
    ///
    /// let value: EitherOrBoth<(u8, char), (i32, u64)> = EitherOrBoth::Right((-2, 10));
    /// assert_eq!(
    ///     value.transpose(),
    ///     (EitherOrBoth::Right(-2), EitherOrBoth::Right(10))
    /// );
    /// ```
    pub fn transpose(self) -> (EitherOrBoth<L1, L2>, EitherOrBoth<R1, R2>) {
        match self {
            Self::Both((l1, r1), (l2, r2)) => (Both(l1, l2), Both(r1, r2)),
            Self::Left((l1, r1)) => (Left(l1), Left(r1)),
            Self::Right((l2, r2)) => (Right(l2), Right(r2)),
        }
    }
}

impl<L, R1, R2> EitherOrBoth<(L, R1), (L, R2)> {
    /// Transposes an `EitherOrBoth` of tuples to a tuple of a single value and an
    /// `EitherOrBoth` with the left value having a uniform type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<(u8, char), (u8, i32)> = EitherOrBoth::Both((1, 'c'), (2, -10));
    /// assert_eq!(
    ///     value.transpose_left(|l, r| l + r),
    ///     (3, EitherOrBoth::Both('c', -10))
    /// );
    ///
    /// let value: EitherOrBoth<(u8, char), (u8, i32)> = EitherOrBoth::Left((1, 'c'));
    /// assert_eq!(
    ///     value.transpose_left(|l, r| l + r),
    ///     (1, EitherOrBoth::Left('c'))
    /// );
    ///
    /// let value: EitherOrBoth<(u8, char), (u8, i32)> = EitherOrBoth::Right((2, -10));
    /// assert_eq!(
    ///     value.transpose_left(|l, r| l + r),
    ///     (2, EitherOrBoth::Right(-10))
    /// );
    /// ```
    pub fn transpose_left<F>(self, f: F) -> (L, EitherOrBoth<R1, R2>)
    where
        F: FnOnce(L, L) -> L,
    {
        match self {
            Self::Both((l1, r1), (l2, r2)) => (f(l1, l2), Both(r1, r2)),
            Self::Left((l, r1)) => (l, Left(r1)),
            Self::Right((l, r2)) => (l, Right(r2)),
        }
    }
}

impl<L1, L2, R> EitherOrBoth<(L1, R), (L2, R)> {
    /// Transposes an `EitherOrBoth` of tuples to a tuple of a single value and an
    /// `EitherOrBoth` with the right value having a uniform type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<(u8, char), (i32, char)> = EitherOrBoth::Both((1, 'c'), (-2, 'a'));
    /// assert_eq!(
    ///     value.transpose_right(|l, r| l.max(r)),
    ///     (EitherOrBoth::Both(1, -2), 'c')
    /// );
    ///
    /// let value: EitherOrBoth<(u8, char), (i32, char)> = EitherOrBoth::Left((1, 'c'));
    /// assert_eq!(
    ///     value.transpose_right(|l, r| l.max(r)),
    ///     (EitherOrBoth::Left(1), 'c')
    /// );
    ///
    /// let value: EitherOrBoth<(u8, char), (i32, char)> = EitherOrBoth::Right((-2, 'a'));
    /// assert_eq!(
    ///     value.transpose_right(|l, r| l.max(r)),
    ///     (EitherOrBoth::Right(-2), 'a')
    /// );
    /// ```
    pub fn transpose_right<F>(self, f: F) -> (EitherOrBoth<L1, L2>, R)
    where
        F: FnOnce(R, R) -> R,
    {
        match self {
            Self::Both((l1, r1), (l2, r2)) => (Both(l1, l2), f(r1, r2)),
            Self::Left((l1, r)) => (Left(l1), r),
            Self::Right((l2, r)) => (Right(l2), r),
        }
    }
}

impl<L, R> EitherOrBoth<Option<L>, Option<R>> {
    /// Transposes an `EitherOrBoth` of [`Options`] to an option of `EitherOrBoths`
    ///
    /// Per [convention], if this variant is [`Both`] and at least one of the values is
    /// `None`, the evaluation of the `None` value takes precedence and results in a
    /// `None`  value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<Option<u8>, Option<char>> = EitherOrBoth::Both(Some(1), Some('c'));
    /// assert_eq!(value.transpose(), Some(EitherOrBoth::Both(1, 'c')));
    ///
    /// let value: EitherOrBoth<Option<u8>, Option<char>> = EitherOrBoth::Both(Some(1), None);
    /// assert_eq!(value.transpose(), None);
    ///
    /// let value: EitherOrBoth<Option<u8>, Option<char>> = EitherOrBoth::Left(Some(1));
    /// assert_eq!(value.transpose(), Some(EitherOrBoth::Left(1)));
    ///
    /// let value: EitherOrBoth<Option<u8>, Option<char>> = EitherOrBoth::Left(None);
    /// assert_eq!(value.transpose(), None);
    /// ```
    ///
    /// [`Options`]: Option
    /// [convention]: ./index.html#conventions-and-edge-cases
    pub fn transpose(self) -> Option<EitherOrBoth<L, R>> {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Some(left), Some(right)) => Some(Both(left, right)),
                _ => None,
            },
            Self::Left(left) => left.map(Left),
            Self::Right(right) => right.map(Right),
        }
    }
}

impl<L, R, E1, E2> EitherOrBoth<Result<L, E1>, Result<R, E2>> {
    /// Transposes an `EitherOrBoth` of [`Results`] to a [`Result`] of `EitherOrBoths`
    ///
    /// Per [convention], if this variant is [`Both`] and at least one of the values is an
    /// error value `Err`, the evaluation of the `Err` value takes precedence and
    /// results in a `Err` value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<Result<u8, char>, Result<i32, u64>> = EitherOrBoth::Both(Ok(1), Ok(-2));
    /// assert_eq!(value.transpose(), Ok(EitherOrBoth::Both(1, -2)));
    ///
    /// // If at least one of both is is an error the result is an error
    /// let value: EitherOrBoth<Result<u8, char>, Result<i32, u64>> =
    ///     EitherOrBoth::Both(Err('c'), Ok(-2));
    /// assert_eq!(value.transpose(), Err(EitherOrBoth::Left('c')));
    ///
    /// let value: EitherOrBoth<Result<u8, char>, Result<i32, u64>> = EitherOrBoth::Left(Ok(1));
    /// assert_eq!(value.transpose(), Ok(EitherOrBoth::Left(1)));
    ///
    /// let value: EitherOrBoth<Result<u8, char>, Result<i32, u64>> = EitherOrBoth::Left(Err('c'));
    /// assert_eq!(value.transpose(), Err(EitherOrBoth::Left('c')));
    /// ```
    ///
    /// [`Results`]: Result
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    pub fn transpose(self) -> Result<EitherOrBoth<L, R>, EitherOrBoth<E1, E2>> {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Ok(left), Ok(right)) => Ok(Both(left, right)),
                (Ok(_), Err(err)) => Err(Right(err)),
                (Err(err), Ok(_)) => Err(Left(err)),
                (Err(err_left), Err(err_right)) => Err(Both(err_left, err_right)),
            },
            Self::Left(left) => match left {
                Ok(ok) => Ok(Left(ok)),
                Err(err) => Err(Left(err)),
            },
            Self::Right(right) => match right {
                Ok(ok) => Ok(Right(ok)),
                Err(err) => Err(Right(err)),
            },
        }
    }
}

impl<L, R, E> EitherOrBoth<Result<L, E>, Result<R, E>> {
    /// Transposes an `EitherOrBoth` of [`Results`] to a [`Result`] with an uniform error
    /// type
    ///
    /// When [`Both`] contains two error values, the provided function merges them into a
    /// unified error.
    ///
    /// Per [convention], if this variant is [`Both`] and at least one of the values is an
    /// error value `Err`, the evaluation of the `Err` value takes precedence and
    /// results in a `Err` value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let x: EitherOrBoth<Result<u8, char>, Result<i32, char>> = EitherOrBoth::Both(Ok(1), Ok(-2));
    /// assert_eq!(
    ///     x.transpose_err(|l, r| l.max(r)),
    ///     Ok(EitherOrBoth::Both(1, -2))
    /// );
    ///
    /// // both are errors so they need to be merged with a given function
    /// let x: EitherOrBoth<Result<u8, char>, Result<i32, char>> =
    ///     EitherOrBoth::Both(Err('c'), Err('a'));
    /// assert_eq!(x.transpose_err(|l, r| l.max(r)), Err('c'));
    ///
    /// // If at least one of the values is an error, the result is an error
    /// let x: EitherOrBoth<Result<u8, char>, Result<i32, char>> = EitherOrBoth::Both(Err('c'), Ok(-2));
    /// assert_eq!(x.transpose_err(|l, r| l.max(r)), Err('c'));
    ///
    /// let x: EitherOrBoth<Result<u8, char>, Result<i32, char>> = EitherOrBoth::Left(Ok(1));
    /// assert_eq!(x.transpose_err(|l, r| l.max(r)), Ok(EitherOrBoth::Left(1)));
    ///
    /// let x: EitherOrBoth<Result<u8, char>, Result<i32, char>> = EitherOrBoth::Left(Err('c'));
    /// assert_eq!(x.transpose_err(|l, r| l.max(r)), Err('c'));
    /// ```
    ///
    /// [`Results`]: Result
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    pub fn transpose_err<F>(self, f: F) -> Result<EitherOrBoth<L, R>, E>
    where
        F: FnOnce(E, E) -> E,
    {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Ok(left), Ok(right)) => Ok(Both(left, right)),
                (Ok(_), Err(err)) | (Err(err), Ok(_)) => Err(err),
                (Err(err_left), Err(err_right)) => Err(f(err_left, err_right)),
            },
            Self::Left(left) => left.map(Left),
            Self::Right(right) => right.map(Right),
        }
    }
}

impl<T, E1, E2> EitherOrBoth<Result<T, E1>, Result<T, E2>> {
    /// Transposes an `EitherOrBoth` of [`Results`] to a [`Result`] with an uniform
    /// correct type
    ///
    /// When [`Both`] contains two correct values, the provided function merges them into
    /// a unified value.
    ///
    /// Per [convention], if this variant is [`Both`] and at least one of the values is an
    /// error value `Err`, the evaluation of the `Err` value takes precedence and
    /// results in a `Err` value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// // both are ok so they need to be merged with a given function
    /// let x: EitherOrBoth<Result<u8, char>, Result<u8, i32>> = EitherOrBoth::Both(Ok(1), Ok(2));
    /// assert_eq!(x.transpose_ok(|l, r| l + r), Ok(3));
    ///
    /// // If at least one of the values is an error, the result is an error
    /// let x: EitherOrBoth<Result<u8, char>, Result<u8, i32>> = EitherOrBoth::Both(Err('c'), Ok(2));
    /// assert_eq!(x.transpose_ok(|l, r| l + r), Err(EitherOrBoth::Left('c')));
    ///
    /// let x: EitherOrBoth<Result<u8, char>, Result<u8, i32>> = EitherOrBoth::Both(Err('c'), Err(-2));
    /// assert_eq!(
    ///     x.transpose_ok(|l, r| l + r),
    ///     Err(EitherOrBoth::Both('c', -2))
    /// );
    ///
    /// let x: EitherOrBoth<Result<u8, char>, Result<u8, i32>> = EitherOrBoth::Left(Ok(2));
    /// assert_eq!(x.transpose_ok(|l, r| l + r), Ok(2));
    ///
    /// let x: EitherOrBoth<Result<u8, char>, Result<u8, i32>> = EitherOrBoth::Left(Err('c'));
    /// assert_eq!(x.transpose_ok(|l, r| l + r), Err(EitherOrBoth::Left('c')));
    /// ```
    ///
    /// [`Results`]: Result
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    pub fn transpose_ok<F>(self, f: F) -> Result<T, EitherOrBoth<E1, E2>>
    where
        F: FnOnce(T, T) -> T,
    {
        match self {
            Self::Both(left, right) => match (left, right) {
                (Ok(left), Ok(right)) => Ok(f(left, right)),
                (Ok(_), Err(err)) => Err(Right(err)),
                (Err(err), Ok(_)) => Err(Left(err)),
                (Err(err_left), Err(err_right)) => Err(Both(err_left, err_right)),
            },
            Self::Left(left) => left.map_err(Left),
            Self::Right(right) => right.map_err(Right),
        }
    }
}
