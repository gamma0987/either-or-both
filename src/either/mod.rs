//! The `Either` enum

macro_rules! map_each {
    ($src:expr) => {
        match $src {
            Self::Left(left) => Left(left),
            Self::Right(left) => Right(left),
        }
    };
}

macro_rules! each {
    ($src:expr, $( $rest:tt )*) => {
        match $src {
            Self::Left(left) => left $($rest)*,
            Self::Right(right) => right $($rest)*,
        }
    };
    ($src:expr) => {
        match $src {
            Self::Left(left) => left,
            Self::Right(right) => right,
        }
    };
}

pub mod iter;
pub mod traits;

use core::mem;
use core::ops::{Deref, DerefMut};
use core::pin::Pin;

use iter::{InnerIterEither, SwapIterEither};
use Either::*;

use crate::iter_either::{IterEither, IterMutEither};
use crate::{unwrap_failed, EitherOrBoth};

/// Represent values with two possibilities. `Either` can be either `Left` or `Right`
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[cfg_attr(feature = "serde", allow(clippy::unsafe_derive_deserialize))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Either<L, R = L> {
    /// Represent a left value
    Left(L),
    /// Represent a right value
    Right(R),
}

impl<L, R> Either<L, R> {
    ////////////////////////////////////////////////////////////////////////////////
    // Boolish
    ////////////////////////////////////////////////////////////////////////////////

    /// Returns `true` if `Either` is a [`Left`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let either = Either::<u8>::Left(1);
    /// assert_eq!(either.is_left(), true);
    ///
    /// let either = Either::<u8>::Right(1);
    /// assert_eq!(either.is_left(), false);
    /// ```
    #[must_use = "if you intended to assert that this is a left value, consider `.unwrap_left()` \
                  instead"]
    #[inline]
    pub const fn is_left(&self) -> bool {
        matches!(self, Self::Left(_))
    }

    /// Returns `true` if `Either` is [`Left`] and the contained value matches a predicate
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let either: Either<u8> = Either::Left(1);
    /// assert_eq!(either.is_left_and(|l| l == 1), true);
    ///
    /// let either: Either<u8> = Either::Left(1);
    /// assert_eq!(either.is_left_and(|l| l == 2), false);
    ///
    /// let either: Either<u8> = Either::Right(1);
    /// assert_eq!(either.is_left_and(|l| l == 1), false);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_left_and<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(_) => false,
        }
    }
    /// Returns `true` if `Either` is [`Left`] or the [`Right`] value matches a predicate
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let either: Either<u8, char> = Either::Left(1);
    /// assert_eq!(either.is_left_or(|r| r == 'c'), true);
    ///
    /// let either: Either<u8, char> = Either::Right('c');
    /// assert_eq!(either.is_left_or(|r| r == 'c'), true);
    ///
    /// let either: Either<u8, char> = Either::Right('c');
    /// assert_eq!(either.is_left_or(|r| r == 'm'), false);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_left_or<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Left(_) => true,
            Self::Right(right) => f(right),
        }
    }

    /// Returns `true` if `Either` is a [`Right`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let either = Either::<u8, char>::Right('c');
    /// assert_eq!(either.is_right(), true);
    ///
    /// let either = Either::<u8, char>::Left(1);
    /// assert_eq!(either.is_right(), false);
    /// ```
    #[must_use = "if you intended to assert that this is a right value, consider `.unwrap_right()` \
                  instead"]
    #[inline]
    pub const fn is_right(&self) -> bool {
        matches!(self, Self::Right(_))
    }

    /// Returns `true` if `Either` is [`Right`] and the contained value matches a
    /// predicate
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let either: Either<u8, char> = Either::Right('c');
    /// assert_eq!(either.is_right_and(|r| r == 'c'), true);
    ///
    /// let either: Either<u8, char> = Either::Right('c');
    /// assert_eq!(either.is_right_and(|r| r == 'm'), false);
    ///
    /// let either: Either<u8, char> = Either::Left(1);
    /// assert_eq!(either.is_right_and(|r| r == 'c'), false);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_right_and<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Right(right) => f(right),
            Self::Left(_) => false,
        }
    }

    /// Returns `true` if `Either` is [`Right`] or the [`Left`] value matches a predicate
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let either: Either<u8, char> = Either::Right('c');
    /// assert_eq!(either.is_right_or(|l| l == 1), true);
    ///
    /// let either: Either<u8, char> = Either::Left(1);
    /// assert_eq!(either.is_right_or(|l| l == 1), true);
    ///
    /// let either: Either<u8, char> = Either::Left(2);
    /// assert_eq!(either.is_right_or(|l| l == 1), false);
    /// ```
    #[must_use]
    #[inline]
    pub fn is_right_or<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Right(_) => true,
            Self::Left(left) => f(left),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // As reference conversions
    ////////////////////////////////////////////////////////////////////////////////

    /// Converts from `&Either<L, R>` to `Either<&L, &R>`.
    ///
    /// # Examples
    ///
    /// Calculate the length of the [strings] without moving the [`Strings`].
    ///
    /// [strings]: String
    /// [`Strings`]: String
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let text: Either<String> = Either::Left("left value".to_owned());
    ///
    /// // Cast `Either<String>` to `Either<&String>` and then apply a map consuming the
    /// // result of `as_ref` instead of `text` itself
    /// let length: Either<usize> = text.as_ref().map(String::len);
    /// assert_eq!(length, Either::Left(10));
    ///
    /// println!("`text` has not been moved: {:?}", &text);
    /// ```
    #[allow(clippy::same_name_method)]
    #[inline]
    pub const fn as_ref(&self) -> Either<&L, &R> {
        map_each!(self)
    }

    /// Converts from `&mut Either<L, R>` to `Either<&mut L, &mut R>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut either: Either<u8, char> = Either::Left(1);
    ///
    /// match either.as_mut() {
    ///     Either::Left(left) => *left = 3,
    ///     Either::Right(right) => *right = 'x',
    /// }
    ///
    /// assert_eq!(either, Either::Left(3))
    /// ```
    #[allow(clippy::same_name_method)]
    #[inline]
    pub fn as_mut(&mut self) -> Either<&mut L, &mut R> {
        map_each!(self)
    }

    /// Converts from `Either<L, R>` to `Either<&L::Target, &R::Target>`.
    ///
    /// This method keeps the original `Either` unchanged, while creating a new instance
    /// that holds a reference to the original. It also coerces the inner values
    /// through the `Deref` trait.
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let values: Either<String> = Either::Left("left".to_owned());
    /// let deref: Either<&str> = values.as_deref();
    /// assert_eq!(deref.left(), Some("left"));
    /// ```
    #[inline]
    pub fn as_deref(&self) -> Either<&<L as Deref>::Target, &<R as Deref>::Target>
    where
        L: Deref,
        R: Deref,
    {
        self.as_ref().bimap(|l| &**l, |r| &**r)
    }

    /// Converts from `Either<L, R>` to `Either<&mut L::Target, &mut R::Target>`.
    ///
    /// This method keeps the original `Either` unchanged, while creating a new instance
    /// that holds a mutable reference to the inner type's [`Deref::Target`] type.
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut value: Either<String> = Either::Left("left".to_owned());
    /// let upper_case: Either<&mut str> = value.as_deref_mut().map(|v| {
    ///     v.make_ascii_uppercase();
    ///     v
    /// });
    ///
    /// assert_eq!(upper_case.left(), Some("LEFT".to_owned().as_mut_str()));
    /// ```
    #[inline]
    pub fn as_deref_mut(&mut self) -> Either<&mut <L as Deref>::Target, &mut <R as Deref>::Target>
    where
        L: DerefMut,
        R: DerefMut,
    {
        self.as_mut().bimap(|l| &mut **l, |r| &mut **r)
    }

    /// Converts from `Pin<&Either<L, R>>` to `Either<Pin<&L>, Pin<&R>>`.
    #[must_use]
    #[inline]
    pub fn as_pin_ref(self: Pin<&Self>) -> Either<Pin<&L>, Pin<&R>> {
        // SAFETY: `x` is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            match Pin::get_ref(self) {
                Self::Left(left) => Left(Pin::new_unchecked(left)),
                Self::Right(right) => Right(Pin::new_unchecked(right)),
            }
        }
    }

    /// Converts from `Pin<&mut Either<L, R>>` to `Either<Pin<&mut L>, Pin<&mut R>>`.
    #[must_use]
    #[inline]
    pub fn as_pin_mut(self: Pin<&mut Self>) -> Either<Pin<&mut L>, Pin<&mut R>> {
        // SAFETY: `x` is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            match Pin::get_unchecked_mut(self) {
                Self::Left(left) => Left(Pin::new_unchecked(left)),
                Self::Right(right) => Right(Pin::new_unchecked(right)),
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Unwrapping the contained values
    ////////////////////////////////////////////////////////////////////////////////

    /// Returns the contained left value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics with a custom panic message provided by `msg` if there is no left value
    /// present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Left("left");
    /// assert_eq!(value.expect_left("should be left"), "left");
    /// ```
    ///
    /// This example panics with the message `should be left`
    ///
    /// ```should_panic
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Right("right");
    /// value.expect_left("should be left");
    /// ```
    #[track_caller]
    #[inline]
    pub fn expect_left(self, msg: &str) -> L {
        match self {
            Self::Left(left) => left,
            Self::Right(_) => unwrap_failed(msg),
        }
    }

    /// Returns the contained right value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics with a custom panic message provided by `msg` if there is no right value
    /// present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Right("right");
    /// assert_eq!(value.expect_right("should be right"), "right");
    /// ```
    ///
    /// The following example panics with the message `should be right`
    ///
    /// ```should_panic
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Left("left");
    /// value.expect_right("should be right");
    /// ```
    #[track_caller]
    #[inline]
    pub fn expect_right(self, msg: &str) -> R {
        match self {
            Self::Left(_) => unwrap_failed(msg),
            Self::Right(right) => right,
        }
    }

    /// Returns the contained left value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics if `Either` is a [`Right`] variant
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Left("left");
    /// assert_eq!(value.unwrap_left(), "left");
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Right("right");
    /// value.unwrap_left(); // panics
    /// ```
    #[track_caller]
    #[inline]
    pub fn unwrap_left(self) -> L {
        self.expect_left("Called `Either::unwrap_left` on a `Right` value")
    }

    /// Returns the contained left value consuming `self`, without checking that the value
    /// is not [`Left`].
    ///
    /// # SAFETY
    ///
    /// Calling this method on a [`Right`] variant is *[undefined behavior]*.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Left("left");
    /// assert_eq!(unsafe { value.unwrap_left_unchecked() }, "left");
    /// ```
    ///
    /// The following example introduces *[undefined behavior]*
    ///
    /// ```no_run
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Right("right");
    /// assert_eq!(unsafe { value.unwrap_left_unchecked() }, "left");
    /// ```
    ///
    /// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
    #[track_caller]
    #[inline]
    pub unsafe fn unwrap_left_unchecked(self) -> L {
        match self {
            Self::Left(left) => left,
            // SAFETY: the safety contract must be upheld by the caller.
            Self::Right(_) => core::hint::unreachable_unchecked(), // cov:excl-line
        }
    }

    /// Returns the contained right value consuming `self`.
    ///
    /// # Panics
    ///
    /// Panics if `Either` is a [`Left`] variant
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Right("right");
    /// assert_eq!(value.unwrap_right(), "right");
    /// ```
    ///
    /// ```should_panic
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Left("left");
    /// value.unwrap_right(); // panics
    /// ```
    #[track_caller]
    #[inline]
    pub fn unwrap_right(self) -> R {
        self.expect_right("Called `Either::unwrap_right` on a `Left` value")
    }

    /// Returns the contained right value consuming `self`, without checking that the
    /// value is not [`Right`].
    ///
    /// # SAFETY
    ///
    /// Calling this method on a [`Left`] variant is *[undefined behavior]*.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Right("right");
    /// assert_eq!(unsafe { value.unwrap_right_unchecked() }, "right");
    /// ```
    ///
    /// The following example introduces *[undefined behavior]*
    ///
    /// ```no_run
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str> = Either::Left("left");
    /// assert_eq!(unsafe { value.unwrap_right_unchecked() }, "right");
    /// ```
    ///
    /// [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
    #[track_caller]
    #[inline]
    pub unsafe fn unwrap_right_unchecked(self) -> R {
        match self {
            // SAFETY: the safety contract must be upheld by the caller.
            Self::Left(_) => core::hint::unreachable_unchecked(), // cov:excl-line
            Self::Right(right) => right,
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Getting the contained values
    ////////////////////////////////////////////////////////////////////////////////

    /// If a left value is present, return `Some` containing the value otherwise return
    /// `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.left(), Some(1));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.left(), None);
    /// ```
    #[inline]
    pub fn left(self) -> Option<L> {
        match self {
            Self::Left(left) => Some(left),
            Self::Right(_) => None,
        }
    }

    /// Returns [`Right`] if the `Either` is [`Right`] otherwise returns `other`.
    ///
    /// The `left_and` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`left_and_then`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let x: Either<u8, char> = Either::Left(1);
    /// let y: Either<&str, char> = Either::Left("left");
    /// assert_eq!(x.left_and(y), Either::Left("left"));
    ///
    /// let x: Either<u8, char> = Either::Right('c');
    /// let y: Either<&str, char> = Either::Left("left");
    /// assert_eq!(x.left_and(y), Either::Right('c'));
    /// ```
    ///
    /// [`left_and_then`]: Either::left_and_then
    #[inline]
    pub fn left_and<T>(self, other: Either<T, R>) -> Either<T, R> {
        match self {
            Self::Left(_) => other,
            Self::Right(right) => Right(right),
        }
    }

    /// Returns [`Right`] otherwise calls `f` with the left value and returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// fn left_to_string(x: u8) -> Either<String, char> {
    ///     Either::Left(x.to_string())
    /// }
    ///
    /// let x: Either<u8, char> = Either::Left(1);
    /// assert_eq!(x.left_and_then(left_to_string), Either::Left(1.to_string()));
    ///
    /// let x: Either<u8, char> = Either::Right('c');
    /// assert_eq!(x.left_and_then(left_to_string), Either::Right('c'));
    /// ```
    #[inline]
    pub fn left_and_then<F, T>(self, f: F) -> Either<T, R>
    where
        F: FnOnce(L) -> Either<T, R>,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => Right(right),
        }
    }

    /// If a left value is present, return `Some` containing the value otherwise return
    /// `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.right(), Some('c'));
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.right(), None);
    /// ```
    #[inline]
    pub fn right(self) -> Option<R> {
        match self {
            Self::Left(_) => None,
            Self::Right(right) => Some(right),
        }
    }

    /// Returns [`Left`] if the `Either` is [`Left`] otherwise returns `other`.
    ///
    /// The `right_and` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`right_and_then`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let x: Either<u8, char> = Either::Right('c');
    /// let y: Either<u8, &str> = Either::Right("right");
    /// assert_eq!(x.right_and(y), Either::Right("right"));
    ///
    /// let x: Either<u8, char> = Either::Left(1);
    /// let y: Either<u8, &str> = Either::Right("right");
    /// assert_eq!(x.right_and(y), Either::Left(1));
    /// ```
    ///
    /// [`right_and_then`]: Either::right_and_then
    #[inline]
    pub fn right_and<T>(self, other: Either<L, T>) -> Either<L, T> {
        match self {
            Self::Left(left) => Left(left),
            Self::Right(_) => other,
        }
    }

    /// Returns [`Left`] otherwise calls `f` with the right value and returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// fn right_to_string(x: char) -> Either<u8, String> {
    ///     Either::Right(x.to_string())
    /// }
    ///
    /// let x: Either<u8, char> = Either::Right('c');
    /// assert_eq!(
    ///     x.right_and_then(right_to_string),
    ///     Either::Right('c'.to_string())
    /// );
    ///
    /// let x: Either<u8, char> = Either::Left(1);
    /// assert_eq!(x.right_and_then(right_to_string), Either::Left(1));
    /// ```
    #[inline]
    pub fn right_and_then<F, T>(self, f: F) -> Either<L, T>
    where
        F: FnOnce(R) -> Either<L, T>,
    {
        match self {
            Self::Left(left) => Left(left),
            Self::Right(right) => f(right),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Iterators
    ////////////////////////////////////////////////////////////////////////////////

    /// Consumes the inner iterators and returns an iterator that yields items of type
    /// `Either<L, R>`.
    ///
    ///
    /// This iterator allows traversing inner iterators with different types
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let x: Either<_, Vec<char>> = Either::Left(vec![1, 2]);
    /// let mut iter = x.into_iter_swap(); // moves `x` and the iterators
    ///
    /// assert_eq!(iter.next(), Some(Either::Left(1)));
    /// assert_eq!(iter.next(), Some(Either::Left(2)));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn into_iter_swap(
        self,
    ) -> SwapIterEither<<L as IntoIterator>::IntoIter, <R as IntoIterator>::IntoIter>
    where
        L: IntoIterator,
        R: IntoIterator,
    {
        SwapIterEither::new(self.bimap(IntoIterator::into_iter, IntoIterator::into_iter))
    }

    /// Borrow the inner iterators and returns an iterator that yields items of type
    /// `Either<&L, &R>`.
    ///
    /// This iterator allows traversing inner iterators with different types
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let x: Either<_, Vec<char>> = Either::Left(vec![1, 2]);
    /// let mut iter = x.iter_swap();
    ///
    /// assert_eq!(iter.next(), Some(Either::Left(&1)));
    /// assert_eq!(iter.next(), Some(Either::Left(&2)));
    /// assert_eq!(iter.next(), None);
    ///
    /// println!("{x:?}"); // still can access `x`
    /// ```
    pub fn iter_swap(
        &self,
    ) -> SwapIterEither<<&L as IntoIterator>::IntoIter, <&R as IntoIterator>::IntoIter>
    where
        for<'a> &'a L: IntoIterator,
        for<'a> &'a R: IntoIterator,
    {
        SwapIterEither::new(
            self.as_ref()
                .bimap(IntoIterator::into_iter, IntoIterator::into_iter),
        )
    }

    /// Mutably borrows the inner iterators returning an iterator that yields items of
    /// type `Either<&mut L, &mut R>`.
    ///
    /// This iterator allows traversing inner iterators with different types
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut x: Either<_, Vec<char>> = Either::Left(vec![1, 2]);
    /// let mut iter = x.iter_swap_mut();
    ///
    /// assert_eq!(iter.next(), Some(Either::Left(&mut 1)));
    /// assert_eq!(iter.next(), Some(Either::Left(&mut 2)));
    /// assert_eq!(iter.next(), None);
    ///
    /// println!("{x:?}"); // still can access `x`
    /// ```
    pub fn iter_swap_mut(
        &mut self,
    ) -> SwapIterEither<<&mut L as IntoIterator>::IntoIter, <&mut R as IntoIterator>::IntoIter>
    where
        for<'a> &'a mut L: IntoIterator,
        for<'a> &'a mut R: IntoIterator,
    {
        SwapIterEither::new(
            self.as_mut()
                .bimap(IntoIterator::into_iter, IntoIterator::into_iter),
        )
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Conversions
    ////////////////////////////////////////////////////////////////////////////////

    /// Converts `Either<L, R>` to `Either<R, L>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.flip(), Either::Right(1));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.flip(), Either::Left('c'));
    /// ```
    #[must_use]
    #[inline]
    pub fn flip(self) -> Either<R, L> {
        match self {
            Self::Left(left) => Right(left),
            Self::Right(right) => Left(right),
        }
    }

    /// Applies mapping functions to the left and right values returning an `Either<T,
    /// U>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let map_left = |left: u8| left.to_string();
    /// let map_right = |right: char| right as i32;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(
    ///     value.bimap(map_left, map_right),
    ///     Either::Left("1".to_owned())
    /// );
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.bimap(map_left, map_right), Either::Right(99));
    /// ```
    #[inline]
    pub fn bimap<F, G, T, U>(self, f: F, g: G) -> Either<T, U>
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> U,
    {
        match self {
            Self::Left(left) => Left(f(left)),
            Self::Right(right) => Right(g(right)),
        }
    }

    /// Applies a mapping function to the left value returning an `Either<T, R>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let map_left = |left: u8| left.to_string();
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.map_left(map_left), Either::Left("1".to_owned()));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.map_left(map_left), Either::Right('c'));
    /// ```
    #[inline]
    pub fn map_left<F, T>(self, f: F) -> Either<T, R>
    where
        F: FnOnce(L) -> T,
    {
        match self {
            Self::Left(left) => Left(f(left)),
            Self::Right(right) => Right(right),
        }
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
    /// use either_or_both::Either;
    ///
    /// let map_left = |left: u8| left as u64 + 1;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.map_left_or(42, map_left), 42u64);
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.map_left_or(42, map_left), 2u64);
    /// ```
    ///
    /// [`map_left_or_else`]: Either::map_left_or_else
    #[inline]
    pub fn map_left_or<F, T>(self, default: T, f: F) -> T
    where
        F: FnOnce(L) -> T,
    {
        self.map_left_or_else(|| default, f)
    }

    /// Applies the given function to the left value , mapping `L` to `T` otherwise
    /// returns the [default value] for type `T`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let map_left = |left: u8| left as u64 + 1;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.map_left_or_default(map_left), 0u64);
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.map_left_or_default(map_left), 2u64);
    /// ```
    ///
    /// [default value]: Default::default
    #[inline]
    pub fn map_left_or_default<F, T>(self, f: F) -> T
    where
        F: FnOnce(L) -> T,
        T: Default,
    {
        self.map_left_or_else(T::default, f)
    }

    /// Applies the given function to the left value, mapping `L` to `T` otherwise applies
    /// a different function.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let map_left = |left: u8| left.to_string();
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(
    ///     value.map_left_or_else(|| String::from("left"), map_left),
    ///     String::from("left")
    /// );
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(
    ///     value.map_left_or_else(|| String::from("left"), map_left),
    ///     1.to_string()
    /// );
    /// ```
    #[inline]
    pub fn map_left_or_else<D, F, T>(self, default: D, f: F) -> T
    where
        F: FnOnce(L) -> T,
        D: FnOnce() -> T,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(_) => default(),
        }
    }

    /// Applies a mapping function to the right value returning an `Either<L, T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let map_right = |right: char| right.to_string();
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.map_right(map_right), Either::Right("c".to_owned()));
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.map_right(map_right), Either::Left(1));
    /// ```
    #[inline]
    pub fn map_right<F, T>(self, f: F) -> Either<L, T>
    where
        F: FnOnce(R) -> T,
    {
        match self {
            Self::Left(left) => Left(left),
            Self::Right(right) => Right(f(right)),
        }
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
    /// use either_or_both::Either;
    ///
    /// let map_right = |right: char| right as i32;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.map_right_or(42, map_right), 99i32);
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.map_right_or(42, map_right), 42i32);
    /// ```
    ///
    /// [`map_right_or_else`]: Either::map_right_or_else
    #[inline]
    pub fn map_right_or<F, T>(self, default: T, f: F) -> T
    where
        F: FnOnce(R) -> T,
    {
        self.map_right_or_else(|| default, f)
    }

    /// Applies the given function to the right value, mapping `R` to `T` otherwise
    /// returns the [default value] for type `T`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let map_right = |right: char| right as u64 + 1;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.map_right_or_default(map_right), 100u64);
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.map_right_or_default(map_right), 0u64);
    /// ```
    ///
    /// [default value]: Default::default
    #[inline]
    pub fn map_right_or_default<F, T>(self, f: F) -> T
    where
        F: FnOnce(R) -> T,
        T: Default,
    {
        self.map_right_or_else(T::default, f)
    }

    /// Applies the given function to the right value, mapping `R` to `T` otherwise
    /// applies a different function.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let map_right = |right: char| right.to_string();
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(
    ///     value.map_right_or_else(|| String::from("right"), map_right),
    ///     "c".to_owned()
    /// );
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(
    ///     value.map_right_or_else(|| String::from("right"), map_right),
    ///     String::from("right")
    /// );
    /// ```
    #[inline]
    pub fn map_right_or_else<D, F, T>(self, default: D, f: F) -> T
    where
        F: FnOnce(R) -> T,
        D: FnOnce() -> T,
    {
        match self {
            Self::Left(_) => default(),
            Self::Right(right) => f(right),
        }
    }

    /// Calls functions with a reference to the contained values returning the original
    /// `Either`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// // Prints a single line with "Left is: 1"
    /// let value: Either<u8, char> = Either::Left(1);
    /// let left = value
    ///     .biinspect(|l| println!("Left is: {l}"), |r| println!("Right is: {r}"))
    ///     .expect_left("should be a left value");
    /// assert_eq!(left, 1);
    ///
    /// // Prints a single line with "Right is: c"
    /// let value: Either<u8, char> = Either::Right('c');
    /// let right = value
    ///     .biinspect(|l| println!("Left is: {l}"), |r| println!("Right is: {r}"))
    ///     .expect_right("should be a right value");
    /// assert_eq!(right, 'c');
    /// ```
    #[inline]
    pub fn biinspect<F, G>(self, f: F, g: G) -> Self
    where
        for<'a> F: Fn(&'a L),
        for<'a> G: Fn(&'a R),
    {
        match &self {
            Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }
        self
    }

    /// Calls a function with a reference to the contained left value returning the
    /// original `Either`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// // Prints a single line with "Left is: 1"
    /// let value: Either<u8, char> = Either::Left(1);
    /// let left = value
    ///     .inspect_left(|l| println!("Left is: {l}"))
    ///     .expect_left("should be a left value");
    /// assert_eq!(left, 1);
    ///
    /// // Prints nothing
    /// let value: Either<u8, char> = Either::Right('c');
    /// let right = value
    ///     .inspect_left(|l| println!("Left is: {l}"))
    ///     .expect_right("should be a right value");
    /// assert_eq!(right, 'c');
    /// ```
    #[inline]
    pub fn inspect_left<F>(self, f: F) -> Self
    where
        for<'a> F: Fn(&'a L),
    {
        match &self {
            Self::Left(left) => f(left),
            Self::Right(_) => {}
        }
        self
    }

    /// Calls a function with a reference to the contained right value returning the
    /// original `Either`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// // Prints a single line with "Right is: c"
    /// let value: Either<u8, char> = Either::Right('c');
    /// let right = value
    ///     .inspect_right(|r| println!("Right is: {r}"))
    ///     .expect_right("should be a right value");
    /// assert_eq!(right, 'c');
    ///
    /// // Prints nothing
    /// let value: Either<u8, char> = Either::Left(1);
    /// let left = value
    ///     .inspect_right(|r| println!("Right is: {r}"))
    ///     .expect_left("should be a left value");
    /// assert_eq!(left, 1);
    /// ```
    #[inline]
    pub fn inspect_right<F>(self, f: F) -> Self
    where
        for<'a> F: Fn(&'a R),
    {
        match &self {
            Self::Left(_) => {}
            Self::Right(right) => f(right),
        }
        self
    }

    /// Consumes this `Either` applying functions to the contained values taking mutable
    /// references to capture variables.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut left = vec![];
    /// let mut right = vec![];
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// value.biapply(|l| left.push(l), |r| right.push(r)); // moves `value`
    ///
    /// assert_eq!(left, vec![1]);
    /// ```
    ///
    /// The following example will not compile with the error: "cannot borrow `both` as
    /// mutable more than once at a time".
    ///
    /// If you need to apply a function that requires mutable access to both elements
    /// simultaneously, consider using [`biapply_with`] instead.
    ///
    /// ```compile_fail
    /// use either_or_both::Either;
    ///
    /// let mut both = vec![];
    ///
    /// let value: Either<u8, char> = Either::Both(1, 'c');
    /// value.biapply(|l| both.push(l), |r| both.push(r as u8));
    /// ```
    ///
    /// [`biapply_with`]: Either::biapply_with
    #[inline]
    pub fn biapply<F, G>(self, mut f: F, mut g: G)
    where
        F: FnMut(L),
        G: FnMut(R),
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }
    }

    /// Consumes this `Either` applying functions to the contained values and a given
    /// accumulator.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut both = vec![];
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// value.biapply_with(&mut both, |acc, l| acc.push(l), |acc, r| acc.push(r as u8));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// value.biapply_with(&mut both, |acc, l| acc.push(l), |acc, r| acc.push(r as u8));
    ///
    /// assert_eq!(both, vec![1, 99]);
    /// ```
    ///
    /// [`flip`]: Either::flip
    #[inline]
    pub fn biapply_with<F, G, Acc>(self, acc: Acc, mut f: F, mut g: G)
    where
        F: FnMut(Acc, L),
        G: FnMut(Acc, R),
    {
        match self {
            Self::Left(left) => f(acc, left),
            Self::Right(right) => g(acc, right),
        }
    }

    /// Consumes this `Either` applying a function to the contained left value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut left = vec![];
    /// let value: Either<u8, char> = Either::Left(1);
    /// value.apply_left(|l| left.push(l));
    /// assert_eq!(left, vec![1]);
    ///
    /// let mut left = vec![];
    /// let value: Either<u8, char> = Either::Right('c');
    /// value.apply_left(|l| left.push(l));
    /// assert_eq!(left.is_empty(), true);
    /// ```
    #[inline]
    pub fn apply_left<F>(self, mut f: F)
    where
        F: FnMut(L),
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(_) => {}
        }
    }

    /// Consumes this `Either` applying a function to the contained right value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut right = vec![];
    /// let value: Either<u8, char> = Either::Right('c');
    /// value.apply_right(|r| right.push(r));
    /// assert_eq!(right, vec!['c']);
    ///
    /// let mut right = vec![];
    /// let value: Either<u8, char> = Either::Left(1);
    /// value.apply_right(|r| right.push(r));
    /// assert_eq!(right.is_empty(), true);
    /// ```
    #[inline]
    pub fn apply_right<F>(self, mut f: F)
    where
        F: FnMut(R),
    {
        match self {
            Self::Right(right) => f(right),
            Self::Left(_) => {}
        }
    }

    /// Returns the contained values applying a mapping function which converts the `L`
    /// and `R` values to a uniform type.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(
    ///     value.bireduce(|l| l.to_string(), |r| r.to_string()),
    ///     "1".to_owned()
    /// );
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(
    ///     value.bireduce(|l| l.to_string(), |r| r.to_string()),
    ///     "c".to_owned()
    /// );
    /// ```
    #[inline]
    pub fn bireduce<F, G, T>(self, f: F, g: G) -> T
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> T,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => g(right),
        }
    }

    /// Returns the left value otherwise applies a function to a [`Right`] variant,
    /// converting it into an `L` value.
    ///
    /// # Example
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.reduce_left(|r| r as u8), 99);
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.reduce_left(|r| r as u8), 1);
    /// ```
    #[inline]
    pub fn reduce_left<F>(self, f: F) -> L
    where
        F: FnOnce(R) -> L,
    {
        match self {
            Self::Left(left) => left,
            Self::Right(right) => f(right),
        }
    }

    /// Returns the right value otherwise applies a function to a [`Left`] variant,
    /// converting it into an `R` value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.reduce_right(|l| l as char), 'c');
    ///
    /// let value: Either<u8, char> = Either::Left(0);
    /// assert_eq!(value.reduce_right(|l| l as char), '\0');
    /// ```
    #[inline]
    pub fn reduce_right<F>(self, f: F) -> R
    where
        F: FnOnce(L) -> R,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => right,
        }
    }

    /// Transforms the `Either<L, R>` into a `Result<R, L>`.
    ///
    /// Following the [convention], the left value represents an error and a right value
    /// represents a correct value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<&str, u8> = Either::Right(1);
    /// assert_eq!(value.ok(), Ok(1));
    ///
    /// let value: Either<&str, u8> = Either::Left("this is an error");
    /// assert_eq!(value.ok(), Err("this is an error"));
    /// ```
    ///
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    #[inline]
    pub fn ok(self) -> Result<R, L> {
        match self {
            Self::Left(left) => Err(left),
            Self::Right(right) => Ok(right),
        }
    }

    /// Transforms the `Either<L, R>` into a `Result<R, L>` using the provided `error` as
    /// error value.
    ///
    /// Following the [convention], the left value represents an error and a right value
    /// represents a correct value.
    ///
    /// The `ok_or` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`ok_or_else`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.ok_or("error message"), Ok('c'));
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.ok_or("error message"), Err("error message"));
    /// ```
    ///
    /// [`ok_or_else`]: Either::ok_or_else
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    #[inline]
    pub fn ok_or<E>(self, error: E) -> Result<R, E> {
        self.ok_or_else(|| error)
    }

    /// Transforms the `Either<L, R>` into a `Result<R, L>` using the result of an `error`
    /// function as error value.
    ///
    /// Following the [convention], the left value represents an error and a right value
    /// represents a correct value.
    ///
    /// The `ok_or` combinator eagerly evaluates its arguments, which can result in
    /// unnecessary computations. When chaining operations that involve function
    /// calls, use [`ok_or_else`] instead. It evaluates the function lazily.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.ok_or_else(|| String::from("error message")), Ok('c'));
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(
    ///     value.ok_or_else(|| String::from("error message")),
    ///     Err(String::from("error message"))
    /// );
    /// ```
    ///
    /// [`ok_or_else`]: Either::ok_or_else
    /// [convention]: ./index.html#conventions-and-edge-cases
    #[allow(clippy::missing_errors_doc)]
    #[inline]
    pub fn ok_or_else<F, E>(self, error: F) -> Result<R, E>
    where
        F: FnOnce() -> E,
    {
        match self {
            Self::Left(_) => Err(error()),
            Self::Right(ok) => Ok(ok),
        }
    }

    /// Returns a tuple (L, R) with the provided values filling in any missing left or
    /// right value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.or(2, 'm'), (1, 'm'));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.or(2, 'm'), (2, 'c'));
    /// ```
    #[inline]
    pub fn or(self, left: L, right: R) -> (L, R) {
        match self {
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
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.or_default(), (1, '\0'));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.or_default(), (0, 'c'));
    /// ```
    #[inline]
    pub fn or_default(self) -> (L, R)
    where
        L: Default,
        R: Default,
    {
        match self {
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
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.or_else(|| 1 + 1, || char::from(100)), (1, 'd'));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.or_else(|| 1 + 1, || char::from(100)), (2, 'c'));
    /// ```
    #[inline]
    pub fn or_else<F, G>(self, f: F, g: G) -> (L, R)
    where
        F: FnOnce() -> L,
        G: FnOnce() -> R,
    {
        match self {
            Self::Left(left) => (left, g()),
            Self::Right(right) => (f(), right),
        }
    }

    /// Inserts `left` into the `Either` consuming it and returning an `EitherOrBoth`
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.inject_left(2), EitherOrBoth::Left(2));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.inject_left(1), EitherOrBoth::Both(1, 'c'));
    /// ```
    #[inline]
    pub fn inject_left(self, left: L) -> EitherOrBoth<L, R> {
        match self {
            Self::Left(_) => EitherOrBoth::Left(left),
            Self::Right(right) => EitherOrBoth::Both(left, right),
        }
    }

    /// Inserts `right` into the `Either` consuming it and returning an `EitherOrBoth`
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::{Either, EitherOrBoth};
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.inject_right('m'), EitherOrBoth::Right('m'));
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.inject_right('m'), EitherOrBoth::Both(1, 'm'));
    /// ```
    #[inline]
    pub fn inject_right(self, right: R) -> EitherOrBoth<L, R> {
        match self {
            Self::Left(left) => EitherOrBoth::Both(left, right),
            Self::Right(_) => EitherOrBoth::Right(right),
        }
    }

    /// Converts into a [`Left`] variant, using the contained left value or applying a
    /// mapping function to the [`Right`] value.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// assert_eq!(value.into_left(|r| r as u8), Either::Left(1));
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.into_left(|r| r as u8), Either::Left(99));
    /// ```
    #[inline]
    pub fn into_left<F>(self, f: F) -> Self
    where
        F: FnOnce(R) -> L,
    {
        match self {
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
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Right('c');
    /// assert_eq!(value.into_right(|l| l as char), Either::Right('c'));
    ///
    /// let value: Either<u8, char> = Either::Left(0);
    /// assert_eq!(value.into_right(|l| l as char), Either::Right('\0'));
    /// ```
    #[inline]
    pub fn into_right<F>(self, f: F) -> Self
    where
        F: FnOnce(L) -> R,
    {
        match self {
            Self::Left(left) => Self::Right(f(left)),
            Self::Right(_) => self,
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Replacing values
    ////////////////////////////////////////////////////////////////////////////////

    /// Replaces the `left` value returning the old value if present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut value: Either<u8, char> = Either::Left(1);
    /// let old = value.replace_left(2);
    /// assert_eq!(old, Some(1));
    /// assert_eq!(value, Either::Left(2));
    ///
    /// let mut value: Either<u8, char> = Either::Right('c');
    /// let old = value.replace_left(2);
    /// assert_eq!(old, None);
    /// assert_eq!(value, Either::Right('c'));
    /// ```
    #[inline]
    pub fn replace_left(&mut self, value: L) -> Option<L> {
        match self {
            Self::Left(left) => Some(mem::replace(left, value)),
            Self::Right(_) => None,
        }
    }

    /// Replaces the `right` value returning the old value if present
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut value: Either<u8, char> = Either::Right('c');
    /// let old = value.replace_right('m');
    /// assert_eq!(old, Some('c'));
    /// assert_eq!(value, Either::Right('m'));
    ///
    /// let mut value: Either<u8, char> = Either::Left(1);
    /// let old = value.replace_right('m');
    /// assert_eq!(old, None);
    /// assert_eq!(value, Either::Left(1));
    /// ```
    #[inline]
    pub fn replace_right(&mut self, value: R) -> Option<R> {
        match self {
            Self::Right(right) => Some(mem::replace(right, value)),
            Self::Left(_) => None,
        }
    }
}

impl<T> Either<T, T> {
    /// Consumes this `Either` applying a function to the contained value (of a uniform
    /// type) taking mutable references to capture variables.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut both = vec![];
    ///
    /// let value: Either<char> = Either::Left('c');
    /// value.apply(|c| both.push(c));
    ///
    /// let value: Either<char> = Either::Right('a');
    /// value.apply(|c| both.push(c));
    ///
    /// assert_eq!(both, vec!['c', 'a']);
    /// ```
    #[inline]
    pub fn apply<F>(self, mut f: F)
    where
        F: FnMut(T),
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => f(right),
        }
    }

    /// Calls a function with a reference to the contained value (of a uniform type)
    /// returning the original `Either`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// // Prints a single line with "The value is: c"
    /// let value: Either<char> = Either::Left('c');
    /// let left = value
    ///     .inspect(|c| println!("The value is: {c}"))
    ///     .expect_left("should be a left value");
    /// assert_eq!(left, 'c');
    ///
    /// // Prints a single line with "The value is: a"
    /// let value: Either<char> = Either::Right('a');
    /// let right = value
    ///     .inspect(|c| println!("The value is: {c}"))
    ///     .expect_right("should be a right value");
    ///
    /// assert_eq!(right, 'a');
    /// ```
    #[inline]
    pub fn inspect<F>(self, f: F) -> Self
    where
        for<'a> F: Fn(&'a T),
    {
        match &self {
            Self::Left(left) => f(left),
            Self::Right(right) => f(right),
        }

        self
    }

    /// Returns an iterator over the contained value of a uniform type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Left('c');
    /// let mut iter = value.iter();
    /// assert_eq!(iter.next(), Some(&'c'));
    /// assert_eq!(iter.next(), None);
    ///
    /// let value: EitherOrBoth<char> = EitherOrBoth::Right('c');
    /// let mut iter = value.iter();
    /// assert_eq!(iter.next(), Some(&'c'));
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    pub fn iter(&self) -> IterEither<'_, T> {
        IterEither::new(self)
    }

    /// Returns an iterator over the contained mutable value of a uniform type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::EitherOrBoth;
    ///
    /// let mut value: EitherOrBoth<char> = EitherOrBoth::Left('c');
    /// let mut iter = value.iter_mut();
    /// assert_eq!(iter.next(), Some(&mut 'c'));
    /// assert_eq!(iter.next(), None);
    ///
    /// let mut value: EitherOrBoth<char> = EitherOrBoth::Right('c');
    /// let mut iter = value.iter_mut();
    /// assert_eq!(iter.next(), Some(&mut 'c'));
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    pub fn iter_mut(&mut self) -> IterMutEither<'_, T> {
        IterMutEither::new(self)
    }

    /// Consumes the `Either`, returning an iterator over the contained iterator of a
    /// uniform type
    ///
    /// For iteration over contained iterators with non-uniform types, you can use
    /// [`into_iter_swap`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<Vec<char>> = Either::Left(vec!['c', 'a']);
    /// let collected: Vec<char> = value.into_iter_inner().collect();
    /// assert_eq!(collected, vec!['c', 'a']);
    /// ```
    ///
    /// [`into_iter_swap`]: Either::into_iter_swap
    pub fn into_iter_inner(self) -> InnerIterEither<<T as IntoIterator>::IntoIter>
    where
        T: IntoIterator,
    {
        InnerIterEither::new(self.map(IntoIterator::into_iter))
    }

    /// Returns an iterator over the contained iterator of a uniform type
    ///
    /// For iteration over contained iterators with non-uniform types, you can use
    /// [`iter_swap`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<Vec<char>> = Either::Left(vec!['c', 'a']);
    /// let collected: Vec<&char> = value.iter_inner().collect();
    /// assert_eq!(collected, vec![&'c', &'a']);
    /// ```
    ///
    /// [`iter_swap`]: Either::into_iter_swap
    pub fn iter_inner(&self) -> InnerIterEither<<&T as IntoIterator>::IntoIter>
    where
        for<'a> &'a T: IntoIterator,
    {
        InnerIterEither::new(self.as_ref().map(IntoIterator::into_iter))
    }

    /// Returns an iterator over the mutable values of the contained iterator of a uniform
    /// type
    ///
    /// For iteration over contained iterators with non-uniform types, you can use
    /// [`iter_swap_mut`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut value: Either<Vec<char>> = Either::Left(vec!['c', 'a']);
    /// let collected: Vec<&mut char> = value.iter_inner_mut().collect();
    /// assert_eq!(collected, vec![&mut 'c', &mut 'a']);
    /// ```
    ///
    /// [`iter_swap_mut`]: Either::iter_swap_mut
    pub fn iter_inner_mut(&mut self) -> InnerIterEither<<&mut T as IntoIterator>::IntoIter>
    where
        for<'a> &'a mut T: IntoIterator,
    {
        InnerIterEither::new(self.as_mut().map(IntoIterator::into_iter))
    }

    /// Applies a mapping function to the left and right values (of a uniform type)
    /// returning an `Either<U, U>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<char> = Either::Left('c');
    /// assert_eq!(value.map(|c| c.to_string()), Either::Left("c".to_owned()));
    ///
    /// let value: Either<char> = Either::Right('a');
    /// assert_eq!(value.map(|c| c.to_string()), Either::Right("a".to_owned()));
    /// ```
    #[inline]
    pub fn map<F, U>(self, f: F) -> Either<U, U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Left(left) => Left(f(left)),
            Self::Right(right) => Right(f(right)),
        }
    }

    /// Returns the contained [`Left`] or [`Right`] value of uniform type.
    ///
    /// # Example
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8> = Either::Left(1);
    /// assert_eq!(value.reduce(), 1);
    ///
    /// let value: Either<u8> = Either::Right(2);
    /// assert_eq!(value.reduce(), 2);
    /// ```
    #[inline]
    pub fn reduce(self) -> T {
        match self {
            Self::Left(left) => left,
            Self::Right(right) => right,
        }
    }

    /// Returns the contained [`Left`] or [`Right`] value applying a mapping function to
    /// the contained values.
    ///
    /// # Example
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8> = Either::Left(1);
    /// assert_eq!(value.reduce_map(|v| v + 1), 2);
    ///
    /// let value: Either<u8> = Either::Right(2);
    /// assert_eq!(value.reduce_map(|v| v + 1), 3);
    /// ```
    #[inline]
    pub fn reduce_map<F, U>(self, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => f(right),
        }
    }
}

impl<L, R> Either<&L, &R> {
    /// Converts an `Either<&L, &R>` into an `Either<L, R>` by cloning its contents
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// let refs: Either<&u8, &char> = value.as_ref();
    /// assert_eq!(refs.cloned(), Either::Left(1));
    /// ```
    #[must_use = "`self` will be dropped if the result is not used"]
    pub fn cloned(self) -> Either<L, R>
    where
        L: Clone,
        R: Clone,
    {
        match self {
            Self::Left(left) => Left(left.clone()),
            Self::Right(right) => Right(right.clone()),
        }
    }

    /// Converts an `Either<&L, &R>` into an `Either<L, R>` by copying its contents.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<u8, char> = Either::Left(1);
    /// let refs: Either<&u8, &char> = value.as_ref();
    /// assert_eq!(refs.copied(), Either::Left(1));
    /// ```
    #[must_use = "`self` will be dropped if the result is not used"]
    pub const fn copied(self) -> Either<L, R>
    where
        L: Copy,
        R: Copy,
    {
        match self {
            Self::Left(left) => Left(*left),
            Self::Right(right) => Right(*right),
        }
    }
}

impl<L, R> Either<&mut L, &mut R> {
    /// Converts an `Either<&mut L, &mut R>` into an `Either<L, R>` by cloning its
    /// contents.
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut value: Either<u8, char> = Either::Left(1);
    /// let refs: Either<&mut u8, &mut char> = value.as_mut();
    /// assert_eq!(refs.cloned(), Either::Left(1));
    /// ```
    #[must_use = "`self` will be dropped if the result is not used"]
    pub fn cloned(self) -> Either<L, R>
    where
        L: Clone,
        R: Clone,
    {
        match self {
            Self::Left(left) => Left(left.clone()),
            Self::Right(right) => Right(right.clone()),
        }
    }

    /// Converts an `Either<&mut L, &mut R>` into an `Either<L, R>` by copying its
    /// contents
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let mut value: Either<u8, char> = Either::Left(1);
    /// let refs: Either<&mut u8, &mut char> = value.as_mut();
    /// assert_eq!(refs.copied(), Either::Left(1));
    /// ```
    #[must_use = "`self` will be dropped if the result is not used"]
    pub fn copied(self) -> Either<L, R>
    where
        L: Copy,
        R: Copy,
    {
        match self {
            Self::Left(left) => Left(*left),
            Self::Right(right) => Right(*right),
        }
    }
}

impl<L1, L2, R1, R2> Either<(L1, R1), (L2, R2)> {
    /// Transposes a `Either` of tuples to a tuple of `Eithers`
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<(u8, char), (i32, u64)> = Either::Left((1, 'c'));
    /// assert_eq!(value.transpose(), (Either::Left(1), Either::Left('c')));
    ///
    /// let value: Either<(u8, char), (i32, u64)> = Either::Right((-2, 10));
    /// assert_eq!(value.transpose(), (Either::Right(-2), Either::Right(10)));
    /// ```
    pub fn transpose(self) -> (Either<L1, L2>, Either<R1, R2>) {
        match self {
            Self::Left((l1, r1)) => (Left(l1), Left(r1)),
            Self::Right((l2, r2)) => (Right(l2), Right(r2)),
        }
    }
}

impl<L, R, T> Either<(T, L), (T, R)> {
    /// Transposes an `Either` of tuples to a tuple of a single value and an `Either` with
    /// the left value having a uniform type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<(u8, char), (u8, i32)> = Either::Left((1, 'c'));
    /// assert_eq!(value.transpose_left(), (1, Either::Left('c')));
    ///
    /// let value: Either<(u8, char), (u8, i32)> = Either::Right((2, -10));
    /// assert_eq!(value.transpose_left(), (2, Either::Right(-10)));
    /// ```
    pub fn transpose_left(self) -> (T, Either<L, R>) {
        match self {
            Self::Left((target, left)) => (target, Left(left)),
            Self::Right((target, right)) => (target, Right(right)),
        }
    }
}

impl<L, R, T> Either<(L, T), (R, T)> {
    /// Transposes an `Either` of tuples to a tuple of a single value and an `Either` with
    /// the right value having a uniform type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<(u8, char), (i32, char)> = Either::Left((1, 'c'));
    /// assert_eq!(value.transpose_right(), (Either::Left(1), 'c'));
    ///
    /// let value: Either<(u8, char), (i32, char)> = Either::Right((-2, 'a'));
    /// assert_eq!(value.transpose_right(), (Either::Right(-2), 'a'));
    /// ```
    pub fn transpose_right(self) -> (Either<L, R>, T) {
        match self {
            Self::Left((left, target)) => (Left(left), target),
            Self::Right((right, target)) => (Right(right), target),
        }
    }
}

impl<L, R> Either<Option<L>, Option<R>> {
    /// Transposes an `Either` of [`Options`] to an option of an `Either`
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<Option<u8>, Option<char>> = Either::Left(Some(1));
    /// assert_eq!(value.transpose(), Some(Either::Left(1)));
    ///
    /// let value: Either<Option<u8>, Option<char>> = Either::Left(None);
    /// assert_eq!(value.transpose(), None);
    ///
    /// let value: Either<Option<u8>, Option<char>> = Either::Right(Some('c'));
    /// assert_eq!(value.transpose(), Some(Either::Right('c')));
    /// ```
    ///
    /// [`Options`]: Option
    #[inline]
    pub fn transpose(self) -> Option<Either<L, R>> {
        match self {
            Self::Left(left) => left.map(Left),
            Self::Right(right) => right.map(Right),
        }
    }
}

impl<L, R, E1, E2> Either<Result<L, E1>, Result<R, E2>> {
    /// Transposes an `Either` of [`Results`] to a [`Result`] of an `Either`
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let value: Either<Result<u8, char>, Result<i32, u64>> = Either::Left(Ok(1));
    /// assert_eq!(value.transpose(), Ok(Either::Left(1)));
    ///
    /// let value: Either<Result<u8, char>, Result<i32, u64>> = Either::Left(Err('c'));
    /// assert_eq!(value.transpose(), Err(Either::Left('c')));
    ///
    /// let value: Either<Result<u8, char>, Result<i32, u64>> = Either::Right(Ok(-2));
    /// assert_eq!(value.transpose(), Ok(Either::Right(-2)));
    /// ```
    ///
    /// [`Results`]: Result
    #[allow(clippy::missing_errors_doc)]
    #[inline]
    pub fn transpose(self) -> Result<Either<L, R>, Either<E1, E2>> {
        match self {
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

impl<L, R, E> Either<Result<L, E>, Result<R, E>> {
    /// Transposes an `Either` of [`Results`] to a [`Result`] with an uniform error type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let x: Either<Result<u8, char>, Result<i32, char>> = Either::Left(Ok(1));
    /// assert_eq!(x.transpose_err(), Ok(Either::Left(1)));
    ///
    /// let x: Either<Result<u8, char>, Result<i32, char>> = Either::Left(Err('c'));
    /// assert_eq!(x.transpose_err(), Err('c'));
    ///
    /// let x: Either<Result<u8, char>, Result<i32, char>> = Either::Right(Ok(-2));
    /// assert_eq!(x.transpose_err(), Ok(Either::Right(-2)));
    /// ```
    ///
    /// [`Results`]: Result
    #[allow(clippy::missing_errors_doc)]
    #[inline]
    pub fn transpose_err(self) -> Result<Either<L, R>, E> {
        match self {
            Self::Left(left) => left.map(Left),
            Self::Right(right) => right.map(Right),
        }
    }
}

impl<T, E1, E2> Either<Result<T, E1>, Result<T, E2>> {
    /// Transposes an `Either` of [`Results`] to a [`Result`] with an uniform correct type
    ///
    /// # Examples
    ///
    /// ```
    /// use either_or_both::Either;
    ///
    /// let x: Either<Result<u8, char>, Result<u8, i32>> = Either::Left(Ok(2));
    /// assert_eq!(x.transpose_ok(), Ok(2));
    ///
    /// let x: Either<Result<u8, char>, Result<u8, i32>> = Either::Left(Err('c'));
    /// assert_eq!(x.transpose_ok(), Err(Either::Left('c')));
    ///
    /// let x: Either<Result<u8, char>, Result<u8, i32>> = Either::Right(Ok(2));
    /// assert_eq!(x.transpose_ok(), Ok(2));
    /// ```
    ///
    /// [`Results`]: Result
    #[allow(clippy::missing_errors_doc)]
    #[inline]
    pub fn transpose_ok(self) -> Result<T, Either<E1, E2>> {
        match self {
            Self::Left(left) => left.map_err(Left),
            Self::Right(right) => right.map_err(Right),
        }
    }
}
