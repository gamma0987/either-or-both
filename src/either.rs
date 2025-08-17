//! The `Either` enum
macro_rules! each {
    ($src:expr, $( $methods:tt )*) => {
        match $src {
            Self::Left(left) => left $($methods)*,
            Self::Right(right) => right $($methods)*,
        }
    };
}

use core::ops::{Deref, DerefMut};
use core::pin::Pin;
use core::{fmt, mem};

use crate::iter_either::{IterEither, SwapIterEither};

// TODO: Docs are partially still from EitherOrBoth

/// Either left or right can be present
#[derive(Debug, Clone)]
pub enum Either<L, R = L> {
    /// The left value
    Left(L),
    /// The right value
    Right(R),
}

// TODO: Double check that all generics are used (especially F)
// TODO: iterators
// TODO: Something like add_left replacing the left value if Left and if Right -> EitherOrBoth::Both
impl<L, R> Either<L, R> {
    /// TODO: DOCS
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left(_))
    }

    /// Returns true if `Left` and it matches a predicate
    pub fn is_left_and<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(_) => false,
        }
    }

    /// Returns true if `Left` or the right value matches a predicate
    pub fn is_left_or<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Left(_) => true,
            Self::Right(right) => f(right),
        }
    }

    /// TODO: DOCS
    pub fn is_right(&self) -> bool {
        matches!(self, Self::Right(_))
    }

    /// Returns true if `Right` and it matches a predicate
    pub fn is_right_and<F>(self, f: F) -> bool
    where
        F: FnOnce(R) -> bool,
    {
        match self {
            Self::Right(right) => f(right),
            Self::Left(_) => false,
        }
    }

    /// Returns true if `Right` or the right value matches a predicate
    pub fn is_right_or<F>(self, f: F) -> bool
    where
        F: FnOnce(L) -> bool,
    {
        match self {
            Self::Right(_) => true,
            Self::Left(left) => f(left),
        }
    }

    /// TODO: DOCS
    pub fn as_ref(&self) -> Either<&L, &R> {
        match self {
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => Either::Right(right),
        }
    }

    /// TODO: DOCS
    pub fn as_mut(&mut self) -> Either<&mut L, &mut R> {
        match self {
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => Either::Right(right),
        }
    }

    /// Converts from `EitherOrBoth<L, R>` to `EitherOrBoth<&L::Target, &R::Target>`.
    ///
    /// This method leaves the original `EitherOrBoth` unchanged, creating a new one with a
    /// reference to the original one, additionally coercing the contents via Deref.
    pub fn as_deref(&self) -> Either<&<L as Deref>::Target, &<R as Deref>::Target>
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
    pub fn as_deref_mut(&mut self) -> Either<&mut <L as Deref>::Target, &mut <R as Deref>::Target>
    where
        L: DerefMut,
        R: DerefMut,
    {
        self.as_mut().bimap(|l| &mut **l, |r| &mut **r)
    }

    /// Converts from `Pin<&EitherOrBoth<L, R>>` to `EitherOrBoth<Pin<&L>, Pin<&R>>`.
    pub fn as_pin_ref(self: Pin<&Self>) -> Either<Pin<&L>, Pin<&R>> {
        // SAFETY: `x` is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            match Pin::get_ref(self) {
                Self::Left(left) => Either::Left(Pin::new_unchecked(left)),
                Self::Right(right) => Either::Right(Pin::new_unchecked(right)),
            }
        }
    }

    /// Converts from `Pin<&EitherOrBoth<L, R>>` to `EitherOrBoth<Pin<&L>, Pin<&R>>`.
    pub fn as_pin_mut(self: Pin<&mut Self>) -> Either<Pin<&mut L>, Pin<&mut R>> {
        // SAFETY: `x` is guaranteed to be pinned because it comes from `self` which is pinned.
        unsafe {
            match Pin::get_unchecked_mut(self) {
                Self::Left(left) => Either::Left(Pin::new_unchecked(left)),
                Self::Right(right) => Either::Right(Pin::new_unchecked(right)),
            }
        }
    }

    /// TODO: DOCS
    pub fn expect_left(self, msg: &str) -> L {
        match self {
            Self::Left(left) => left,
            Self::Right(_) => unwrap_failed(msg),
        }
    }

    /// TODO: DOCS
    pub fn expect_right(self, msg: &str) -> R {
        match self {
            Self::Left(_) => unwrap_failed(msg),
            Self::Right(right) => right,
        }
    }

    /// TODO: DOCS
    pub fn unwrap_left(self) -> L {
        match self {
            Self::Left(left) => left,
            Self::Right(_) => unwrap_failed("Called `Either::unwrap_left` on a `Right` value"),
        }
    }

    /// TODO: DOCS
    ///
    /// # Safety
    pub unsafe fn unwrap_left_unchecked(self) -> L {
        match self {
            Self::Left(left) => left,
            // SAFETY: the safety contract must be upheld by the caller.
            Self::Right(_) => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    /// TODO: DOCS
    pub fn unwrap_right(self) -> R {
        match self {
            Self::Left(_) => unwrap_failed("Called `Either::unwrap_right` on a `Left` value"),
            Self::Right(right) => right,
        }
    }

    /// TODO: DOCS
    ///
    /// # Safety
    pub unsafe fn unwrap_right_unchecked(self) -> R {
        match self {
            // SAFETY: the safety contract must be upheld by the caller.
            Self::Left(_) => unsafe { core::hint::unreachable_unchecked() },
            Self::Right(right) => right,
        }
    }

    /// TODO: DOCS
    pub fn left(self) -> Option<L> {
        match self {
            Self::Left(left) => Some(left),
            Self::Right(_) => None,
        }
    }

    /// TODO: DOCS
    pub fn left_and<T>(self, other: Either<T, R>) -> Either<T, R> {
        match self {
            Self::Left(_) => other,
            Self::Right(right) => Either::Right(right),
        }
    }

    /// TODO: DOCS
    pub fn left_and_then<F, T>(self, f: F) -> Either<T, R>
    where
        F: FnOnce(L) -> Either<T, R>,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => Either::Right(right),
        }
    }

    /// TODO: DOCS
    pub fn right(self) -> Option<R> {
        match self {
            Self::Left(_) => None,
            Self::Right(right) => Some(right),
        }
    }

    /// TODO: DOCS
    pub fn right_and<T>(self, other: Either<L, T>) -> Either<L, T> {
        match self {
            Self::Left(left) => Either::Left(left),
            Self::Right(_) => other,
        }
    }

    /// TODO: DOCS
    pub fn right_and_then<F, T>(self, f: F) -> Either<L, T>
    where
        F: FnOnce(R) -> Either<L, T>,
    {
        match self {
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => f(right),
        }
    }

    /// Converts `Either<L, R>` to `Either<R, L>`.
    pub fn flip(self) -> Either<R, L> {
        match self {
            Self::Left(left) => Either::Right(left),
            Self::Right(right) => Either::Left(right),
        }
    }

    /// TODO: DOCS
    pub fn into_iter_swap(
        self,
    ) -> SwapIterEither<<L as IntoIterator>::IntoIter, <R as IntoIterator>::IntoIter>
    where
        L: IntoIterator,
        R: IntoIterator,
    {
        SwapIterEither::new(self.bimap(IntoIterator::into_iter, IntoIterator::into_iter))
    }

    /// TODO: DOCS
    pub fn iter_swap<'a>(
        &'a self,
    ) -> SwapIterEither<<&'a L as IntoIterator>::IntoIter, <&'a R as IntoIterator>::IntoIter>
    where
        &'a L: IntoIterator,
        &'a R: IntoIterator,
    {
        SwapIterEither::new(
            self.as_ref()
                .bimap(IntoIterator::into_iter, IntoIterator::into_iter),
        )
    }

    /// TODO: DOCS
    pub fn iter_swap_mut<'a>(
        &'a mut self,
    ) -> SwapIterEither<<&'a mut L as IntoIterator>::IntoIter, <&'a mut R as IntoIterator>::IntoIter>
    where
        &'a mut L: IntoIterator,
        &'a mut R: IntoIterator,
    {
        SwapIterEither::new(
            self.as_mut()
                .bimap(IntoIterator::into_iter, IntoIterator::into_iter),
        )
    }

    /// TODO: DOCS
    pub fn bimap<F, G, T, U>(self, f: F, g: G) -> Either<T, U>
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> U,
    {
        match self {
            Self::Left(left) => Either::Left(f(left)),
            Self::Right(right) => Either::Right(g(right)),
        }
    }

    /// TODO: DOCS
    pub fn map_left<F, T>(self, f: F) -> Either<T, R>
    where
        F: FnOnce(L) -> T,
    {
        match self {
            Self::Left(left) => Either::Left(f(left)),
            Self::Right(right) => Either::Right(right),
        }
    }

    /// TODO: DOCS
    pub fn map_left_or<F, U>(self, default: U, f: F) -> U
    where
        F: FnOnce(L) -> U,
    {
        self.map_left_or_else(|| default, f)
    }

    /// TODO: DOCS
    pub fn map_left_or_default<F, U>(self, f: F) -> U
    where
        F: FnOnce(L) -> U,
        U: Default,
    {
        self.map_left_or_else(U::default, f)
    }

    /// TODO: DOCS
    pub fn map_left_or_else<D, F, U>(self, default: D, f: F) -> U
    where
        F: FnOnce(L) -> U,
        D: FnOnce() -> U,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(_) => default(),
        }
    }

    /// TODO: DOCS
    pub fn map_right<F, T>(self, f: F) -> Either<L, T>
    where
        F: FnOnce(R) -> T,
    {
        match self {
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => Either::Right(f(right)),
        }
    }

    /// TODO: DOCS
    pub fn map_right_or<F, T>(self, default: T, f: F) -> T
    where
        F: FnOnce(R) -> T,
    {
        self.map_right_or_else(|| default, f)
    }

    /// TODO: DOCS
    pub fn map_right_or_default<F, T>(self, f: F) -> T
    where
        F: FnOnce(R) -> T,
        T: Default,
    {
        self.map_right_or_else(T::default, f)
    }

    /// TODO: DOCS
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

    /// TODO: DOCS
    pub fn biinspect<F, G>(self, f: F, g: G) -> Self
    where
        F: FnOnce(&L),
        G: FnOnce(&R),
    {
        self.as_ref().biconsume(f, g);
        self
    }

    /// TODO: DOCS
    pub fn inspect_left<F>(self, f: F) -> Self
    where
        F: FnOnce(&L),
    {
        self.as_ref().consume_left(f);
        self
    }

    /// TODO: DOCS
    pub fn inspect_right<F>(self, f: F) -> Self
    where
        F: FnOnce(&R),
    {
        self.as_ref().consume_right(f);
        self
    }

    /// TODO: DOCS
    pub fn biconsume<F, G>(self, f: F, g: G)
    where
        F: FnOnce(L),
        G: FnOnce(R),
    {
        match self {
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
            Self::Left(left) => f(left),
            Self::Right(_) => {}
        }
    }

    /// TODO: DOCS
    pub fn consume_right<F>(self, f: F)
    where
        F: FnOnce(R),
    {
        match self {
            Self::Right(right) => f(right),
            Self::Left(_) => {}
        }
    }

    /// TODO: DOCS
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

    /// TODO: DOCS
    pub fn reduce_left<F, T>(self, f: F) -> L
    where
        F: FnOnce(R) -> L,
    {
        match self {
            Self::Left(left) => left,
            Self::Right(right) => f(right),
        }
    }

    /// TODO: DOCS
    pub fn reduce_right<F, T>(self, f: F) -> R
    where
        F: FnOnce(L) -> R,
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => right,
        }
    }

    /// TODO: DOCS
    pub fn ok(self) -> Result<R, L> {
        match self {
            Self::Left(left) => Err(left),
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
            Self::Left(_) => Err(error()),
            Self::Right(ok) => Ok(ok),
        }
    }

    /// Returns `Both` if present otherwise the missing value supplied by `left` or `right`
    ///
    /// TODO: Eagerly versus lazily
    pub fn or(self, left: L, right: R) -> (L, R) {
        match self {
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
            Self::Left(left) => (left, g()),
            Self::Right(right) => (f(), right),
        }
    }

    /// TODO: DOCS
    pub fn replace_left(&mut self, value: L) -> Option<L> {
        match self {
            Self::Left(left) => Some(mem::replace(left, value)),
            Self::Right(_) => None,
        }
    }

    /// TODO: DOCS
    pub fn replace_right(&mut self, value: R) -> Option<R> {
        match self {
            Self::Right(right) => Some(mem::replace(right, value)),
            Self::Left(_) => None,
        }
    }

    /// TODO: DOCS
    pub fn into_left<F>(self, f: F) -> Self
    where
        F: FnOnce(R) -> L,
    {
        match self {
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
            Self::Left(left) => Self::Right(f(left)),
            Self::Right(_) => self,
        }
    }
}

impl<T> Either<T, T> {
    /// TODO: DOCS
    pub fn consume<F>(self, f: F)
    where
        F: Fn(T),
    {
        match self {
            Self::Left(left) => f(left),
            Self::Right(right) => f(right),
        }
    }

    /// TODO: DOCS
    pub fn inspect<F>(self, f: F) -> Self
    where
        F: Fn(&T),
    {
        self.as_ref().consume(f);
        self
    }

    /// TODO: DOCS
    pub fn iter<'a>(&'a self) -> IterEither<<&'a T as IntoIterator>::IntoIter>
    where
        &'a T: IntoIterator,
    {
        IterEither::new(self.as_ref().map(IntoIterator::into_iter))
    }

    /// TODO: DOCS
    pub fn iter_mut<'a>(&'a mut self) -> IterEither<<&'a mut T as IntoIterator>::IntoIter>
    where
        &'a mut T: IntoIterator,
    {
        IterEither::new(self.as_mut().map(IntoIterator::into_iter))
    }

    /// TODO: DOCS
    pub fn map<F, U>(self, f: F) -> Either<U, U>
    where
        F: Fn(T) -> U,
    {
        match self {
            Self::Left(left) => Either::Left(f(left)),
            Self::Right(right) => Either::Right(f(right)),
        }
    }

    /// TODO: DOCS
    pub fn reduce(self) -> T {
        match self {
            Self::Left(left) => left,
            Self::Right(right) => right,
        }
    }
}

impl<L, R> Either<&L, &R> {
    /// TODO: DOCS
    pub fn cloned(self) -> Either<L, R>
    where
        L: Clone,
        R: Clone,
    {
        match self {
            Self::Left(left) => Either::Left(left.clone()),
            Self::Right(right) => Either::Right(right.clone()),
        }
    }

    /// TODO: DOCS
    pub fn copied(self) -> Either<L, R>
    where
        L: Copy,
        R: Copy,
    {
        match self {
            Self::Left(left) => Either::Left(*left),
            Self::Right(right) => Either::Right(*right),
        }
    }
}

impl<L, R> Either<&mut L, &mut R> {
    /// TODO: DOCS
    pub fn cloned(self) -> Either<L, R>
    where
        L: Clone,
        R: Clone,
    {
        match self {
            Self::Left(left) => Either::Left(left.clone()),
            Self::Right(right) => Either::Right(right.clone()),
        }
    }

    /// TODO: DOCS
    pub fn copied(self) -> Either<L, R>
    where
        L: Copy,
        R: Copy,
    {
        match self {
            Self::Left(left) => Either::Left(*left),
            Self::Right(right) => Either::Right(*right),
        }
    }
}

impl<L, R> Either<Option<L>, Option<R>> {
    /// TODO: DOCS
    pub fn transpose(self) -> Option<Either<L, R>> {
        match self {
            Self::Left(left) => left.map(Either::Left),
            Self::Right(right) => right.map(Either::Right),
        }
    }
}

impl<L, R, E1, E2> Either<Result<L, E1>, Result<R, E2>> {
    /// TODO: DOCS
    pub fn bimap_err<F, G, X1, X2>(self, f: F, g: G) -> Either<Result<L, X1>, Result<R, X2>>
    where
        F: FnOnce(E1) -> X1,
        G: FnOnce(E2) -> X2,
    {
        self.bimap(|l| l.map_err(f), |r| r.map_err(g))
    }

    /// TODO: DOCS
    pub fn bimap_ok<F, G, T, U>(self, f: F, g: G) -> Either<Result<T, E1>, Result<U, E2>>
    where
        F: FnOnce(L) -> T,
        G: FnOnce(R) -> U,
    {
        self.bimap(|l| l.map(f), |r| r.map(g))
    }

    /// TODO: DOCS
    pub fn transpose(self) -> Result<Either<L, R>, Either<E1, E2>> {
        match self {
            Self::Left(left) => match left {
                Ok(ok) => Ok(Either::Left(ok)),
                Err(err) => Err(Either::Left(err)),
            },
            Self::Right(right) => match right {
                Ok(ok) => Ok(Either::Right(ok)),
                Err(err) => Err(Either::Right(err)),
            },
        }
    }
}

impl<L, R, E> Either<Result<L, E>, Result<R, E>> {
    /// TODO: DOCS
    pub fn transpose_ok(self) -> Result<Either<L, R>, E> {
        match self {
            Self::Left(left) => left.map(Either::Left),
            Self::Right(right) => right.map(Either::Right),
        }
    }

    /// TODO: DOCS
    pub fn map_err<F, X>(self, f: F) -> Either<Result<L, X>, Result<R, X>>
    where
        F: Fn(E) -> X,
    {
        self.bimap(|l| l.map_err(&f), |r| r.map_err(&f))
    }
}

impl<T, E1, E2> Either<Result<T, E1>, Result<T, E2>> {
    /// TODO: DOCS
    pub fn transpose_err(self) -> Result<T, Either<E1, E2>> {
        match self {
            Self::Left(left) => left.map_err(Either::Left),
            Self::Right(right) => right.map_err(Either::Right),
        }
    }
}

impl<L, R> From<Result<R, L>> for Either<L, R> {
    fn from(value: Result<R, L>) -> Self {
        match value {
            Ok(ok) => Self::Right(ok),
            Err(err) => Self::Left(err),
        }
    }
}

impl<T> IntoIterator for Either<T>
where
    T: IntoIterator,
{
    type Item = T::Item;
    type IntoIter = IterEither<<T as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        IterEither::new(self.map(IntoIterator::into_iter))
    }
}

impl<'a, T> IntoIterator for &'a Either<T>
where
    &'a T: IntoIterator,
{
    type Item = <&'a T as IntoIterator>::Item;
    type IntoIter = IterEither<<&'a T as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        IterEither::new(self.as_ref().map(IntoIterator::into_iter))
    }
}

// TODO: STOPPED with iterators, but they should be finished
impl<'a, T> IntoIterator for &'a mut Either<T>
where
    &'a mut T: IntoIterator,
{
    type Item = <&'a mut T as IntoIterator>::Item;
    type IntoIter = IterEither<<&'a mut T as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        IterEither::new(self.as_mut().map(IntoIterator::into_iter))
    }
}

// TODO: CONTINUE implementing useful traits
impl<L, R> fmt::Write for Either<L, R>
where
    L: fmt::Write,
    R: fmt::Write,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        each!(self, .write_str(s))
    }
}

#[cfg(feature = "std")]
impl<L, R> std::io::Write for Either<L, R>
where
    L: std::io::Write,
    R: std::io::Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        each!(self, .write(buf))
    }

    fn flush(&mut self) -> std::io::Result<()> {
        each!(self, .flush())
    }
}

#[cold]
#[track_caller]
fn unwrap_failed(msg: &str) -> ! {
    panic!("{msg}");
}

#[test]
fn test_iter() {
    // use std::vec;
    // use std::vec::Vec;
    //
    // let mut either: Either<Vec<i32>, Vec<i32>> = Either::Left(vec![1, 2]);
    // for e in &mut either {
    //     *e = 1;
    // }
}
