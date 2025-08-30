//! The traits implemented for `Either`

use core::fmt::{self, Display};
use core::future::Future;
use core::ops::{Deref, DerefMut};
use core::pin::Pin;
#[cfg(feature = "std")]
use std::error::Error;
#[cfg(feature = "std")]
use std::io::{BufRead, Read, Seek};

use crate::iter_either::{IntoIterEither, IterEither, IterMutEither};
use crate::Either;

impl<L, R, T> AsMut<T> for Either<L, R>
where
    T: ?Sized,
    L: AsMut<T>,
    R: AsMut<T>,
{
    #[inline]
    fn as_mut(&mut self) -> &mut T {
        each!(self, .as_mut())
    }
}

impl<L, R, T> AsRef<T> for Either<L, R>
where
    T: ?Sized,
    L: AsRef<T>,
    R: AsRef<T>,
{
    #[inline]
    fn as_ref(&self) -> &T {
        each!(self, .as_ref())
    }
}

#[cfg(feature = "std")]
impl<L, R> BufRead for Either<L, R>
where
    L: BufRead,
    R: BufRead,
{
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        each!(self, .fill_buf())
    }

    fn consume(&mut self, amount: usize) {
        each!(self, .consume(amount));
    }
}

impl<L, R> Deref for Either<L, R>
where
    L: Deref<Target = R::Target>,
    R: Deref,
{
    type Target = R::Target;

    #[inline]
    fn deref(&self) -> &Self::Target {
        each!(self)
    }
}

impl<L, R> DerefMut for Either<L, R>
where
    L: DerefMut<Target = R::Target>,
    R: DerefMut,
{
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        each!(self)
    }
}

impl<L, R> Display for Either<L, R>
where
    L: Display,
    R: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        each!(self, .fmt(f))
    }
}

#[cfg(feature = "std")]
impl<L, R> Error for Either<L, R>
where
    L: Error,
    R: Error,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        each!(self, .source())
    }
}

impl<A, T> Extend<A> for Either<T>
where
    T: Extend<A>,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = A>,
    {
        each!(self, .extend(iter));
    }
}

impl<'a, L, R> From<&'a Either<L, R>> for Either<&'a L, &'a R> {
    #[inline]
    fn from(value: &'a Either<L, R>) -> Self {
        value.as_ref()
    }
}

impl<'a, L, R> From<&'a mut Either<L, R>> for Either<&'a mut L, &'a mut R> {
    #[inline]
    fn from(value: &'a mut Either<L, R>) -> Self {
        value.as_mut()
    }
}

impl<L, R> From<Result<R, L>> for Either<L, R> {
    #[inline]
    fn from(value: Result<R, L>) -> Self {
        match value {
            Ok(ok) => Self::Right(ok),
            Err(err) => Self::Left(err),
        }
    }
}

impl<L, R> Future for Either<L, R>
where
    L: Future<Output = R::Output>,
    R: Future,
{
    type Output = R::Output;

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        match self.as_pin_mut() {
            Either::Left(left) => left.poll(cx),
            Either::Right(right) => right.poll(cx),
        }
    }
}

impl<T> IntoIterator for Either<T> {
    type Item = T;
    type IntoIter = IntoIterEither<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIterEither::new(self)
    }
}

impl<'a, T> IntoIterator for &'a Either<T> {
    type Item = &'a T;
    type IntoIter = IterEither<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        IterEither::new(self)
    }
}

impl<'a, T> IntoIterator for &'a mut Either<T> {
    type Item = &'a mut T;
    type IntoIter = IterMutEither<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        IterMutEither::new(self)
    }
}

#[cfg(feature = "std")]
impl<L, R> Read for Either<L, R>
where
    L: Read,
    R: Read,
{
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        each!(self, .read(buf))
    }
}

#[cfg(feature = "std")]
impl<L, R> Seek for Either<L, R>
where
    L: Seek,
    R: Seek,
{
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        each!(self, .seek(pos))
    }
}

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
