//! The library

#![no_std]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "either")]
mod either;
mod either_or_both;
mod error;
pub mod iter;
#[cfg(feature = "either")]
pub mod iter_either;

#[cfg(feature = "either")]
pub use either::Either;
pub use either_or_both::EitherOrBoth;
pub use error::Error;
