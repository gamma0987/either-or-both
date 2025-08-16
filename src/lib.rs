//! The library

#![no_std]

#[cfg(feature = "std")]
extern crate std;

mod either;
mod either_or_both;
mod error;
pub mod iter;

pub use either::Either;
pub use either_or_both::EitherOrBoth;
pub use error::Error;
