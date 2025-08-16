//! The library

#![no_std]

#[cfg(feature = "std")]
extern crate std;

pub mod either;
mod either_or_both;
pub mod error;
pub mod iter;

pub use either_or_both::EitherOrBoth;
