//! The library

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "either")]
mod either;
mod either_or_both;
mod error;

#[cfg(feature = "either")]
pub use either::{iter as iter_either, traits as iter_traits, Either};
pub use either_or_both::{iter, traits, EitherOrBoth};
pub use error::TryFromOptionsError;

#[cold]
#[track_caller]
fn unwrap_failed(msg: &str) -> ! {
    panic!("{msg}");
}
