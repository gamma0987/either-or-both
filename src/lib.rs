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
#[cfg(feature = "std")]
pub use either_or_both::WriteIo;
pub use either_or_both::{EitherOrBoth, WriteFmt};
pub use error::Error;

#[cold]
#[track_caller]
fn unwrap_failed(msg: &str) -> ! {
    panic!("{msg}");
}
