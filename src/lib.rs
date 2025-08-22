//! The enums [`EitherOrBoth`] with the three variants `Left`, `Right`, `Both` and [`Either`] with
//! the `Left` and `Right` variants.
//!
//! The [`Either`] enum represents a value with two possibilities and [`EitherOrBoth`] represents
//! values which can additionally be both present simultaneously.
//!
//! # Design Philosophy and Goals
//!
//! The api for `either_or_both` is heavily inspired by [`Option`] from the rust standard library
//! and is designed to be consistent for the [`Either`] and [`EitherOrBoth`] enums. Some methods
//! (like `bimap`) have their root in functional programming languages like `Haskell`. Where it
//! makes sense, the methods of [`Option`] are implemented for [`Either`] and [`EitherOrBoth`]. As
//! far as possible, the names of methods with similar functionality are transferred to both enums.
//! All in all, if you're familiar with the methods for [`Option`], it should be easy to figure out
//! what the equivalent method in [`Either`] and [`EitherOrBoth`] does.
//!
//! # Conventions and edge cases
//!
//! In most cases, [`Either`] and [`EitherOrBoth`] do not prioritize the right over the left value
//! and vice versa. However, in rare situations, `Left` holds an error value, while `Right` holds
//! the correct value (with "right" serving as a mnemonic for "correct"). If applicable or if in
//! doubt, try to adhere to this convention to simplify conversions between [`Either`] and
//! [`Result`].
//!
//! With [`EitherOrBoth`], it can happen that the `Both` variant holds an error and a correct value,
//! and if not specified otherwise the evaluation of an error value takes precedence over the
//! correct value. Same goes for the `None` and `Some` values of an [`Option`], in which case
//! evaluating `None` takes precedence.
//!
//! # Method Overview
//!
//! [`Either`] and [`EitherOrBoth`] provide a wide variety of different methods.
//!
//! ## Querying the variant
//!
//! Similar to [`Option::is_some`] and [`Option::is_none`], [`Either::is_left`],
//! [`Either::is_right`] return `true` if the [`Either`] is `Left` or `Right` respectively. Same for
//! [`EitherOrBoth::is_left`] and [`EitherOrBoth::is_right`], but additionally
//! [`EitherOrBoth::is_both`] returns true if [`EitherOrBoth`] is `Both`. Since [`EitherOrBoth`] can
//! hold a left value in the `Both` and `Left` case, the method [`EitherOrBoth::has_left`] returns
//! true if the variant is either `Both` or `Left`. Symmetrically, the right value is covered with
//! [`EitherOrBoth::has_right`].
//!
//! Methods like [`Either::is_left_and`], [`Either::is_left_or`], [`Either::is_right_and`], ...,
//! [`EitherOrBoth::has_left_and`], [`EitherOrBoth::has_left_or`], ... apply a function to the
//! contents of [`Either`] or [`EitherOrBoth`] to compute a boolean value.

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "either")]
mod either;
mod either_or_both;
mod error;

#[cfg(feature = "either")]
pub use either::{iter as iter_either, Either};
pub use either_or_both::{iter, EitherOrBoth};
pub use error::TryFromOptionsError;

#[cold]
#[track_caller]
fn unwrap_failed(msg: &str) -> ! {
    panic!("{msg}");
}
