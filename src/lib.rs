//! # `EitherOrBoth` and `Either`
//!
//! `either-or-both` provides two enums: [`Either`] and [`EitherOrBoth`].
//!
//! * **`Either<L, R>`** — a value that is either `Left(L)` or `Right(R)`
//! * **`EitherOrBoth<L, R>`** — a value that can be `Left(L)`, `Right(R)`, or `Both(L, R)`
//!
//! While `Either` is useful for representing mutually exclusive values, `EitherOrBoth` extends this
//! idea by allowing both values to be present simultaneously.
//!
//! # Conventions and edge cases
//!
//! In most cases, [`Either`] and [`EitherOrBoth`] do not prioritize `Left` over `Right` values, or
//! vice versa. However, in some contexts, `Left` is used to represent an error and `Right` a
//! correct value - using "right" as a mnemonic for "correct".
//!
//! With [`EitherOrBoth`], the `Both` variant may contain both an error and a correct value. Unless
//! explicitly stated otherwise, the error value takes precedence during evaluation. The same
//! principle applies to [`Option`] types, where `None` takes precedence over `Some`.
//!
//! Uniform type versions of bi-functional methods drop the bi- prefix. For example, bimap applies
//! two functions to potentially different types (`L` and `R`), while map applies the same function
//! to both sides of a uniform type `T`.
//!
//! When evaluation order matters for [`EitherOrBoth`] methods, values are processed left to right.
//! To reverse this order, use [flip][EitherOrBoth::flip].
//!
//! # Method Overview
//!
//! [`Either`] and [`EitherOrBoth`] provide a wide variety of different methods. Often, the links
//! and descriptions in the following sections use [`EitherOrBoth`] but if not specified otherwise,
//! the same methods are available for [`Either`].
//!
//! ## Querying the variant
//!
//! Similar to [`Option::is_some`] and [`Option::is_none`], [`is_left`][Either::is_left] and
//! [`is_right`][Either::is_right] return `true` if the [`Either`] or [`EitherOrBoth`] is `Left` or
//! `Right` respectively. Additionally, [`is_both`][EitherOrBoth::is_both] returns true if
//! [`EitherOrBoth`] is `Both`. Since [`EitherOrBoth`] can hold a left value in the `Both` and
//! `Left` case, the method [`has_left`][EitherOrBoth::has_left] returns true if the variant is
//! either `Both` or `Left`. Symmetrically, the right value is covered with
//! [`has_right`][EitherOrBoth::has_right].
//!
//! Methods like [`is_left_and`][Either::is_left_and], [`is_left_or`][Either::is_left_or],
//! [`has_left_and`][EitherOrBoth::has_left_and], [`has_left_or`][EitherOrBoth::has_left_or], ...
//! apply a function to the contents of [`Either`] or [`EitherOrBoth`] to compute a boolean value.
//!
//! ## Adapters for working with references
//!
//! * [`as_ref`] converts from [`&EitherOrBoth<L, R>`][EitherOrBoth] to [`EitherOrBoth<&L,
//!   &R>`][EitherOrBoth]
//! * [`as_mut`] converts from [`&mut EitherOrBoth<L, R>`][EitherOrBoth] to [`EitherOrBoth<&mut L,
//!   &mut R>`][EitherOrBoth]
//! * [`as_deref`] converts from [`&EitherOrBoth<L, R>`][EitherOrBoth] to [`EitherOrBoth<&L::Target,
//!   &R::Target>`][EitherOrBoth]
//! * [`as_deref_mut`] converts from [`&mut EitherOrBoth<L, R>`][EitherOrBoth] to
//!   [`EitherOrBoth<&mut L::Target, &mut R::Target>`][EitherOrBoth]
//! * [`as_pin_ref`] converts from [`Pin<&EitherOrBoth<L, R>>`][EitherOrBoth] to
//!   [`EitherOrBoth<Pin<&L>, Pin<&R>>`][EitherOrBoth]
//! * [`as_pin_mut`] converts from [`Pin<&mut EitherOrBoth<L, R>>`][EitherOrBoth] to
//!   [`EitherOrBoth<Pin<&mut L>, Pin<&mut R>>`][EitherOrBoth]
//!
//! [`as_deref`]: EitherOrBoth::as_deref
//! [`as_deref_mut`]: EitherOrBoth::as_deref_mut
//! [`as_mut`]: EitherOrBoth::as_mut
//! [`as_pin_mut`]: EitherOrBoth::as_pin_mut
//! [`as_pin_ref`]: EitherOrBoth::as_pin_ref
//! [`as_ref`]: EitherOrBoth::as_ref
//!
//! ## Extracting the contained values
//!
//! These methods extract the contained value in an [`EitherOrBoth<L, R>`] or [`Either<L, R>`]
//!
//! * [`expect_left`] panics with a provided custom message if the variant is `Right`
//! * [`expect_right`] panics with a provided custom message if the variant is `Left`
//! * [`unwrap_left`] panics with a generic message if the variant is `Right`
//! * [`unwrap_right`] panics with a generic message if the variant is `Left`
//! * [`unwrap_left_unchecked`] produces *[undefined behavior]* if the variant is `Right`
//! * [`unwrap_right_unchecked`] produces *[undefined behavior]* if the variant is `Left`
//!
//! [`EitherOrBoth`] provides additional methods:
//!
//! * [`expect_both`] panics with a provided custom message if the variant is `Right` or `Left`
//! * [`unwrap_both`] panics with a generic message if the variant is `Right` or `Left`
//! * [`unwrap_both_unchecked`] produces *[undefined behavior]* if the variant is `Right` or `Left`
//! * [`expect_only_left`] panics with a provided custom message if the variant is `Right` or `Both`
//! * [`unwrap_only_left`] panics with a generic message if the variant is `Right` or `Both`
//! * [`unwrap_only_left_unchecked`] produces *[undefined behavior]* if the variant is `Right` or
//!   `Both`
//! * [`expect_only_right`] panics with a provided custom message if the variant is `Left` or `Both`
//! * [`unwrap_only_right`] panics with a generic message if the variant is `Left` or `Both`
//! * [`unwrap_only_right_unchecked`] produces *[undefined behavior]* if the variant is `Left` or
//!   `Both`
//!
//! [`expect_both`]: EitherOrBoth::expect_both
//! [`unwrap_both`]: EitherOrBoth::unwrap_both
//! [`unwrap_both_unchecked`]: EitherOrBoth::unwrap_both_unchecked
//! [`expect_left`]: EitherOrBoth::expect_left
//! [`unwrap_left`]: EitherOrBoth::unwrap_left
//! [`unwrap_left_unchecked`]: EitherOrBoth::unwrap_left_unchecked
//! [`expect_right`]: EitherOrBoth::expect_right
//! [`unwrap_right`]: EitherOrBoth::unwrap_right
//! [`unwrap_right_unchecked`]: EitherOrBoth::unwrap_right_unchecked
//! [`expect_only_left`]: EitherOrBoth::expect_left
//! [`unwrap_only_left`]: EitherOrBoth::unwrap_left
//! [`unwrap_only_left_unchecked`]: EitherOrBoth::unwrap_left_unchecked
//! [`expect_only_right`]: EitherOrBoth::expect_right
//! [`unwrap_only_right`]: EitherOrBoth::unwrap_right
//! [`unwrap_only_right_unchecked`]: EitherOrBoth::unwrap_right_unchecked
//! [undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
//!
//! Non-panicking methods, optionally extract the contained values:
//!
//! * [`left`] returns `Some` with the left value if present otherwise `None`
//! * [`right`] returns `Some` with the right value if present otherwise `None`
//!
//! The methods [`both`], [`only_left`], [`only_right`] are exclusive to [`EitherOrBoth`] and return
//! `Some` with both values if `Both`, and respectively `Some` with the left value if only `Left`
//! and `Some` with the right value if only `Right`. Finally, [`unzip`] produces a 2-tuple of
//! [`Options`][Option] containing both values if present.
//!
//! [`both`]: EitherOrBoth::both
//! [`left`]: EitherOrBoth::left
//! [`only_left`]: EitherOrBoth::only_left
//! [`right`]: EitherOrBoth::right
//! [`only_right`]: EitherOrBoth::only_right
//! [`unzip`]: EitherOrBoth::unzip
//!
//! The [`left_and`], [`right_and`] methods take another [`EitherOrBoth`] or [`Either`] as input and
//! produce an [`EitherOrBoth`] or [`Either`] as output.
//!
//! [`left_and`]: EitherOrBoth::left_and
//! [`right_and`]: EitherOrBoth::right_and
//!
//! Accordingly, [`left_and_then`], [`right_and_then`] methods evaluate lazily by taking a function
//! as input.
//!
//! [`left_and_then`]: EitherOrBoth::left_and_then
//! [`right_and_then`]: EitherOrBoth::right_and_then
//!
//! The following non-panicking methods extract the contained value of an [`EitherOrBoth<L, R>`] and
//! [`Either<L, R>`] by providing default values or methods if one of the values is missing:
//!
//! * [`or`] produces a tuple `(L, R)` with the given values supplying a possibly missing left or
//!   right value
//! * [`or_default`] returns a tuple with `L::default` or `R::default` if one of them is missing.
//! * [`or_else`] returns a tuple computing the missing left or right values with a given function.
//!
//! [`or`]: EitherOrBoth::or
//! [`or_default`]: EitherOrBoth::or_default
//! [`or_else`]: EitherOrBoth::or_else
//!
//! Reducing methods extract the contained value into a single value, potentially converting it to a
//! different type `T`, rather than returning a tuple:
//!
//! * [`reduce_left`] extracts the `L` value, otherwise applies a given function converting the `R`
//!   value to a `L` value
//! * [`reduce_right`] extracts the `R` value, otherwise applies a given function converting the `L`
//!   value to a `R` value
//!
//! [`reduce_left`]: EitherOrBoth::reduce_left
//! [`reduce_right`]: EitherOrBoth::reduce_right
//!
//! Other reductions work differently for [`EitherOrBoth`] and [`Either`]. With [`Either`]
//!
//! * [`bireduce`] applies the given functions converting the `L` and `R` to a type `T`. The uniform
//!   type version of [`bireduce`] is named [`reduce`].
//!
//! [`bireduce`]: Either::bireduce
//! [`reduce`]: Either::reduce
//!
//! [`EitherOrBoth<L, R>`] doesn't have a single `bireduce` method, but instead two methods:
//!
//! * [`reduce_map_left`] which applies the given function to the `L` value if the variant is `Left`
//!   or `Both` and another function to the `R` value if the variant is `Right`.
//! * [`reduce_map_right`] which applies a given function to the `R` value if the variant is `Right`
//!   or `Both` and another function to the `L` value if the variant is `Left`.
//!
//! There is a uniform type version [`EitherOrBoth::reduce`], which extracts `Left` or `Right`,
//! otherwise it applies a function reducing `Both` to a single value.
//!
//! [`reduce_map_left`]: EitherOrBoth::reduce_map_left
//! [`reduce_map_right`]: EitherOrBoth::reduce_map_right
//!
//! ## Conversions
//!
//! The most basic conversion [`EitherOrBoth<L, R>`] to [`EitherOrBoth<R, L>`] can be achieved with
//! [`flip`][EitherOrBoth::flip]. Similar [`Either<L, R>`] to [`Either<R, L>`] with
//! [`flip`][Either::flip].
//!
//! These methods convert to the other variant without changing the signature:
//!
//! * [`into_left`] converts a `Right` or `Both` variant into a `Left` variant by applying a
//!   provided conversion function to the `Right` value if necessary
//! * [`into_right`] converts a `Left` or `Both` variant into a `Right` variant by applying a
//!   provided conversion function to the `Left` value if necessary
//!
//! [`into_left`]: EitherOrBoth::into_left
//! [`into_right`]: EitherOrBoth::into_right
//!
//! ## Transforming contained values
//!
//! The methods below convert to a [`Result`] as described in the [convention]:
//!
//! * [`ok`]: This method transforms [`Left(v)`] to [`Err(v)`], [`Right(v)`] to [`Ok(v)`] and in the
//!   case of [`EitherOrBoth`], [`Both(l, r)`] to [`Err(l)`]
//! * [`ok_or`] transforms [`Right(v)`] to [`Ok(v)`] and a left value to [`Err`] with the provided
//!   `err` value.
//! * [`ok_or_else`] transforms [`Right(v)`] to [`Ok(v)`] and a left value to [`Err`] using the
//!   provided function.
//!
//! [`ok`]: Either::ok
//! [`ok_or`]: Either::ok_or
//! [`ok_or_else`]: Either::ok_or
//! [`Left`]: Either::Left
//! [`Left(v)`]: Either::Left
//! [`Right(v)`]: Either::Right
//! [`Ok(v)`]: Result::Ok
//! [`Err(v)`]: Result::Err
//! [`Err(l)`]: Result::Err
//! [`Err(r)`]: Result::Err
//! [`Both(l, r)`]: EitherOrBoth::Both
//! [convention]: #conventions-and-edge-cases
//!
//! These methods transform the inner values to other types using provided functions:
//!
//! * [`bimap`] transforms [`EitherOrBoth<L, R>`] to [`EitherOrBoth<T, U>`]
//! * [`map_left`] transforms [`EitherOrBoth<L, R>`] to [`EitherOrBoth<T, R>`]
//! * [`map_right`] transforms [`EitherOrBoth<L, R>`] to [`EitherOrBoth<L, U>`]
//! * [`map`] transforms [`EitherOrBoth<T, T>`] to [`EitherOrBoth<U, U>`]
//!
//! [`bimap`]: EitherOrBoth::bimap
//! [`map_left`]: EitherOrBoth::map_left
//! [`map_right`]: EitherOrBoth::map_right
//! [`map`]: EitherOrBoth::map
//!
//! These methods transform [`EitherOrBoth<L, R>`] to a value of a possibly different type `T`:
//!
//! * [`map_left_or`] applies the provided function to the left value or returns the provided
//!   default value if there is only a right value.
//! * [`map_left_or_else`] applies the provided function to the left value or returns the result of
//!   evaluating the provided fallback function if there is only a right value.
//! * [`map_left_or_default`] applies the provided function to the left value or returns the default
//!   of the target value if there is only a right value.
//!
//! The [`map_right_or`], [`map_right_or_else`] and [`map_right_or_default`] methods are the
//! equivalent methods applying a provided function to the right value.
//!
//! [`map_left_or`]: EitherOrBoth::map_left_or
//! [`map_left_or_default`]: EitherOrBoth::map_left_or_default
//! [`map_left_or_else`]: EitherOrBoth::map_left_or_else
//! [`map_right_or`]: EitherOrBoth::map_right_or
//! [`map_right_or_default`]: EitherOrBoth::map_right_or_default
//! [`map_right_or_else`]: EitherOrBoth::map_right_or_else
//!
//! Swapping an [`EitherOrBoth`] or [`Either`] of an inner value to the inner value of an
//! [`EitherOrBoth`] or [`Either`] is possible with [`transpose`] if the inner values are a
//! [`Result`], [`Option`] or tuple respectively. The `Both` variant can be tricky because it can
//! contain an error (or `None`) and correct value (or `Some`) simultaneously. As per [convention]
//! mentioned above, the error (or `None`) value takes precedence.
//!
//! | From | To |
//! | --- | --- |
//! | [`EitherOrBoth<Result<L1, E1>, Result<L2, E2>>`] | [`Result<EitherOrBoth<L1, L2> EitherOrBoth<E1, E2>>`] |
//! | [`EitherOrBoth<Option<L>, Option<R>>`] | [`Option<EitherOrBoth<L, R>>`] |
//! | [`EitherOrBoth<(L1, R1), (L2, R2)>`][EitherOrBoth] | [`(EitherOrBoth<L1, L2>, EitherOrBoth<R1, R2>)`][EitherOrBoth] |
//!
//! The uniform type cases can be handled separately with [`transpose_ok`] and [`transpose_err`] for
//! [`Result`] and [`transpose_left`] and [`transpose_right`] for tuples.
//!
//! [`transpose`]: EitherOrBoth::transpose
//! [`transpose_left`]: EitherOrBoth::transpose_left
//! [`transpose_right`]: EitherOrBoth::transpose_right
//! [`transpose_ok`]: EitherOrBoth::transpose_ok
//! [`transpose_err`]: EitherOrBoth::transpose_err
//!
//! ## Consumers
//!
//! These methods apply a method to the contained values and the mutable environment consuming the
//! [`EitherOrBoth`]. The evaluation order is from left to right if `Both` values are present.
//! Evaluation from right to left is possible by first [`flipping`][`EitherOrBoth::flip`] the
//! values.
//!
//! * [`biapply`] applies the provided functions to the contained values taking mutable references
//!   to capture variables.
//! * [`biapply_with`] works similar but also takes a mutable accumulator which is available in both
//!   functions.
//! * [`apply_left`] applies the provided function to the contained left value
//! * [`apply_right`] applies the provided function to the contained right value
//! * [`apply`] is the uniform type method equivalent to [`biapply`]
//!
//! [`biapply`]: EitherOrBoth::biapply
//! [`biapply_with`]: EitherOrBoth::biapply_with
//! [`apply_left`]: EitherOrBoth::apply_left
//! [`apply_right`]: EitherOrBoth::apply_right
//! [`apply`]: EitherOrBoth::apply
//!
//! These methods are lighter versions, consuming the [`EitherOrBoth`] or [`Either`] then apply
//! functions to the contained values by reference and eventually returning the [`EitherOrBoth`] or
//! [`Either`] again.
//!
//! * [`biinspect`] applies the provided functions to the contained values starting with the left
//!   and continuing with the right value.
//! * [`inspect_left`] applies the provided function to the contained left value
//! * [`inspect_right`] applies the provided function to the contained right value
//! * [`inspect`] is the uniform type method equivalent to [`biinspect`]
//!
//! [`biinspect`]: EitherOrBoth::biinspect
//! [`inspect_left`]: EitherOrBoth::inspect_left
//! [`inspect_right`]: EitherOrBoth::inspect_right
//! [`inspect`]: EitherOrBoth::inspect
//!
//! ## Iterating
//!
//! Iterating over [`Either`] and [`EitherOrBoth`] is slightly different
//!
//! ### Iterating over [`EitherOrBoth`]
//!
//! [`EitherOrBoth`] can be iterated over directly if the types are uniform [`EitherOrBoth<T>`]. The
//! iterator produces a single value if `Left` or `Right` and two values if `Both` by iterating from
//! left to right. The uniform iterators over the type `T` come in in three types:
//!
//! * [`into_iter`][EitherOrBoth::into_iter] consumes the [`EitherOrBoth`]
//! * [`iter`][EitherOrBoth::iter] produces immutable references of type `&T` to the contained
//!   values
//! * [`iter_mut`][EitherOrBoth::iter_mut] produces mutable references of type `&mut T` to the
//!   contained values
//!
//! If the uniform inner types implement [`IntoIterator`] the [`EitherOrBoth`] can be iterated over
//! by chaining both iterators from left to right:
//!
//! * [`into_iter_chain`] consumes the [`EitherOrBoth`]
//! * [`iter_chain`] produces immutable references of type `&T` of the chained iterators
//! * [`iter_chain_mut`] produces mutable references of type `&mut T` of the chained iterators
//!
//! [`into_iter_chain`]: EitherOrBoth::into_iter_chain
//! [`iter_chain`]: EitherOrBoth::iter_chain
//! [`iter_chain_mut`]: EitherOrBoth::iter_chain_mut
//!
//! If the types are not uniform iterating over [`EitherOrBoth`] can be achieved if the inner types
//! implement [`IntoIterator`]. Instead of [`EitherOrBoth<Iterator<Item = L>, Iterator<Item =
//! R>>`][EitherOrBoth] the iterators are swapped [`Iterator<Item = EitherOrBoth<L, R>>`][Iterator]:
//!
//! * [`into_iter_swap`] consumes the [`EitherOrBoth`]
//! * [`iter_swap`] produces immutable references of [`EitherOrBoth<&L, &R>`]
//! * [`iter_swap_mut`] produces mutable references of [`EitherOrBoth<&mut L, &mut R>`]
//!
//! [`into_iter_swap`]: EitherOrBoth::into_iter_swap
//! [`iter_swap`]: EitherOrBoth::iter_swap
//! [`iter_swap_mut`]: EitherOrBoth::iter_swap_mut
//!
//! ### Iterating over [`Either`]
//!
//! Like [`EitherOrBoth`], an [`Either`] can be iterated over directly if the types are uniform
//! [`Either<T>`]. The iterator produces a single value. The uniform iterators over the type `T`
//! come in in three types:
//!
//! * [`into_iter`][Either::into_iter] consumes the [`Either`]
//! * [`iter`][Either::iter] produces an immutable reference of type `&T` to the contained value
//! * [`iter_mut`][Either::iter_mut] produces an mutable reference of type `&mut T` to the contained
//!   value
//!
//! If the uniform inner types implement [`IntoIterator`], the [`Either`] can be iterator over with:
//!
//! * [`into_iter_inner`][Either::into_iter_inner] consumes the [`Either`]
//! * [`iter_inner`][Either::iter_inner] produces immutable references of type `&T` to the inner
//!   iterator values
//! * [`iter_inner_mut`][Either::iter_inner_mut] produces mutable references of type `&mut T` to the
//!   inner iterator values
//!
//! If the [`Either`] types are non-uniform, the swap iterator for [`Either`] work exactly the same
//! as the [`EitherOrBoth`] swap iterators:
//!
//! * [`into_iter_swap`][Either::into_iter_swap] consumes the [`Either`]
//! * [`iter_swap`][Either::iter_swap] produces immutable references of [`Either<&L, &R>`]
//! * [`iter_swap_mut`][Either::iter_swap_mut] produces mutable references of [`Either<&mut L, &mut
//!   R>`]
//!
//! ## Collecting into [`EitherOrBoth`]
//!
//! [`EitherOrBoth`] implements the [`FromIterator`] trait which allows an iterator over [`Either`]
//! or [`EitherOrBoth`] to be collected into an [`EitherOrBoth`] of two collections, the left
//! collection containing the left values and the right collection containing the right values.
//!
//! ```ignore
//! use either_or_both::{Either, EitherOrBoth};
//!
//! let vector: Vec<Either<i32>> = vec![Either::Left(2), Either::Left(1)];
//! let either_or_both: EitherOrBoth<Vec<i32>> = vector.into_iter().collect();
//!
//! assert_eq!(either_or_both, EitherOrBoth::Left(vec![2, 1]));
//!
//! let vector: Vec<Either<i32>> = vec![Either::Left(2), Either::Right(1)];
//! let either_or_both: EitherOrBoth<Vec<i32>> = vector.into_iter().collect();
//!
//! assert_eq!(either_or_both, EitherOrBoth::Both(vec![2], vec![1]));
//! ```
//!
//! ## Modifying in-place
//!
//! Both enums support replacing the left or right values:
//!
//! * [`replace_left`] as the name suggests replaces the left value returning the old value if
//!   successful
//! * [`replace_right`] replaces the right value returning the old value if successful
//!
//! [`replace_any`] is special to [`EitherOrBoth`] and replaces all values returning the old
//! [`EitherOrBoth`].
//!
//! [`replace_left`]: EitherOrBoth::replace_left
//! [`replace_right`]: EitherOrBoth::replace_right
//! [`replace_any`]: EitherOrBoth::replace_any
//!
//! Insertion is supported for [`EitherOrBoth`] and these methods return a mutable reference to the
//! contained value:
//!
//! * [`insert_left`]: inserts a new left value and if `Right` changes to a `Both` value
//! * [`insert_right`]: inserts a new right value and if `Left` changes to a `Both` value
//! * [`left_or_insert`]: returns a mutable reference to the left value or if `Right`, inserts a
//!   left value converting `self` to a `Both` value
//! * [`left_or_insert_with`]: returns a mutable reference to the left value or if `Right`, inserts
//!   a lazily computed left value converting `self` to a `Both` value
//! * [`right_or_insert`]: returns a mutable reference to the right value or if `Left`, inserts a
//!   right value converting `self` to a `Both` value
//! * [`right_or_insert_with`]: returns a mutable reference to the right value or if `Left`, inserts
//!   a lazily computed right value converting `self` to a `Both` value
//!
//! [`insert_left`]: EitherOrBoth::insert_left
//! [`insert_right`]: EitherOrBoth::insert_right
//! [`left_or_insert`]: EitherOrBoth::left_or_insert
//! [`left_or_insert_with`]: EitherOrBoth::left_or_insert_with
//! [`right_or_insert`]: EitherOrBoth::right_or_insert
//! [`right_or_insert_with`]: EitherOrBoth::right_or_insert_with

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "either")]
mod either;
mod either_or_both;
mod error;

#[cfg(feature = "either")]
pub use either::{iter as iter_either, Either};
pub use error::TryFromOptionsError;

pub use crate::either_or_both::{iter, EitherOrBoth};

#[cold]
#[track_caller]
fn unwrap_failed(msg: &str) -> ! {
    panic!("{msg}");
}
