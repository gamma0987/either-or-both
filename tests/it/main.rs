//! The main tests module

#[cfg(feature = "either")]
mod either;
mod either_or_both;
#[cfg(feature = "std")]
mod either_or_both_std;
#[cfg(all(feature = "either", feature = "std"))]
mod either_std;
#[cfg(feature = "either")]
mod iter_either;
