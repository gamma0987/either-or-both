//! The crate's error

use core::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// The error if converting from a 2-tuple of `Options` to [`crate::EitherOrBoth`] fails
pub struct TryFromOptionsError;

impl Display for TryFromOptionsError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Either the left, right or both values must be present")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for TryFromOptionsError {}
