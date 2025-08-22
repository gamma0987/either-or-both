//! The crate's error

use core::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// TODO: DOCS
pub struct TryFromOptionsError;

impl Display for TryFromOptionsError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Either the left, right or both values must be present")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for TryFromOptionsError {}
