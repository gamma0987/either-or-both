//! The crate's error

use core::fmt::Display;

/// TODO: DOCS
#[derive(Debug)]
pub enum Error {
    /// TODO: DOCS
    TryFromOptions,
}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::TryFromOptions => {
                write!(f, "Either the left, right or both values must be present")
            }
        }
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}
