//! The `Either` enum

/// Either left or right can be present
#[derive(Debug, Clone)]
pub enum Either<L, R = L> {
    /// The left value
    Left(L),
    /// The right value
    Right(R),
}
