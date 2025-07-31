//! `EitherOrBoth` with different types

/// Either left or right can be present
pub enum Either<L, R> {
    /// The left value
    Left(L),
    /// The right value
    Right(R),
}

/// Either left or right or both can be present
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum EitherOrBoth<L, R> {
    /// Both values are present
    Both(L, R),
    /// The left value
    Left(L),
    /// The right value
    Right(R),
}
