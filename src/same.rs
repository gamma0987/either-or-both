//! `EitherOrBoth` with same type

/// Either left or right can be present
pub enum Either<T> {
    /// The left value
    Left(T),
    /// The right value
    Right(T),
}

/// Either left or right or both can be present
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum EitherOrBoth<T> {
    /// Both values are present
    Both(T, T),
    /// The left value
    Left(T),
    /// The right value
    Right(T),
}
