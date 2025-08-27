//! Tests for trait implementations of `EitherOrBoth`

#[cfg(feature = "either")]
use either_or_both::Either;
use either_or_both::EitherOrBoth::{self, *};
use either_or_both::TryFromOptionsError;
use rstest::rstest;

#[cfg(feature = "std")]
#[rstest]
#[case::both_empty(vec![], Both(vec![], vec![]))]
#[case::both_and_both(vec![Both(1, 'c'), Both(2, 'm')], Both(vec![1, 2], vec!['c', 'm']))]
#[case::both_and_left(vec![Both(1, 'c'), Left(2)], Both(vec![1, 2], vec!['c']))]
#[case::both_and_right(vec![Both(1, 'c'), Right('m')], Both(vec![1], vec!['c', 'm']))]
#[case::left(vec![Left(1)], Left(vec![1]))]
#[case::right(vec![Right('c')], Right(vec!['c']))]
fn from_iter(
    #[case] either_or_both: Vec<EitherOrBoth<u8, char>>,
    #[case] expected: EitherOrBoth<Vec<u8>, Vec<char>>,
) {
    assert_eq!(
        either_or_both
            .into_iter()
            .collect::<EitherOrBoth<Vec<u8>, Vec<char>>>(),
        expected
    );
}

#[cfg(all(feature = "std", feature = "either"))]
#[rstest]
#[case::left(vec![Either::Left(1)], Left(vec![1]))]
#[case::right(vec![Either::Right('c')], Right(vec!['c']))]
#[case::left_and_right(vec![Either::Left(1), Either::Right('c')], Both(vec![1], vec!['c']))]
fn from_iter_either(
    #[case] either: Vec<Either<u8, char>>,
    #[case] expected: EitherOrBoth<Vec<u8>, Vec<char>>,
) {
    assert_eq!(
        either
            .into_iter()
            .collect::<EitherOrBoth<Vec<u8>, Vec<char>>>(),
        expected
    );
}

#[cfg(feature = "either")]
#[rstest]
#[case::left(Either::Left(1), Left(1))]
#[case::right(Either::Right('c'), Right('c'))]
fn from_either(#[case] either: Either<u8, char>, #[case] expected: EitherOrBoth<u8, char>) {
    assert_eq!(EitherOrBoth::from(either), expected);
}

#[test]
fn from_tuple() {
    assert_eq!(EitherOrBoth::from((1, 'c')), Both(1, 'c'));
}

#[rstest]
#[case::some((Some(1), 'c'), Both(1, 'c'))]
#[case::none((None, 'c'), Right('c'))]
fn from_tuple_option_left(
    #[case] either: (Option<u8>, char),
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(EitherOrBoth::from(either), expected);
}

#[rstest]
#[case::some((1, Some('c')), Both(1, 'c'))]
#[case::none((1, None), Left(1))]
fn from_tuple_option_right(
    #[case] either: (u8, Option<char>),
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(EitherOrBoth::from(either), expected);
}

#[rstest]
#[case::both_some((Some(1), Some('c')), Both(1, 'c'))]
#[case::left_some((Some(1), None), Left(1))]
#[case::right_some((None, Some('c')), Right('c'))]
fn try_from_tuple_when_ok(
    #[case] either: (Option<u8>, Option<char>),
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(EitherOrBoth::try_from(either).unwrap(), expected);
}

#[test]
fn try_from_tuple_when_error() {
    assert_eq!(
        EitherOrBoth::<u8, char>::try_from((None, None)).unwrap_err(),
        TryFromOptionsError
    );
}
