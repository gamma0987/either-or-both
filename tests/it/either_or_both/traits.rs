//! Tests for trait implementations of `EitherOrBoth`

#![allow(clippy::type_complexity)]

use core::cmp::Ordering;
#[cfg(feature = "std")]
use std::collections::{HashMap, HashSet};

#[cfg(feature = "either")]
use either_or_both::Either;
use either_or_both::EitherOrBoth::{self, *};
use either_or_both::TryFromOptionsError;
#[cfg(all(feature = "indexmap", feature = "std"))]
use indexmap::{IndexMap, IndexSet};
use rstest::rstest;

#[cfg(feature = "std")]
#[derive(Debug)]
struct SuperError;
#[cfg(feature = "std")]
impl std::error::Error for SuperError {}
#[cfg(feature = "std")]
impl core::fmt::Display for SuperError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("super error")
    }
}

#[cfg(feature = "std")]
#[derive(Debug)]
struct SomeError(Option<SuperError>);
#[cfg(feature = "std")]
impl core::fmt::Display for SomeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("some error")
    }
}
#[cfg(feature = "std")]
impl std::error::Error for SomeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.as_ref().map(|e| e as &dyn std::error::Error)
    }
}

#[cfg(feature = "std")]
#[rstest]
#[case::both_empty(vec![], Both(vec![], vec![]))]
#[case::both_and_both(vec![Both(1, 'c'), Both(2, 'm')], Both(vec![1, 2], vec!['c', 'm']))]
#[case::both_and_left(vec![Both(1, 'c'), Left(2)], Both(vec![1, 2], vec!['c']))]
#[case::both_and_right(vec![Both(1, 'c'), Right('m')], Both(vec![1], vec!['c', 'm']))]
#[case::left(vec![Left(1)], Left(vec![1]))]
#[case::right(vec![Right('c')], Right(vec!['c']))]
fn from_iter_for_vec(
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

#[cfg(feature = "std")]
#[rstest]
#[case::both_empty(vec![], Both(HashMap::new(), HashMap::new()))]
#[case::both_and_both(
    vec![ Both(("both_l", 1), ("both_r", 'c')), Both(("left", 2), ("right", 'm'))],
    Both(
        HashMap::from([("both_l", 1), ("left", 2)]),
        HashMap::from([("both_r", 'c'), ("right", 'm')])
    )
)]
#[case::both_and_left(
    vec![ Both(("both_l", 1), ("both_r", 'c')), Left(("left", 2))],
    Both(
        HashMap::from([("both_l", 1), ("left", 2)]),
        HashMap::from([("both_r", 'c')])
    )
)]
#[case::both_and_right(
    vec![ Both(("both_l", 1), ("both_r", 'c')), Right(("right", 'm'))],
    Both(
        HashMap::from([("both_l", 1)]),
        HashMap::from([("both_r", 'c'), ("right", 'm')])
    )
)]
#[case::left( vec![Left(("left", 2))], Left(HashMap::from([("left", 2)])))]
#[case::right( vec![Right(("right", 'c'))], Right(HashMap::from([("right", 'c')])))]
fn from_iter_for_hashmap(
    #[case] either_or_both: Vec<EitherOrBoth<(&str, u8), (&str, char)>>,
    #[case] expected: EitherOrBoth<HashMap<&str, u8>, HashMap<&str, char>>,
) {
    assert_eq!(
        either_or_both
            .into_iter()
            .collect::<EitherOrBoth<HashMap<_, _>, HashMap<_, _>>>(),
        expected
    );
}

#[cfg(feature = "std")]
#[rstest]
#[case::both_empty(vec![], Both(HashSet::new(), HashSet::new()))]
#[case::both_and_both(
    vec![ Both(1, 'c'), Both(2, 'm')],
    Both(
        HashSet::from([1, 2]),
        HashSet::from(['c', 'm'])
    )
)]
#[case::both_and_left(
    vec![ Both(1, 'c'), Left(2)],
    Both(
        HashSet::from([1, 2]),
        HashSet::from(['c'])
    )
)]
#[case::both_and_right(
    vec![ Both(1, 'c'), Right('m')],
    Both(
        HashSet::from([1]),
        HashSet::from(['c', 'm'])
    )
)]
#[case::left(vec![Left(2)], Left(HashSet::from([2])))]
#[case::right(vec![Right('c')], Right(HashSet::from(['c'])))]
fn from_iter_for_hashset(
    #[case] either_or_both: Vec<EitherOrBoth<u8, char>>,
    #[case] expected: EitherOrBoth<HashSet<u8>, HashSet<char>>,
) {
    assert_eq!(
        either_or_both
            .into_iter()
            .collect::<EitherOrBoth<HashSet<_>, HashSet<_>>>(),
        expected
    );
}

#[cfg(all(feature = "std", feature = "indexmap"))]
#[rstest]
#[case::both_empty(vec![], Both(IndexMap::new(), IndexMap::new()))]
#[case::both_and_both(
    vec![ Both(("both_l", 1), ("both_r", 'c')), Both(("left", 2), ("right", 'm'))],
    Both(
        IndexMap::from([("both_l", 1), ("left", 2)]),
        IndexMap::from([("both_r", 'c'), ("right", 'm')])
    )
)]
#[case::both_and_left(
    vec![ Both(("both_l", 1), ("both_r", 'c')), Left(("left", 2))],
    Both(
        IndexMap::from([("both_l", 1), ("left", 2)]),
        IndexMap::from([("both_r", 'c')])
    )
)]
#[case::both_and_right(
    vec![ Both(("both_l", 1), ("both_r", 'c')), Right(("right", 'm'))],
    Both(
        IndexMap::from([("both_l", 1)]),
        IndexMap::from([("both_r", 'c'), ("right", 'm')])
    )
)]
#[case::left(vec![Left(("left", 2))], Left(IndexMap::from([("left", 2)])))]
#[case::right(vec![Right(("right", 'c'))], Right(IndexMap::from([("right", 'c')])))]
fn from_iter_for_indexmap(
    #[case] either_or_both: Vec<EitherOrBoth<(&str, u8), (&str, char)>>,
    #[case] expected: EitherOrBoth<IndexMap<&str, u8>, IndexMap<&str, char>>,
) {
    assert_eq!(
        either_or_both
            .into_iter()
            .collect::<EitherOrBoth<IndexMap<_, _>, IndexMap<_, _>>>(),
        expected
    );
}

#[cfg(all(feature = "std", feature = "indexmap"))]
#[rstest]
#[case::both_empty(vec![], Both(IndexSet::new(), IndexSet::new()))]
#[case::both_and_both(
    vec![ Both(1, 'c'), Both(2, 'm')],
    Both(
        IndexSet::from([1, 2]),
        IndexSet::from(['c', 'm'])
    )
)]
#[case::both_and_left(
    vec![ Both(1, 'c'), Left(2)],
    Both(
        IndexSet::from([1, 2]),
        IndexSet::from(['c'])
    )
)]
#[case::both_and_right(
    vec![ Both(1, 'c'), Right('m')],
    Both(
        IndexSet::from([1]),
        IndexSet::from(['c', 'm'])
    )
)]
#[case::left(vec![Left(2)], Left(IndexSet::from([2])))]
#[case::right(vec![Right('c')], Right(IndexSet::from(['c'])))]
fn from_iter_for_indexset(
    #[case] either_or_both: Vec<EitherOrBoth<u8, char>>,
    #[case] expected: EitherOrBoth<IndexSet<u8>, IndexSet<char>>,
) {
    assert_eq!(
        either_or_both
            .into_iter()
            .collect::<EitherOrBoth<IndexSet<_>, IndexSet<_>>>(),
        expected
    );
}

#[cfg(all(feature = "std", feature = "either"))]
#[rstest]
#[case::left(vec![Either::Left(1)], Left(vec![1]))]
#[case::right(vec![Either::Right('c')], Right(vec!['c']))]
#[case::left_and_right(vec![Either::Left(1), Either::Right('c')], Both(vec![1], vec!['c']))]
fn from_iter_either_vec(
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

#[cfg(all(feature = "std", feature = "either"))]
#[rstest]
#[case::left(vec![Either::Left(("left", 2))], Left(HashMap::from([("left", 2)])))]
#[case::right(vec![Either::Right(("right", 'c'))], Right(HashMap::from([("right", 'c')])))]
#[case::left_and_right(
    vec![Either::Left(("left", 2)), Either::Right(("right", 'c'))],
    Both(HashMap::from([("left", 2)]),HashMap::from([("right", 'c')]))
)]
fn from_iter_either_for_hashmap(
    #[case] either: Vec<Either<(&str, u8), (&str, char)>>,
    #[case] expected: EitherOrBoth<HashMap<&str, u8>, HashMap<&str, char>>,
) {
    assert_eq!(
        either
            .into_iter()
            .collect::<EitherOrBoth<HashMap<_, _>, HashMap<_, _>>>(),
        expected
    );
}

#[cfg(all(feature = "std", feature = "either"))]
#[rstest]
#[case::left(vec![Either::Left(2)], Left(HashSet::from([2])))]
#[case::right(vec![Either::Right('c')], Right(HashSet::from(['c'])))]
#[case::left_and_right(
    vec![Either::Left(2), Either::Right('c')],
    Both(HashSet::from([2]), HashSet::from(['c']))
)]
fn from_iter_either_for_hashset(
    #[case] either_or_both: Vec<Either<u8, char>>,
    #[case] expected: EitherOrBoth<HashSet<u8>, HashSet<char>>,
) {
    assert_eq!(
        either_or_both
            .into_iter()
            .collect::<EitherOrBoth<HashSet<_>, HashSet<_>>>(),
        expected
    );
}

#[cfg(all(feature = "std", feature = "indexmap", feature = "either"))]
#[rstest]
#[case::left(vec![Either::Left(("left", 2))], Left(IndexMap::from([("left", 2)])))]
#[case::right(vec![Either::Right(("right", 'c'))], Right(IndexMap::from([("right", 'c')])))]
#[case::left_and_right(
    vec![Either::Left(("left", 2)), Either::Right(("right", 'c'))],
    Both(IndexMap::from([("left", 2)]),IndexMap::from([("right", 'c')]))
)]
fn from_iter_either_for_indexmap(
    #[case] either: Vec<Either<(&str, u8), (&str, char)>>,
    #[case] expected: EitherOrBoth<IndexMap<&str, u8>, IndexMap<&str, char>>,
) {
    assert_eq!(
        either
            .into_iter()
            .collect::<EitherOrBoth<IndexMap<_, _>, IndexMap<_, _>>>(),
        expected
    );
}

#[cfg(all(feature = "std", feature = "indexmap", feature = "either"))]
#[rstest]
#[case::left(vec![Either::Left(2)], Left(IndexSet::from([2])))]
#[case::right(vec![Either::Right('c')], Right(IndexSet::from(['c'])))]
#[case::left_and_right(
    vec![Either::Left(2), Either::Right('c')],
    Both(IndexSet::from([2]), IndexSet::from(['c']))
)]
fn from_iter_either_for_indexset(
    #[case] either_or_both: Vec<Either<u8, char>>,
    #[case] expected: EitherOrBoth<IndexSet<u8>, IndexSet<char>>,
) {
    assert_eq!(
        either_or_both
            .into_iter()
            .collect::<EitherOrBoth<IndexSet<_>, IndexSet<_>>>(),
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

#[rstest]
#[case::left_when_equal(Left(1), Left(1), Ordering::Equal)]
#[case::left_when_less(Left(1), Left(2), Ordering::Less)]
#[case::left_when_greater(Left(2), Left(1), Ordering::Greater)]
#[case::left_and_both(Left(1), Both(1, 2), Ordering::Less)]
#[case::left_and_right(Left(1), Right(1), Ordering::Less)]
#[case::right_when_equal(Right(1), Right(1), Ordering::Equal)]
#[case::right_when_less(Right(1), Right(2), Ordering::Less)]
#[case::right_when_greater(Right(2), Right(1), Ordering::Greater)]
#[case::right_and_both(Right(1), Both(1, 2), Ordering::Greater)]
#[case::right_and_left(Right(1), Left(1), Ordering::Greater)]
#[case::both_when_equal(Both(1, 2), Both(1, 2), Ordering::Equal)]
#[case::both_when_left_is_less(Both(1, 2), Both(2, 2), Ordering::Less)]
#[case::both_when_right_is_less(Both(1, 1), Both(1, 2), Ordering::Less)]
#[case::both_and_left(Both(1, 2), Left(1), Ordering::Greater)]
#[case::both_and_right(Both(1, 2), Right(1), Ordering::Less)]
fn impl_ord(#[case] a: EitherOrBoth<u8>, #[case] b: EitherOrBoth<u8>, #[case] expected: Ordering) {
    assert_eq!(a.cmp(&b), expected);
}

#[test]
fn impl_partial_ord() {
    assert_eq!(
        EitherOrBoth::<u8>::Left(1u8).partial_cmp(&EitherOrBoth::Left(2)),
        Some(Ordering::Less)
    );
}

#[rstest]
#[case::left(Left(1), "1")]
#[case::right(Right(1), "1")]
#[case::both(Both(1, 2), "1\n2")]
fn impl_display(#[case] either_or_both: EitherOrBoth<u8>, #[case] expected: &str) {
    assert_eq!(either_or_both.to_string(), expected);
}

#[cfg(feature = "std")]
#[rstest]
#[case::left(Left(SomeError(Some(SuperError {  }))), Some(SuperError {}))]
#[case::right(Right(SomeError(Some(SuperError {  }))), Some(SuperError {}))]
#[case::both_when_left_is_some_right_is_none(
    Both(SomeError(Some(SuperError {  })), SomeError(None)),
    Some(SuperError {})
)]
#[case::both_when_left_is_none_right_is_some(
    Both(SomeError(None), SomeError(Some(SuperError {}))),
    None
)]
fn impl_error(
    #[case] either_or_both: EitherOrBoth<SomeError>,
    #[case] expected: Option<SuperError>,
) {
    use std::error::Error;

    match (either_or_both.source(), expected) {
        (Some(_), None) => panic!("Expected no source but found one"),
        (None, Some(_)) => panic!("Expected a source but found none"),
        (None, None) | (Some(_), Some(_)) => {}
    }
}

#[cfg(feature = "std")]
#[rstest]
#[case::left_no_source(Left(SomeError(None)), (None, None))]
#[case::left_with_source(Left(SomeError(Some(SuperError {}))), (Some(SuperError {}), None))]
#[case::right_no_source(Right(SomeError(None)), (None, None))]
#[case::right_with_source(Right(SomeError(Some(SuperError {}))), (None, Some(SuperError {})))]
#[case::both_no_sources(Both(SomeError(None), SomeError(None)), (None, None))]
#[case::both_left_source_only(
    Both(SomeError(Some(SuperError {})), SomeError(None)),
    (Some(SuperError {}), None)
)]
#[case::both_right_source_only(
    Both(SomeError(None), SomeError(Some(SuperError {}))),
    (None, Some(SuperError {}))
)]
#[case::both_both_sources(
    Both(SomeError(Some(SuperError {})), SomeError(Some(SuperError {}))),
    (Some(SuperError {}), Some(SuperError {}))
)]
fn error_sources(
    #[case] either_or_both: EitherOrBoth<SomeError>,
    #[case] expected: (Option<SuperError>, Option<SuperError>),
) {
    let (left, right) = either_or_both.error_sources();
    match (left, expected.0) {
        (Some(_), None) => panic!("expected no left source but found one"),
        (None, Some(_)) => panic!("expected a left source but found none"),
        (None, None) | (Some(_), Some(_)) => {}
    }
    match (right, expected.1) {
        (Some(_), None) => panic!("expected no right source but found one"),
        (None, Some(_)) => panic!("expected a right source but found none"),
        (None, None) | (Some(_), Some(_)) => {}
    }
}
