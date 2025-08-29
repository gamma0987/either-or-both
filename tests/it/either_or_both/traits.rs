//! Tests for trait implementations of `EitherOrBoth`

#![allow(clippy::type_complexity)]

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
