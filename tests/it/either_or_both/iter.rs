//! Test for iterators with the same type in `EitherOrBoth`

use core::ops::{Range, RangeFrom};

use either_or_both::EitherOrBoth::{self, *};
use rstest::rstest;

#[rstest]
#[case::both(Both(1, 2), [Some(1), Some(2), None])]
#[case::left(Left(1), [Some(1), None, None])]
#[case::right(Right(2), [Some(2), None, None])]
fn into_iter_next(#[case] either_or_both: EitherOrBoth<i32>, #[case] expected: [Option<i32>; 3]) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter();
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(1, 2), vec![1, 2])]
#[case::left(Left(1), vec![1])]
#[case::right(Right(2), vec![2])]
fn into_iter_for_loop(#[case] either_or_both: EitherOrBoth<i32>, #[case] expected: Vec<i32>) {
    let mut actual = vec![];
    for i in either_or_both {
        actual.push(i);
    }

    assert_eq!(actual, expected);
}

#[rstest]
#[case::both(Both(1, 2), [Some(2), Some(1), None])]
#[case::left(Left(1), [Some(1), None, None])]
#[case::right(Right(2), [Some(2), None, None])]
fn into_iter_next_back(
    #[case] either_or_both: EitherOrBoth<i32>,
    #[case] expected: [Option<i32>; 3],
) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter();
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(1, 2), (2, Some(2)))]
#[case::left(Left(1), (1, Some(1)))]
#[case::right(Right(2), (1, Some(1)))]
fn into_iter_size_hint(
    #[case] either_or_both: EitherOrBoth<i32>,
    #[case] expected: (usize, Option<usize>),
) {
    let iter = either_or_both.into_iter();
    assert_eq!(iter.size_hint(), expected);
}

#[rstest]
#[case::both(Both(1, 2), [Some(&1), Some(&2), None])]
#[case::left(Left(1), [Some(&1), None, None])]
#[case::right(Right(2), [Some(&2), None, None])]
fn iter_next(#[case] either_or_both: EitherOrBoth<i32>, #[case] expected: [Option<&i32>; 3]) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.iter();
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(1, 2), vec![&1, &2])]
#[case::left(Left(1), vec![&1])]
#[case::right(Right(2), vec![&2])]
fn iter_for_loop(#[case] either_or_both: EitherOrBoth<i32>, #[case] expected: Vec<&i32>) {
    let mut actual = vec![];
    for i in &either_or_both {
        actual.push(i);
    }

    assert_eq!(actual, expected);
}

#[rstest]
#[case::both(Both(1, 2), [Some(&2), Some(&1), None])]
#[case::left(Left(1), [Some(&1), None, None])]
#[case::right(Right(2), [Some(&2), None, None])]
fn iter_next_back(#[case] either_or_both: EitherOrBoth<i32>, #[case] expected: [Option<&i32>; 3]) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.iter();
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(1, 2), (2, Some(2)))]
#[case::left(Left(1), (1, Some(1)))]
#[case::right(Right(2), (1, Some(1)))]
fn iter_size_hint(
    #[case] either_or_both: EitherOrBoth<i32>,
    #[case] expected: (usize, Option<usize>),
) {
    let iter = either_or_both.iter();
    assert_eq!(iter.size_hint(), expected);
}

#[rstest]
#[case::both(Both(1, 2), [Some(1), Some(2), None])]
#[case::left(Left(1), [Some(1), None, None])]
#[case::right(Right(2), [Some(2), None, None])]
fn iter_mut_next(
    #[case] mut either_or_both: EitherOrBoth<i32>,
    #[case] mut expected: [Option<i32>; 3],
) {
    let mut expected_iter = expected.iter_mut().map(Option::as_mut);
    let mut iter = either_or_both.iter_mut();
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(1, 2), vec![1, 2])]
#[case::left(Left(1), vec![1])]
#[case::right(Right(2), vec![2])]
fn iter_mut_for_loop(#[case] mut either_or_both: EitherOrBoth<i32>, #[case] mut expected: Vec<i32>) {
    let expected = expected.iter_mut().collect::<Vec<&mut i32>>();

    let mut actual = vec![];
    for i in &mut either_or_both {
        actual.push(i);
    }

    assert_eq!(actual, expected);
}

#[rstest]
#[case::both(Both(1, 2), [Some(2), Some(1), None])]
#[case::left(Left(1), [Some(1), None, None])]
#[case::right(Right(2), [Some(2), None, None])]
fn iter_mut_next_back(
    #[case] mut either_or_both: EitherOrBoth<i32>,
    #[case] mut expected: [Option<i32>; 3],
) {
    let mut expected_iter = expected.iter_mut().map(Option::as_mut);
    let mut iter = either_or_both.iter_mut();
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(1, 2), (2, Some(2)))]
#[case::left(Left(1), (1, Some(1)))]
#[case::right(Right(2), (1, Some(1)))]
fn iter_mut_size_hint(
    #[case] mut either_or_both: EitherOrBoth<i32>,
    #[case] expected: (usize, Option<usize>),
) {
    let iter = either_or_both.iter_mut();
    assert_eq!(iter.size_hint(), expected);
}

#[rstest]
#[case::both(Both(vec![1], vec![2]), [Some(1), Some(2), None])]
#[case::left(Left(vec![1]), [Some(1), None, None])]
#[case::right(Right(vec![2]), [Some(2), None, None])]
fn into_iter_chain_next(
    #[case] either_or_both: EitherOrBoth<Vec<i32>>,
    #[case] expected: [Option<i32>; 3],
) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter_chain();
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(vec![1], vec![2]), [Some(2), Some(1), None])]
#[case::left(Left(vec![1]), [Some(1), None, None])]
#[case::right(Right(vec![2]), [Some(2), None, None])]
fn into_iter_chain_next_back(
    #[case] either_or_both: EitherOrBoth<Vec<i32>>,
    #[case] expected: [Option<i32>; 3],
) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter_chain();
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(vec![1], vec![2]), [Some(&1), Some(&2), None])]
#[case::left(Left(vec![1]), [Some(&1), None, None])]
#[case::right(Right(vec![2]), [Some(&2), None, None])]
fn iter_chain_next(
    #[case] either_or_both: EitherOrBoth<Vec<i32>>,
    #[case] expected: [Option<&i32>; 3],
) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.iter_chain();
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(vec![1], vec![2]), [Some(1), Some(2), None])]
#[case::left(Left(vec![1]), [Some(1), None, None])]
#[case::right(Right(vec![2]), [Some(2), None, None])]
fn iter_chain_mut_next(
    #[case] mut either_or_both: EitherOrBoth<Vec<i32>>,
    #[case] mut expected: [Option<i32>; 3],
) {
    let mut expected_iter = expected.iter_mut().map(Option::as_mut);
    let mut iter = either_or_both.iter_chain_mut();
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(vec![1], vec![2]), [(2, Some(2)), (1, Some(1)), (0, Some(0))])]
#[case::left(Left(vec![1]), [(1, Some(1)), (0, Some(0)), (0, Some(0))])]
#[case::right(Right(vec![2]), [(1, Some(1)), (0, Some(0)), (0, Some(0))])]
fn iter_chain_size_hint(
    #[case] either_or_both: EitherOrBoth<Vec<i32>>,
    #[case] expected: [(usize, Option<usize>); 3],
) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter_chain();
    assert_eq!(iter.size_hint(), expected_iter.next().unwrap());
    iter.next();
    assert_eq!(iter.size_hint(), expected_iter.next().unwrap());
    iter.next();
    assert_eq!(iter.size_hint(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both(Both(vec![1], vec!['c']), [Some(Both(1, 'c')), None])]
#[case::both_left_empty(Both(vec![], vec!['c']), [Some(Right('c')), None])]
#[case::both_right_empty(Both(vec![1], vec![]), [Some(Left(1)), None])]
#[case::both_empty(Both(vec![], vec![]), [None, None])]
#[case::left(Left(vec![1]), [Some(Left(1)), None])]
#[case::right(Right(vec!['c']), [Some(Right('c')), None])]
fn into_iter_swap_next(
    #[case] either_or_both: EitherOrBoth<Vec<u8>, Vec<char>>,
    #[case] expected: [Option<EitherOrBoth<u8, char>>; 2],
) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter_swap();
    assert_eq!(iter.next(), expected_iter.next().unwrap());
    assert_eq!(iter.next(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both_left_more_than_right(
    Both(vec![1, 2], vec!['c']), [Some(Both(2, 'c')), Some(Left(1)), None]
)]
#[case::both_right_more_than_left(
    Both(vec![1], vec!['c', 'm']), [Some(Both(1, 'm')), Some(Right('c')), None]
)]
#[case::left(Left(vec![1, 2]), [Some(Left(2)), Some(Left(1)), None])]
#[case::right(Right(vec!['c', 'm']), [Some(Right('m')), Some(Right('c')), None])]
fn into_iter_swap_next_back(
    #[case] either_or_both: EitherOrBoth<Vec<u8>, Vec<char>>,
    #[case] expected: [Option<EitherOrBoth<u8, char>>; 3],
) {
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter_swap();
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
    assert_eq!(iter.next_back(), expected_iter.next().unwrap());
}

#[rstest]
#[case::both_left_is_infinite(
    Both(0.., -1..0), [(usize::MAX, None), (usize::MAX, None), (usize::MAX, None)]
)]
#[case::both_right_is_infinite(
    Both(0..2, 0..), [(usize::MAX, None), (usize::MAX, None), (usize::MAX, None)]
)]
#[case::both_are_infinite(
    Both(0.., 0..), [(usize::MAX, None), (usize::MAX, None), (usize::MAX, None)]
)]
#[case::both_left_more_than_right(
    Both(1..=2, -1..0), [(2, Some(2)), (1, Some(1)), (0, Some(0))]
)]
#[case::both_right_more_than_left(
    Both(1..2, -2..0), [(2, Some(2)), (1, Some(1)), (0, Some(0))]
)]
#[case::left(
    EitherOrBoth::<Range<u8>, Range<i32>>::Left(0..2),
    [(2, Some(2)), (1, Some(1)), (0, Some(0))]
)]
#[case::left_is_infinite(
    EitherOrBoth::<RangeFrom<u8>, Range<i32>>::Left(0..),
    [(usize::MAX, None), (usize::MAX, None), (usize::MAX, None)]
)]
#[case::right(
    EitherOrBoth::<Range<u8>, Range<i32>>::Right(0..2),
    [(2, Some(2)), (1, Some(1)), (0, Some(0))]
)]
#[case::right_is_infinite(
    EitherOrBoth::<Range<u8>, RangeFrom<i32>>::Right(0..),
    [(usize::MAX, None), (usize::MAX, None), (usize::MAX, None)]
)]
fn into_iter_swap_size_hint<L, R>(
    #[case] either_or_both: EitherOrBoth<L, R>,
    #[case] expected: [(usize, Option<usize>); 3],
) where
    L: IntoIterator<Item = u8>,
    R: IntoIterator<Item = i32>,
{
    let mut expected_iter = expected.into_iter();
    let mut iter = either_or_both.into_iter_swap();
    assert_eq!(iter.size_hint(), expected_iter.next().unwrap());
    iter.next();
}

#[rstest]
#[case::both(Both(vec![1], vec!['c']), vec![Both(&1, &'c')])]
#[case::left(Left(vec![1]), vec![Left(&1)])]
#[case::right(Right(vec!['c']), vec![Right(&'c')])]
fn iter_swap(
    #[case] either_or_both: EitherOrBoth<Vec<u8>, Vec<char>>,
    #[case] expected: Vec<EitherOrBoth<&u8, &char>>,
) {
    let actual = either_or_both
        .iter_swap()
        .collect::<Vec<EitherOrBoth<&u8, &char>>>();
    assert_eq!(actual, expected);
}

#[rstest]
#[case::both(Both(vec![1], vec!['c']), vec![Both(1, 'c')])]
#[case::left(Left(vec![1]), vec![Left(1)])]
#[case::right(Right(vec!['c']), vec![Right('c')])]
fn iter_swap_mut(
    #[case] mut either_or_both: EitherOrBoth<Vec<u8>, Vec<char>>,
    #[case] mut expected: Vec<EitherOrBoth<u8, char>>,
) {
    let expected = expected
        .iter_mut()
        .map(|e| e.as_mut())
        .collect::<Vec<EitherOrBoth<&mut u8, &mut char>>>();

    let actual = either_or_both
        .iter_swap_mut()
        .collect::<Vec<EitherOrBoth<&mut u8, &mut char>>>();
    assert_eq!(actual, expected);
}
