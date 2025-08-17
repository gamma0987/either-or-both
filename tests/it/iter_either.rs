//! Tests for the iterators of `Either`

use either_or_both::Either;
use rstest::rstest;

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn into_iter_either(#[case] either: Either<Vec<i32>, Vec<i32>>) {
    let items = either.into_iter().collect::<Vec<i32>>();
    assert_eq!(items, vec![1, 2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn into_iter_either_in_for_loop(#[case] either: Either<Vec<i32>, Vec<i32>>) {
    let mut items = vec![];
    for i in either {
        items.push(i);
    }

    assert_eq!(items, vec![1, 2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn iter_either(#[case] either: Either<Vec<i32>, Vec<i32>>) {
    let items = either.iter().collect::<Vec<&i32>>();
    assert_eq!(items, vec![&1, &2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn iter_either_in_for_loop(#[case] either: Either<Vec<i32>, Vec<i32>>) {
    let mut items = vec![];
    for i in &either {
        items.push(i);
    }

    assert_eq!(items, vec![&1, &2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn iter_either_mut(#[case] mut either: Either<Vec<i32>, Vec<i32>>) {
    let items = either.iter_mut().collect::<Vec<&mut i32>>();
    assert_eq!(items, vec![&mut 1, &mut 2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn iter_either_mut_in_for_loop(#[case] mut either: Either<Vec<i32>, Vec<i32>>) {
    let mut items = vec![];
    for i in &mut either {
        items.push(i);
    }

    assert_eq!(items, vec![&mut 1, &mut 2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn into_iter_swap(#[case] either: Either<Vec<i32>, Vec<i32>>, #[case] expected: Vec<Either<i32>>) {
    let items = either.into_iter_swap().collect::<Vec<Either<i32, i32>>>();
    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn into_iter_swap_in_for_loop(
    #[case] either: Either<Vec<i32>, Vec<i32>>,
    #[case] expected: Vec<Either<i32>>,
) {
    let mut items = vec![];
    for item in either.into_iter_swap() {
        items.push(item);
    }

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(&1), Either::Left(&2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(&1), Either::Right(&2)])]
fn iter_swap(#[case] either: Either<Vec<i32>, Vec<i32>>, #[case] expected: Vec<Either<&i32>>) {
    let items = either.iter_swap().collect::<Vec<Either<&i32, &i32>>>();
    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(&1), Either::Left(&2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(&1), Either::Right(&2)])]
fn iter_swap_in_for_loop(
    #[case] either: Either<Vec<i32>, Vec<i32>>,
    #[case] expected: Vec<Either<&i32>>,
) {
    let mut items = vec![];
    for item in either.iter_swap() {
        items.push(item);
    }

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn iter_swap_mut(
    #[case] mut either: Either<Vec<i32>, Vec<i32>>,
    #[case] mut expected: Vec<Either<i32>>,
) {
    let expected = expected
        .iter_mut()
        .map(Either::as_mut)
        .collect::<Vec<Either<&mut i32, &mut i32>>>();

    let items = either
        .iter_swap_mut()
        .collect::<Vec<Either<&mut i32, &mut i32>>>();

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn iter_swap_mut_in_for_loop(
    #[case] mut either: Either<Vec<i32>, Vec<i32>>,
    #[case] mut expected: Vec<Either<i32>>,
) {
    let expected = expected
        .iter_mut()
        .map(Either::as_mut)
        .collect::<Vec<Either<&mut i32, &mut i32>>>();

    let mut items = vec![];
    for item in either.iter_swap_mut() {
        items.push(item);
    }

    assert_eq!(items, expected);
}
