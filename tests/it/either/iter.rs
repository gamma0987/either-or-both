//! Tests for the iterators of `Either`

use either_or_both::Either;
use rstest::rstest;

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn into_iter_either(#[case] either: Either<u8>) {
    let mut iter = either.into_iter();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), None);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn into_iter_either_backwards(#[case] either: Either<u8>) {
    let mut iter = either.into_iter();
    assert_eq!(iter.next_back(), Some(1));
    assert_eq!(iter.next_back(), None);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn into_iter_either_in_for_loop(#[case] either: Either<u8>) {
    let mut items = vec![];
    for item in either {
        items.push(item);
    }
    assert_eq!(items, vec![1]);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn into_iter_either_size_hint(#[case] either: Either<u8>) {
    let mut iter = either.into_iter();
    assert_eq!(iter.size_hint(), (1, Some(1)));
    iter.next();
    assert_eq!(iter.size_hint(), (0, Some(0)));
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_either(#[case] either: Either<u8>) {
    let mut iter = either.iter();
    assert_eq!(iter.next(), Some(&1));
    assert_eq!(iter.next(), None);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_either_backwards(#[case] either: Either<u8>) {
    let mut iter = either.iter();
    assert_eq!(iter.next_back(), Some(&1));
    assert_eq!(iter.next_back(), None);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_either_size_hint(#[case] either: Either<u8>) {
    let mut iter = either.iter();
    assert_eq!(iter.size_hint(), (1, Some(1)));
    iter.next();
    assert_eq!(iter.size_hint(), (0, Some(0)));
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_either_in_for_loop(#[case] either: Either<u8>) {
    let mut items = vec![];
    for item in &either {
        items.push(item);
    }
    assert_eq!(items, vec![&1]);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_mut_either(#[case] mut either: Either<u8>) {
    let mut iter = either.iter_mut();
    assert_eq!(iter.next(), Some(&mut 1));
    assert_eq!(iter.next(), None);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_mut_either_backwards(#[case] mut either: Either<u8>) {
    let mut iter = either.iter_mut();
    assert_eq!(iter.next_back(), Some(&mut 1));
    assert_eq!(iter.next_back(), None);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_mut_either_in_for_loop(#[case] mut either: Either<u8>) {
    let mut items = vec![];
    for item in &mut either {
        items.push(item);
    }
    assert_eq!(items, vec![&mut 1]);
}

#[rstest]
#[case::left(Either::Left(1))]
#[case::right(Either::Right(1))]
fn iter_mut_either_size_hint(#[case] mut either: Either<u8>) {
    let mut iter = either.iter_mut();
    assert_eq!(iter.size_hint(), (1, Some(1)));
    iter.next();
    assert_eq!(iter.size_hint(), (0, Some(0)));
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn into_iter_inner(#[case] either: Either<Vec<i32>>) {
    let items = either.into_iter_inner().collect::<Vec<i32>>();
    assert_eq!(items, vec![1, 2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn iter_inner(#[case] either: Either<Vec<i32>>) {
    let items = either.iter_inner().collect::<Vec<&i32>>();
    assert_eq!(items, vec![&1, &2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), (2, Some(2)))]
#[case::zero(Either::Left(vec![]), (0, Some(0)))]
#[case::right(Either::Right(vec![1, 2]), (2, Some(2)))]
fn iter_inner_size_hint(#[case] either: Either<Vec<i32>>, #[case] expected: (usize, Option<usize>)) {
    let actual = either.into_iter_inner().size_hint();
    assert_eq!(actual, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn iter_inner_backwards(#[case] either: Either<Vec<i32>>) {
    let items = either.iter_inner().rfold(vec![], |mut acc, f| {
        acc.push(*f);
        acc
    });
    assert_eq!(items, vec![2, 1]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]))]
#[case::right(Either::Right(vec![1, 2]))]
fn iter_inner_mut(#[case] mut either: Either<Vec<i32>>) {
    let items = either.iter_inner_mut().collect::<Vec<&mut i32>>();
    assert_eq!(items, vec![&mut 1, &mut 2]);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn into_iter_swap(#[case] either: Either<Vec<i32>>, #[case] expected: Vec<Either<i32>>) {
    let items = either.into_iter_swap().collect::<Vec<Either<i32>>>();
    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn into_iter_swap_in_for_loop(#[case] either: Either<Vec<i32>>, #[case] expected: Vec<Either<i32>>) {
    let mut items = vec![];
    for item in either.into_iter_swap() {
        items.push(item);
    }

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(&1), Either::Left(&2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(&1), Either::Right(&2)])]
fn iter_swap(#[case] either: Either<Vec<i32>>, #[case] expected: Vec<Either<&i32>>) {
    let items = either.iter_swap().collect::<Vec<Either<&i32>>>();
    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), (2, Some(2)))]
#[case::left(Either::Right(vec![1, 2]), (2, Some(2)))]
fn iter_swap_size_hint(#[case] either: Either<Vec<i32>>, #[case] expected: (usize, Option<usize>)) {
    let actual = either.iter_swap().size_hint();
    assert_eq!(actual, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(&2), Either::Left(&1)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(&2), Either::Right(&1)])]
fn iter_swap_backwards(#[case] either: Either<Vec<i32>>, #[case] expected: Vec<Either<&i32>>) {
    let items = either.iter_swap().rfold(vec![], |mut acc, elem| {
        acc.push(elem);
        acc
    });
    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(&1), Either::Left(&2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(&1), Either::Right(&2)])]
fn iter_swap_in_for_loop(#[case] either: Either<Vec<i32>>, #[case] expected: Vec<Either<&i32>>) {
    let mut items = vec![];
    for item in either.iter_swap() {
        items.push(item);
    }

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn iter_swap_mut(#[case] mut either: Either<Vec<i32>>, #[case] mut expected: Vec<Either<i32>>) {
    let expected = expected
        .iter_mut()
        .map(Either::as_mut)
        .collect::<Vec<Either<&mut i32>>>();

    let items = either.iter_swap_mut().collect::<Vec<Either<&mut i32>>>();

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(vec![1, 2]), vec![Either::Left(1), Either::Left(2)])]
#[case::left(Either::Right(vec![1, 2]), vec![Either::Right(1), Either::Right(2)])]
fn iter_swap_mut_in_for_loop(
    #[case] mut either: Either<Vec<i32>>,
    #[case] mut expected: Vec<Either<i32>>,
) {
    let expected = expected
        .iter_mut()
        .map(Either::as_mut)
        .collect::<Vec<Either<&mut i32>>>();

    let mut items = vec![];
    for item in either.iter_swap_mut() {
        items.push(item);
    }

    assert_eq!(items, expected);
}
