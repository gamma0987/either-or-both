//! Tests for `Either`

use core::pin::Pin;

use either_or_both::Either;
use rstest::rstest;

#[rstest]
#[case::left(Either::Left(1), true)]
#[case::right(Either::Right('c'), false)]
fn is_left(#[case] either: Either<i32, char>, #[case] expected: bool) {
    assert_eq!(either.is_left(), expected);
}

#[rstest]
#[case::left(Either::Left(1), |i| i == 1, true)]
#[case::left_when_false(Either::Left(1), |i| i != 1, false)]
#[case::right(Either::Right('c'), |i| i == 1, false)]
fn is_left_and(
    #[case] either: Either<i32, char>,
    #[case] and: fn(i32) -> bool,
    #[case] expected: bool,
) {
    assert_eq!(either.is_left_and(and), expected);
}

#[rstest]
#[case::left(Either::Left(1), |c| c == 'c', true)]
#[case::right_when_true(Either::Right('c'), |c| c == 'c', true)]
#[case::right_when_false(Either::Right('c'), |c| c != 'c', false)]
fn is_left_or(
    #[case] either: Either<i32, char>,
    #[case] or: fn(char) -> bool,
    #[case] expected: bool,
) {
    assert_eq!(either.is_left_or(or), expected);
}

#[rstest]
#[case::left(Either::Left(1), false)]
#[case::right(Either::Right('c'), true)]
fn is_right(#[case] either: Either<i32, char>, #[case] expected: bool) {
    assert_eq!(either.is_right(), expected);
}

#[rstest]
#[case::left(Either::Left(1), |c| c == 'c', false)]
#[case::right_when_true(Either::Right('c'), |c| c == 'c', true)]
#[case::right_when_false(Either::Right('c'), |c| c != 'c', false)]
fn is_right_and(
    #[case] either: Either<i32, char>,
    #[case] and: fn(char) -> bool,
    #[case] expected: bool,
) {
    assert_eq!(either.is_right_and(and), expected);
}

#[rstest]
#[case::left_when_true(Either::Left(1), |i| i == 1, true)]
#[case::left_when_false(Either::Left(1), |i| i != 1, false)]
#[case::right(Either::Right('c'), |i| i == 1, true)]
fn is_right_or(
    #[case] either: Either<i32, char>,
    #[case] or: fn(i32) -> bool,
    #[case] expected: bool,
) {
    assert_eq!(either.is_right_or(or), expected);
}

#[rstest]
#[case::left(&Either::Left(1), Either::Left(&1))]
#[case::right(&Either::Right(1), Either::Right(&1))]
fn as_ref(#[case] either: &Either<i32>, #[case] expected: Either<&i32>) {
    assert_eq!(either.as_ref(), expected);
}

#[rstest]
#[case::left(&mut Either::Left(1), Either::Left(&mut 1))]
#[case::right(&mut Either::Right(1), Either::Right(&mut 1))]
fn as_mut(#[case] either: &mut Either<i32>, #[case] expected: Either<&mut i32>) {
    assert_eq!(either.as_mut(), expected);
}

#[rstest]
#[case::left(Either::Left(String::from("left")), Either::Left("left"))]
#[case::right(Either::Right(String::from("right")), Either::Right("right"))]
fn as_deref(#[case] either: Either<String>, #[case] expected: Either<&str>) {
    assert_eq!(either.as_deref(), expected);
}

#[rstest]
#[case::left(Either::Left(String::from("left")), Either::Left(String::from("LEFT")))]
#[case::right(
    Either::Right(String::from("right")),
    Either::Right(String::from("RIGHT"))
)]
fn as_deref_mut(#[case] mut either: Either<String>, #[case] expected: Either<String>) {
    match either.as_deref_mut() {
        Either::Left(item) | Either::Right(item) => item.make_ascii_uppercase(),
    }

    assert_eq!(either, expected);
}

#[test]
fn as_pin_ref() {
    let value = Either::Left::<i32>(10);
    let pinned = Pin::new(&value);
    assert_eq!(pinned.as_pin_ref(), Either::Left(Pin::new(&10)));

    let value = Either::Right::<i32>(10);
    let pinned = Pin::new(&value);
    assert_eq!(pinned.as_pin_ref(), Either::Right(Pin::new(&10)));
}

#[test]
fn as_pin_mut() {
    let mut value = Either::Left::<i32>(10);
    let pinned = Pin::new(&mut value);
    assert_eq!(pinned.as_pin_mut(), Either::Left(Pin::new(&mut 10)));

    let mut value = Either::Right::<i32>(10);
    let pinned = Pin::new(&mut value);
    assert_eq!(pinned.as_pin_mut(), Either::Right(Pin::new(&mut 10)));
}

#[rstest]
#[case::left(Either::Left(10), 10)]
#[should_panic = "should be left"]
#[case::right(Either::Right(10), 10)]
fn expect_left(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.expect_left("should be left"), expected);
}

#[rstest]
#[case::right(Either::Right(10), 10)]
#[should_panic = "should be right"]
#[case::left(Either::Left(10), 10)]
fn expect_right(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.expect_right("should be right"), expected);
}

#[rstest]
#[case::left(Either::Left(10), 10)]
#[should_panic = "Called `Either::unwrap_left` on a `Right` value"]
#[case::right(Either::Right(10), 10)]
fn unwrap_left(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.unwrap_left(), expected);
}

#[rstest]
#[case::right(Either::Right(10), 10)]
#[should_panic = "Called `Either::unwrap_right` on a `Left` value"]
#[case::left(Either::Left(10), 10)]
fn unwrap_right(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.unwrap_right(), expected);
}

#[test]
fn unwrap_left_unchecked() {
    let either = Either::Left::<i32>(10);
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(either.unwrap_left_unchecked(), 10);
    }
}

#[test]
fn unwrap_right_unchecked() {
    let either = Either::Right::<i32>(10);
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(either.unwrap_right_unchecked(), 10);
    }
}

#[rstest]
#[case::left(Either::Left(10), Some(10))]
#[case::right(Either::Right(10), None)]
fn left(#[case] either: Either<i32>, #[case] expected: Option<i32>) {
    assert_eq!(either.left(), expected);
}

#[rstest]
#[case::left(Either::Left(10), Either::Left(5), Either::Left(5))]
#[case::right(Either::Right('c'), Either::Left(5), Either::Right('c'))]
fn left_and(
    #[case] either: Either<i32, char>,
    #[case] other: Either<i32, char>,
    #[case] expected: Either<i32, char>,
) {
    assert_eq!(either.left_and(other), expected);
}

#[rstest]
#[case::left(Either::Left(10), Either::Left('m'))]
#[case::right(Either::Right('c'), Either::Right('c'))]
fn left_and_then(#[case] either: Either<u8, char>, #[case] expected: Either<char>) {
    assert_eq!(
        either.left_and_then(|i| Either::Left((b'c' + i) as char)),
        expected
    );
}

#[rstest]
#[case::right(Either::Right(10), Some(10))]
#[case::left(Either::Left(10), None)]
fn right(#[case] either: Either<i32>, #[case] expected: Option<i32>) {
    assert_eq!(either.right(), expected);
}

#[rstest]
#[case::left(Either::Left(10), Either::Right('m'), Either::Left(10))]
#[case::right(Either::Right('c'), Either::Right('m'), Either::Right('m'))]
fn right_and(
    #[case] either: Either<i32, char>,
    #[case] other: Either<i32, char>,
    #[case] expected: Either<i32, char>,
) {
    assert_eq!(either.right_and(other), expected);
}

#[rstest]
#[case::left(Either::Left(10), Either::Left(10))]
#[case::right(Either::Right('c'), Either::Right(104))]
fn right_and_then(#[case] either: Either<u8, char>, #[case] expected: Either<u8>) {
    assert_eq!(
        either.right_and_then(|c| Either::Right(c as u8 + 5)),
        expected
    );
}

#[rstest]
#[case::left(Either::Left(10), Either::Right(10))]
#[case::right(Either::Right('c'), Either::Left('c'))]
fn flip(#[case] either: Either<u8, char>, #[case] expected: Either<char, u8>) {
    assert_eq!(either.flip(), expected);
}

#[rstest]
#[case::left(Either::Left(10), Either::Left(20))]
#[case::right(Either::Right('c'), Either::Right('m'))]
fn bimap(#[case] either: Either<i32, char>, #[case] expected: Either<i32, char>) {
    assert_eq!(
        either.bimap(|i| i + 10, |c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
#[case::left(Either::Left(10), Either::Left(20))]
#[case::right(Either::Right('c'), Either::Right('c'))]
fn map_left(#[case] either: Either<i32, char>, #[case] expected: Either<i32, char>) {
    assert_eq!(either.map_left(|i| i + 10), expected);
}

#[rstest]
#[case::left(Either::Left(10), 20)]
#[case::right(Either::Right('c'), 30)]
fn map_left_or(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.map_left_or(30, |i| i + 10), expected);
}

#[rstest]
#[case::left(Either::Left(10), 20)]
#[case::right(Either::Right('c'), 0)]
fn map_left_or_default(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.map_left_or_default(|i| i + 10), expected);
}

#[rstest]
#[case::left(Either::Left(10), 20)]
#[case::right(Either::Right('c'), 30)]
fn map_left_or_else(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.map_left_or_else(|| 30, |i| i + 10), expected);
}

#[rstest]
#[case::left(Either::Left(10), Either::Left(10))]
#[case::right(Either::Right('c'), Either::Right('m'))]
fn map_right(#[case] either: Either<i32, char>, #[case] expected: Either<i32, char>) {
    assert_eq!(either.map_right(|c| (c as u8 + 10) as char), expected);
}

#[rstest]
#[case::left(Either::Left(10), 'x')]
#[case::right(Either::Right('c'), 'm')]
fn map_right_or(#[case] either: Either<i32, char>, #[case] expected: char) {
    assert_eq!(
        either.map_right_or('x', |c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
#[case::left(Either::Left(10), '\0')]
#[case::right(Either::Right('c'), 'm')]
fn map_right_or_default(#[case] either: Either<i32, char>, #[case] expected: char) {
    assert_eq!(
        either.map_right_or_default(|c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
#[case::left(Either::Left(10), 'x')]
#[case::right(Either::Right('c'), 'm')]
fn map_right_or_else(#[case] either: Either<i32, char>, #[case] expected: char) {
    assert_eq!(
        either.map_right_or_else(|| 'x', |c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
fn biinspect() {
    let mut left = None;
    let mut right = None;
    let either = Either::Left::<i32>(10);
    either.biinspect(|l| left = Some(*l), |r| right = Some(*r));

    assert_eq!(left, Some(10));
    assert_eq!(right, None);

    let mut left = None;
    let mut right = None;
    let either = Either::Right::<i32>(10);
    either.biinspect(|l| left = Some(*l), |r| right = Some(*r));

    assert_eq!(left, None);
    assert_eq!(right, Some(10));
}
