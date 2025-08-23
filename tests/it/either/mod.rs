//! Tests for `Either`

mod iter;
#[cfg(feature = "std")]
mod std;

use core::fmt::Write;
use core::future::Future;
use core::pin::Pin;
use core::task;
use core::task::Poll;

use either_or_both::{Either, EitherOrBoth};
use rstest::rstest;
use Either::*;

const RIGHT_VALUE: char = 'c';
const LEFT_VALUE: u8 = 1;

fn left_variant() -> Either<u8, char> {
    Left(LEFT_VALUE)
}

fn right_variant() -> Either<u8, char> {
    Right(RIGHT_VALUE)
}

fn left_is_false(i: u8) -> bool {
    i != LEFT_VALUE
}

fn left_is_true(i: u8) -> bool {
    i == LEFT_VALUE
}

fn right_is_false(c: char) -> bool {
    c != RIGHT_VALUE
}

fn right_is_true(c: char) -> bool {
    c == RIGHT_VALUE
}

#[rstest]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn is_left(#[case] either: Either<u8, char>, #[case] expected: bool) {
    assert_eq!(either.is_left(), expected);
}

#[rstest]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn is_left_and_when_true(#[case] either: Either<u8, char>, #[case] expected: bool) {
    assert_eq!(either.is_left_and(left_is_true), expected);
}

#[rstest]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_left_and_when_false(#[case] either: Either<u8, char>) {
    assert!(!either.is_left_and(left_is_false));
}

#[rstest]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_left_or_when_true(#[case] either: Either<u8, char>) {
    assert!(either.is_left_or(right_is_true));
}

#[rstest]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn is_left_or_when_false(#[case] either: Either<u8, char>, #[case] expected: bool) {
    assert_eq!(either.is_left_or(right_is_false), expected);
}

#[rstest]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn is_right(#[case] either: Either<u8, char>, #[case] expected: bool) {
    assert_eq!(either.is_right(), expected);
}

#[rstest]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn is_right_and_when_true(#[case] either: Either<u8, char>, #[case] expected: bool) {
    assert_eq!(either.is_right_and(right_is_true), expected);
}

#[rstest]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_right_and_when_false(#[case] either: Either<u8, char>) {
    assert!(!either.is_right_and(right_is_false));
}

#[rstest]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_right_or_when_true(#[case] either: Either<u8, char>) {
    assert!(either.is_right_or(left_is_true));
}

#[rstest]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn is_right_or_when_false(#[case] either: Either<u8, char>, #[case] expected: bool) {
    assert_eq!(either.is_right_or(left_is_false), expected);
}

#[rstest]
#[case::left(&left_variant(), Left(&1))]
#[case::right(&right_variant(), Right(&'c'))]
fn as_ref(#[case] either: &Either<u8, char>, #[case] expected: Either<&u8, &char>) {
    assert_eq!(either.as_ref(), expected);
}

#[rstest]
#[case::left(&mut left_variant(), Left(&mut 1))]
#[case::right(&mut right_variant(), Right(&mut 'c'))]
fn as_mut(#[case] either: &mut Either<u8, char>, #[case] expected: Either<&mut u8, &mut char>) {
    assert_eq!(either.as_mut(), expected);
}

#[rstest]
#[case::left(Left(String::from("hello")), Left(String::from("HELLO")))]
#[case::right(Right(String::from("hello")), Right(String::from("HELLO")))]
fn impl_as_mut(#[case] mut either: Either<String>, #[case] expected: Either<String>) {
    let string: &mut str = AsMut::as_mut(&mut either);
    string.make_ascii_uppercase();
    assert_eq!(either, expected);
}

#[rstest]
#[case::left(Left(String::from("hello")), Left(String::from("HELLO")))]
#[case::right(Right(String::from("hello")), Right(String::from("HELLO")))]
fn impl_as_deref_mut(#[case] mut either: Either<String>, #[case] expected: Either<String>) {
    let mut string: Either<&mut String> = either.as_mut();
    string.make_ascii_uppercase();
    assert_eq!(either, expected);
}

#[rstest]
#[case::left(Left(String::from("hello")))]
#[case::right(Right(String::from("hello")))]
fn impl_as_ref(#[case] either: Either<String>) {
    let string: &str = AsRef::as_ref(&either);
    assert!(string.is_ascii());
}

#[rstest]
#[case::left(Left(String::from("hello")))]
#[case::right(Right(String::from("hello")))]
fn impl_as_deref(#[case] either: Either<String>) {
    let string: Either<&String> = either.as_ref();
    assert!(string.is_ascii());
}

#[rstest]
#[case::left(Left(String::from("left")), Left("left"))]
#[case::right(Right(String::from("right")), Right("right"))]
fn as_deref(#[case] either: Either<String>, #[case] expected: Either<&str>) {
    assert_eq!(either.as_deref(), expected);
}

#[rstest]
#[case::left(Left(String::from("left")), Left(String::from("LEFT")))]
#[case::right(Right(String::from("right")), Right(String::from("RIGHT")))]
fn as_deref_mut(#[case] mut either: Either<String>, #[case] expected: Either<String>) {
    match either.as_deref_mut() {
        Left(item) | Right(item) => item.make_ascii_uppercase(),
    }

    assert_eq!(either, expected);
}

#[test]
fn as_pin_ref() {
    let value = Left::<i32>(10);
    let pinned = Pin::new(&value);
    assert_eq!(pinned.as_pin_ref(), Left(Pin::new(&10)));

    let value = Right::<i32>(10);
    let pinned = Pin::new(&value);
    assert_eq!(pinned.as_pin_ref(), Right(Pin::new(&10)));
}

#[test]
fn as_pin_mut() {
    let mut value = Left::<i32>(10);
    let pinned = Pin::new(&mut value);
    assert_eq!(pinned.as_pin_mut(), Left(Pin::new(&mut 10)));

    let mut value = Right::<i32>(10);
    let pinned = Pin::new(&mut value);
    assert_eq!(pinned.as_pin_mut(), Right(Pin::new(&mut 10)));
}

#[rstest]
#[case::left(left_variant())]
#[should_panic = "should be left"]
#[case::right(right_variant())]
fn expect_left(#[case] either: Either<u8, char>) {
    assert_eq!(either.expect_left("should be left"), LEFT_VALUE);
}

#[rstest]
#[case::right(right_variant())]
#[should_panic = "should be right"]
#[case::left(left_variant())]
fn expect_right(#[case] either: Either<u8, char>) {
    assert_eq!(either.expect_right("should be right"), RIGHT_VALUE);
}

#[rstest]
#[case::left(left_variant())]
#[should_panic = "Called `Either::unwrap_left` on a `Right` value"]
#[case::right(right_variant())]
fn unwrap_left(#[case] either: Either<u8, char>) {
    assert_eq!(either.unwrap_left(), LEFT_VALUE);
}

#[rstest]
#[case::right(right_variant())]
#[should_panic = "Called `Either::unwrap_right` on a `Left` value"]
#[case::left(left_variant())]
fn unwrap_right(#[case] either: Either<u8, char>) {
    assert_eq!(either.unwrap_right(), RIGHT_VALUE);
}

#[test]
fn unwrap_left_unchecked() {
    let either = left_variant();
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(either.unwrap_left_unchecked(), LEFT_VALUE);
    }
}

#[test]
fn unwrap_right_unchecked() {
    let either = right_variant();
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(either.unwrap_right_unchecked(), RIGHT_VALUE);
    }
}

#[rstest]
#[case::left(left_variant(), Some(LEFT_VALUE))]
#[case::right(right_variant(), None)]
fn left(#[case] either: Either<u8, char>, #[case] expected: Option<u8>) {
    assert_eq!(either.left(), expected);
}

#[rstest]
#[case::left(left_variant(), Left(5), Left(5))]
#[case::right(right_variant(), Left(5), right_variant())]
fn left_and(
    #[case] either: Either<u8, char>,
    #[case] other: Either<u8, char>,
    #[case] expected: Either<u8, char>,
) {
    assert_eq!(either.left_and(other), expected);
}

#[rstest]
#[case::left(left_variant(), Left(3))]
#[case::right(right_variant(), right_variant())]
fn left_and_then(#[case] either: Either<u8, char>, #[case] expected: Either<u8, char>) {
    assert_eq!(either.left_and_then(|i| Left(i + 2)), expected);
}

#[rstest]
#[case::right(right_variant(), Some(RIGHT_VALUE))]
#[case::left(left_variant(), None)]
fn right(#[case] either: Either<u8, char>, #[case] expected: Option<char>) {
    assert_eq!(either.right(), expected);
}

#[rstest]
#[case::left(left_variant(), Right('m'), left_variant())]
#[case::right(right_variant(), Right('m'), Right('m'))]
fn right_and(
    #[case] either: Either<u8, char>,
    #[case] other: Either<u8, char>,
    #[case] expected: Either<u8, char>,
) {
    assert_eq!(either.right_and(other), expected);
}

#[rstest]
#[case::left(left_variant(), left_variant())]
#[case::right(right_variant(), Right('d'))]
fn right_and_then(#[case] either: Either<u8, char>, #[case] expected: Either<u8, char>) {
    assert_eq!(
        either.right_and_then(|c| Right((c as u8 + 1) as char)),
        expected
    );
}

#[rstest]
#[case::left(left_variant(), Right(LEFT_VALUE))]
#[case::right(right_variant(), Left(RIGHT_VALUE))]
fn flip(#[case] either: Either<u8, char>, #[case] expected: Either<char, u8>) {
    assert_eq!(either.flip(), expected);
}

#[rstest]
#[case::left(Left(10), Left(20))]
#[case::right(Right(20), Right(30))]
fn map(#[case] either: Either<i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.map(|i| i + 10), expected);
}

#[rstest]
#[case::left(Left(10), Left(20))]
#[case::right(Right('c'), Right('m'))]
fn bimap(#[case] either: Either<i32, char>, #[case] expected: Either<i32, char>) {
    assert_eq!(
        either.bimap(|i| i + 10, |c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
#[case::left(Left(10), Left(20))]
#[case::right(Right('c'), Right('c'))]
fn map_left(#[case] either: Either<i32, char>, #[case] expected: Either<i32, char>) {
    assert_eq!(either.map_left(|i| i + 10), expected);
}

#[rstest]
#[case::left(Left(10), 20)]
#[case::right(Right('c'), 30)]
fn map_left_or(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.map_left_or(30, |i| i + 10), expected);
}

#[rstest]
#[case::left(Left(10), 20)]
#[case::right(Right('c'), 0)]
fn map_left_or_default(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.map_left_or_default(|i| i + 10), expected);
}

#[rstest]
#[case::left(Left(10), 20)]
#[case::right(Right('c'), 30)]
fn map_left_or_else(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.map_left_or_else(|| 30, |i| i + 10), expected);
}

#[rstest]
#[case::left(Left(10), Left(10))]
#[case::right(Right('c'), Right('m'))]
fn map_right(#[case] either: Either<i32, char>, #[case] expected: Either<i32, char>) {
    assert_eq!(either.map_right(|c| (c as u8 + 10) as char), expected);
}

#[rstest]
#[case::left(Left(10), 'x')]
#[case::right(Right('c'), 'm')]
fn map_right_or(#[case] either: Either<i32, char>, #[case] expected: char) {
    assert_eq!(
        either.map_right_or('x', |c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
#[case::left(Left(10), '\0')]
#[case::right(Right('c'), 'm')]
fn map_right_or_default(#[case] either: Either<i32, char>, #[case] expected: char) {
    assert_eq!(
        either.map_right_or_default(|c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
#[case::left(Left(10), 'x')]
#[case::right(Right('c'), 'm')]
fn map_right_or_else(#[case] either: Either<i32, char>, #[case] expected: char) {
    assert_eq!(
        either.map_right_or_else(|| 'x', |c| (c as u8 + 10) as char),
        expected
    );
}

#[rstest]
#[case::left(Left(10), 10)]
#[case::right(Right(20), 20)]
fn inspect(#[case] either: Either<i32>, #[case] expected: i32) {
    // The assertion is a side effect to provoke a panic and is not part of the test assertions
    let actual = either.inspect(|i| assert_eq!(*i, expected));
    assert_eq!(actual, either);
}

#[rstest]
#[case::left(Left(10), Some(10), None)]
#[case::right(Right('c'), None, Some('c'))]
fn biinspect(
    #[case] either: Either<i32, char>,
    #[case] expected_left: Option<i32>,
    #[case] expected_right: Option<char>,
) {
    let actual = either.biinspect(
        |i| assert_eq!(Some(*i), expected_left),
        |r| assert_eq!(Some(*r), expected_right),
    );
    assert_eq!(actual, either);
}

#[rstest]
#[case::left(Left(10), Some(10))]
#[case::right(Right('c'), None)]
fn inspect_left(#[case] either: Either<i32, char>, #[case] expected_left: Option<i32>) {
    let actual = either.inspect_left(|l| assert_eq!(Some(*l), expected_left));
    assert_eq!(actual, either);
}

#[rstest]
#[case::left(Left(10), None)]
#[case::right(Right('c'), Some('c'))]
fn inspect_right(#[case] either: Either<i32, char>, #[case] expected_right: Option<char>) {
    let actual = either.inspect_right(|r| assert_eq!(Some(*r), expected_right));
    assert_eq!(actual, either);
}

#[rstest]
#[case::left(Left(10), Some(10))]
#[case::right(Right(20), Some(20))]
fn apply(#[case] either: Either<i32>, #[case] expected: Option<i32>) {
    let mut option = None;
    either.apply(|i| option = Some(i));

    assert_eq!(option, expected);
}

#[rstest]
#[case::left(Left(10), Some(10), None)]
#[case::right(Right('c'), None, Some('c'))]
fn biapply(
    #[case] either: Either<i32, char>,
    #[case] expected_left: Option<i32>,
    #[case] expected_right: Option<char>,
) {
    let mut left = None;
    let mut right = None;
    either.biapply(|l| left = Some(l), |r| right = Some(r));

    assert_eq!(left, expected_left);
    assert_eq!(right, expected_right);
}

#[rstest]
#[case::left(Left(10), vec![10])]
#[case::right(Right('c'), vec![99])]
fn biapply_with(#[case] either: Either<i32, char>, #[case] expected: Vec<i32>) {
    let mut items = vec![];
    either.biapply_with(
        &mut items,
        |acc, l| acc.push(l),
        |acc, r| acc.push(r as i32),
    );

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Left(10), vec![10])]
#[case::right(Right('c'), vec![])]
fn biapply_left(#[case] either: Either<i32, char>, #[case] expected: Vec<i32>) {
    let mut items = vec![];
    either.apply_left(|l| items.push(l));

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Left(10), vec![])]
#[case::right(Right('c'), vec![99])]
fn biapply_right(#[case] either: Either<i32, char>, #[case] expected: Vec<i32>) {
    let mut items = vec![];
    either.apply_right(|r| items.push(r as i32));

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Left(10), 10)]
#[case::right(Right(20), 20)]
fn reduce(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.reduce(), expected);
}

#[rstest]
#[case::left(Left(10), 20)]
#[case::right(Right(20), 30)]
fn reduce_map(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.reduce_map(|i| i + 10), expected);
}

#[rstest]
#[case::left(Left(10), "10")]
#[case::right(Right('c'), "c")]
fn bireduce(#[case] either: Either<i32, char>, #[case] expected: String) {
    assert_eq!(
        either.bireduce(|i| i.to_string(), |c| c.to_string()),
        expected
    );
}

#[rstest]
#[case::left(Left(10), 10)]
#[case::right(Right('c'), 99)]
fn reduce_left(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.reduce_left(|c| c as i32), expected);
}

#[rstest]
#[case::left(Left(109), 'm')]
#[case::right(Right('c'), 'c')]
fn reduce_right(#[case] either: Either<u8, char>, #[case] expected: char) {
    assert_eq!(either.reduce_right(|i| i as char), expected);
}

#[rstest]
#[case::left(Left("error message".to_owned()), Err("error message".to_owned()))]
#[case::right(Right(10), Ok(10))]
fn ok(#[case] either: Either<String, i32>, #[case] expected: Result<i32, String>) {
    assert_eq!(either.ok(), expected);
}

#[rstest]
#[case::left(Left("error message".to_owned()), Err("other".to_owned()))]
#[case::right(Right(10), Ok(10))]
fn ok_or(#[case] either: Either<String, i32>, #[case] expected: Result<i32, String>) {
    assert_eq!(either.ok_or("other".to_owned()), expected);
}

#[rstest]
#[case::left(Left("error message".to_owned()), Err("other".to_owned()))]
#[case::right(Right(10), Ok(10))]
fn ok_or_else(#[case] either: Either<String, i32>, #[case] expected: Result<i32, String>) {
    assert_eq!(either.ok_or_else(|| "other".to_owned()), expected);
}

#[rstest]
#[case::left(Left(109), (109, 'm'))]
#[case::right(Right('c'), (200, 'c'))]
fn or(#[case] either: Either<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either.or(200, 'm'), expected);
}

#[rstest]
#[case::left(Left(109), (109, 'm'))]
#[case::right(Right('c'), (200, 'c'))]
fn or_else(#[case] either: Either<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either.or_else(|| 200, || 'm'), expected);
}

#[rstest]
#[case::left(Left(109), (109, '\0'))]
#[case::right(Right('c'), (0, 'c'))]
fn or_default(#[case] either: Either<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either.or_default(), expected);
}

#[rstest]
#[case::left(Left(109), EitherOrBoth::Left(200))]
#[case::right(Right('c'), EitherOrBoth::Both(200, 'c'))]
fn inject_left(#[case] either: Either<u8, char>, #[case] expected: EitherOrBoth<u8, char>) {
    assert_eq!(either.inject_left(200), expected);
}

#[rstest]
#[case::left(Left(109), EitherOrBoth::Both(109, 'm'))]
#[case::right(Right('c'), EitherOrBoth::Right('m'))]
fn inject_right(#[case] either: Either<u8, char>, #[case] expected: EitherOrBoth<u8, char>) {
    assert_eq!(either.inject_right('m'), expected);
}

#[rstest]
#[case::left(Left(109), Some(109), Left(200))]
#[case::right(Right('c'), None, Right('c'))]
fn replace_left(
    #[case] mut either: Either<u8, char>,
    #[case] expected: Option<u8>,
    #[case] expected_either: Either<u8, char>,
) {
    assert_eq!(either.replace_left(200), expected);
    assert_eq!(either, expected_either);
}

#[rstest]
#[case::left(Left(109), None, Left(109))]
#[case::right(Right('c'), Some('c'), Right('m'))]
fn replace_right(
    #[case] mut either: Either<u8, char>,
    #[case] expected: Option<char>,
    #[case] expected_either: Either<u8, char>,
) {
    assert_eq!(either.replace_right('m'), expected);
    assert_eq!(either, expected_either);
}

#[rstest]
#[case::left(Left(109), Left(109))]
#[case::right(Right('c'), Left(99))]
fn into_left(#[case] either: Either<u8, char>, #[case] expected: Either<u8, char>) {
    assert_eq!(either.into_left(|c| c as u8), expected);
}

#[rstest]
#[case::left(Left(109), Right('m'))]
#[case::right(Right('c'), Right('c'))]
fn into_right(#[case] either: Either<u8, char>, #[case] expected: Either<u8, char>) {
    assert_eq!(either.into_right(|i| i as char), expected);
}

#[rstest]
#[case::left(Left(&109), Left(109))]
#[case::right(Right(&20), Right(20))]
fn cloned(#[case] either: Either<&i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.cloned(), expected);
}

#[rstest]
#[case::left(Left(&mut 109), Left(109))]
#[case::right(Right(&mut 20), Right(20))]
fn cloned_mut(#[case] either: Either<&mut i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.cloned(), expected);
}

#[rstest]
#[case::left(Left(&109), Left(109))]
#[case::right(Right(&20), Right(20))]
fn copied(#[case] either: Either<&i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.copied(), expected);
}

#[rstest]
#[case::left(Left(&mut 109), Left(109))]
#[case::right(Right(&mut 20), Right(20))]
fn copied_mut(#[case] either: Either<&mut i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.copied(), expected);
}

#[rstest]
#[case::left(Left((1, 'c')), (Left(1), Left('c')))]
#[case::right(Right((-2, 10)), (Right(-2), Right(10)))]
fn transpose_tuples(
    #[case] either: Either<(u8, char), (i32, u64)>,
    #[case] expected: (Either<u8, i32>, Either<char, u64>),
) {
    assert_eq!(either.transpose(), expected);
}

#[rstest]
#[case::left(Left((1, 'c')), (1, Left('c')))]
#[case::right(Right((2, 10)), (2, Right(10)))]
fn transpose_left(
    #[case] either: Either<(u8, char), (u8, u64)>,
    #[case] expected: (u8, Either<char, u64>),
) {
    assert_eq!(either.transpose_left(), expected);
}

#[rstest]
#[case::left(Left((1, 'c')), (Left(1), 'c'))]
#[case::right(Right((-2, 'm')), (Right(-2), 'm'))]
fn transpose_right(
    #[case] either: Either<(u8, char), (i32, char)>,
    #[case] expected: (Either<u8, i32>, char),
) {
    assert_eq!(either.transpose_right(), expected);
}

#[rstest]
#[case::left(Left(Some(10)), Some(Left(10)))]
#[case::right(Right(Some(10)), Some(Right(10)))]
fn transpose_option(#[case] either: Either<Option<i32>>, #[case] expected: Option<Either<i32>>) {
    assert_eq!(either.transpose(), expected);
}

#[rstest]
#[case::left_ok(Left(Ok(-10)), Ok(Left(-10)))]
#[case::left_err(Left(Err("10")), Err(Left("10")))]
#[case::right(Right(Ok(10)), Ok(Right(10)))]
#[case::right_err(Right(Err('c')), Err(Right('c')))]
fn transpose_result(
    #[case] either: Either<Result<i32, &str>, Result<u8, char>>,
    #[case] expected: Result<Either<i32, u8>, Either<&str, char>>,
) {
    assert_eq!(either.transpose(), expected);
}

#[rstest]
#[case::left_ok(Left(Ok(-10)), Ok(Left(-10)))]
#[case::left_err(Left(Err("10")), Err("10"))]
#[case::right(Right(Ok(10)), Ok(Right(10)))]
#[case::right_err(Right(Err("20")), Err("20"))]
fn transpose_ok(
    #[case] either: Either<Result<i32, &str>, Result<u8, &str>>,
    #[case] expected: Result<Either<i32, u8>, &str>,
) {
    assert_eq!(either.transpose_ok(), expected);
}

#[rstest]
#[case::left_ok(Left(Ok(-10)), Ok(-10))]
#[case::left_err(Left(Err("10")), Err(Left("10")))]
#[case::right(Right(Ok(10)), Ok(10))]
#[case::right_err(Right(Err('c')), Err(Right('c')))]
fn transpose_err(
    #[case] either: Either<Result<i32, &str>, Result<i32, char>>,
    #[case] expected: Result<i32, Either<&str, char>>,
) {
    assert_eq!(either.transpose_err(), expected);
}

#[rstest]
#[case::left(&Left(10), Left(&10))]
#[case::left(&Right('c'), Right(&'c'))]
fn from_ref(#[case] either: &Either<u8, char>, #[case] expected: Either<&u8, &char>) {
    assert_eq!(Either::from(either), expected);
}

#[rstest]
#[case::left(&mut Left(10), Left(&mut 10))]
#[case::left(&mut Right('c'), Right(&mut 'c'))]
fn from_mut(#[case] either: &mut Either<u8, char>, #[case] expected: Either<&mut u8, &mut char>) {
    assert_eq!(Either::from(either), expected);
}

#[rstest]
#[case::ok(Ok(10), Right(10))]
#[case::err(Err("some error"), Left("some error"))]
fn from_result(#[case] result: Result<i32, &str>, #[case] expected: Either<&str, i32>) {
    assert_eq!(Either::from(result), expected);
}

#[rstest]
#[case::left(Left(String::new()), Left("some".to_owned()))]
#[case::right(Right(String::new()), Right("some".to_owned()))]
fn impl_fmt_write(#[case] either: Either<String>, #[case] expected: Either<String>) {
    let mut cloned = either.clone();
    cloned.write_str("some").unwrap();
    assert_eq!(cloned, expected);

    let mut cloned = either;
    write!(cloned, "some").unwrap();
    assert_eq!(cloned, expected);
}

#[rstest]
#[case::left(Left("hello"))]
#[case::right(Right("hello"))]
fn impl_fmt_display(#[case] either: Either<&str>) {
    assert_eq!(either.to_string(), String::from("hello"));
}

#[rstest]
#[case::left(Left(vec![1]), Left(vec![1, 2, 3]))]
#[case::right(Right(vec![1]), Right(vec![1, 2, 3]))]
fn impl_extend(#[case] mut either: Either<Vec<u8>>, #[case] expected: Either<Vec<u8>>) {
    either.extend(vec![2, 3]);
    assert_eq!(either, expected);
}

struct FutureTest(i32);
impl Future for FutureTest {
    type Output = i32;

    fn poll(self: Pin<&mut Self>, _: &mut task::Context<'_>) -> Poll<Self::Output> {
        Poll::Ready(self.0)
    }
}

#[rstest]
#[case::left(Left(FutureTest(1)))]
#[case::right(Right(FutureTest(1)))]
#[tokio::test]
async fn impl_future(#[case] either: Either<FutureTest>) {
    let r = either.await;
    assert_eq!(r, 1);
}
