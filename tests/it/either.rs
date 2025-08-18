//! Tests for `Either`

use core::fmt::Write;
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
#[case::right(Either::Right(20), Either::Right(30))]
fn map(#[case] either: Either<i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.map(|i| i + 10), expected);
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
#[case::left(Either::Left(10), 10)]
#[case::right(Either::Right(20), 20)]
fn inspect(#[case] either: Either<i32>, #[case] expected: i32) {
    // The assertion is a side effect to provoke a panic and is not part of the test assertions
    let actual = either.inspect(|i| assert_eq!(*i, expected));
    assert_eq!(actual, either);
}

#[rstest]
#[case::left(Either::Left(10), Some(10), None)]
#[case::right(Either::Right('c'), None, Some('c'))]
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
#[case::left(Either::Left(10), Some(10))]
#[case::right(Either::Right('c'), None)]
fn inspect_left(#[case] either: Either<i32, char>, #[case] expected_left: Option<i32>) {
    let actual = either.inspect_left(|l| assert_eq!(Some(*l), expected_left));
    assert_eq!(actual, either);
}

#[rstest]
#[case::left(Either::Left(10), None)]
#[case::right(Either::Right('c'), Some('c'))]
fn inspect_right(#[case] either: Either<i32, char>, #[case] expected_right: Option<char>) {
    let actual = either.inspect_right(|r| assert_eq!(Some(*r), expected_right));
    assert_eq!(actual, either);
}

#[rstest]
#[case::left(Either::Left(10), Some(10))]
#[case::right(Either::Right(20), Some(20))]
fn consume(#[case] either: Either<i32>, #[case] expected: Option<i32>) {
    let mut option = None;
    either.consume(|i| option = Some(i));

    assert_eq!(option, expected);
}

#[rstest]
#[case::left(Either::Left(10), Some(10), None)]
#[case::right(Either::Right('c'), None, Some('c'))]
fn biconsume(
    #[case] either: Either<i32, char>,
    #[case] expected_left: Option<i32>,
    #[case] expected_right: Option<char>,
) {
    let mut left = None;
    let mut right = None;
    either.biconsume(|l| left = Some(l), |r| right = Some(r));

    assert_eq!(left, expected_left);
    assert_eq!(right, expected_right);
}

#[rstest]
#[case::left(Either::Left(10), vec![10])]
#[case::right(Either::Right('c'), vec![99])]
fn biconsume_with(#[case] either: Either<i32, char>, #[case] expected: Vec<i32>) {
    let mut items = vec![];
    either.biconsume_with(
        &mut items,
        |acc, l| acc.push(l),
        |acc, r| acc.push(r as i32),
    );

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(10), vec![10])]
#[case::right(Either::Right('c'), vec![])]
fn consume_left(#[case] either: Either<i32, char>, #[case] expected: Vec<i32>) {
    let mut items = vec![];
    either.consume_left(|l| items.push(l));

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(10), vec![])]
#[case::right(Either::Right('c'), vec![99])]
fn consume_right(#[case] either: Either<i32, char>, #[case] expected: Vec<i32>) {
    let mut items = vec![];
    either.consume_right(|r| items.push(r as i32));

    assert_eq!(items, expected);
}

#[rstest]
#[case::left(Either::Left(10), 10)]
#[case::right(Either::Right(20), 20)]
fn reduce(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.reduce(), expected);
}

#[rstest]
#[case::left(Either::Left(10), 20)]
#[case::right(Either::Right(20), 30)]
fn reduce_map(#[case] either: Either<i32>, #[case] expected: i32) {
    assert_eq!(either.reduce_map(|i| i + 10), expected);
}

#[rstest]
#[case::left(Either::Left(10), "10")]
#[case::right(Either::Right('c'), "c")]
fn bireduce(#[case] either: Either<i32, char>, #[case] expected: String) {
    assert_eq!(
        either.bireduce(|i| i.to_string(), |c| c.to_string()),
        expected
    );
}

#[rstest]
#[case::left(Either::Left(10), 10)]
#[case::right(Either::Right('c'), 99)]
fn reduce_left(#[case] either: Either<i32, char>, #[case] expected: i32) {
    assert_eq!(either.reduce_left(|c| c as i32), expected);
}

#[rstest]
#[case::left(Either::Left(109), 'm')]
#[case::right(Either::Right('c'), 'c')]
fn reduce_right(#[case] either: Either<u8, char>, #[case] expected: char) {
    assert_eq!(either.reduce_right(|i| i as char), expected);
}

#[rstest]
#[case::left(Either::Left("error message".to_owned()), Err("error message".to_owned()))]
#[case::right(Either::Right(10), Ok(10))]
fn ok(#[case] either: Either<String, i32>, #[case] expected: Result<i32, String>) {
    assert_eq!(either.ok(), expected);
}

#[rstest]
#[case::left(Either::Left("error message".to_owned()), Err("other".to_owned()))]
#[case::right(Either::Right(10), Ok(10))]
fn ok_or(#[case] either: Either<String, i32>, #[case] expected: Result<i32, String>) {
    assert_eq!(either.ok_or("other".to_owned()), expected);
}

#[rstest]
#[case::left(Either::Left("error message".to_owned()), Err("other".to_owned()))]
#[case::right(Either::Right(10), Ok(10))]
fn ok_or_else(#[case] either: Either<String, i32>, #[case] expected: Result<i32, String>) {
    assert_eq!(either.ok_or_else(|| "other".to_owned()), expected);
}

#[rstest]
#[case::left(Either::Left(109), (109, 'm'))]
#[case::right(Either::Right('c'), (200, 'c'))]
fn or(#[case] either: Either<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either.or(200, 'm'), expected);
}

#[rstest]
#[case::left(Either::Left(109), (109, 'm'))]
#[case::right(Either::Right('c'), (200, 'c'))]
fn or_else(#[case] either: Either<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either.or_else(|| 200, || 'm'), expected);
}

#[rstest]
#[case::left(Either::Left(109), (109, '\0'))]
#[case::right(Either::Right('c'), (0, 'c'))]
fn or_default(#[case] either: Either<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either.or_default(), expected);
}

#[rstest]
#[case::left(Either::Left(109), Some(109), Either::Left(200))]
#[case::right(Either::Right('c'), None, Either::Right('c'))]
fn replace_left(
    #[case] mut either: Either<u8, char>,
    #[case] expected: Option<u8>,
    #[case] expected_either: Either<u8, char>,
) {
    assert_eq!(either.replace_left(200), expected);
    assert_eq!(either, expected_either);
}

#[rstest]
#[case::left(Either::Left(109), None, Either::Left(109))]
#[case::right(Either::Right('c'), Some('c'), Either::Right('m'))]
fn replace_right(
    #[case] mut either: Either<u8, char>,
    #[case] expected: Option<char>,
    #[case] expected_either: Either<u8, char>,
) {
    assert_eq!(either.replace_right('m'), expected);
    assert_eq!(either, expected_either);
}

#[rstest]
#[case::left(Either::Left(109), Either::Left(109))]
#[case::right(Either::Right('c'), Either::Left(99))]
fn into_left(#[case] either: Either<u8, char>, #[case] expected: Either<u8, char>) {
    assert_eq!(either.into_left(|c| c as u8), expected);
}

#[rstest]
#[case::left(Either::Left(109), Either::Right('m'))]
#[case::right(Either::Right('c'), Either::Right('c'))]
fn into_right(#[case] either: Either<u8, char>, #[case] expected: Either<u8, char>) {
    assert_eq!(either.into_right(|i| i as char), expected);
}

#[rstest]
#[case::left(Either::Left(&109), Either::Left(109))]
#[case::right(Either::Right(&20), Either::Right(20))]
fn cloned(#[case] either: Either<&i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.cloned(), expected);
}

#[rstest]
#[case::left(Either::Left(&mut 109), Either::Left(109))]
#[case::right(Either::Right(&mut 20), Either::Right(20))]
fn cloned_mut(#[case] either: Either<&mut i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.cloned(), expected);
}

#[rstest]
#[case::left(Either::Left(&109), Either::Left(109))]
#[case::right(Either::Right(&20), Either::Right(20))]
fn copied(#[case] either: Either<&i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.copied(), expected);
}

#[rstest]
#[case::left(Either::Left(&mut 109), Either::Left(109))]
#[case::right(Either::Right(&mut 20), Either::Right(20))]
fn copied_mut(#[case] either: Either<&mut i32>, #[case] expected: Either<i32>) {
    assert_eq!(either.copied(), expected);
}

#[rstest]
#[case::left(Either::Left(Some(10)), Some(Either::Left(10)))]
#[case::right(Either::Right(Some(10)), Some(Either::Right(10)))]
fn transpose_option(#[case] either: Either<Option<i32>>, #[case] expected: Option<Either<i32>>) {
    assert_eq!(either.transpose(), expected);
}

#[rstest]
#[case::left_ok(Either::Left(Ok(-10)), Ok(Either::Left(-10)))]
#[case::left_err(Either::Left(Err("10")), Err(Either::Left("10")))]
#[case::right(Either::Right(Ok(10)), Ok(Either::Right(10)))]
#[case::right_err(Either::Right(Err('c')), Err(Either::Right('c')))]
fn transpose_result(
    #[case] either: Either<Result<i32, &str>, Result<u8, char>>,
    #[case] expected: Result<Either<i32, u8>, Either<&str, char>>,
) {
    assert_eq!(either.transpose(), expected);
}

#[rstest]
#[case::left_ok(Either::Left(Ok(-10)), Ok(Either::Left(-10)))]
#[case::left_err(Either::Left(Err("10")), Err("10"))]
#[case::right(Either::Right(Ok(10)), Ok(Either::Right(10)))]
#[case::right_err(Either::Right(Err("20")), Err("20"))]
fn transpose_ok(
    #[case] either: Either<Result<i32, &str>, Result<u8, &str>>,
    #[case] expected: Result<Either<i32, u8>, &str>,
) {
    assert_eq!(either.transpose_ok(), expected);
}

#[rstest]
#[case::left_ok(Either::Left(Ok(-10)), Ok(-10))]
#[case::left_err(Either::Left(Err("10")), Err(Either::Left("10")))]
#[case::right(Either::Right(Ok(10)), Ok(10))]
#[case::right_err(Either::Right(Err('c')), Err(Either::Right('c')))]
fn transpose_err(
    #[case] either: Either<Result<i32, &str>, Result<i32, char>>,
    #[case] expected: Result<i32, Either<&str, char>>,
) {
    assert_eq!(either.transpose_err(), expected);
}

#[rstest]
#[case::left(&Either::Left(10), Either::Left(&10))]
#[case::left(&Either::Right('c'), Either::Right(&'c'))]
fn from_ref(#[case] either: &Either<u8, char>, #[case] expected: Either<&u8, &char>) {
    assert_eq!(Either::from(either), expected);
}

#[rstest]
#[case::left(&mut Either::Left(10), Either::Left(&mut 10))]
#[case::left(&mut Either::Right('c'), Either::Right(&mut 'c'))]
fn from_mut(#[case] either: &mut Either<u8, char>, #[case] expected: Either<&mut u8, &mut char>) {
    assert_eq!(Either::from(either), expected);
}

#[rstest]
#[case::ok(Ok(10), Either::Right(10))]
#[case::err(Err("some error"), Either::Left("some error"))]
fn from_result(#[case] result: Result<i32, &str>, #[case] expected: Either<&str, i32>) {
    assert_eq!(Either::from(result), expected);
}

#[rstest]
#[case::left(Either::Left(String::new()), Either::Left("some".to_owned()))]
#[case::right(Either::Right(String::new()), Either::Right("some".to_owned()))]
fn impl_fmt_write(#[case] either: Either<String>, #[case] expected: Either<String>) {
    let mut cloned = either.clone();
    cloned.write_str("some").unwrap();
    assert_eq!(cloned, expected);

    let mut cloned = either;
    write!(cloned, "some").unwrap();
    assert_eq!(cloned, expected);
}
