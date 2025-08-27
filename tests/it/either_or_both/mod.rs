//! Tests for `EitherOrBoth`

mod iter;
mod same_type;
mod traits;

use core::pin::Pin;

use either_or_both::EitherOrBoth;
use either_or_both::EitherOrBoth::*;
use rstest::rstest;

const RIGHT_VALUE: char = 'c';
const LEFT_VALUE: u8 = 1;
const BOTH_VALUES: (u8, char) = (LEFT_VALUE, RIGHT_VALUE);

fn left_variant() -> EitherOrBoth<u8, char> {
    Left(LEFT_VALUE)
}

fn right_variant() -> EitherOrBoth<u8, char> {
    Right(RIGHT_VALUE)
}

fn both_variant() -> EitherOrBoth<u8, char> {
    Both(LEFT_VALUE, RIGHT_VALUE)
}

fn both_string() -> EitherOrBoth<String> {
    Both(String::from("left"), String::from("right"))
}

fn left_string() -> EitherOrBoth<String> {
    Left(String::from("left"))
}

fn right_string() -> EitherOrBoth<String> {
    Right(String::from("right"))
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
#[case::both(both_variant(), true)]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn has_left(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.has_left(), expected);
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn has_left_and_when_true(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.has_left_and(left_is_true), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn has_left_and_when_false(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(!either_or_both.has_left_and(left_is_false));
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn has_left_or_when_true(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(either_or_both.has_left_or(right_is_true));
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn has_left_or_when_false(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.has_left_or(right_is_false), expected);
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn has_right(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.has_right(), expected);
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn has_right_and_when_true(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.has_right_and(right_is_true), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn has_right_and_when_false(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(!either_or_both.has_right_and(right_is_false));
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn has_right_or_when_true(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(either_or_both.has_right_or(left_is_true));
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn has_right_or_when_false(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.has_right_or(left_is_false), expected);
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), false)]
fn is_both(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.is_both(), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_both_and_when_false(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(!either_or_both.is_both_and(|l, r| left_is_false(l) && right_is_false(r)));
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), false)]
fn is_both_and_when_true(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(
        either_or_both.is_both_and(|l, r| left_is_true(l) && right_is_true(r)),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), true)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), false)]
fn is_both_or_when_false(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(
        either_or_both.is_both_or(left_is_false, right_is_false),
        expected
    );
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_both_or_when_true(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(either_or_both.is_both_or(left_is_true, right_is_true));
}

#[rstest]
#[case::both(both_variant(), false)]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn is_left(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.is_left(), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_left_and_when_false(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(!either_or_both.is_left_and(left_is_false));
}

#[rstest]
#[case::both(both_variant(), false)]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn is_left_and_when_true(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.is_left_and(left_is_true), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_left_or_when_true(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(either_or_both.is_left_or(right_is_true));
}

#[rstest]
#[case::both(both_variant(), false)]
#[case::left(left_variant(), true)]
#[case::right(right_variant(), false)]
fn is_left_or_when_false(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.is_left_or(right_is_false), expected);
}

#[rstest]
#[case::both(both_variant(), false)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn is_right(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.is_right(), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_right_and_when_false(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(!either_or_both.is_right_and(right_is_false));
}

#[rstest]
#[case::both(both_variant(), false)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn is_right_and_when_true(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.is_right_and(right_is_true), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn is_right_or_when_true(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert!(either_or_both.is_right_or(left_is_true));
}

#[rstest]
#[case::both(both_variant(), false)]
#[case::left(left_variant(), false)]
#[case::right(right_variant(), true)]
fn is_right_or_when_false(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: bool) {
    assert_eq!(either_or_both.is_right_or(left_is_false), expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn as_ref(#[case] either_or_both: EitherOrBoth<u8, char>) {
    let _: EitherOrBoth<&u8, &char> = either_or_both.as_ref();
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn as_mut(#[case] mut either_or_both: EitherOrBoth<u8, char>) {
    let _: EitherOrBoth<&mut u8, &mut char> = either_or_both.as_mut();
}

#[rstest]
#[case::both(both_string())]
#[case::left(left_string())]
#[case::right(right_string())]
fn as_deref(#[case] either_or_both: EitherOrBoth<String>) {
    let _: EitherOrBoth<&str> = either_or_both.as_deref();
}

#[rstest]
#[case::both(both_string())]
#[case::left(left_string())]
#[case::right(right_string())]
fn as_deref_mut(#[case] mut either_or_both: EitherOrBoth<String>) {
    let _: EitherOrBoth<&mut str> = either_or_both.as_deref_mut();
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn as_pin_ref(#[case] either_or_both: EitherOrBoth<u8, char>) {
    let pinned: Pin<&EitherOrBoth<u8, char>> = Pin::new(&either_or_both);
    let _: EitherOrBoth<Pin<&u8>, Pin<&char>> = pinned.as_pin_ref();
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn as_pin_mut(#[case] mut either_or_both: EitherOrBoth<u8, char>) {
    let pinned: Pin<&mut EitherOrBoth<u8, char>> = Pin::new(&mut either_or_both);
    let _: EitherOrBoth<Pin<&mut u8>, Pin<&mut char>> = pinned.as_pin_mut();
}

#[rstest]
#[case::both(both_variant())]
#[should_panic = "should be both"]
#[case::left(left_variant())]
#[should_panic = "should be both"]
#[case::right(right_variant())]
fn expect_both(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.expect_both("should be both"), BOTH_VALUES);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[should_panic = "should be left"]
#[case::right(right_variant())]
fn expect_left(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.expect_left("should be left"), LEFT_VALUE);
}

#[rstest]
#[should_panic = "should be left"]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[should_panic = "should be left"]
#[case::right(right_variant())]
fn expect_only_left(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(
        either_or_both.expect_only_left("should be left"),
        LEFT_VALUE
    );
}

#[rstest]
#[case::both(both_variant())]
#[should_panic = "should be right"]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn expect_right(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.expect_right("should be right"), RIGHT_VALUE);
}

#[rstest]
#[should_panic = "should be right"]
#[case::both(both_variant())]
#[should_panic = "should be right"]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn expect_only_right(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(
        either_or_both.expect_only_right("should be right"),
        RIGHT_VALUE
    );
}

#[rstest]
#[case::both(both_variant())]
#[should_panic = "Called `EitherOrBoth::unwrap_both` on a `Left` or `Right` value"]
#[case::left(left_variant())]
#[should_panic = "Called `EitherOrBoth::unwrap_both` on a `Left` or `Right` value"]
#[case::right(right_variant())]
fn unwrap_both(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.unwrap_both(), BOTH_VALUES);
}

#[test]
fn unwrap_both_unchecked() {
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(both_variant().unwrap_both_unchecked(), BOTH_VALUES);
    }
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[should_panic = "Called `EitherOrBoth::unwrap_left` on a `Right` value"]
#[case::right(right_variant())]
fn unwrap_left(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.unwrap_left(), LEFT_VALUE);
}

#[test]
fn unwrap_left_unchecked() {
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(left_variant().unwrap_left_unchecked(), LEFT_VALUE);
    }
}

#[rstest]
#[should_panic = "Called `EitherOrBoth::unwrap_only_left` on a `Both` or `Right` value"]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[should_panic = "Called `EitherOrBoth::unwrap_only_left` on a `Both` or `Right` value"]
#[case::right(right_variant())]
fn unwrap_only_left(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.unwrap_only_left(), LEFT_VALUE);
}

#[test]
fn unwrap_only_left_unchecked() {
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(left_variant().unwrap_only_left_unchecked(), LEFT_VALUE);
    }
}

#[rstest]
#[case::both(both_variant())]
#[should_panic = "Called EitherOrBoth::unwrap_right` on a `Left` value"]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn unwrap_right(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.unwrap_right(), RIGHT_VALUE);
}

#[test]
fn unwrap_right_unchecked() {
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(right_variant().unwrap_right_unchecked(), RIGHT_VALUE);
    }
}

#[rstest]
#[should_panic = "Called EitherOrBoth::unwrap_only_right` on a `Both` or `Left` value"]
#[case::both(both_variant())]
#[should_panic = "Called EitherOrBoth::unwrap_only_right` on a `Both` or `Left` value"]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn unwrap_only_right(#[case] either_or_both: EitherOrBoth<u8, char>) {
    assert_eq!(either_or_both.unwrap_only_right(), RIGHT_VALUE);
}

#[test]
fn unwrap_only_right_unchecked() {
    // SAFETY: This is a test for safety
    unsafe {
        assert_eq!(right_variant().unwrap_only_right_unchecked(), RIGHT_VALUE);
    }
}

#[rstest]
#[case::both(both_variant(), Some(BOTH_VALUES))]
#[case::left(left_variant(), None)]
#[case::right(right_variant(), None)]
fn both(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Option<(u8, char)>) {
    assert_eq!(either_or_both.both(), expected);
}

#[rstest]
#[case::both(both_variant(), Some(LEFT_VALUE))]
#[case::left(left_variant(), Some(LEFT_VALUE))]
#[case::right(right_variant(), None)]
fn left(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Option<u8>) {
    assert_eq!(either_or_both.left(), expected);
}

#[rstest]
#[case::both(both_variant(), Left(2))]
#[case::left(left_variant(), Left(2))]
#[case::right(right_variant(), right_variant())]
fn left_and(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.left_and(Left(2)), expected);
}

#[rstest]
#[case::both(both_variant(), Left(2))]
#[case::left(left_variant(), Left(2))]
#[case::right(right_variant(), right_variant())]
fn left_and_then(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.left_and_then(|l| Left(l + 1)), expected);
}

#[rstest]
#[case::both(both_variant(), None)]
#[case::left(left_variant(), Some(LEFT_VALUE))]
#[case::right(right_variant(), None)]
fn only_left(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Option<u8>) {
    assert_eq!(either_or_both.only_left(), expected);
}

#[rstest]
#[case::both(both_variant(), Some(RIGHT_VALUE))]
#[case::left(left_variant(), None)]
#[case::right(right_variant(), Some(RIGHT_VALUE))]
fn right(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Option<char>) {
    assert_eq!(either_or_both.right(), expected);
}

#[rstest]
#[case::both(both_variant(), Right('m'))]
#[case::left(left_variant(), left_variant())]
#[case::right(right_variant(), Right('m'))]
fn right_and(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.right_and(Right('m')), expected);
}

#[rstest]
#[case::both(both_variant(), Right('d'))]
#[case::left(left_variant(), left_variant())]
#[case::right(right_variant(), Right('d'))]
fn right_and_then(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(
        either_or_both.right_and_then(|c| Right((c as u8 + 1) as char)),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), None)]
#[case::left(left_variant(), None)]
#[case::right(right_variant(), Some(RIGHT_VALUE))]
fn only_right(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Option<char>) {
    assert_eq!(either_or_both.only_right(), expected);
}

#[rstest]
#[case::both(both_variant(), (Some(LEFT_VALUE), Some(RIGHT_VALUE)))]
#[case::left(left_variant(), (Some(LEFT_VALUE), None))]
#[case::right(right_variant(), (None, Some(RIGHT_VALUE)))]
fn unzip(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: (Option<u8>, Option<char>),
) {
    assert_eq!(either_or_both.unzip(), expected);
}

#[rstest]
#[case::both(both_variant(), Both(RIGHT_VALUE, LEFT_VALUE))]
#[case::left(left_variant(), Right(LEFT_VALUE))]
#[case::right(right_variant(), Left(RIGHT_VALUE))]
fn flip(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: EitherOrBoth<char, u8>) {
    assert_eq!(either_or_both.flip(), expected);
}

#[rstest]
#[case::both(both_variant(), Both(2, 100))]
#[case::left(left_variant(), Left(2))]
#[case::right(right_variant(), Right(100))]
fn bimap(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: EitherOrBoth<u64, i32>) {
    assert_eq!(
        either_or_both.bimap(|l| u64::from(l + 1), |c| c as i32 + 1),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), Both(2, 'c'))]
#[case::left(left_variant(), Left(2))]
#[case::right(right_variant(), Right('c'))]
fn map_left(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u64, char>,
) {
    assert_eq!(either_or_both.map_left(|l| u64::from(l + 1)), expected);
}

#[rstest]
#[case::both(both_variant(), 2)]
#[case::left(left_variant(), 2)]
#[case::right(right_variant(), 10)]
fn map_left_or(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: u64) {
    assert_eq!(
        either_or_both.map_left_or(10, |l| u64::from(l + 1)),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), 2)]
#[case::left(left_variant(), 2)]
#[case::right(right_variant(), 0)]
fn map_left_or_default(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: u64) {
    assert_eq!(
        either_or_both.map_left_or_default(|l| u64::from(l + 1)),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), 2)]
#[case::left(left_variant(), 2)]
#[case::right(right_variant(), 10)]
fn map_left_or_else(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: u64) {
    assert_eq!(
        either_or_both.map_left_or_else(|| 10, |l| u64::from(l + 1)),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), Both(1, 99))]
#[case::left(left_variant(), Left(1))]
#[case::right(right_variant(), Right(99))]
fn map_right(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, i32>,
) {
    assert_eq!(either_or_both.map_right(|c| c as i32), expected);
}

#[rstest]
#[case::both(both_variant(), 99)]
#[case::left(left_variant(), 100)]
#[case::right(right_variant(), 99)]
fn map_right_or(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: i32) {
    assert_eq!(either_or_both.map_right_or(100, |c| c as i32), expected);
}

#[rstest]
#[case::both(both_variant(), 99)]
#[case::left(left_variant(), 0)]
#[case::right(right_variant(), 99)]
fn map_right_or_default(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: i32) {
    assert_eq!(either_or_both.map_right_or_default(|c| c as i32), expected);
}

#[rstest]
#[case::both(both_variant(), 99)]
#[case::left(left_variant(), 100)]
#[case::right(right_variant(), 99)]
fn map_right_or_else(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: i32) {
    assert_eq!(
        either_or_both.map_right_or_else(|| 100, |c| c as i32),
        expected
    );
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn biinspect(#[case] either_or_both: EitherOrBoth<u8, char>) {
    let r = either_or_both.biinspect(
        |i| assert_eq!(i, &LEFT_VALUE),
        |c| assert_eq!(c, &RIGHT_VALUE),
    );
    assert_eq!(r, either_or_both);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn inspect_left(#[case] either_or_both: EitherOrBoth<u8, char>) {
    let r = either_or_both.inspect_left(|i| assert_eq!(i, &LEFT_VALUE));
    assert_eq!(r, either_or_both);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn inspect_right(#[case] either_or_both: EitherOrBoth<u8, char>) {
    let r = either_or_both.inspect_right(|c| assert_eq!(c, &RIGHT_VALUE));
    assert_eq!(r, either_or_both);
}

#[rstest]
#[case::both(both_variant(), vec![LEFT_VALUE], vec![RIGHT_VALUE])]
#[case::left(left_variant(), vec![LEFT_VALUE], vec![])]
#[case::right(right_variant(), vec![], vec![RIGHT_VALUE])]
fn biapply(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected_left: Vec<u8>,
    #[case] expected_right: Vec<char>,
) {
    let mut left = vec![];
    let mut right = vec![];
    either_or_both.biapply(|l| left.push(l), |r| right.push(r));

    assert_eq!(left, expected_left);
    assert_eq!(right, expected_right);
}

#[rstest]
#[case::both(both_variant(), vec![LEFT_VALUE, RIGHT_VALUE as u8])]
#[case::left(left_variant(), vec![LEFT_VALUE])]
#[case::right(right_variant(), vec![RIGHT_VALUE as u8])]
fn biapply_with(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Vec<u8>) {
    let mut both = vec![];
    either_or_both.biapply_with(&mut both, |acc, l| acc.push(l), |acc, r| acc.push(r as u8));

    assert_eq!(both, expected);
}

#[rstest]
#[case::both(both_variant(), vec![LEFT_VALUE])]
#[case::left(left_variant(), vec![LEFT_VALUE])]
#[case::right(right_variant(), vec![])]
fn apply_left(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Vec<u8>) {
    let mut left = vec![];
    either_or_both.apply_left(|l| left.push(l));

    assert_eq!(left, expected);
}

#[rstest]
#[case::both(both_variant(), vec![RIGHT_VALUE])]
#[case::left(left_variant(), vec![])]
#[case::right(right_variant(), vec![RIGHT_VALUE])]
fn apply_right(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Vec<char>) {
    let mut right = vec![];
    either_or_both.apply_right(|r| right.push(r));

    assert_eq!(right, expected);
}

#[rstest]
#[case::both(both_variant(), LEFT_VALUE)]
#[case::left(left_variant(), LEFT_VALUE)]
#[case::right(right_variant(), 99)]
fn reduce_left(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: u8) {
    assert_eq!(either_or_both.reduce_left(|r| r as u8), expected);
}

#[rstest]
#[case::both(both_variant(), u64::from(LEFT_VALUE))]
#[case::left(left_variant(), u64::from(LEFT_VALUE))]
#[case::right(right_variant(), RIGHT_VALUE as u64)]
fn reduce_map_left(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: u64) {
    assert_eq!(
        either_or_both.reduce_map_left(u64::from, |r| r as u64),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), RIGHT_VALUE)]
#[case::left(left_variant(), 'e')]
#[case::right(right_variant(), RIGHT_VALUE)]
fn reduce_right(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: char) {
    assert_eq!(either_or_both.reduce_right(|l| (l + 100) as char), expected);
}

#[rstest]
#[case::both(both_variant(), RIGHT_VALUE as u64)]
#[case::left(left_variant(), u64::from(LEFT_VALUE))]
#[case::right(right_variant(), RIGHT_VALUE as u64)]
fn reduce_map_right(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: u64) {
    assert_eq!(
        either_or_both.reduce_map_right(u64::from, u64::from),
        expected
    );
}

#[rstest]
#[case::both(both_variant(), Err(LEFT_VALUE))]
#[case::left(left_variant(), Err(LEFT_VALUE))]
#[case::right(right_variant(), Ok(RIGHT_VALUE))]
fn ok(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Result<char, u8>) {
    assert_eq!(either_or_both.ok(), expected);
}

#[rstest]
#[case::both(both_variant(), Err(2))]
#[case::left(left_variant(), Err(2))]
#[case::right(right_variant(), Ok(RIGHT_VALUE))]
fn ok_or(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Result<char, u8>) {
    assert_eq!(either_or_both.ok_or(2), expected);
}

#[rstest]
#[case::both(both_variant(), Err(2))]
#[case::left(left_variant(), Err(2))]
#[case::right(right_variant(), Ok(RIGHT_VALUE))]
fn ok_or_else(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: Result<char, u8>) {
    assert_eq!(either_or_both.ok_or_else(|| 2), expected);
}

#[rstest]
#[case::both(both_variant(), BOTH_VALUES)]
#[case::left(left_variant(), (LEFT_VALUE, 'm'))]
#[case::right(right_variant(), (2, RIGHT_VALUE))]
fn or(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either_or_both.or(2, 'm'), expected);
}

#[rstest]
#[case::both(both_variant(), BOTH_VALUES)]
#[case::left(left_variant(), (LEFT_VALUE, 'm'))]
#[case::right(right_variant(), (2, RIGHT_VALUE))]
fn or_else(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either_or_both.or_else(|| 2, || 'm'), expected);
}

#[rstest]
#[case::both(both_variant(), BOTH_VALUES)]
#[case::left(left_variant(), (LEFT_VALUE, '\0'))]
#[case::right(right_variant(), (0, RIGHT_VALUE))]
fn or_default(#[case] either_or_both: EitherOrBoth<u8, char>, #[case] expected: (u8, char)) {
    assert_eq!(either_or_both.or_default(), expected);
}

#[rstest]
#[case::both(both_variant(), Both(2, RIGHT_VALUE))]
#[case::left(left_variant(), Left(2))]
#[case::right(right_variant(), Both(2, RIGHT_VALUE))]
fn insert_left(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.insert_left(2), &mut 2);
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), Both(LEFT_VALUE, 'm'))]
#[case::left(left_variant(), Both(LEFT_VALUE, 'm'))]
#[case::right(right_variant(), Right('m'))]
fn insert_right(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.insert_right('m'), &mut 'm');
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), both_variant(), LEFT_VALUE)]
#[case::left(left_variant(), left_variant(), LEFT_VALUE)]
#[case::right(right_variant(), Both(2, RIGHT_VALUE), 2)]
fn left_or_insert(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] mut expected_value: u8,
) {
    assert_eq!(either_or_both.left_or_insert(2), &mut expected_value);
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), both_variant(), LEFT_VALUE)]
#[case::left(left_variant(), left_variant(), LEFT_VALUE)]
#[case::right(right_variant(), Both(0, RIGHT_VALUE), 0)]
fn left_or_insert_default(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] mut expected_value: u8,
) {
    assert_eq!(either_or_both.left_or_insert_default(), &mut expected_value);
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), both_variant(), LEFT_VALUE)]
#[case::left(left_variant(), left_variant(), LEFT_VALUE)]
#[case::right(right_variant(), Both(2, RIGHT_VALUE), 2)]
fn left_or_insert_with(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] mut expected_value: u8,
) {
    assert_eq!(
        either_or_both.left_or_insert_with(|| 2),
        &mut expected_value
    );
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), both_variant(), RIGHT_VALUE)]
#[case::left(left_variant(), Both(LEFT_VALUE, 'm'), 'm')]
#[case::right(right_variant(), right_variant(), RIGHT_VALUE)]
fn right_or_insert(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] mut expected_value: char,
) {
    assert_eq!(either_or_both.right_or_insert('m'), &mut expected_value);
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), both_variant(), RIGHT_VALUE)]
#[case::left(left_variant(), Both(LEFT_VALUE, '\0'), '\0')]
#[case::right(right_variant(), right_variant(), RIGHT_VALUE)]
fn right_or_insert_default(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] mut expected_value: char,
) {
    assert_eq!(
        either_or_both.right_or_insert_default(),
        &mut expected_value
    );
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), both_variant(), RIGHT_VALUE)]
#[case::left(left_variant(), Both(LEFT_VALUE, 'm'), 'm')]
#[case::right(right_variant(), right_variant(), RIGHT_VALUE)]
fn right_or_insert_with(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] mut expected_value: char,
) {
    assert_eq!(
        either_or_both.right_or_insert_with(|| 'm'),
        &mut expected_value
    );
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), Both(2, 'm'), both_variant())]
#[case::left(left_variant(), Left(2), left_variant())]
#[case::right(right_variant(), Right('m'), right_variant())]
fn replace_any(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] expected_old: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.replace_any(2, 'm'), expected_old);
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), Both(2, RIGHT_VALUE), Some(LEFT_VALUE))]
#[case::left(left_variant(), Left(2), Some(LEFT_VALUE))]
#[case::right(right_variant(), Right(RIGHT_VALUE), None)]
fn replace_left(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] expected_old: Option<u8>,
) {
    assert_eq!(either_or_both.replace_left(2), expected_old);
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), Both(LEFT_VALUE, 'm'), Some(RIGHT_VALUE))]
#[case::left(left_variant(), Left(LEFT_VALUE), None)]
#[case::right(right_variant(), Right('m'), Some(RIGHT_VALUE))]
fn replace_right(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
    #[case] expected_old: Option<char>,
) {
    assert_eq!(either_or_both.replace_right('m'), expected_old);
    assert_eq!(either_or_both, expected);
}

#[rstest]
#[case::both(both_variant(), left_variant())]
#[case::left(left_variant(), left_variant())]
#[case::right(right_variant(), Left(99))]
fn into_left(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.into_left(|r| r as u8), expected);
}

#[rstest]
#[case::both(both_variant(), right_variant())]
#[case::left(left_variant(), Right('e'))]
#[case::right(right_variant(), right_variant())]
fn into_right(
    #[case] either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.into_right(|l| (l + 100) as char), expected);
}

#[rstest]
#[case::both(Both(&LEFT_VALUE, &RIGHT_VALUE), both_variant())]
#[case::left(Left(&LEFT_VALUE), left_variant())]
#[case::right(Right(&RIGHT_VALUE), right_variant())]
fn cloned(
    #[case] either_or_both: EitherOrBoth<&u8, &char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.cloned(), expected);
}

#[rstest]
#[case::both(Both(1, 'c'), both_variant())]
#[case::left(Left(1), left_variant())]
#[case::right(Right('c'), right_variant())]
fn cloned_mut(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    let either_or_both = either_or_both.as_mut();
    assert_eq!(either_or_both.cloned(), expected);
}

#[rstest]
#[case::both(Both(&LEFT_VALUE, &RIGHT_VALUE), both_variant())]
#[case::left(Left(&LEFT_VALUE), left_variant())]
#[case::right(Right(&RIGHT_VALUE), right_variant())]
fn copied(
    #[case] either_or_both: EitherOrBoth<&u8, &char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    assert_eq!(either_or_both.copied(), expected);
}

#[rstest]
#[case::both(Both(1, 'c'), both_variant())]
#[case::left(Left(1), left_variant())]
#[case::right(Right('c'), right_variant())]
fn copied_mut(
    #[case] mut either_or_both: EitherOrBoth<u8, char>,
    #[case] expected: EitherOrBoth<u8, char>,
) {
    let either_or_both = either_or_both.as_mut();
    assert_eq!(either_or_both.copied(), expected);
}

#[rstest]
#[case::both(Both((1, 'c'), (-2, 10)), (Both(1, -2), Both('c', 10)))]
#[case::left(Left((1, 'c')), (Left(1), Left('c')))]
#[case::both(Right((-2, 10)), (Right(-2), Right(10)))]
fn transpose_tuples(
    #[case] either_or_both: EitherOrBoth<(u8, char), (i32, u64)>,
    #[case] expected: (EitherOrBoth<u8, i32>, EitherOrBoth<char, u64>),
) {
    assert_eq!(either_or_both.transpose(), expected);
}

#[rstest]
#[case::both(Both((1, 'c'), (2, 10)), (3, Both('c', 10)))]
#[case::left(Left((1, 'c')), (1, Left('c')))]
#[case::both(Right((2, 10)), (2, Right(10)))]
fn transpose_left(
    #[case] either_or_both: EitherOrBoth<(u8, char), (u8, u64)>,
    #[case] expected: (u8, EitherOrBoth<char, u64>),
) {
    assert_eq!(either_or_both.transpose_left(|l, r| l + r), expected);
}

#[rstest]
#[case::both(Both((1, 20), (-2, 10)), (Both(1, -2), 30))]
#[case::left(Left((1, 20)), (Left(1), 20))]
#[case::both(Right((-2, 10)), (Right(-2), 10))]
fn transpose_right(
    #[case] either_or_both: EitherOrBoth<(u8, u64), (i32, u64)>,
    #[case] expected: (EitherOrBoth<u8, i32>, u64),
) {
    assert_eq!(either_or_both.transpose_right(|l, r| l + r), expected);
}

#[rstest]
#[case::both_when_all_some(Both(Some(1), Some(2)), Some(Both(1, 2)))]
#[case::both_when_left_none(Both(None, Some(2)), None)]
#[case::both_when_right_none(Both(Some(1), None), None)]
#[case::both_when_all_none(Both(None, None), None)]
#[case::left(Left(Some(1)), Some(Left(1)))]
#[case::left_when_none(Left(None), None)]
#[case::right(Right(Some(2)), Some(Right(2)))]
#[case::right_when_none(Right(None), None)]
fn transpose_option(
    #[case] either_or_both: EitherOrBoth<Option<u8>, Option<u64>>,
    #[case] expected: Option<EitherOrBoth<u8, u64>>,
) {
    assert_eq!(either_or_both.transpose(), expected);
}

#[rstest]
#[case::both_when_all_ok(Both(Ok(1), Ok(-2)), Ok(Both(1, -2)))]
#[case::both_when_left_err(Both(Err('c'), Ok(-2)), Err(Left('c')))]
#[case::both_when_right_err(Both(Ok(1), Err(2)), Err(Right(2)))]
#[case::both_when_all_err(Both(Err('c'), Err(2)), Err(Both('c', 2)))]
#[case::left_when_ok(Left(Ok(1)), Ok(Left(1)))]
#[case::left_when_err(Left(Err('c')), Err(Left('c')))]
#[case::right_when_ok(Right(Ok(-2)), Ok(Right(-2)))]
#[case::right_when_err(Right(Err(2)), Err(Right(2)))]
fn transpose_result(
    #[case] either_or_both: EitherOrBoth<Result<u8, char>, Result<i32, u64>>,
    #[case] expected: Result<EitherOrBoth<u8, i32>, EitherOrBoth<char, u64>>,
) {
    assert_eq!(either_or_both.transpose(), expected);
}

#[rstest]
#[case::both_when_all_ok(Both(Ok(1), Ok(-2)), Ok(Both(1, -2)))]
#[case::both_when_left_err(Both(Err('c'), Ok(-2)), Err('c'))]
#[case::both_when_right_err(Both(Ok(1), Err('m')), Err('m'))]
#[case::both_when_all_err(Both(Err('c'), Err('m')), Err('m'))]
#[case::left_when_ok(Left(Ok(1)), Ok(Left(1)))]
#[case::left_when_err(Left(Err('c')), Err('c'))]
#[case::right_when_ok(Right(Ok(-2)), Ok(Right(-2)))]
#[case::right_when_err(Right(Err('m')), Err('m'))]
fn transpose_err(
    #[case] either_or_both: EitherOrBoth<Result<u8, char>, Result<i32, char>>,
    #[case] expected: Result<EitherOrBoth<u8, i32>, char>,
) {
    assert_eq!(either_or_both.transpose_err(Ord::max), expected);
}

#[rstest]
#[case::both_when_all_ok(Both(Ok(1), Ok(2)), Ok(3))]
#[case::both_when_left_err(Both(Err('c'), Ok(2)), Err(Left('c')))]
#[case::both_when_right_err(Both(Ok(1), Err(10)), Err(Right(10)))]
#[case::both_when_all_err(Both(Err('c'), Err(10)), Err(Both('c', 10)))]
#[case::left_when_ok(Left(Ok(1)), Ok(1))]
#[case::left_when_err(Left(Err('c')), Err(Left('c')))]
#[case::right_when_ok(Right(Ok(2)), Ok(2))]
#[case::right_when_err(Right(Err(10)), Err(Right(10)))]
fn transpose_ok(
    #[case] either_or_both: EitherOrBoth<Result<u8, char>, Result<u8, u64>>,
    #[case] expected: Result<u8, EitherOrBoth<char, u64>>,
) {
    assert_eq!(either_or_both.transpose_ok(|l, r| l + r), expected);
}
