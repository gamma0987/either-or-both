//! Tests for `EitherOrBoth`

use either_or_both::EitherOrBoth;
use either_or_both::EitherOrBoth::*;
use rstest::rstest;

const RIGHT_VALUE: u8 = 2;
const LEFT_VALUE: u8 = 1;

fn left_variant() -> EitherOrBoth<u8> {
    Left(LEFT_VALUE)
}

fn right_variant() -> EitherOrBoth<u8> {
    Right(RIGHT_VALUE)
}

fn both_variant() -> EitherOrBoth<u8> {
    Both(LEFT_VALUE, RIGHT_VALUE)
}

#[rstest]
#[case::both(both_variant(), vec![LEFT_VALUE, RIGHT_VALUE])]
#[case::left(left_variant(), vec![LEFT_VALUE])]
#[case::right(right_variant(), vec![RIGHT_VALUE])]
fn apply(#[case] either_or_both: EitherOrBoth<u8>, #[case] expected: Vec<u8>) {
    let mut both = vec![];
    either_or_both.apply(|v| both.push(v));

    assert_eq!(both, expected);
}

#[rstest]
#[case::both(both_variant())]
#[case::left(left_variant())]
#[case::right(right_variant())]
fn inspect(#[case] either_or_both: EitherOrBoth<u8>) {
    let r = either_or_both.inspect(|i| assert!(i == &LEFT_VALUE || i == &RIGHT_VALUE));
    assert_eq!(r, either_or_both);
}

#[rstest]
#[case::both(both_variant(), Both(2, 3))]
#[case::left(left_variant(), Left(2))]
#[case::right(right_variant(), Right(3))]
fn map(#[case] either_or_both: EitherOrBoth<u8>, #[case] expected: EitherOrBoth<u64>) {
    assert_eq!(either_or_both.map(|l| u64::from(l + 1)), expected);
}

#[rstest]
#[case::both(both_variant(), 3)]
#[case::left(left_variant(), LEFT_VALUE)]
#[case::right(right_variant(), RIGHT_VALUE)]
fn reduce(#[case] either_or_both: EitherOrBoth<u8>, #[case] expected: u8) {
    assert_eq!(either_or_both.reduce(|l, r| l + r), expected);
}
