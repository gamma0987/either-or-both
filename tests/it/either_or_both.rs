//! Tests for `EitherOrBoth`

use either_or_both::EitherOrBoth;

#[test]
fn some() {
    let either = EitherOrBoth::Both(1, 2);
    assert_eq!(either.left(), Some(1));
    assert_eq!(either.right(), Some(2));
}
