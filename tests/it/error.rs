//! Tests for the errors of this crate

use either_or_both::TryFromOptionsError;

#[test]
fn display() {
    assert_eq!(
        TryFromOptionsError.to_string(),
        "Either the left, right or both values must be present".to_owned()
    );
}
