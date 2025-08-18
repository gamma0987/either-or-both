use std::io::{Cursor, Write};

use either_or_both::Either;
use rstest::rstest;

fn cursor_fixture(buf: &[u8]) -> Cursor<Vec<u8>> {
    use std::io::Write;

    let mut cursor = Cursor::new(Vec::new());
    assert_eq!(cursor.write(buf).unwrap(), buf.len());
    cursor
}

#[rstest]
#[case::left(Either::Left(Cursor::new(Vec::new())), Either::Left(cursor_fixture(&[1, 2])))]
#[case::right(Either::Right(Cursor::new(Vec::new())), Either::Right(cursor_fixture(&[1, 2])))]
fn impl_io_write(
    #[case] mut either: Either<Cursor<Vec<u8>>>,
    #[case] expected: Either<Cursor<Vec<u8>>>,
) {
    let buf = vec![1, 2];

    assert_eq!(either.write(&buf).unwrap(), buf.len());
    assert_eq!(either, expected);

    either.flush().unwrap();
}
