use std::io::{BufRead, Cursor, Read, Seek, SeekFrom, Write};

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

#[rstest]
#[case::left(Either::Left(Cursor::new(String::from("hello\nworld"))))]
#[case::left(Either::Right(Cursor::new(String::from("hello\nworld"))))]
fn impl_buf_read(#[case] mut either: Either<Cursor<String>>) {
    let mut buffer = String::new();
    let size = either.read_line(&mut buffer).unwrap();
    assert_eq!(buffer, "hello\n");
    assert_eq!(size, 6);
}

#[rstest]
#[case::left(Either::Left(Cursor::new(String::from("hello\nworld"))))]
#[case::left(Either::Right(Cursor::new(String::from("hello\nworld"))))]
fn impl_read(#[case] mut either: Either<Cursor<String>>) {
    let mut buffer = String::new();
    let size = either.read_to_string(&mut buffer).unwrap();
    assert_eq!(buffer, "hello\nworld");
    assert_eq!(size, 11);
}

#[rstest]
#[case::left(Either::Left(Cursor::new(String::from("hello\nworld"))))]
#[case::left(Either::Right(Cursor::new(String::from("hello\nworld"))))]
fn impl_seek(#[case] mut either: Either<Cursor<String>>) {
    let pos = either.seek(SeekFrom::End(-5)).unwrap();
    assert_eq!(pos, 6);
}
