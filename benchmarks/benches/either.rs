#![allow(missing_docs)]

use std::fmt::Write;

use either_or_both::Either;
use iai_callgrind::{library_benchmark, library_benchmark_group, main};

#[library_benchmark]
#[bench::left(args = [Either::Left(String::new()), "some"])]
fn write_fmt(mut either: Either<String>, to_write: &str) -> Either<String> {
    write!(either, "{to_write}").unwrap();
    either
}

library_benchmark_group!(name = my_group; benchmarks = write_fmt);
main!(library_benchmark_groups = my_group);
