#![allow(missing_docs)]

use core::hint::black_box;

use either_or_both::EitherOrBoth;
use iai_callgrind::{library_benchmark, library_benchmark_group, main};

fn setup_iter(either_or_both: &EitherOrBoth<i32>) -> impl Iterator<Item = &i32> {
    either_or_both.iter()
}

fn setup_double_iter(either_or_both: &EitherOrBoth<i32>) -> impl DoubleEndedIterator<Item = &i32> {
    either_or_both.iter()
}

#[library_benchmark]
#[benches::some(args = [EitherOrBoth::Left(1), EitherOrBoth::Right(2), EitherOrBoth::Both(1, 2)])]
fn into_iter(either: EitherOrBoth<i32>) -> impl Iterator<Item = i32> {
    black_box(either.into_iter())
}

#[library_benchmark]
#[benches::some(args = [&EitherOrBoth::Left(1), &EitherOrBoth::Right(2), &EitherOrBoth::Both(1, 2)], setup = setup_iter)]
fn iter_next<'a>(mut iter: impl Iterator<Item = &'a i32>) -> Option<&'a i32> {
    black_box(iter.next())
}

#[library_benchmark]
#[benches::some(args = [&EitherOrBoth::Left(1), &EitherOrBoth::Right(2), &EitherOrBoth::Both(1, 2)], setup = setup_double_iter)]
fn iter_next_back<'a>(mut iter: impl DoubleEndedIterator<Item = &'a i32>) -> Option<&'a i32> {
    black_box(iter.next_back())
}

library_benchmark_group!(name = my_group; benchmarks = into_iter, iter_next, iter_next_back);
main!(library_benchmark_groups = my_group);
