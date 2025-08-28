<!-- spell-checker: ignore fixt binstall libtest eprintln usize Gjengset println combinators -->
<!-- markdownlint-disable MD041 MD033 -->

<h1 align="center"><code>EitherOrBoth</code> and <code>Either</code></h1>

<div align="center">
    <a href="https://docs.rs/crate/either-or-both/">Released API Docs</a>
    |
    <a href="https://github.com/gamma0987/either-or-both/blob/main/CHANGELOG.md">Changelog</a>
</div>
<br>
<div align="center">
    <a href="https://github.com/gamma0987/either-or-both/actions/workflows/cicd.yml">
        <img
        src="https://github.com/gamma0987/either-or-both/actions/workflows/cicd.yml/badge.svg"
        alt="GitHub branch checks state"/>
    </a>
    <a href="https://codecov.io/gh/gamma0987/either-or-both" >
         <img
         src="https://codecov.io/gh/gamma0987/either-or-both/graph/badge.svg?token=GHG1BMO029"
         alt="Coverage"/>
     </a>
    <a href="https://crates.io/crates/either-or-both">
        <img src="https://img.shields.io/crates/v/either-or-both.svg" alt="Crates.io"/>
    </a>
    <a href="https://docs.rs/either-or-both/">
        <img src="https://docs.rs/either-or-both/badge.svg" alt="docs.rs"/>
    </a>
</div>
<hr>

`either-or-both` provides two enums: [`Either`] and [`EitherOrBoth`].

* **`Either<L, R>`** — a value that is either `Left(L)` or `Right(R)`
* **`EitherOrBoth<L, R>`** — a value that can be `Left(L)`, `Right(R)`, or `Both(L, R)`

While `Either` is useful for representing mutually exclusive values,
`EitherOrBoth` extends this idea by allowing both values to be present
simultaneously.

## Why Use `EitherOrBoth` and `Either`?

If you often write code like this:

```rust
match options {
    (Some(left), Some(right)) => println!("Left is: {left}, Right is: {right}"),
    (Some(left), None) => println!("Left is: {left}"),
    (None, Some(right)) => println!("Right is: {right}"),
    (None, None) => unreachable!("Should not happen"),
}
```

You can **simplify** and **clarify** your intent using `EitherOrBoth`:

* Removes **boilerplate**
* Improves **readability**
* Eliminates **unreachable patterns** at compile time

Abstracting over multiple types with `Either`:

Suppose you have a function that returns either an in-memory reader or a
file-backed one:

```rust
fn get_reader(path: Option<&str>) -> Either<Cursor<Vec<u8>>, File> {
    match path {
        Some(p) => Either::Right(File::open(p).unwrap()),
        None => Either::Left(Cursor::new(vec![1, 2, 3])),
    }
}
```

This allows you to return different types under a **single unified interface**
(e.g. both implement `Read`), without boxing or trait objects.

Use `Either` when:

* You want to represent **one of two meaningful values**
* You're modeling **branching logic** that's not necessarily an error case
* You need to return **different types** while keeping the API ergonomic and type-safe

## Features

* **Intuitive** API inspired by `Option` and functional programming patterns
* Includes **combinators** like `bimap`, `map`, `and_then`, `apply`, etc.
* `#![no_std]` compatible (when the `std` feature is disabled)
* Fully **documented** on [docs.rs](https://docs.rs/either-or-both)

## Installation

Add `either-or-both` to your `Cargo.toml`:

```toml
[dependencies]
either-or-both = "0.2.0"
```

Or use [`cargo add`](https://github.com/killercup/cargo-edit):

```bash
cargo add either-or-both@0.2.0
```

## Design Philosophy

The API for `either-or-both` is heavily inspired by the `Option` type from the
Rust standard library and aims for consistency between the `Either` and
`EitherOrBoth` enums. Some methods, like `bimap`, are derived from functional
programming languages. Where applicable, methods from `Option` are also
implemented for both `Either` and `EitherOrBoth`. Whenever possible, method
names with similar functionality are shared across both enums. If you're
familiar with the `Option` API, you'll find the `EitherOrBoth` and `Either`
interface intuitive and easy to work with.

## Development Status

The core API is production-ready.

However, as long as the crate version is below `1.0.0`, semver guarantees do not
apply — minor version bumps may include breaking changes.

## License

`either-or-both` is dual licensed under the Apache 2.0 license and the MIT license
at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual licensed as in
[License](#license), without any additional terms or conditions.

[`Either`]: https://docs.rs/either-or-both/latest/either_or_both/enum.Either.html
[`EitherOrBoth`]: https://docs.rs/either-or-both/latest/either_or_both/enum.EitherOrBoth.html
