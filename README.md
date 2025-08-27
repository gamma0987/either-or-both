<!-- spell-checker: ignore fixt binstall libtest eprintln usize Gjengset -->
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
    <a href="https://github.com/rust-lang/rust">
        <img src="https://img.shields.io/badge/MSRV-1.63.0-brightgreen" alt="MSRV"/>
    </a>
</div>
<hr>

The `no_std` compatible enums `EitherOrBoth` with the three variants `Left`,
`Right`, `Both` and `Either` with the `Left` and `Right` variants.

The `Either` enum represents a value with two possibilities and `EitherOrBoth`
represents values which can additionally be both present simultaneously.

## Installation

To start using `either-or-both`, add the following to your `Cargo.toml` file:

```toml
[dependencies]
either-or-both = "0.1.0"
```

or run

```bash
cargo add either-or-both@0.1.0
```

`either-or-both` is fully documented at [docs.rs](https://docs.rs/crate/either-or-both)

## Design Philosophy and Goals

The api for `either-or-both` is heavily inspired by the `Option` type from the
Rust standard library and aims for consistency between the `Either` and
`EitherOrBoth` enums. Some methods, like `bimap`, are derived from functional
programming languages such as Haskell. Where applicable, methods from `Option`
are also implemented for both `Either` and `EitherOrBoth`. Whenever possible,
method names with similar functionality are shared across both enums. If you're
familiar with the `Option` api, you'll find the `EitherOrBoth` and `Either`
interface intuitive and easy to work with.

## Development

The enums and api are fully implemented and functional. Nevertheless, you may
experience big changes between a minor version bump as long as the version is
`0.x`.

### License

`either-or-both` is dual licensed under the Apache 2.0 license and the MIT license
at your option.
