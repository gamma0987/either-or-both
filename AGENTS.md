# AGENTS.md

## Project

Rust library crate providing `Either<L, R>` and `EitherOrBoth<L, R>` enums.
MSRV: 1.63.0. Pre-1.0 — semver guarantees do not apply.

## Commands (via `just`)

- `just test` — run all tests (`--all-features --all-targets`)
- `just test-doc` — run doc tests (requires `DOCS_RS=1`)
- `just test-hack` — test all feature combinations (requires `cargo-hack`)
- `just lint` — clippy with `-D warnings` (uses **stable** toolchain)
- `just check-fmt` — check Rust formatting (uses **nightly** toolchain)
- `just check-fmt-all` — check Rust + TOML + JSON/YAML formatting and spelling
- `just fmt` — auto-format Rust (nightly); `just fmt-toml` for TOML (taplo);
  `just fmt-prettier` for JSON/YAML
- `just build-docs` — build docs with `DOCS_RS=1 --all-features`
- `just deny` — cargo-deny checks

Recommended order: `just check-fmt-all` → `just lint` → `just test` → `just
test-doc`

## Architecture

- `src/lib.rs` — crate root, re-exports, module gating
- `src/either_or_both/` — `EitherOrBoth` type (always compiled)
- `src/either/` — `Either` type (gated behind `feature = "either"`)
- `src/error.rs` — `TryFromOptionsError`
- `tests/it/` — integration tests mirroring `src/` structure

## Key Conventions

- `Either` is feature-gated (`feature = "either"`); default features include it
- `#![no_std]` compatible when `std` feature is disabled
- `missing_docs = "warn"` enforced — all public items need doc comments
- Extensive clippy pedantic lints configured in `Cargo.toml` under
  `[lints.clippy]`
- Doc builds use `--cfg docsrs`; set `DOCS_RS=1` for doc tests
- `rustfmt.toml` uses unstable features — requires nightly (`cargo +nightly
fmt`)
- Version bumps are handled by `bump-my-version` (configs in `.bumpversion/`)
- Benchmarks live in a separate workspace member (`benchmarks/`) using
  `gungraun`
- Tests use `rstest` and `tokio` (dev-dependencies)
