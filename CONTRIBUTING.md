# Getting Started

## Code of Conduct

The either-or-both project adheres to the [Rust Code of
Conduct](https://www.rust-lang.org/policies/code-of-conduct). This describes the
minimum behavior expected from all contributors. See
[`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md) for details.

## Prerequisites

- [Rust](https://www.rust-lang.org/tools/install) (stable and nightly
  toolchains)
- [just](https://github.com/casey/just) — task runner used throughout the
  project

Optional tools for running specific checks:

- [cargo-hack](https://github.com/taiki-e/cargo-hack) — feature powerset
  builds/tests
- [cargo-deny](https://github.com/EmbarkStudios/cargo-deny) — dependency linting
- [taplo](https://github.com/tamasfe/taplo) — TOML formatting
- [cspell](https://cspell.org/) — spell checking
- [prettier](https://prettier.io/) — JSON/YAML formatting (or use `npx
prettier`)
- [grcov](https://github.com/mozilla/grcov) — coverage reporting

## Pull Requests

- Target the `main` branch.
- CI must pass.
- The MSRV is **1.63.0**. All changes must compile on this version.
- Update [`CHANGELOG.md`](CHANGELOG.md) under the `[Unreleased]` heading using
  the [Keep a Changelog](http://keepachangelog.com/en/1.0.0/) format. Reference
  the pull request number.
- Version bumps are handled by maintainers using
  [bump-my-version](https://github.com/callowayproject/bump-my-version)
  (configuration in [`.bumpversion/version.toml`](.bumpversion/version.toml)).
- Contributions are dual-licensed under [Apache-2.0](LICENSE-APACHE) and
  [MIT](LICENSE-MIT), the same as the project.

## Use of AI tools

AI tool use is not discouraged on the `either-or-both` codebase, as long as it
meets our quality standards. We kindly ask you to disclose usage of AI tools in
your contributions. If you used them without disclosing it, we may reject your
contribution on that basis alone due to the assumption that you have, most
likely, not reviewed your own submission (so why should we?).

We may still reject AI-assisted contributions if we deem the quality of the
contribution to be unsatisfactory.

## Building

```sh
just build either-or-both            # Build the library
just build-hack                      # Build with all feature combinations (requires cargo-hack)
```

## Code Style

### Rust formatting

Rust formatting uses the **nightly** toolchain with options configured in
[`rustfmt.toml`](rustfmt.toml).

```sh
just fmt          # Auto-format Rust files
just check-fmt    # Check formatting without making changes
```

### TOML formatting

```sh
just fmt-toml          # Auto-format TOML files
just check-fmt-toml    # Check TOML formatting
```

### JSON and YAML formatting

```sh
just fmt-prettier          # Auto-format JSON/YAML files
just check-fmt-prettier    # Check JSON/YAML formatting
```

### Spell checking

```sh
just check-spelling    # Check spelling with cspell
```

### Run all formatting checks

```sh
just check-fmt-all     # Check Rust, TOML, JSON/YAML formatting and spelling
just fmt-all           # Auto-format everything
```

### Linting

Rust linting uses **stable** toolchain clippy

```sh
just lint             # Run clippy with -D warnings
```

### Dependency linting

```sh
just deny                     # Check advisories, bans, licenses, and sources
just deny check='advisories'  # Check only advisories
```

## Testing

```sh
just test               # Run all tests with all features
just test-doc           # Run doc tests (requires DOCS_RS=1)
just test-hack          # Test all feature combinations (requires cargo-hack)
just build-and-test-docs    # Build and test documentation
```

### Coverage

```sh
just coverage-html          # Generate HTML coverage report
just coverage-lcov          # Generate lcov coverage report
just coverage-html-fresh     # Clean and generate HTML coverage report
just coverage-lcov-fresh     # Clean and generate lcov coverage report
```
