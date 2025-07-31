# spell-checker: ignore nofile nocapture

this_dir := `realpath .`
args := ''
msrv := '1.74.1'

# Check and fix format of rust files (Uses: 'cargo +nightly')
[group('formatting')]
fmt:
    cargo +nightly fmt --all

# Check and fix format of toml files (Uses: 'taplo')
[group('formatting')]
fmt-toml:
    taplo fmt

# Check and fix format of json and yaml files (Uses: 'prettier' or 'npx prettier')
[group('formatting')]
fmt-prettier:
   {{ prettier_bin }} --write '**/*.json' '**/*.yml' --ignore-path '.gitignore' --ignore-path '.prettierignore'

# Run all fmt rules (Depends on: fmt, fmt-toml, fmt-prettier)
[group('formatting')]
fmt-all: fmt fmt-toml fmt-prettier

# Check format of rust files (Uses: 'cargo +nightly')
[group('formatting')]
check-fmt:
    cargo +nightly fmt --all --check

# Check format of toml files with `taplo` (Uses: 'taplo')
[group('formatting')]
check-fmt-toml:
    taplo fmt --check --verbose

# Check format of json and yaml files (Uses: 'prettier' or 'npx prettier')
[group('formatting')]
check-fmt-prettier:
    {{ prettier_bin }} --check --log-level warn '**/*.json' '**/*.yml' --ignore-path '.gitignore' --ignore-path '.prettierignore'

# Check spelling with cspell (Uses: 'cspell' or 'npx cspell')
[group('formatting')]
check-spelling:
    {{ cspell_bin }} lint .

# Run all format checkers (Depends on: check-fmt, check-fmt-toml, check-fmt-prettier, check-spelling)
[group('formatting')]
check-fmt-all: check-fmt check-fmt-toml check-fmt-prettier check-spelling

# Run clippy (Uses 'cargo +stable')
[group('lint')]
lint:
    cargo +stable clippy --all-features --all-targets -- -D warnings

# Run cargo deny check (Uses 'cargo-deny')
[group('dependencies')]
deny +check='all':
    cargo deny check {{ if args != '' { args } else { '' } }} {{ check }}

# Build a package with the optional toolchain (Uses: 'cargo')
[group('build')]
build package:
    cargo build -p {{ package }} {{ if args != '' { args } else { '' } }}

# Build the documentation (Uses: 'cargo')
[group('build')]
build-docs:
    DOCS_RS=1 cargo doc --all-features --no-deps --workspace --document-private-items

# Run all tests in a package. (Uses: 'cargo')
[group('test')]
test package:
    cargo test --package {{ package }} {{ if args != '' { args } else { '' } }}

# Run all doc tests (Uses: 'cargo')
[group('test')]
test-doc:
    DOCS_RS=1 cargo test --all-features --doc

# Test all packages. This excludes client request and benchmark tests which need to be run separately (Uses: 'cargo')
[group('test')]
test-all:
    cargo test --workspace --exclude client-request-tests

# Check minimal version requirements of dependencies. (Uses: 'cargo-minimal-versions')
[group('dependencies')]
minimal-versions:
    cargo minimal-versions check --workspace --all-targets --ignore-private --direct

# Bump the version of either-or-both or the MSRV (Uses: 'cargo', 'grep')
[group('chore')]
bump config part:
    #!/usr/bin/env -S sh -e
    current_version=$(bump-my-version show-bump --config-file ".bumpversion/{{ config }}.toml" --ascii | grep -Eo '^[0-9]+(\.[0-9]+\.[0-9]+)?')
    new_version=$(bump-my-version show-bump --config-file ".bumpversion/{{ config }}.toml" --ascii | grep -Po '(?<={{ part }} - )[0-9]+(\.[0-9]+\.[0-9]+)?')

    bump-my-version bump --no-commit --config-file ".bumpversion/{{ config }}.toml" {{ part }}
    just args="--all-features --lib" build either-or-both
