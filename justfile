# To use this, first run `cargo install just`
# Then run the install subcommand: `just install`
# Running `just` is equivalent to running `just all` because it's the first command

export CARGO_TERM_COLOR := "always"

all: rustup-show test clippy fmt sort audit

rustup-show:
  rustup show

test:
  cargo insta test --workspace

clippy:
  cargo clippy

fmt:
  cargo fmt --all -- --check

sort:
  cargo sort --check --workspace

audit:
  cargo audit

install:
  cargo install --locked cargo-sort cargo-audit
