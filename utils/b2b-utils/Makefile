.PHONY: install-musl-target build-musl

build-musl:
	cargo build --release --target x86_64-unknown-linux-musl

install-musl-target:
	rustup target add x86_64-unknown-linux-musl
