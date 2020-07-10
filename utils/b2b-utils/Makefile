.PHONY: install-musl-target build-musl

build-musl:
	git rev-parse HEAD > src/version
	cargo build --release --target x86_64-unknown-linux-musl

install-musl-target:
	rustup target add x86_64-unknown-linux-musl
