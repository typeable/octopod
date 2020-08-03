.PHONY: build

tag=1.45.0

build:
	git rev-parse HEAD > src/version
	docker pull ekidd/rust-musl-builder:$(tag)
	docker run --rm -it -v "$$PWD":/home/rust/src ekidd/rust-musl-builder:$(tag) cargo build --release
