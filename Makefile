.PHONY: build build-docker repl

build: update-default-nix
	nix-build release.nix

repl: update-default-nix
	nix-shell --pure shell.nix --run "cabal repl lib:dm"

build-docker: update-default-nix
	nix-build --attr docker-container-slim docker.nix

update-default-nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix
