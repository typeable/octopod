.PHONY: build repl

build: update-default-nix
	nix-build release.nix

repl: update-default-nix
	nix-shell --pure shell.nix --run "cabal repl lib:dm"

update-default-nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix
