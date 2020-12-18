.PHONY: build-backend build-octo-cli build-frontend docs backend-docs frontend-docs repl shell shell-ghcjs ghcid ghcid-cli ghcid-frontend push-octopod

build-backend:
	nix-build . -A ghc.octopod-backend

build-octo-cli:
	nix-build . -A ghc.octo-cli

build-frontend:
	nix-build . -A ghcjs.octopod-frontend -o frontend-result

docs: backend-docs frontend-docs

backend-docs:
	nix-build . -A ghc.octopod-backend.doc

frontend-docs:
	nix-build . -A ghcjs.octopod-frontend.doc

repl:
	nix-shell . -A shells.ghc --run "cabal repl lib:octopod-backend"

shell:
	nix-shell . -A shells.ghc

shell-ghcjs:
	nix-shell . -A shells.ghcjs

ghcid-backend:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl octopod-backend"'

ghcid-cli:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl octo-cli"'

ghcid-frontend:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl octopod-frontend -fdevelopment --ghc-options=-Wwarn" --warnings --test 'Main.main''

push-octopod:
	./build.sh build-and-push latest
