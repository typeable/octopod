.PHONY: build-backend build-octo-cli build-frontend docs backend-docs frontend-docs repl shell shell-ghcjs ghcid ghcid-cli ghcid-frontend push-octopod

build-backend:
	nix-build . -A ghc.octopod-backend -j auto

build-octo-cli:
	nix-build . -A ghc.octo-cli -j auto

build-frontend:
	nix-build . -A ghcjs.octopod-frontend -o frontend-result -j auto

docs: backend-docs frontend-docs

backend-docs:
	nix-build . -A ghc.octopod-backend.doc -j auto

frontend-docs:
	nix-build . -A ghcjs.octopod-frontend.doc -j auto

repl:
	nix-shell . -A shells.ghc --run "cabal repl lib:octopod-backend" -j auto

shell:
	nix-shell . -A shells.ghc -j auto

shell-ghcjs:
	nix-shell . -A shells.ghcjs -j auto

ghcid-backend:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl octopod-backend"' -j auto

ghcid-cli:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl octo-cli"' -j auto

ghcid-frontend:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl octopod-frontend -fdevelopment --ghc-options=-Wwarn" --warnings --test 'Main.main'' -j auto

push-octopod:
	./build.sh build-and-push latest
