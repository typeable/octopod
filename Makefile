.PHONY: build-backend build-octo-cli build-frontend backend-docs repl shell ghcid ghcid-cli ghcid-frontend push-octopod run-backend-dev run-frontend-dev

build-backend:
	nix-build . -A octopod-backend.components.exes.octopod-exe -j auto

build-octo-cli:
	nix-build . -A octo-cli.components.exes.octo -j auto

build-frontend:
	nix-build . -A octopod-frontend-pretty -o frontend-result -j auto

backend-docs:
	nix-build . -A octopod-backend.components.library.doc -j auto

repl:
	nix-shell --run "cabal repl lib:octopod-backend" -j auto

shell:
	nix-shell -j auto

ghcid-backend:
	nix-shell --run 'ghcid -c "cabal new-repl octopod-backend"' -j auto

ghcid-cli:
	nix-shell --run 'ghcid -c "cabal new-repl octo-cli"' -j auto

ghcid-frontend:
	nix-shell --run 'ghcid -c "cabal new-repl octopod-frontend -fdevelopment --ghc-options=-Wwarn" --warnings --test 'Main.main'' -j auto

push-octopod:
	./build.sh build-and-push latest

run-backend-dev:
	`nix-build dev -A backend -j auto`

run-frontend-dev:
	`nix-build dev -A frontend -j auto`

run-frontend-prod:
	`nix-build dev -A frontend --arg prod true -j auto`

run-caddy-for-ghcid:
	`nix-build dev -A caddyForGhcid -j auto`
