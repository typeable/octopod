.PHONY: build repl shell ghcid push-dms

build:
	nix-build . -A ghc.dm-backend

repl:
	nix-shell . -A shells.ghc --run "cabal repl lib:dm-backend"

update-b2b-helm-nix:
	nix-shell -p vgo2nix --run 'cd b2b-helm/tool && vgo2nix && cd ../..'

shell:
	nix-shell . -A shells.ghc

shell-ghcjs:
	nix-shell . -A shells.ghcjs

ghcid:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl dm-backend"'

ghcid-frontend:
	nix-shell . -A shells.ghc --run 'ghcid -c "cabal new-repl dm-frontend"'

push-dms: build-docker
	./release.sh
