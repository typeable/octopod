.PHONY: build build-docker repl shell ghcid push-dms

build: update-default-nix
	nix build nixpkgs.haskellPackages.dm -I nixpkgs=nix

repl: update-default-nix
	nix-shell --pure shell.nix --run "cabal repl lib:dm" -I nixpkgs=nix

build-docker: update-default-nix update-b2b-helm-nix
	nix build nixpkgs.dms-container -I nixpkgs=nix -o dms-docker
	nix build nixpkgs.dmc-container -I nixpkgs=nix -o dmc-docker

update-default-nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." -I nixpkgs=nix > default.nix

update-b2b-helm-nix:
	nix-shell -p vgo2nix --run 'cd b2b-helm/tool && vgo2nix && cd ../..'

shell: update-default-nix
	nix-shell -I nixpkgs=nix

ghcid: update-default-nix
	nix-shell --run 'ghcid -c "cabal new-repl"' -I nixpkgs=nix

push-dms: build-docker
	./release.sh
