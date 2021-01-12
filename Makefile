.PHONY: build-backend build-octo-cli build-frontend docs backend-docs frontend-docs repl shell shell-ghcjs ghcid ghcid-cli ghcid-frontend push-octopod run-backend-dev run-frontend-dev

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

run-backend-dev: dev/certs/server_cert.pem dev/certs/server_key.pem
	./dev/dev_backend.sh `nix-build . -A ghc.octopod-backend`

run-frontend-dev: build-frontend
	caddy run

dev/certs/server_cert.pem dev/certs/server_key.pem:
	openssl req -x509 -newkey rsa:4096 -keyout dev/certs/server_key.pem -out dev/certs/server_cert.pem -nodes -subj "/CN=localhost/O=Server"

dev/certs/client_csr.pem dev/certs/client_key.pem:
	openssl req -newkey rsa:4096 -keyout dev/certs/client_key.pem -out dev/certs/client_csr.pem -nodes -subj "/CN=Client"

dev/certs/client_cert.pem: dev/certs/client_csr.pem dev/certs/server_cert.pem dev/certs/server_key.pem
	openssl x509 -req -in dev/certs/client_csr.pem -CA dev/certs/server_cert.pem -CAkey dev/certs/server_key.pem -out dev/certs/client_cert.pem -set_serial 01 -days 3650)
