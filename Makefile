.PHONY: build-backend build-octo-cli build-frontend backend-docs repl shell ghcid ghcid-cli ghcid-frontend push-octopod run-backend-dev run-frontend-dev

build-backend:
	nix-build . -A octopod-backend.components.exes.octopod-exe -j auto

build-octo-cli:
	nix-build . -A octo-cli.components.exes.octo -j auto

build-frontend:
	nix-build . -A projectCross.ghcjs.hsPkgs.octopod-frontend.components.exes.frontend -o frontend-result -j auto

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
