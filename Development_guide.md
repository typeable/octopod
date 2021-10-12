# Development guide

## Git flow

Feature branches are merged into `master`. There is no `develop` branch.
## Building

### Nix Installation

Everything is built with [nix](https://nixos.org). To build the project you will need to install it.

```bash
curl https://nixos.org/nix/install | sh
```

### Nix cache

#### Haskell.nix cache

To speedup initial project builds you will want to set up the haskell.nix binary nix cache – append the following to `/etc/nix/nix.conf`:

```
binary-caches = https://hydra.iohk.io https://cache.nixos.org/
binary-cache-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
binary-caches-parallel-connections = 40
```

#### Octopod cache

The Octopod cache will also be useful to speed up builds:

1. Install [Cachix](https://cachix.org):

   ```bash
   nix-env -iA cachix -f https://cachix.org/api/v1/install
   ```
2. Add cache:
   ```bash
   cachix use octopod
   ```

## Development

We have written a `Makefile` with common targets used during development.

### Building

- `build-backend` – builds a release backend executable.
- `build-octo-cli` – builds a release octo CLI executable. NOTE: this is not the octo CLI executable that is used for distribution but the dependencies are close enough for development purposes.
- `build-frontend` – build the frontend release.

### Development

For development, we have set up `ghcid` commands that rebuild the project every time you make a change. The targets should self-explanatory:

- `ghcid-backend`
- `ghcid-cli`
- `ghcid-frontend`

### Running the project locally

We have two commands to run the backend and frontend in the `Makefile`:

- `run-backend-dev`

   Builds a production version of the backend server, runs database migrations and starts the server with mock control scripts. You can see the used config in [`dev/dev_backend.sh`](./dev/dev_backend.sh).

   ### NOTE:

   1. You need to have a [Postgres](https://www.postgresql.org) database running on `localhost:5432` (the default port).

   2. You need to create an empty `octopod` database.

   3. You also need to have an `octopod:octopod` user set up as it will be used by the server to access the database.

      The easiest way to do this is by running the following command after you have _Postgres running_:
      ```bash
      psql -c "CREATE ROLE IF NOT EXISTS octopod WITH PASSWORD 'octopod' SUPERUSER LOGIN;"
      ```

   4. You will need [sqitch](https://sqitch.org) installed on your system as it will be used to run migrations on the database.

- `run-frontend-dev`

   Build a production version of the frontend and runs it locally, pointing it to the locally running backend server.

   ### NOTE:

   You need to have [_Caddy 2_](https://caddyserver.com/v2) installed on your system as it is automatically used as a proxy.

### Stack

For convenience, the repo currently also contains a `stack.yaml` that can be used for development. It is only used to build the macOS octo CLI release but supports building both octo CLI and the _Octopod Server_ in an environment close enough to the release environment to be useful during development if you prefer stack.
