# Development guide

## Git flow

`master` contains the latest "release version" only.

All development should be done in the `develop` branch.

Feature PRs are created to the `develop` branch and merged with all commits **squashed**. This leads to us having every commit in the `develop` branch corresponds to exactly one feature or bug fix.

When a release is ready, the `develop` branch is merged into the `master` branch using **rebase and merge**. This makes the `master` branch have every commit be a feature or bug fix. Merging to master triggers a CI script that collects all commits since the last merge and creates a new release with a change log of all commits.

## Building

### Nix Installation

Everything is built with [nix](https://nixos.org). To build the project you will need to install it.

```bash
curl https://nixos.org/nix/install | sh
```

### Nix cache

#### Reflex platform cache

To speedup initial project builds you will want to set up the Reflex Platform binary nix cache – append the following to `/etc/nix/nix.conf`:

```
binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
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

### Frontend proxy

The frontend should be accessed through a proxy. We have set up [caddy](https://caddyserver.com) configs to ease development. You will need place an `octopod-config.json` file at the root of the repository containing a [config](../../charts/octopod/templates/octopod-nginx-configmap.yaml#L15-L20). `app_auth` can be an arbitrary string – it will not affect anything when running locally.

### Stack

For convenience, the repo currently also contains a `stack.yaml` that can be used for development. It is only used to build the macOS octo CLI release but supports building both octo CLI and the _Octopod Server_ in an environment close enough to the release environment to be useful during development if you prefer stack.
