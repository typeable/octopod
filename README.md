# DM

Multi-staging Deployment Manager (DM). MD consists of the client and server parts (DMC and DMS  accordingly).

# Installation

1. Install Nix

```bash
curl https://nixos.org/nix/install | sh
```

2. Build the project

```bash
make build
```

# Interact with DMC

```bash
$ result/bin/dmc-exe --help
DMC

Usage: dmc-exe (create | list | edit | destroy | update | info)

Available options:
  -h,--help                Show this help text

Available commands:
  create
  list
  edit
  destroy
  update
  info
```

# Interact with DMS

```bash
$ result/bin/dms-exe --help
DMS

Usage: dms-exe --port INT --db TEXT --db-pool-size INT

Available options:
  -h,--help                Show this help text
```

# How to deploy a new staging

```bash
dmc create --name STAGING_NAME -t DOCKER_IMAGE_TAG -e b2b-app.env.ENV_VAR1=env_val1 -e b2b-app.env.ENV_VAR2=env_val2
```

Note that in our ECR(hosted docker registry) we use a convention of
DOCKER_IMAGE_TAG = GIT_SHA. So, if this build was successfully built on github
actions CI and docker image was pushed to ECR, you can deploy it without any
intermediate steps.

# How to override an environment variable in an application

```bash
dmc edit --name STAGING_NAME
```

`dmc edit` opens an `$EDITOR` where you can override staging variables. To
override an environment variable `FOO` with a value `foo`, add a line like this:

```
b2b-app.env.FOO=foo
```

To commit the changes, save the file and quit your default `$EDITOR`. `:wq` in
vim. Note that to discard your changes, you have to exit your `$EDITOR` with a
not-zero exit code. You can do this by typing `:cq` in vim.
