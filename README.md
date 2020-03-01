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
