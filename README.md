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
$ result/bin/dmc-exe
Missing: (create | list | edit | destroy | update | info)

Usage: dmc-exe (create | list | edit | destroy | update | info)
```

# Interact with DMS

```bash
$ result/bin/dms-exe
Missing: --port INT --db TEXT --db-pool-size INT

Usage: dms-exe --port INT --db TEXT --db-pool-size INT
```
