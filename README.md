# DM

Multi-staging Deployment Manager. Consists of the client (DMC) and server (DMS) parts.

# Installation

```bash
make build
```

# Interact with DMC

```bash
$ result/bin/dmc-exe
Missing: (create | list | edit | destroy | update)

Usage: dmc-exe (create | list | edit | destroy | update)
```

# Interact with DMS

```bash
$ result/bin/dms-exe
Missing: --port INT --db TEXT --db-pool-size INT

Usage: dms-exe --port INT --db TEXT --db-pool-size INT
```
