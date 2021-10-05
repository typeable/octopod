# Octo CLI User Guide

## Environment variables

All commands _octo CLI_ executes require the executable to send authenticated requests to the _Octopod Server_. For this purpose _octo CLI_ needs both a way to reach your particular instance of _Octopod Server_, and a way for _Octopod Server_ to identify that you are allowed to make the given request.

### `OCTOPOD_URL`

`OCTOPOD_URL` is an environment variable _octo CLI_ reads to find your particular _Octopod Server_ installation. For example, it could contain `https://octopod-power-app.example.com:443`.

### `OCTO_AUTHORIZATION_HEADER`

`OCTO_AUTHORIZATION_HEADER` is an environment variable that should contain the `Authorization` header to use to authenticate requests to your Octopod server. You can obtain the header value by running the following command after installing Octopod:

```bash
kubectl -n octopod get secret octopod-cli-auth-secret -o jsonpath='{.data.cli-secret}' | base64 -d
```

## Commands

> <a id="star"></a>***NOTE:*** If an argument is marked with a ⭐, it means that the argument can be passed any number of times.

### create

#### Description

Creates a new deployment.

#### Options

- `-n,--name ARG` – The name of the deployment to create
- `-t,--tag ARG` – The _Docker tag_ to deploy
- `-e,--set-app-config ARG` [⭐](#star) – Set an application-level configuration. Expects a string in the format `KEY=VALUE`.
- `-o,--set-deployment-config ARG` [⭐](#star) – Set a deployment-level configuration. Expects a string in the format `KEY=VALUE`.

#### Usage example

```bash
$ octo create -n hello-octopod -t ca5fd1fe08389f6422a506a59b68a5272ac37ba6 -e KEY1=VALUE1 -e KEY2=VALUE2
```

### list

#### Description

Gets a list of all deployment names – both archived and active.

#### Options

This command does not require any arguments.

#### Usage example

```bash
$ octo list
hello-octopod
foo
bar
```

### archive

#### Description

Archives a given deployment.

#### Options

- `-n,--name ARG` – The name of the deployment to archive.

#### Usage example

```bash
$ octo archive -n hello-octopod
```

### update

#### Description

Updates the parameters of a given deployment.

#### Options

- `-n,--name ARG` – The name of the deployment to update
- `-t,--tag ARG` – The new _Docker tag_ to update the deployment to
- `-e,--set-app-config ARG` [⭐](#star) – Add a new or replace an existing application-level configuration. Expects a string in the format `KEY=VALUE`.
- `-E,--unset-app-env-configuration ARG` [⭐](#star) – Removes an existing application-level configuration.
- `-o,--set-deployment-config ARG` [⭐](#star) – Add a new or replace an existing deployment-level configuration. Expects a string in the format `KEY=VALUE`.
- `-O,--unset-deployment-configuration` [⭐](#star) – Removes an existing deployment-level configuration.

#### Usage example

```bash
$ octo update -n octopod -t 015f16ecf398fcadaac508c1855ae160af0969c4 -E KEY1 -e KEY2=VALUE22222 -a KEY3=VALUE8
```

### info

#### Description

Gets detailed information about a deployment, including a log of all preformed actions and the current parameters.

#### Options

- `-n,--name ARG` – The name of the deployment

#### Usage example

```bash
$ octo info -n hello-octopod
Current settings:
tag: v1
application configurations: app=1 (Public)

deployment configurations: dep=2 (Public)

metadata:
  app: https://ree.lvh.me

Last logs:
┏━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━┳━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━┓
┃     Created at      ┃ Action id ┃ Action ┃ App configurations  ┃ Deployment configurations ┃ Exit code ┃
┡━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━╇━━━━━━━━╇━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━┩
│ 2020-11-02T17:14:03 │     7     │ create │ app=1 (Public) │ dep=2 (Public)       │     1     │
├─────────────────────┼───────────┼────────┼────────────────┼──────────────────────┼───────────┤
│ 2020-11-02T19:01:02 │     8     │ update │ app=1 (Public) │ dep=2 (Public)       │     1     │
└─────────────────────┴───────────┴────────┴────────────────┴──────────────────────┴───────────┘
```

### cleanup

#### Description

Frees all resources used by a given archived deployment. It will not succeed if the deployment is not archived. You can not recover the deployment after this command.

#### Options

- `-n,--name ARG` – The name of the deployment

#### Usage example

```bash
$ octo cleanup -n hello-octopod
```

### restore

#### Description

Restores a previously archived deployment.

#### Options

- `-n,--name ARG` – The name of the deployment

#### Usage example

```bash
$ octo restore -n hello-octopod
```
#### Options

This command does not have any options.

#### Usage example

```bash
$ octo clean-archive
```

### logs

#### Description

Outputs the logs collected while running an action on a deployment. For example when deploying or updating a deployment.

#### Options

- `-a,--action ARG` – the id of the action to print logs for
- `-l,--log-type ARG` – the types of logs that should be printed. Possible values are: `stdout`, `stderr`, `all`. The default value is `all`.

#### Usage example

```
$ octo logs -a 13
		stdout:


		stderr:

error: Found argument '--deployment-configuration' which wasn't expected, or isn't valid in this context

USAGE:
    update --app-env-configuration <app-env-configuration>... --base-domain <base-domain> --name <name> --namespace <namespace> --project-name <project-name> --tag <tag>

For more information try --help
```
