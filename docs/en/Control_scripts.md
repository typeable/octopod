# Control scripts

<details>
  <summary>Table of contents</summary>

- [General behavior](#general-behavior)
- [Scripts](#scripts)
  - [🔁 init](#-init)
    - [Description](#description)
    - [Sample implementation](#sample-implementation)
  - [✨ create](#-create)
    - [Description](#description-1)
    - [Execution example](#execution-example)
    - [Sample implementation](#sample-implementation-1)
  - [🔧 update](#-update)
    - [Description](#description-2)
    - [Execution example](#execution-example-1)
    - [Sample implementation](#sample-implementation-2)
  - [🗃 archive](#-archive)
    - [Description](#description-3)
    - [Execution example](#execution-example-2)
    - [Sample implementation](#sample-implementation-3)
  - [✅ check](#-check)
    - [Description](#description-4)
    - [Execution example](#execution-example-3)
    - [Sample implementation](#sample-implementation-4)
  - [🚮 cleanup](#-cleanup)
    - [Description](#description-5)
    - [Execution example](#execution-example-4)
    - [Sample implementation](#sample-implementation-5)
  - [🗃✅ archive_check](#-archive_check)
    - [Description](#description-6)
    - [Execution example](#execution-example-5)
    - [Sample implementation](#sample-implementation-6)
  - [🐋✅ tag_check](#-tag_check)
    - [Description](#description-7)
    - [Execution example](#execution-example-6)
  - [🔔 notifications](#-notifications)
    - [Description](#description-8)
    - [Execution example](#execution-example-7)

</details>

## General behavior

All _control scripts_ receive input as CLI arguments. After executing the required logic they must finish with an _exit code_ of `0` if no errors have occurred and the required actions have all completed. If there was an error and some steps were not executed, the *script* should exit with an *exit code* **distinct from `0`**. Any non-zero exit code will indicate an error.

Everything the _scripts_ write to _stdout_ and _stderr_ will be collected and stored. DevOps engineers can then view these logs from the _octo CLI_, should that be needed.

> *NOTE: Logs from `check`, `archive_check` and `tag_check` are not collected because they are called very often.*

There are four arguments that are passed to **all** *scripts*. The first three arguments come from the [_Kubernetes ConfigMap_][configmap]:
* `--project-name` – the name of the project. It is supplied mostly for informational purposes and can be useful for sending notifications if that is necessary.
* `--base-domain` – the base domain. It can be useful for generating the URLs of deployments.
* `--namespace` – The namespace in which the deployment should be created.
* `--name` – The name of the deployment supplied in the _Web UI_. It can be useful for generating the deployment URL.

<a id="star"></a>*NOTE:* If an argument is marked with a ⭐, it means that the argument can be passed any number of times.

## Scripts

### 🔁 init

#### Description

This script is called **once** during the creation of the `Octopod Server` *Kubernetes Pod* to set up the proper environment to execute all other scripts.

It is guaranteed that this script will be called **before** any of the other scripts.

You could, for example, set up access to your *version control system*, *cloud providers*, etc. This can be achieved by saving the configuration into files in the `$HOME` directory.

Unlike all other scripts, this script receives no arguments.

#### Sample implementation

```bash
mkdir $HOME/.ssh
echo -e "Host github.com\nHostname github.com\nPort 22\nUser git\nIdentityFile $HOME/.ssh/deploy.key" > $HOME/.ssh/config
echo "MY_DEPLOY_KEY" > $HOME/.ssh/deploy.key"
```

### ✨ create

#### Description

Creates a new deployment in the _Kubernetes_ cluster.

This script receives the following additional command-line arguments as input:
* `--tag` – The _Docker Image tag_ that should be deployed. (In practice you can use some other string that identifies a version of your system to deploy – you will need to process it accordingly in the script.)
* `--app-env-override` [⭐](#star) – App-level overrides. These overrides should be passed to the server being deployed. These overrides are specified in the _Web UI_. They are passed in the format of `KEY=VALUE` pairs.
* `--deployment-override` [⭐](#star) – Deployment-level overrides. These overrides should be used to set up the deployment environment itself, rather than be passed to the server being deployed. These overrides are specified in the _Web UI_. They are passed in the format of `KEY=VALUE` pairs.

#### Execution example

The script might be called something like this:

```bash
create --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button" --tag "c9bbc3fcc69e5aa094bca110c6f79419ab7be77a" --app-env-override "EMAIL_TOKEN=123123" --app-env-override "SECRET_BUTTON_ENABLED=True" --deployment-override "FANCY_DATABASE=True"
```

#### Sample implementation

```bash
helm upgrade --install --namespace "$namespace" "$name" "$deployment_chart" \
    --set "global.project-name=$project_name" \
    --set "global.base-domain=$base-domain" \
    --set "app.tag=$tag" \
    --set "app.env.foo=$app_env_override_1" \
    --set "app.bar=$deployment_override_1" \
    --wait \
    --timeout 300
```

### 🔧 update

#### Description

Updates a deployment in _Kubernetes_ to a new *Docker Image tag*.

This script receives the same additional command-line arguments as [`create`](#-create):
* `--tag` – The _Docker Image tag_ that should be deployed. (In practice you can use some other string that identifies a version of your system to deploy – you will need to process it accordingly in the script.)
* `--app-env-override` [⭐](#star) – App-level overrides. These overrides should be passed to the server being deployed. These overrides are specified in the _Web UI_. They are passed in the format of `KEY=VALUE` pairs.
* `--deployment-override` [⭐](#star) – Deployment-level overrides. These overrides should be used to set up the deployment environment itself, rather than be passed to the server being deployed. These overrides are specified in the _Web UI_. They are passed in the format of `KEY=VALUE` pairs.

#### Execution example

The script might be called something like this:

```bash
update --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button" --tag "c9bbc3fcc69e5aa094bca110c6f79419ab7be77a" --app-env-override "EMAIL_TOKEN=123123" --app-env-override "SECRET_BUTTON_ENABLED=True" --deployment-override "FANCY_DATABASE=True"
```

#### Sample implementation

```bash
helm upgrade --install --namespace "$namespace" "$name" "$deployment_chart" \
    --set "global.project-name=$project_name" \
    --set "global.base-domain=$base-domain" \
    --set "app.tag=$tag" \
    --set "app.env.foo=$app_env_override_1" \
    --set "app.bar=$deployment_override_1" \
    --wait \
    --timeout 300
```

### 🗃 archive

#### Description

"Archives" a deployment. This script should only free the computational resources used by the deployment ― it should remove the _Kubernetes Pods_, but not remove any _Persistent Volumes_ associated with the deployment. It is done this way to provide a period of time in which the user can recover a deployment in the state it was in.

Deleting the _Persistent Volume Claims_ should be done in the [`cleanup`](#-cleanup) script.

This script should in some sense be the inverse of [`create`](#-create) (up to _Persistent Volumes_).

This script receives only [the default command-line arguments](#general-behavior) as input.

#### Execution example

The script might be called something like this:

```bash
archive --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button"
```

#### Sample implementation

```bash
helm delete "$name" --purge
```

### ✅ check

#### Description

This script checks the status of the deployment.

If the script exits with `0`, it means that the deployment is healthy and up. If the script exits with a non-zero exit code, it means that the deployment is not healthy or down.

This script receives only [the default command-line arguments](#general-behavior) as input.

#### Execution example

The script might be called something like this:

```bash
check --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button"
```

#### Sample implementation

```bash
echo "{\"Deployments\": [{\"ResourceName\": \"app-${name}\", \"Namespace\": \"${namespace}\"}], \"StatefulSets\": [{\"ResourceName\": \"db-${name}\", \"Namespace\": \"${namespace}\"}]}" | \
    kubedog multitrack -t 3
```

### 🚮 cleanup

#### Description

Cleans up any persistent resources a deployment might have allocated, such as _Persistent Volumes_.

This script will always be called **after** [`archive`](#-archive) has been called on the given deployment.

This script receives only [the default command-line arguments](#general-behavior) as input.

#### Execution example

The script might be called something like this:

```bash
cleanup --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button"
```

#### Sample implementation

```bash
kubectl delete pvc -n $namespace -l "app=$name"
```

### 🗃✅ archive_check

#### Description

This script checks that a given deployment really has been archived and is no longer running.

If the scripts exits with `0`, it means that the deployment has been archived successfully. If the script exits with a non-zero exit code, it means that the deployment has not been archived.


This script receives only [the default command-line arguments](#general-behavior) as input.

#### Execution example

The script might be called something like this:

```bash
archive_check --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button"
```

#### Sample implementation

```bash
helm status $name
```

### 🐋✅ tag_check

#### Description

This script is called right before [`create`](#-create) and [`update`](#-update) scripts to check that a given _Docker Image tag_ exists. This can be useful since it can be very easy to make a typo in the _Docker Image tag_ and deployments are typically not instant. Implementing this script would allow the user of the _Web UI_ to instantly get an error specifically about a wrong _Docker Image tag_.

This script receives the following additional command-line arguments as input:
* `--tag` – The _Docker Image tag_ that should be checked.

#### Execution example

The script might be called something like this:

```bash
tag_check --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button" --tag "c9bbc3fcc69e5aa094bca110c6f79419ab7be77a"
```

### 🔔 notifications

#### Description

This script gets called every time a deployment changes its status (apart from creation and deletion). It might be useful if you want to send notifications about certain deployment status transitions. The complete list of statuses and their transitions can be found in the [technical architecture document](Technical_architecture.md#️-deployment-state-transitions).

It is optional and can be omitted altogether.

This script receives the following additional command-line arguments as input:
* `--tag` – The _Docker Image tag_ that should be deployed. (In practice you can use some other string that identifies a version of your system to deploy – you will need to process it accordingly in the script.)
- `--old-status` – The previous status the deployment was in.
- `--new-status` – The new status the deployment transitioned to.

#### Execution example

The script might be called something like this:

```bash
notification --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button" --tag "c9bbc3fcc69e5aa094bca110c6f79419ab7be77a" --old-status "UpdatePending" --new-status "Running"
```

[configmap]: https://kubernetes.io/docs/concepts/configuration/configmap/
