# Control scripts


## General behavior

All _control scripts_ receive input as CLI arguments. After executing the required logic they must finish with an _exit code_ of `0` if no errors have occurred and the required actions have all completed. If there was an error and some steps were not executed, the *script* should exit with an *exit code* **distinct from `0`**. Any non-zero exit code will indicate an error.

Control scripts can be roughly split into two categories: (1) control scripts involved in the deployment lifecycle and (2) service control scripts which handle everything else.

## Deployment lifecycle scripts

_stdout_ and _stderr_ output from the most important scripts involved in the deployment lifecycle will be collected and stored. DevOps engineers can then view these logs from the _octo CLI_, should that be needed.

All of the _scripts_ in this category receive the exact same exact same set of command line arguments.

There are four "static" arguments that are passed to all *scripts* in this category. The first three arguments come from the [_Kubernetes ConfigMap_][configmap]:
* `--project-name` – the name of the project. It is supplied mostly for informational purposes and can be useful for sending notifications if that is necessary.
* `--base-domain` – the base domain. It can be useful for generating the URLs of deployments.
* `--namespace` – The namespace in which the deployment should be created.
* `--name` – The name of the deployment supplied in the _Web UI_. It can be useful for generating the deployment URL.

Additionally, _scripts_ in this category receive the deployment configuration – a set of key/value pairs. There are two types of configurations that can be set from the _Web UI_: deployment
configuration and application configuration.

Both types of configurations can have any number of key/value pairs. Every pair is passed a separate command line argument:

* `--application-config <KEY>=<VALUE>` represents an application configuration key/value pair.
* `--deployment-config <KEY>=<VALUE>` represents an deployment configuration key/value pair.

Any of these two command line arguments can be passed any number of times.

So, a _script_ might be called something like this:

```bash
create --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button" --application-config "EMAIL_TOKEN=123123" --application-config "SECRET_BUTTON_ENABLED=True" --deployment-config "FANCY_DATABASE=True"
```

### ✨ create

#### Description

Creates a new deployment in the _Kubernetes_ cluster.

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

### 🎛✅ config_check

#### Description

This script is called right before [`create`](#-create) and [`update`](#-update) scripts to check that the given set of arguments is valid input for other commands. This is run synchronously and the _stdout_ text is report to the user in the _Web UI_. This means that the command should run relatively fast. You can check thing like that the needed arguments are present and that the required docker tags exist in the docker registry.

As with other commands, exit code `0` indicates that everything is fine and a non-zero exit code indicates that something is wrong and _stdout_ should be shown to the user.

### 👀 info

#### Description

This script returns user-facing metadata about a deployment. Currently, the metadata consists of URLs that are relevant for the deployment. Things like the deployment URL, the URL to view logs, and the database URL.

The script should return the metadata as a two-column CSV table to _stdout_:

```
app,https://foo.example.com
api,https://api.foo.example.com
```

#### Sample implementation

```bash
echo "app,https://${name}.example.com"
echo "api,https://api.${name}.example.com"
```
### 🔧 update

#### Description

Updates a deployment in _Kubernetes_ to a new *Docker Image tag*.

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

A good way of doing this is to scale the number of replicas of your server to 0.

#### Sample implementation

```bash
helm delete "$name" --purge
```

### 🗃↩️ unarchive

#### Description

This command should undo whatever the [`archive`](#-archive) command did. So, if the [`archive`](#-archive) command scaled the server to 0 replicas, this command should scale it back up to the number of replicas required for normal operation.

### ✅ check

#### Description

This script checks the status of the deployment.

If the script exits with `0`, it means that the deployment is healthy and up. If the script exits with a non-zero exit code, it means that the deployment is not healthy or down.

You can specify exactly what error occured using exit codes:
- `1` – a generic failure.
- `2` – the deployment is partially down (some containers are unhealthy).
- `3` – tag mismatch, the deployment has not been updated to the expected version.

#### Sample implementation

```bash
echo "{\"Deployments\": [{\"ResourceName\": \"app-${name}\", \"Namespace\": \"${namespace}\"}], \"StatefulSets\": [{\"ResourceName\": \"db-${name}\", \"Namespace\": \"${namespace}\"}]}" | \
    kubedog multitrack -t 3
```

### 🚮 cleanup

#### Description

Cleans up any persistent resources a deployment might have allocated, such as _Persistent Volumes_.

This script will always be called **after** [`archive`](#-archive) has been called on the given deployment.

#### Sample implementation

```bash
kubectl delete pvc -n $namespace -l "app=$name"
```

### 🗃✅ archive_check

#### Description

This script checks that a given deployment really has been archived and is no longer running.

If the scripts exits with `0`, it means that the deployment has been archived successfully. If the script exits with a non-zero exit code, it means that the deployment has not been archived.

#### Sample implementation

```bash
helm status $name
```

## Service control scripts

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

### 🔔 notifications

#### Description

This script gets called every time a deployment changes its status (apart from creation and deletion). It might be useful if you want to send notifications about certain deployment status transitions. The complete list of statuses and their transitions can be found in the [technical architecture document](Technical_architecture.md#️-deployment-state-transitions).

It is optional and can be omitted altogether.

This script receives the following additional command-line arguments as input:
* `--project-name` – the name of the project. It is supplied mostly for informational purposes and can be useful for sending notifications if that is necessary.
* `--base-domain` – the base domain. It can be useful for generating the URLs of deployments.
* `--namespace` – The namespace in which the deployment should be created.
* `--name` – The name of the deployment supplied in the _Web UI_. It can be useful for generating the deployment URL.
* `--old-status` – The previous status the deployment was in.
* `--new-status` – The new status the deployment transitioned to.

The last two arguments can have one of the following values:
- `Running`
- `GenericFailure`
- `TagMismatch`
- `PartialAvailability`
- `CreatePending`
- `UpdatePending`
- `ArchivePending`
- `Archived`
- `CleanupFailed`

#### Execution example

The script might be called something like this:

```bash
notification --project-name "Cactus store" --base-domain "cactus-store.com" --namespace "cactus" --name "orange-button" --old-status "UpdatePending" --new-status "Running"
```

### 🎛🛠️ deployment_config

#### Description

This script is used to generate default deployment configuration key/value pairs. This configuration is shown to the user in the _Web UI_ with the ability to modify it.

The key/value pairs are read from the _stdout_ stream of the script. It should be formatted as a CSV file with two columns.

This script receives the following arguments:

* `--project-name` – the name of the project. It is supplied mostly for informational purposes and can be useful for sending notifications if that is necessary.
* `--base-domain` – the base domain. It can be useful for generating the URLs of deployments.
* `--namespace` – The namespace in which the deployment should be created.

#### Sample implementation

```bash
echo "image.tag,"
echo "application_api_version,1.0"
```

### 🎛🛠️🔑 deployment_config_keys

#### Description

This script is used to get the set of keys that the deployment configuration can have. This is useful to suggest possible values when the user wants to add new values to in the _Web UI_.

The keys are read from the _stdout_ stream of the script. It should be formatted as a CSV file with one column (a list separated by newlines).

This script receives the same arguments as the [`deployment_config`](#-deployment_config) command.


#### Sample implementation

```bash
echo "image.tag"
echo "application_api_version"
echo "database_version"
```

### 🎛📱 application_config

#### Description

This script is used to generate default application configuration key/value pairs. This configuration is shown to the user in the _Web UI_ with the ability to modify it.

The key/value pairs are read from the _stdout_ stream of the script. It should be formatted as a CSV file with two columns.

This script receives the same arguments as the [`deployment_config`](#-deployment_config) command.

Additionally, the script receives the deployment configuration key/value pairs as described in the ["Deployment lifecycle scripts" section](#deployment-lifecycle-scripts): any number of `--deployment-config <KEY>=<VALUE>` arguments.

#### Sample implementation

```bash
echo "image.tag,"
echo "application_api_version,1.0"
```

### 🎛📱🔑 application_config_keys

#### Description

This script is used to get the set of keys that the application configuration can have. This is useful to suggest possible values when the user wants to add new values to in the _Web UI_.

The keys are read from the _stdout_ stream of the script. It should be formatted as a CSV file with one column (a list separated by newlines).

This script receives the same arguments as the [`application_config` script](#-application_config).

#### Sample implementation

```bash
echo "image.tag"
echo "application_api_version"
echo "database_version"
```

[configmap]: https://kubernetes.io/docs/concepts/configuration/configmap/
