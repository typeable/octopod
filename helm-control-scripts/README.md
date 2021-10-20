# Generic scripts

This is a complete suite of Octopod control scripts to deploy helm charts.
You can get pre-built docker images from [typeable/octopod-helm-control-scripts](https://hub.docker.com/repository/docker/typeable/octopod-helm-control-scripts).

### Contents

- create – a script to install a helm release
- update – a script to upgrade a helm release
- archive – a script to scale to zero all deployments and statefulsets
- archive_check – a script to check if all resources scaled to zero correctly
- unarchive - scale all deployments and statefulsets back (right now it will only scale to 1 and not to previous value)
- info – a script to print comma-separated pairs which will be placed as links in the Octopod Web UI
- cleanup – a script to delete dangling PVCs and letsencrypt certs left after `helm uninstall` command
- check – a script to check if resources created by helm are healthy. Right now only deployments and statefuls sets are checked
- init – a script to initialize something. This is the only script not run by Octopod, but as an init container.
- config_check – a script to check that configuration (such as image tag name) passed to Octopod is correct. This script is invoked before deployment creation.
- app_overrides - a scirpt which returns a list of default app overrides wichh were passed in default_overrides parameter described below.
- deployment_overrides - a scirpt which returns a list of default deployment overrides from default paramaters option.
- deployment_keys - a scripts which returns a list of the possible deployment overrides keys user can use.
- app_keys - a scripts which returns a list of the possible app overrides keys. These are parsed from an output of the `helm show values` command.

### Parameters

All scipts accept common parametes which are passed by Octopod when it invokes them.

Also, several environment variables are used to parametrize the default behavior:

- HELM_BIN – the path to the `helm` executable
- HELM_USER – the (optional) user for a private helm registry
- HELM_PASS – the (optional) password for a private helm registry
- DEFAUTLS – the json with the default parameters (described below)
- HELM_ON_INIT_ONLY - run helm add and update repository as a part of init script execution only. Otherwise `helm repo add` and `helm repo update` will be executed every time before any other helm command.
- INGRESS_HOST_KEY - key name which will be populated with domain name generated for the Octopod deployment. Defaults to `ingress.hostname`.

#### Default parameters
- default_overrides – an array with key-value pairs which will be passed as a `--set` flags for helm for each deployment
- chart_repo_url – the url for a chart repo
- chart_version – the version of a chart you want to install
- chart_name – the name of a chart you want to install

These parameters, if set up in the `DEFAULTS` variable, will be passed to every deployment unless overridden in the "deployment overrides" section of an Octopod deployment configuration.

### Authorization notes

Most of the control scripts require that credentials for various services be passed to them to function properly. Some credentials, like the Helm ones, are quite obvious, but others are not.

`config_check`, in addition to the Helm credentials, requires access to your docker registry to check the availability of the docker image tags. 

Right now only plain docker registries and ECR are supported. ECR clients need to have valid AWS credentials set via [environment variables](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-envvars.html), but if you are using EKS you don't need to add any additional configuration.

Private registries, like Harbor with authorization haven't been tested with this set of scripts. If you are using a private registry and have troubles with the `config_check` script, please file an issue.

<br />

<p align="center">
  <i>Star the project of you like it</i>
</p>

<p align="center"><a href="https://typeable.io"><img src="../img/typeable_logo.svg" width="177px"></img></a></p>
