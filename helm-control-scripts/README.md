# Generic scripts

This is a complete suite of Octopod control scripts to deploy helm charts.
You can get pre-built docker images from [hub.docker.com/r/typeable/octopod-generic-utils](https://hub.docker.com/r/typeable/octopod-generic-utils).

### Contents

- create – a script to install a helm release
- update – a script to upgrade a helm release
- archive – a script to uninstall a helm release
- info – a script to print comma-separated pairs which will be placed as links in the Octopod Web UI
- cleanup – a script to delete dangling PVCs and letsencrypt certs left after `helm uninstall` command
- check – a script to check if resources created by helm are healthy. Right now only deployments and statefuls sets are checked
- archive_check – a script to check if `helm uninstall` really deleted the release
- init – a script to initialize something. This is the only script not run by Octopod, but as an init container.
- tag_check – a script to check that images in helm release are present in registries before invoking create script 

### Parameters

All scipts accept common parametes which are passed by Octopod when it invokes them.

Also, several environment variables are used to parametrize the default behavior:

- HELM_BIN – the path to the `helm` executable
- KUBECTL_BIN – the path to the `kubectl` executable
- HELM_USER – the (optional) user for a private helm registry
- HELM_PASS – the (optional) password for a private helm registry
- DEFAUTLS – the json with the default parameters (described below)

#### Default parameters
- default_overrides – an array with key-value pairs which will be passed as a `--set` flags for helm for each deployment
- chart_repo_url – the url for a chart repo
- chart_repo_name – the name of a chart repo (could be any string)
- chart_version – the version of a chart you want to install
- chart_name – the name of a chart you want to install

These parameters, if set up in the `DEFAULTS` variable, will be passed to every deployment unless overridden in the "deployment overrides" section of an Octopod deployment configuration.