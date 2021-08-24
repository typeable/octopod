# Generic scripts

This is a complete suite of the octopod control scripts to deploy helm charts.
You can get builded docker images [here](https://hub.docker.com/r/typeable/octopod-generic-utils)

### Contents

- create - to install a helm release
- update - to upgrade a helm release
- archive - to uninstall a helm release
- info - to print comma-separated pairs which will be placed as links in octpod ui
- cleanup - to delete dangling pvcs and letsencrypt certs left after `helm uninstall` command
- check - to check if resources created by helm are healthy. Right now only deployments and statefuls sets are checked
- archive_check - to check if `helm uninstall` really deleted the release
- init - to initialize something. This is the only script not being run by octopod, but as an init container.
- tag_check - to check that images in helm release are present in registries before invoking create script 

### Parameters

All scipts accept common parametes which are passed by Octopod when it invokes them.
Also several environment variables are used to parametrize default behavior

- HELM_BIN - path to a helm executable
- KUBECTL_BIN - path to a kubectl executable
- HELM_USER - (optional) user for a private helm registry
- HELM_PASS - (optional) password for a private helm registry
- DEFAUTLS - json with the default parameters (described below)

#### Default parameters
- default_overrides - array with key value pairs which will be passed as a `--set` flags for helm for each deployment
- chart_repo_url - url for a chart repo
- chart_repo_name - name of a chart repo (could be any string)
- chart_version - version of a chart you want to install
- chart_name - name of a chart you want to install

These parameters, if set up in the `DEFAULTS` variable, will be passed to every deployment until overridden in the "deployment overrides" section of an Octopod deployment configuration.