# Octopod

[Octopod](https://github.com/typeable/octopod) is a fully open-source self-hosted solution for managing multiple deployments in a Kubernetes cluster with a user-friendly web interface. Managing deployments does not require any technical expertise.

## TL;DR
```console
$ helm repo add typeable https://typeable.github.io/octopod/
$ helm repo update
$ kubectl create namespace octopod-deployment
$ helm install octopod typeable/octopod --set octopod.baseDomain="your-domain.com"
```

## Introduction

This chart bootstraps Octopod deployment in a [Kubernetes](http://kubernetes.io) cluster using the [Helm](https://helm.sh) package manager.

## Prerequisites

### Mandatory

- Kubernetes version >= 1.12.0 <= 1.22.0
- PVC support (for PostgreSQL persistence)
- [NGINX Ingress](https://kubernetes.github.io/ingress-nginx/) contoller version <= 0.49.3 installed. NGINX Ingress controller v1.x.x is not currently supported

### Optional

- Cert Manager ([cert-manager](https://cert-manager.io/docs/installation/)) installed, if you want to get SSL certificates from Let's Encrypt automatically.
- Cluster issuer ([ACME Issuer](https://cert-manager.io/docs/configuration/acme/#creating-a-basic-acme-issuer)) created. By default we assume that it would be named `letsencrypt`. You can change it by setting `ingress.tls.clusterIssuer` parameter.


## Installing the Chart
This chart will not create or delete any namespaces for you.

You'll need to create 2 namespaces before installing:

1. A namespace in which Octopod itself will be installed:
   ```console
   $ kubectl create namespace octopod
   ```
2. A namespace in which Octopod will deploy all your deployments (configured in `octopod.deploymentNamespace`):
   ```console
   $ kubectl create namespace octopod-deployment
   ```
  
To install the chart with the release name `my-release` execute:

```console
$ helm repo add typeable https://typeable.github.io/octopod/
$ helm repo update
$ helm -n octopod install my-release typeable/octopod --set octopod.baseDomain="your-domain.com"
```

The command deploys Octopod on the Kubernetes cluster in the default configuration inside octopod namespace. The [Parameters](#parameters) section lists the parameters that can be configured during installation.

## Uninstalling the Chart

To uninstall/delete the `my-release` deployment:

```console
$ helm -n octopod delete my-release
```

The command removes all the Kubernetes components except PVCs associated with the postgres chart, and deletes the release.

## Note about generated values
Some values (such as passwords) in this chart (and its dependencies) are generated automatically, but due to [the limitation in Helm](https://github.com/helm/charts/issues/5167) the values are changing on every upgrade. To prevent this you must set these values explicitly by providing them via `--set` flags or in the [values file](https://helm.sh/docs/chart_template_guide/values_files/).

These values are:
- `postgresql.postgresqlPassword` ― main db password
- `postgresql.postgresqlPostgresPassword` ― password for "postgres" user
- `octopod.cliAuthSecret` ― auth header for octo cli tool
- `octopod.uiAuthSecret` ― basic auth secret for ui->octopod communication

Note: if these values are not provided, the `helm upgrade` command can fail or Octopod will not work after the upgrade.

## Parameters

The following tables lists the configurable parameters of the Octopod chart and their default values.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| affinity | object | `{}` | Octopod deployment affinity |
| controlScripts.image.pullPolicy | string | `"IfNotPresent"` | control scripts image pull policy |
| controlScripts.image.repository | string | `"typeable/octopod-helm-example"` | Control scripts image repository (you probably want to use your own control scripts) |
| controlScripts.image.tag | float | `1.1` | Control scitpts image tag |
| fullnameOverride | string | `""` | Override chart full name  |
| image.pullPolicy | string | `"IfNotPresent"` | Octopod image pull policy |
| image.repository | string | `"typeable/octopod"` | Octopod image repository |
| image.tag | string | `""` | Octopod image tag (default values is taken from chart metadata) |
| imagePullSecrets | list | `[]` | Pull secrets if you want to use private registry  |
| ingress.app.annotations | object | `{}` | Additional ingress annotations for app service |
| ingress.app.host | string | `null` | Hostname override for app service by default it will be generated using your baseDomain |
| ingress.enabled | bool | `true` | Create ingress objects or not |
| ingress.ingressClass | string | `"nginx"` | Ingress class |
| ingress.powerApp.annotations | object | `{}` | Additional ingress annotations for power-app service  |
| ingress.powerApp.host | string | `null` | Hostname override for powerapp by default it will be generated using your baseDomain |
| ingress.tls.clusterIssuer | string | `"letsencrypt"` | Name of cluster issuer you want to use |
| ingress.tls.enabled | bool | `true` | Use https for all services |
| ingress.ui.annotations | object | `{}` | Additional ingress annotations for ui service |
| ingress.ui.host | string | `null` | Hostname override for main UI by default it will be generated using your baseDomain |
| ingress.ws.annotations | object | `{}` | Additional ingress annotations for ws service |
| ingress.ws.host | string | `null` | Hostname override for websockets ingress by default it will be generated using your baseDomain |
| kubernetesDashboard.enabled | bool | `false` | Set this if you are using kubernetes dashboard and want to enable Details featire of octopod |
| kubernetesDashboard.url | bool | `""` | URL to your kubernetes dashboard |
| nameOverride | string | `""` | Name ovveride (default is Release.Name) |
| nodeSelector | object | `{}` | Node selector if you want octopod to use specific nodes onn your cluster |
| octopod.archiveRetention | int | `1209600` |  |
| octopod.baseDomain | string | `""` | Domain that will be used as a ase for Octopod deploymets and ingress hosts|
| octopod.cliAuthSecret | string | `nil` | Auth Header for accessing octopod using octo CLI |
| octopod.uiAuthSecret | string | `nil` | Basic auth secret for securing communcation between octopod UI and backend API  |
| octopod.deploymentNamespace | string | `"octopod-deployment"` | Name of a namespace which will be used for all Octopod deployments (you need to create it yourself) |
| octopod.env | object | `{}` | key value map for supplying additional environment variables for octopod or your control scripts |
| octopod.migrations.enabled | bool | `true` | Enable or not automatic DB schema migrations |
| octopod.projectName | string | `"Octopod"` | Project name |
| octopod.statusUpdateTimeout | int | `600` | Time to wait before deployment is marked as failed |
| octopod.vaultEnv | object | `{}` | key value map for reference kv secrets stored in Hashicorp's Vault |
| podAnnotations | object | `{}` | Additional pod annotations |
| podSecurityContext | object | `{}` | Additional security context |
| postgresql.enabled | bool | `true` | Use bitnami postgres chart |
| postgresql.postgresqlDatabase | string | `"octopod"` | Database name for octopod |
| postgresql.postgresqlUsername | string | `"octopod"` | Octopod DB username |
| rbac.create | bool | `true` | Creates ClusterRoles and Bindings for Octopod service account |
| replicaCount | int | `1` | Number of Octopod replicas |
| resources.limits.cpu | string | `"200m"` | CPU limits |
| resources.limits.memory | string | `"512Mi"` | Memory limits |
| resources.requests.cpu | string | `"200m"` | CPU requests |
| resources.requests.memory | string | `"256Mi"` | Memory requests |
| securityContext.runAsGroup | int | `1000` | Use non-root GID |
| securityContext.runAsUser | int | `1000` | Use non-root UID |
| service.ports.app | int | `4000` | App service port |
| service.ports.powerApp | int | `4443` | Power app service port |
| service.ports.ui | int | `80` | UI service port |
| service.ports.ws | int | `4020` | WebSockets service port  |
| service.type | string | `"ClusterIP"` | Octopod service type |
| serviceAccount.annotations | object | `{}` | Additional anotations for Octopod's SA |
| serviceAccount.create | bool | `true` | Create ServiceAccount |
| serviceAccount.name | string | `""` | ServiceAccount name override |
| sqitch.image.pullPolicy | string | `"IfNotPresent"` | squitch image pull policy |
| sqitch.image.repository | string | `"typeable/sqitch"` | squitch image repository |
| sqitch.image.tag | string | `"v2.0.0"` | squitch image tag |
| tolerations | list | `[]` | Octpod deploymet tolerations |
| vault.enabled | bool | `false` | Set to true if you want to add Vault agent annotations (vaultEnv must contain at least one reference) |
| vault.clusterName | string | `""` | Name of your cluster authentication method in Vault |


Specify each parameter using the `--set key=value[,key=value]` argument to `helm install`. For example,

```console
$ helm -n octopod install my-release \
  --set controlScripts.image.repository=registry.example.com/control,controlScripts.image.tag=0.0.1 \
    .
```

The above command sets control scripts image to your custom repository

Alternatively, a YAML file that specifies the values for the parameters can be provided while installing the chart. For example,

```console
$ helm install my-release -f values.yaml .
```

> **Tip**: You can use the default [values.yaml](values.yaml)

## Configuration and installation details

If you want to have authentication for Octopod UI you can use project like [Oauth2-Proxy](https://github.com/oauth2-proxy/oauth2-proxy) or directly use your oauth provider.
After that you can add following annotations to UI ingress (considering that your oauth provider installed on oauth.example.com):
```yaml
ingress:
  ui:
    annotations:
      nginx.ingress.kubernetes.io/auth-url: "https://oauth.example.com/oauth2/auth"
      nginx.ingress.kubernetes.io/auth-signin: "https://oauth.example.com/oauth2/start?rd=/redirect/$http_host$request_uri"
```

<br />

<p align="center">
  <i>Star the project of you like it</i>
</p>

<p align="center"><a href="https://typeable.io"><img src="../../img/typeable_logo.svg" width="177px"></img></a></p>
