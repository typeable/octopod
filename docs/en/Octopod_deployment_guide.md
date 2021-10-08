# Octopod Server deployment guide

## Installation options
There are several options to install Octopod depending on your needs.

### If you have a Kubernetes cluster
You can install Octopod in any Kubernetes cluster using our [Helm chart](../../charts/octopod).


Your cluster must satisfy the following requirements:
- PVC support (for PostgreSQL persistence)
- Ingress contoller version <= 0.49.3 ([ingress-nginx](https://kubernetes.github.io/ingress-nginx/)) installed. Ingress controller v1 is not currently supported
- Kubernetes version >= 1.19.0
- Cert Manager ([cert-manager](https://cert-manager.io/docs/installation/)) installed
- Cluster issuer ([ACME Issuer](https://cert-manager.io/docs/configuration/acme/#creating-a-basic-acme-issuer)) created
- DNS is configured either using [ExternalDNS](https://github.com/kubernetes-sigs/external-dns) (recommended) or by creating a wildcard A record for your base domain that points to the Load Balancer IP-address

After ensuring that your cluster satisfies the requirements you can follow [the Helm installation instruction](../../charts/octopod/README.md) provided with our helm chart.

### If you want to try it locally

You can use [octopod_local_install.sh](../../octopod_local_install.sh) script to bootstrap kind cluster with octopod installed in it.

Before running the script make sure that you have following tools installed:
- [docker](https://docs.docker.com/engine/install/)
- [kind](https://kind.sigs.k8s.io/docs/user/quick-start/#installation)
- [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)
- [helm 3](https://helm.sh/docs/intro/quickstart/#install-helm)

After you have all of the necessary tools installed, you can run the script:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/typeable/octopod/master/octopod_local_install.sh)"
```

Script will expose octopod ui at `octopod.lvh.me`.

`lvh.me` it's a special domain name which always resolves to `127.0.0.1`.

## What next?

Now you may want to check how to [install helm charts](Helm-based_deployment_guide.md) with Octopod.

<br />

<p align="center">
  <i>Star the project of you like it</i>
</p>

<p align="center"><a href="https://typeable.io"><img src="../../img/typeable_logo.svg" width="177px"></img></a></p>
