# Octopod Server deployment guide

## Installation options
There are several options to install Octopod depending on your needs.

### If you have a Kubernetes cluster
You can install Octopod in any Kubernetes cluster using our [Helm chart](../../charts/octopod).


Your cluster must satisfy the following requirements:

#### Mandatory requirements

- Kubernetes version >= 1.12.0 <= 1.22.0
- PVC support (for PostgreSQL persistence)
- [NGINX Ingress](https://kubernetes.github.io/ingress-nginx/) contoller version <= 0.49.3 installed. NGINX Ingress controller v1.x.x is not currently supported

#### Optional requirements

- Cert Manager ([cert-manager](https://cert-manager.io/docs/installation/)) installed, if you want to get SSL certificates from Let's Encrypt automatically.

After ensuring that your cluster satisfies the requirements you can follow [the Helm installation instruction](../../charts/octopod/README.md) provided with our Helm chart.

### Running Ocopod in production considerations.

You must consider several things before running Octopod in production and onboarding your team.

#### DNS records

Octopod will create a lot of on-demand environments and they must be reachable for your team. Usually it implies creating a lot of DNS records, pointing to Octopod managed environments. This process must be automated.

We highly recommend you to use [ExternalDNS](https://github.com/kubernetes-sigs/external-dns) as a very versatile solution for DNS records automation.
In certain cases, however, it isn't possible to use ExternalDNS. In this case you can use a wildcard DNS record, pointing to the service endpoint of your ingress controller. We do not recommend using wildcard DNS records. Even though they are much easier to implement, they could lead to hard-to-trace errors and they are also implementation-dependent.

#### Certificates

If you want to request SSL certificates dynamically from Let's Encrypt you must be aware of their [limits](https://letsencrypt.org/docs/rate-limits/). Because of these limits we recommend you to use a wildcard certificate for all Octopod deployments. The wildcard certificate has its limitations, like being valid only for one subdomain. This implies a need for planning your deployments DNS naming.

#### Resources

This is the most obvious one of the three, but you need to plan your cluster capacity based on your team needs. Setting [resource limits](https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/) for your workloads is one of the best practices of using Kubernetes, and you probably have done this already.
Also consider using autoscaling since it really helps to reduce costs in highly dynamic environments.


### If you want to try it locally

You can use [octopod_local_install.sh](../../octopod_local_install.sh) script to bootstrap kind cluster with octopod installed in it.

Before running the script make sure that you have following tools installed:
- [docker](https://docs.docker.com/engine/install/)
- [kind](https://kind.sigs.k8s.io/docs/user/quick-start/#installation)
- [kubectl](https://kubernetes.io/docs/tasks/tools/#kubectl)
- [helm 3](https://helm.sh/docs/intro/quickstart/#install-helm)

> **ℹ️ NOTE:** using Octopod involves deploying many instances of some service (many instances of Wordpress in our example). This means, it will use up a lot of resources. Many local installations of Docker and Kubenetes have resource limits set quite low by default. If you experience performance issues you should try increasing the resource limits. Here are some useful links:
> - Docker resource limits in Docker Desktop for Mac – [docs.docker.com/desktop/mac/#resources](https://docs.docker.com/desktop/mac/#resources)
> - Docker resource limits in Docker Desktop for Windows – [docs.docker.com/desktop/windows/#resources](https://docs.docker.com/desktop/windows/#resources)
> - General Docker resource constraint documentation – [docs.docker.com/config/containers/resource_constraints](https://docs.docker.com/config/containers/resource_constraints/)

After you have all of the necessary tools installed, you can run the script:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/typeable/octopod/master/octopod_local_install.sh)"
```

Script will expose octopod ui at `octopod.lvh.me`.

`lvh.me` it's a special domain name which always resolves to `127.0.0.1`.

## What next?

Now you may want to check how to [install Helm charts](Helm-based_deployment_guide.md) with Octopod.

<br />

<p align="center">
  <i>Star the project of you like it</i>
</p>

<p align="center"><a href="https://typeable.io"><img src="../../img/typeable_logo.svg" width="177px"></img></a></p>
