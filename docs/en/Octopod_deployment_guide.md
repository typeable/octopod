# Octopod Server deployment guide

<details>
  <summary>Table of contents</summary>

- [Installation options](#installation-options)
  - [If you have a kubernetes cluster](#if-you-have-a-kubernetes-cluster)
  - [If you want to try it locally](#if-you-want-to-try-it-locally)
</details>

## Installation options
There are several options to install octopod depending on your needs.

### If you have a kubernetes cluster
You can install octopod on any kubernetes cluster using the [Helm chart](../../charts/helm3/octopod).
Your cluster must satisfy following requirements:
- PVC support
- Ingress contoller ([ingress-nginx](https://kubernetes.github.io/ingress-nginx/)) installed
- Kubernetes version >= 1.19.0

After checking the requirements you can follow [instruction](../../charts/helm3/octopod/README.md) provided with the helm chart to install octopod on your cluster.

### If you want to try it locally
