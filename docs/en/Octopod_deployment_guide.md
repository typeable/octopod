# Octopod Server deployment guide

<details>
  <summary>Table of contents</summary>

- [Installing required utilities](#installing-required-utilities)
- [Setting up your cluster](#setting-up-your-cluster)
  - [General utilities](#general-utilities)
  - [Tiller (Helm)](#tiller-helm)
    - [Cluster access privileges](#cluster-access-privileges)
  - [A word about TLS](#a-word-about-tls)
- [Downloading project sources code](#downloading-project-sources-code)
- [Creating required namespaces](#creating-required-namespaces)
- [Creating required _Service Accounts_](#creating-required-service-accounts)
  - [Creating the actual service account](#creating-the-actual-service-account)
  - [Giving the appropriate _Service Account_ roles](#giving-the-appropriate-service-account-roles)
- [Web UI authentication secrets](#web-ui-authentication-secrets)
- [_octo CLI_ authentication certificates](#octo-cli-authentication-certificates)
  - [Creating SSL certificates](#creating-ssl-certificates)
  - [Enabling SSL passthrough](#enabling-ssl-passthrough)
- [Setting up DNS](#setting-up-dns)
  - [Deploying _Octopod_ on localhost](#deploying-octopod-on-localhost)
- [Installing _Octopod_ infrastructure](#installing-octopod-infrastructure)
  - [Installing the appropriate _Storage Class_](#installing-the-appropriate-storage-class)
  - [Installing the actual infrastructure](#installing-the-actual-infrastructure)
- [Installing _Octopod Server_](#installing-octopod-server)

</details>

## Installing required utilities

Installing _Octopod Server_ in your cluster will require that you have the following tools installed on your system:
1. [_kubectl_][kubectl]
2. [_helm 2_][helm]

## Setting up your cluster

### General utilities

_Octopod Server_ requires the following utilities to be installed in your cluster:

1. [_Ingress Nginx_][ingress-nginx]
2. [_Cert Manager_][cert-manager]

_Octopod Server_ require the following minimal resources to function properly: 2 CPU, 2 GB of RAM. Make sure you have sufficient resources in your cluster.

By default _Octopod Server_ will be deployed on nodes with the `role=stand` label. Please make sue you have the appropriate label set in your cluster:

```bash
kubectl label node <your_node> role=stand
```

### Tiller (Helm)

[_Tiller_][tiller] is a cluster-side service used by [_helm 2_][helm] to manage deployments. The easiest way to install it is using the following command:

```bash
helm init
```

#### Cluster access privileges

When installing _Octopod Server_ you might encounter [a problem with cluster access privileges](https://github.com/helm/helm/issues/5100) related to [_Tiller_][tiller].

To give sufficient privileges to [_Tiller_][tiller] you can use the following commands:

```bash
kubectl create -n kube-system serviceaccount tiller
kubectl --namespace kube-system create clusterrolebinding tiller-cluster-admin --clusterrole=cluster-admin --serviceaccount=kube-system:tiller
kubectl --namespace kube-system patch deploy tiller-deploy -p '{"spec":{"template":{"spec":{"serviceAccount":"tiller"}}}}'
```

### A word about TLS

To function properly _Octopod_ needs to generate three TLS certificates for the three subdomains it will be using. [_Cert Manager_][cert-manager] creates TLS certificates through [_Let’s Encrypt_][lets-encrypt] and [_Let’s Encrypt_][lets-encrypt] has [a limit on the amount of certificates][lets-encrypt-rate-limits] you can issue within a given time interval. If you exceed this limit you will start getting a _too many registrations for this IP_ error. If that is the case moving the [_Cert Manager_][cert-manager] _Pod_ might help.

## Downloading project sources code

To download the source code required to install _Octopod Sever_ you will need to clone the git repository:

```bash
git clone --branch master https://github.com/typeable/octopod.git /tmp/octopod
```

## Creating required namespaces

_Octopod_ uses the following namespaces in your cluster:

1. `deployment` – as the name would suggest your deployments will be installed in this namespace
2. `octopod` – this namespace will be used to install the _Octopod_ infrastructure

To create the two namespaces you can use these commands:

```bash
kubectl create namespace deployment
kubectl create namespace octopod
```

## Creating required [_Service Accounts_][kubernetes-service-account]

### Creating the actual service account

_Octopod Server_ requires an `octopod` [_Service Account_][kubernetes-service-account] to function. You can create it using the following command:

```bash
kubectl create -n octopod serviceaccount octopod
```

### Giving the appropriate _Service Account_ roles

1. If you are planning to use [_helm 2_][helm] in your [_Control scripts_](Control_scripts.md) to deploy your deployments, you will need to give appropriate permissions to the `octopod` _Service Account_:
   ```bash
   cd /tmp/octopod/charts
   helm install --name octopod-helm-access ./helm-access
   ```

2. If you are planning to delete [_Persistent Volumes Claims_][kubernetes-pvc] in your [_Control scripts_](Control_scripts.md) (might be useful for the `cleanup` script), you will need to give appropriate permissions to the `octopod` _Service Account_:

   ```bash
   cd /tmp/octopod/charts
   helm install --name octopod-pvc-control ./pvc-control
   ```

3. If you are planning to use _Octopod_ to delete unused certificates in your [_Control scripts_](Control_scripts.md) (might be useful for the `cleanup` script), you will need to give appropriate permissions to the `octopod` _Service Account_:

   ```bash
   cd /tmp/octopod/charts
   helm install --name octopod-cert-control ./cert-control
   ```

4. If you are planning to use [_kubedog_][kubedog] to check the state of your deployments in your [_Control scripts_](Control_scripts.md) (might be useful for the `check` script), you will need to give appropriate permissions to the `octopod` _Service Account_:

   ```bash
   cd /tmp/octopod/charts
   helm install --name octopod-kubedog-access ./kubedog-access
   ```

## Web UI authentication secrets

[Authentication](Security_model.md#web-ui-authentication) between _Octopod Server_ and the _Web UI_ is done through _Basic Auth_. This implies that there needs to be a username and password associated with it.

You can generate the username and password, and push into your cluster using the following command (of course you will want to generate a secure pair):

```bash
username="octopod"
password="password" # Please change it to a more secure password
kubectl create secret generic octopod-basic-auth -n octopod --from-literal=auth=$(htpasswd -bn $username $password)
```

## _octo CLI_ authentication certificates

### Creating SSL certificates

[Authentication](Security_model.md#octo-cli-authentication) between _octo CLI_ and _Octopod Server_ is performed through self-signed SSL certificates.

You can generate the certificates and push them into your cluster using the following commands:

```bash
mkdir certs
(cd certs && \
openssl req -x509 -newkey rsa:4096 -keyout server_key.pem -out server_cert.pem -nodes -subj "/CN=localhost/O=Server" && \
openssl req -newkey rsa:4096 -keyout client_key.pem -out client_csr.pem -nodes -subj "/CN=Client" && \
openssl x509 -req -in client_csr.pem -CA server_cert.pem -CAkey server_key.pem -out client_cert.pem -set_serial 01 -days 3650)
kubectl create configmap octopod-certs -n octopod --from-file=./certs
```

After executing these command you will find a new `certs` directory containing the certificates used for authentication between _octo CLI_ and _Octopod Server_. `client_key.pem` and `client_cert.pem` should then be [passed to _octo CLI_ through environment variables](Octo_user_guide.md#tls_cert_path-and-tls_key_path).

### Enabling SSL passthrough

Since we use custom self-signed SSL certificates for authentication, we will need the certificates used with requests to be passed to the server as-is without any modification. This is not support in default [_ingress-nginx_][ingress-nginx] configurations so you will most likely need to modify it manually.

Enabling SSL passthrough in [_ingress-nginx_][ingress-nginx] can be done by adding the `--enable-ssl-passthrough` command-line argument to the [_ingress-nginx_][ingress-nginx] config in your cluster.

To do this you can execute a command similar to this (you will need to lookup the names of the namespace and the deployment in your particular cluster):

```bash
kubectl edit deploy -n ingress-nginx ingress-nginx-controller
```

An editor with a YAML config should open up. You will need to modify to it have, among other things, this parameter:

```yaml
spec:
  ...
  template:
    ...
    spec:
      ...
      containers:
      ...
      - args:
        ...
        - --enable-ssl-passthrough
```


## Setting up DNS

You will need to set up DNS records to point subdomains of your domain to the IP address of your cluster. The DNS record should look something like this:

```
*.octo.example.com A 1.2.3.4
octo.example.com A 1.2.3.4
```

### Deploying _Octopod_ on localhost

If you are deploying locally and don't have a separate domain you are trying set up, the lvh.me domain can be useful – it is set up to point to `localhost` and you can use it to work with subdomains. Even so, deploying a fully-functional version of _Octopod_ on `localhost` is non-trivial and will require modifying the deployment _Charts_ to disable HTTPS redirects. (This guide does not cover that.)

## Installing _Octopod_ infrastructure

### Installing the appropriate _Storage Class_

Before installing the infrastructure you will first need to make sure you have a [_Storage Class_][kubernetes-storage-classes] named `default` installed in your cluster. You can check installed [_Storage Classes_][kubernetes-storage-classes] with the following command:

```bash
kubectl get storageclass
```

If you do not have it, you will need to install it. Installing the [_Storage Class_][kubernetes-storage-classes] in [_minikube_][minikube] can be done with the following command (you will need to modify it to suit your cluster hosting provider):

```bash
cat <<EOF | kubectl apply -f-
---
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: default
provisioner: k8s.io/minikube-hostpath
reclaimPolicy: Delete
volumeBindingMode: Immediate
EOF
```

### Installing the actual infrastructure

The only infrastructure _Octopod_ currently requires is _PostgreSQL_. You can install it in your cluster using the following command:

```bash
cd /tmp/octopod/charts
helm upgrade --install octopod-infra ./octopod-infra --namespace octopod\
    --wait --timeout 600 --debug
```

## Installing _Octopod Server_

To install _Octopod Server_ in your cluster you will need to customize the variables in the following script and run it:

 ```bash
cd /tmp/octopod/charts

#################################################
# Octopod Images Setup
#
# you probably don't need to change it
#################################################

registry="typeable"
tag="1.0"
image="octopod"
octo_image="octo"


#################################################
# General Octopod Setup
#################################################

# The name of your project – only used to display in the Web UI
project_name="MyProject"

# The email used to register Let's Encrypt SSL certificates
acme_registration_email="certbot@example.com"


#################################################
# Control Scripts Setup
#
# if you are just testing things out you can paste the values
# from the Helm Deployment Guide example
#################################################

# The name of the registry with control scripts
utils_registry="registry_name"

# The name of the image with control scripts
utils_image="utils"

# The tag of the image to use
utils_image_tag="1.0"


#################################################
# Web UI OAuth Authentication
#
# These parameters are passed to ingress-nginx to
# enable authentication for user accessing the
# Web UI.
#
# You can use OAuth2 Proxy to provide OAuth2 authentication.
#
# For more information see the Security Model doc.
#
# You can leave both these variables blank to disable
# authentication in your Web UI altogether.
#################################################

# URL for the OAuth authentication service
auth_url="https://oauth.exmaple.com/oauth2/auth"

# URL for the login page on the OAuth authentication service
auth_signin="https://oauth.exmaple.com/oauth2/start?rd=/redirect/$http_host$request_uri"


#################################################
# Domain Setup
#################################################

# The domain from which the Web UI should be served
domain="octo.example.com"

# The domain from which the user API should be served
# (used by the Web UI)
app_domain="api.octo.example.com"

# The domain from which the WebSocket notification service should be served
# (used by the Web UI)
ws_domain="ws.octo.example.com"

# The domain from which the power user API should be served
# (used by octo CLI)
power_app_domain="power.octo.example.com"

# The domain under which deployment subdomains should be created
base_domain="octo.example.com"


#################################################
# Basic Auth Setup
#
# These parameters should match the ones used in the
# "Web UI authentication secrets" step
#################################################

username="octopod"
password="password"


#################################################
# Other Setup
#################################################

# NOTE: on macOS you will need to replace `sha256sum` with `shasum -a 256`
sha256_sum=$(sha256sum octopod/values.yaml octopod/templates/* | awk '{print $1}' | sha256sum | awk '{print $1}')

base64_of_username_and_password=$(echo -n "$username:$password" | base64)
status_update_timeout=600


#################################################
# Actual installation in the cluster
#################################################

helm upgrade --install octopod ./octopod \
    --namespace octopod \
    --set "global.deploy_checksum=$sha256_sum" \
    --set "global.image_prefix=$registry" \
    --set "global.image_tag=$tag" \
    --set "global.image=$image" \
    --set "global.octo_image=$octo_image" \
    --set "global.utils_image_prefix=$utils_registry" \
    --set "global.utils_image=$utils_image" \
    --set "global.utils_image_tag=$utils_image_tag" \
    --set "global.acme_registration_email=$acme_registration_email" \
    --set "global.auth_url=$auth_url" \
    --set "global.auth_signin=$auth_signin" \
    --set "basic_auth_token=$base64_of_username_and_password" \
    --set "project_name=$project_name" \
    --set "domain=$domain" \
    --set "app_domain=$app_domain" \
    --set "ws_domain=$ws_domain" \
    --set "power_app_domain=$power_app_domain" \
    --set "base_domain=$base_domain" \
    --set "status_update_timeout=$status_update_timeout" \
    --wait --timeout 600 --debug
 ```

[kubectl]: https://kubernetes.io/docs/tasks/tools/install-kubectl/
[helm]: https://v2.helm.sh/docs/using_helm/#installing-helm
[ingress-nginx]: https://kubernetes.github.io/ingress-nginx
[cert-manager]: https://cert-manager.io/docs/
[kubernetes-service-account]: https://kubernetes.io/docs/tasks/configure-pod-container/configure-service-account
[kubernetes-pvc]: https://kubernetes.io/docs/concepts/storage/persistent-volumes/#expanding-persistent-volumes-claims
[kubernetes-storage-classes]: https://kubernetes.io/docs/concepts/storage/storage-classes
[minikube]: https://kubernetes.io/ru/docs/tasks/tools/install-minikube/
[tiller]: https://v2.helm.sh/docs/install/
[kubedog]: https://github.com/werf/kubedog
[lets-encrypt]: https://letsencrypt.org
[lets-encrypt-rate-limits]: https://letsencrypt.org/docs/rate-limits
