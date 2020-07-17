# DM

Multi-staging Deployment Manager (DM). DM consists of the client and server parts (DMC and DMS  accordingly).

# Documentation checklist

1. [ ] Case study(Project Management Perspective). We discuss three common cases: production + staging, production + staging + dev boxes, production + multiple stagings.
2. [ ] Case study(Technical perspective). We consider different of approaches mentioned in 1. We cover isolation, reliability, time management.
3. [ ] Technical architecture. Description of all components. Diagrams. We should talk about the choice of running of top Kubernetes and Helm, asynchronous architecture, status updates.
4. [ ] User guide. Mentions all the major actions user can do, talks about container/app level overrides.
5. [ ] Deployment guide for Helm-based projects. Includes a description of all the necessary steps to deploy the DM in such an environment, `examples/helm` with code.
6. [ ] Deployment guide for Helmless projects. Includes a description of all the necessary steps to deploy the DM in such an environment, `examples/k8s` with code. (e.g. bash+curl)
7. [ ] Motivating example dealing with cloud APIs, secret storages projects. Includes a motivation and examples of writing a wrapper passing the secrets to the app managed by DM, `examples/secrets` with code.
8. [ ] Announcement blog post containing key points from 1 and 2, linking the documentation 3-6.
9. [ ] Video showcasing the common use-case: create a new staging, it ends up in the `running` status, we show the resources on the View Staging page, click on them and go visit the app. Then we edit it, let it update(we skip it in the video), go to the app again and showcase the change somehow.
10. [ ] Slideshow presentation accompanying the DM demo.
11. [ ] Better project title.
12. [ ] Project Icon. Depends on 11.
13. [ ] README.md mentioning all the documents mentioned above and cross-linking all documentation chapters.
14. [ ] Proper documentation in the code. Haddocks.
15. [ ] Information about the DM project available on the typeable.io website.
16. [ ] Security model and K8S RBAC configuration for the staging cluster
17. [ ] Superuser documentation - command line utils
18. [ ] Integration into existing development flows

# Nix Installation

```bash
curl https://nixos.org/nix/install | sh
```

# Development

Build the project

```bash
make build
```

# Interact with DMC

```bash
$ result/bin/dmc-exe --help
DMC

Usage: dmc-exe (create | list | edit | delete | update | info | cleanup |
                 restore | cleanarchive)

Available options:
  -h,--help                Show this help text

Available commands:
  create
  list
  edit
  delete
  update
  info
  cleanup
  restore
  cleanarchive
```

# Interact with DMS

```bash
$ result/bin/dms-exe --help
DMS

Usage: dms-exe --port INT --ui-port INT --db TEXT --db-pool-size INT
               --tls-cert-path TEXT --tls-key-path TEXT --tls-store-path TEXT

Available options:
  -h,--help                Show this help text
```

# Build and publish docker images to ECR

For a build and publish of Docker images you must have needed AWS permissions: access to SSM and ECR!

```
./release.sh <user|ci>
```

# Kubernetes installation

1. Deploy DMS's infra

```bash
./b2b-helm-tool -d deploy dms-infra
```

2. Clone b2b-helm to `/tmp`

```bash
git clone https://github.com/Aviora/b2b-helm.git /tmp/b2b-helm
```

3. Give needed permissions for DMS

```bash
cd /tmp/b2b-helm/charts/admin && helm install --name dm-helm-access ./helm-access
cd /tmp/b2b-helm/charts/admin && helm install --name dm-pvc-control ./pvc-control
cd /tmp/b2b-helm/charts/admin && helm install --name dm-cert-control ./cert-control
```

4. Deploy DMS

```bash
./b2b-helm-tool -d deploy dms:latest
```

# How to deploy a new staging

```bash
dmc create --name STAGING_NAME -t DOCKER_IMAGE_TAG -e ENV_VAR1=env_val1 -e ENV_VAR2=env_val2
```

Or use Docker:
```bash
docker run -ti --rm 560065381221.dkr.ecr.us-east-1.amazonaws.com/dmc:latest create --name STAGING_NAME -t DOCKER_IMAGE_TAG -e ENV_VAR1=env_val1 -e ENV_VAR2=env_val2
```

Note that in our ECR(hosted docker registry) we use a convention of
DOCKER_IMAGE_TAG = GIT_SHA. So, if this build was successfully built on github
actions CI and docker image was pushed to ECR, you can deploy it without any
intermediate steps.

# How to override an environment variable in an application

```bash
dmc edit --name STAGING_NAME
```

Or use Docker:
```bash
# use Vim
docker run -ti --rm 560065381221.dkr.ecr.us-east-1.amazonaws.com/dmc:latest edit --name STAGING_NAME

# use Emacs
docker run -ti -e EDITOR=emacs --rm 560065381221.dkr.ecr.us-east-1.amazonaws.com/dmc:latest edit --name STAGING_NAME
```

`dmc edit` opens an `$EDITOR` where you can override staging variables. To
override an environment variable `FOO` with a value `foo`, add a line like this:

```
FOO=foo
```

To commit the changes, save the file and quit your default `$EDITOR`. `:wq` in
vim. Note that to discard your changes, you have to exit your `$EDITOR` with a
not-zero exit code. You can do this by typing `:cq` in vim.

# How clean up Docker cache

DMC using a certificate for authentication in DMS.
After deploy of new DMS's version needs to use a new certificate.

To make sure the actual image is being used, remove the old docker image and pull the new one before with:

```bash
docker rmi 560065381221.dkr.ecr.us-east-1.amazonaws.com/dmc:latest
docker pull 560065381221.dkr.ecr.us-east-1.amazonaws.com/dmc:latest
```

# Develop

Cache set up:
```
binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
binary-caches-parallel-connections = 40
```

Enter shell to work on the backend:
```
make shell
```

Enter shell to work on the frontend (with GHC):
```
make shell
```

Enter shell to work on the frontend (with GHCJS):
```
make shell-ghcjs
```
