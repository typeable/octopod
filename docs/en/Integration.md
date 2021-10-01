# Integration into existing CI/CD pipelines


You likely already have some form of CI integration with your version control system, such as *GitHub Action* or *Travis CI*, to run various checks on your code. Most of these services are set up by providing what is essentially just a shell script that is run under specific conditions.

You might want to automate deployments even further ― you might want deployments to be automatically created and updated when developers create and update *Pull Requests*.

_Octopod_ can be interacted with through the _octo CLI_ tool. This tool can be easily called from within a *CI* script.

## ✨ Creating deployments

To create a deployment (given that you have already obtained a *Docker Image* and uploaded it to your _Image Registry_ in one of the previous *CI* steps) you simply need to call _octo CLI_ with the following arguments:

```bash
octo create -n $NAME -t $IMAGE_TAG
```

`$NAME` is the name of the deployment you want to create. You can set it to be the name of the branch for example.

`$IMAGE_TAG` is the _tag_ of the docker image you want to deploy.

## 🚀 Updating deployments

Updating deployments is done using the same arguments, but you need to call `create` command, instead of the `update` command:

```bash
octo update -n $NAME -t $IMAGE_TAG
```
