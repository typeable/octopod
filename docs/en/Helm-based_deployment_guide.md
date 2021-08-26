# Helm-based deployment guide

In this guide we'll show you how to deploy bitnami's [wordpress chart](https://github.com/bitnami/charts/tree/master/bitnami/wordpress) with Octopod.

## Your first deployment

### Install Octopod

Make sure that you have Octopod installed before going further. If you haven't, go check our [Octopod deployment guide](Octopod_deployment_guide.md).

Note: In this guide we assume that you have octopod installed on your local machine.
Open up Octopod in your browser and you'll see something like this:

![](../images/octopod_blank.png)

### Create the deployment

Click on the _New Deployment_ button:

![](../images/octopod_deployment_blank.png)

### The config

Here you can fill your deployment parameters. Let's have it filled up!

![](../images/octopod_deployment_filled.png)

**Name** ― we've chosen `wordpress`, but you can choose whatever name you like.

**Tag** ― `5.8.0`. We took the tag name from [chart parameters](https://github.com/bitnami/charts/blob/master/bitnami/wordpress/Chart.yaml#L4)

**App Overrides:**


|                Key | Value              |
| -----------------: | ------------------ |
|  `ingress.enabled` | `true`             |
| `ingress.hostname` | `wordpress.lvh.me` |


We took these overrides from the [chart documentation](https://github.com/bitnami/charts/tree/master/bitnami/wordpress#traffic-exposure-parameters). Basically you can tweak any parameters from there.

### Deploy

When you have it all filled out. Click _Save_ button and wait until the deployment transitions to a _running_ state:
![](../images/octopod_deployment_filled.png)

Now you can click on a _wordpress_ link in the _links_ section and you'll be redirected to your wordpress instance:
![](../images/wordpress_blank.png)

So here you have it, your first Octopod deployment!

## Going further

Right now you may be wondering, how did Octopod take wordpress from the bitnami repository when we haven't filled any repo information whatsoever? This is because we've set this up on the chart level [here](../../charts/octopod/values.yaml#L90).

You can override all of this using deployment overrides. Let's dive right in!

First, let's archive our wordpress deployment:

![](../images/octopod_archive.png)

### The config

And create one more deployment, this time using a different set of overrides:

![](../images/octopod_in_octopod_deployment.png)

**Name:** octopod-internal

**Tag:** 1.3.1

**App Overrides:**

|                   Key | Value                     |
| --------------------: | ------------------------- |
|  `octopod.baseDomain` | `octopod-internal.lvh.me` |
| `ingress.tls.enabled` | `false`                   |


**Deployment Overrides:**

|               Key | Value                                 |
| ----------------: | ------------------------------------- |
|      `chart_name` | `octopod`                             |
| `chart_repo_name` | `typeable`                            |
|  `chart_repo_url` | `https://typeable.github.io/octopod/` |
|   `chart_version` | `0.5.1`                               |


As in the previous example we took the _App Overrides_ from the [chart documentation](../../charts/octopod/README.md#Parameters), but the _Deployment Overrides_ are passed as configuration to the control scripts. You can read more about these parameters in the [control script docs](../../helm-control-scripts/README.md).

Now you have Octopod inside Octopod! Now you can install your own helm chart!
