# Octopod technical case study

In this case study, we consider developing a system that is mostly comprised of a server.

## Single staging model

This model implies having a single machine (physical or virtual) which is considered a staging environment. This single machine essentially becomes the contact point between Developers and QA engineers.

This in essence means that at any given point a single build of the service in a single configuration is available to the QA engineers.

## The multi-staging model

This model implies having a separate machine (physical or virtual) for *every* developed feature. Of course, deployments that are no longer needed will be removed.

This means that QA engineers can have access to an indefinite amount of independent builds and configurations of the product.

## Implications

### Isolation – separate configurations

It is sometimes the case that certain features need a "special" environment to be tested in, for example testing a feature might require payment errors to occur that only happen in a production payment processing environment. Having a single staging will require either not testing the said feature, or testing **all** "current" features with a production payment processor. This might prove to be either more difficult than necessary, or might lead to other undesirable consequences.

"Current" is used here to denote features that have been completed after the last time QA engineers had the opportunity to test the staging of the given project. It is assumed that the completed features are merged into something analogous to a `master` branch in git.

A way to mitigate the issue might be to have fine-grained control over what features are allowed to be merged into the `master` branch so that when we deploy it, we will have only the feature which needs the "special" environment, in a sense, batching features based on the environment they require. The main problem with this is that the payment processor is in all likelihood a tiny sliver, a single dimension of the whole space of possible environment setups. Trying to capture and institutionalize the whole breadth of a production environment in your development workflow seems like a futile endeavor.

Having per-feature stagings allow you to have completely separate and independent configurations for every single feature you develop and test at no additional cost.

### Isolation – more information

Sometimes making a change in one part of the codebase can lead to unexpected behavior in seemingly unrelated parts of the project. For example, an implementation of a feature might break the login form altogether.

With a single staging after discovering that the newly deployed version of your project breaks the login form, you will need to inspect every single feature implemented since the last deployment of your staging environment to find the offending feature. Depending on your workflow, there can be a large number of features.

With per-feature stagings, you will know precisely what change broke the login form since every single deployment would differ from a known-to-be-stable version by *exactly* one feature implementation in most cases – if you detect an unexpected breakage on one of the stagings, then you know exactly which feature broke it.

### Isolation – independent state

Some developed features might require some intricate and fragile state to be properly tested, for example, you might want something special for your 1000th customer each day. (This is a very contrived example. In reality, there will be numerous more subtle situations.) With a single staging environment, it might be very easy to trigger the desired state accidentally while testing a different feature. Having per-feature stagings implies that every staging has a separate state – this problem is mitigated.

As a bonus, it reduces the amount of data on the staging – this might make reproducing found bugs easier for the developer since the database would have orders of magnitude less data, and only the data relevant to the feature will be present.

### Isolation – freedom to experiment

Sometimes it can be useful for a business to test out an idea that might end up not being suitable for production at this time for whatever reason. (We are assuming that the feature is such that this becomes obvious only after implementing a prototype.)

When you have a single staging, the only real way to test out an idea is to merge into the rest of the codebase and deploy it to the staging in the usual way. If the feature is deemed an improvement – all is well, just continue development. If it is decided that the feature should not be pushed to production, you now need to "unmerge" the changes from the rest of the codebase. Depending on the timeline, other features might have been implemented that rely on the code. In most cases rolling a feature back is a non-trivial task.

With per-feature stagings, you could, in essence, make a branch of your whole product, and experiment with it however you like without fear that the changes will be relied upon by the rest of the team. Rolling back a feature becomes as easy as removing the git branch.

### Infrastructure reuse

When setting up Octopod you will need to implement the exact steps required to set up your whole project infrastructure in a reliable and reproducible way. This clarity, which is otherwise most likely absent due to ad-hoc deployment procedures, allows you to bootstrap new projects quickly. New projects will likely reuse some of the technologies your teams have experience with on other projects; you will be able to easily copy over the already implemented parts of the infrastructure.

### Continuous staging deployment

It is easy to set up continuous deployment procedures with Octopod. (Continuous deployment here refers to the process of automatically deploying branches of your repository when they are updated.) This allows you to optimize your development workflow even further.

A not-so-obvious usage might be to set up a staging to be automatically updated with the `master` branch of your project repository. This has the advantage that anyone can at any time easily test the behavior of the `master` branch.
