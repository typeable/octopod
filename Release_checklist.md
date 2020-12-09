# Release checklist

1. Merge the `develop` branch into `master`.
2. CI will automatically create a new release in GitHub with _octo CLI_ and update the `latest` tag for both `octo` and `octopod`. Wait for CI to complete.
3. Edit the created release in GitHub to match the version you are releasing.
   1. Change the release name to the version being released.
   2. Uncheck "This is a pre-release"
4. Push the new release of `octo` and `octopod`. To do this run `./release.sh <version>`.
5. Update the referenced tags in documentation
6. If there were changes to the examples:
   1. Build and push the new containers:
      1. octopod-web-app-example
      2. octopod-helm-example
   2. Create a new tag incrementing the integer version number of the tag:
      1. Pull the image (`docker pull`)
      2. Tag it with the new `v<integer>` (`docker tag`)
      3. Push the new tag (`docker push`)
   3. Update docs where the tags are referenced.
