# Release checklist

1. Update the referenced tags in the documentation
2. If there were changes to the examples:
   1. Build and push the new containers:
      1. octopod-web-app-example
      2. octopod-helm-example
   2. Create a new tag incrementing the integer version number of the tag:
      1. Build the updated images
      2. Tag it with the new `<integer>` (`docker tag`)
      3. Push the new tag (`docker push`)
   3. Update docs where the tags are referenced.
3. Merge the `develop` branch into `master`.
4. CI will automatically create a new release in GitHub with _octo CLI_ and update the `latest` tag for both `octo` and `octopod`. Wait for CI to complete.
5. Edit the created release in GitHub to match the version you are releasing.
   1. Change the release name to the version being released.
   2. Uncheck "This is a pre-release"
6. Push the new release of `octo` and `octopod`. To do this run `./release.sh <version>`.
