#!/usr/bin/env bash

if test -z "$1"
then
    echo "usage:"
    echo "  $0 <version>          <version> will be the tag on Docker Hub. Use the format v1.0"
    exit 1
fi

echo "Releasing version $1"

for x in octopod octo
do
  echo "Releasing $x"
  docker pull typeable/$x:latest
  docker tag typeable/$x:latest typeable/$x:$1
  docker push typeable/$x:$1
  docker rmi typeable/$x:latest
  docker rmi typeable/$x:$1
done
