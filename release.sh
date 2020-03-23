#!/usr/bin/env bash

set -e

export tag=$(git rev-parse HEAD)
$(aws ecr get-login --no-include-email --region us-east-1)
make build-docker
export image=$(ls -ls dms-docker | awk '{print $12}')
echo "size: $(du -sh $image)"
export outfile=latest-dms-docker
docker load --input $image | tee $outfile
export nixcontainer=$(cat $outfile | awk '{print $3}')
rm $outfile
docker tag $nixcontainer 560065381221.dkr.ecr.us-east-1.amazonaws.com/default:dms-${tag}
docker push 560065381221.dkr.ecr.us-east-1.amazonaws.com/default:dms-${tag}
echo "Published: dms-${tag}"
