#!/usr/bin/env bash

set -e

build_octo_cli_docker_image() {
    nix build ./nix -A octo-cli-container -o "$1"
}

build_octopod_server_docker_image() {
    nix build ./nix -A octopod-server-container --arg migrations "$1" -o "$2"
}

push_docker_images() {
    outfile=latest-octopod-server-docker
    for image_name in $octo_cli_docker $octopod_server_docker; do
        image_type=$(echo "$image_name" | cut -d- -f1)
        image=$(ls -ls "$image_name" | awk '{print $12}')
        echo "size: $(du -sh $image)"
        docker load --input "$image" | tee "$outfile"
        nixcontainer=$(awk '{print $3}' $outfile)
        docker tag "$nixcontainer" "typeable/${image_type}:$1"
        docker push "typeable/${image_type}:$1"
        echo "Published: ${image_type}:$1"
    done
    rm $outfile
}

build_docker_images() {
    build_octo_cli_docker_image "$octo_cli_docker"
    build_octopod_server_docker_image "$migrations" "$octopod_server_docker"
}

export tag=$(git rev-parse HEAD)
export migrations="./migrations"
export octo_cli_docker="octo-docker"
export octopod_server_docker="octopod-server-docker"

case "$1" in
build-and-push)
    echo "$1 mode"

    if test -z "$2"; then
        echo "Please provide a tag to upload to"
        exit 1
    fi

    build_docker_images
    push_docker_images $2
    ;;
build)
    echo "$1 mode"
    build_docker_images
    ;;
*)
    echo "usage:"
    echo "  $0 build                    Builds the docker images."
    echo "  $0 build-and-push <tag>     Builds the docker images and uploads it to Docker Hub under the tag <tag>."
    exit 1
    ;;
esac
