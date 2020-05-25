#!/usr/bin/env bash

set -e

aws_login() {
    aws ecr get-login-password | docker login --username AWS --password-stdin 560065381221.dkr.ecr.us-east-1.amazonaws.com
}

build_passwd() {
    echo "root:x:0:0:root:/root:/bin/bash" > $configs/passwd
}

build_ssh_configs() {
    echo -e "Host github.com\nHostname github.com\nPort 22\nUser git\nIdentityFile /root/.ssh/deploy.key" > $configs/ssh_config

    echo "|1|wrBnhg5iUmQx07loxtzNieZp4Gs=|zZDaK51GEIn+RiA8+US9aEKn+s4= ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==" > $configs/ssh_known_hosts
    echo "|1|YxKPhPv5F8JFFTL6W+p46Vc1Dl4=|GgqpjQ6ViPn3oU948zb4YrdDs84= ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==" >> $configs/ssh_known_hosts
}

build_credentials() {
    openssl req -x509 -newkey rsa:4096 \
        -keyout $configs/server_key.pem \
        -out $configs/server_cert.pem \
        -nodes -subj "/CN=localhost/O=Server"
    openssl req -newkey rsa:4096 \
        -keyout $configs/client_key.pem \
        -out $configs/client_csr.pem \
        -nodes -subj "/CN=Client"
    openssl x509 -req \
        -in $configs/client_csr.pem \
        -CA $configs/server_cert.pem \
        -CAkey $configs/server_key.pem \
        -out $configs/client_cert.pem -set_serial 01 -days $cert_expiration
}

download_deploy_key() {
    key=`aws ssm get-parameter --name "$1" --with-decryption | jq '.Parameter.Value'`
    echo -e $key | sed -r 's/"//g' > $configs/deploy.key
}

build_configs() {
    build_passwd
    build_ssh_configs
    build_credentials
    download_deploy_key $1
}


update_dependencies() {
    make update-default-nix
    make update-b2b-helm-nix
}

build_dmc_docker_image() {
    nix build nixpkgs.dmc-container \
        --arg client-cert $1/client_cert.pem \
        --arg client-key $1/client_key.pem \
        -I nixpkgs=nix \
        -o $2
}

build_dms_docker_image() {
    nix build nixpkgs.dms-container \
        --arg migrations $2 \
        --arg server-cert $1/server_cert.pem \
        --arg server-key $1/server_key.pem \
        --arg passwd $1/passwd \
        --arg ssh-config $1/ssh_config \
        --arg ssh-known-hosts $1/ssh_known_hosts \
        --arg deploy-key $1/deploy.key \
        -I nixpkgs=nix \
        -o $3
}

push_docker_images() {
    outfile=latest-dms-docker
    for image_name in $dmc_docker $dms_docker; do
        image_type=`echo $image_name | cut -d- -f1`
        image=`ls -ls $image_name | awk '{print $12}'`
        echo "size: $(du -sh $image)"
        docker load --input $image | tee $outfile
        nixcontainer=`cat $outfile | awk '{print $3}'`
        docker tag $nixcontainer 560065381221.dkr.ecr.us-east-1.amazonaws.com/${image_type}:${tag}
        docker tag $nixcontainer 560065381221.dkr.ecr.us-east-1.amazonaws.com/${image_type}:latest
        docker push 560065381221.dkr.ecr.us-east-1.amazonaws.com/${image_type}:${tag}
        echo "Published: ${image_type}:${tag}"
    done
    docker push 560065381221.dkr.ecr.us-east-1.amazonaws.com/dmc:latest
    docker push 560065381221.dkr.ecr.us-east-1.amazonaws.com/dms:latest
    echo "Published: dmc:latest"
    echo "Published: dms:latest"
    rm $outfile
}

build_docker_images() {
    build_configs $key
    update_dependencies
    build_dmc_docker_image $configs $dmc_docker
    build_dms_docker_image $configs $migrations $dms_docker
}

cleanup() {
    rm -r $configs
    echo "'$configs' directory deleted"
}

export tag=$(git rev-parse HEAD)
export migrations="./migrations"
export dmc_docker="dmc-docker"
export dms_docker="dms-docker"
export cert_expiration=730

case "$1" in
    user)
        echo "$1 mode"
        export key="b2b-helm-repo-user-deploy-key"
        export configs=`mktemp -d`
        echo "using '$configs' directory"
        aws_login
        build_docker_images
        push_docker_images
        cleanup
        ;;
    ci)
        echo "$1 mode"
        export key="b2b-helm-repo-ci-deploy-key"
        if [ -d "$2" ]; then
            export configs="$2"
        else
            echo "cannot access '$2': No such directory"
            exit 1
        fi
        echo "using '$configs' directory"
        build_docker_images
        ;;
    *)
        echo "usage: $0 <mode> [<work_dir>]"
        echo "mode = user | ci"
        exit 1
        ;;
esac
