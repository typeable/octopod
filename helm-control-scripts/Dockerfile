FROM alpine:3
ARG HELM_BIN=/utils/helm
ARG KUBECTL_BIN=/utils/kubectl
ENV HELM_BIN=$HELM_BIN
ENV KUBECTL_BIN=$KUBECTL_BIN

ADD https://get.helm.sh/helm-v3.6.0-linux-amd64.tar.gz /tmp/helm.tar.gz
ADD https://dl.k8s.io/release/v1.21.0/bin/linux/amd64/kubectl $KUBECTL_BIN

RUN tar -xf /tmp/helm.tar.gz -C /tmp &&\
    mkdir -p /utils &&\
    cp /tmp/linux-amd64/helm $HELM_BIN &&\
    chmod +x $HELM_BIN &&\
    chmod +x $KUBECTL_BIN &&\
    rm -r /tmp/*

ADD target/x86_64-unknown-linux-musl/release/archive /utils/
ADD target/x86_64-unknown-linux-musl/release/archive_check /utils/
ADD target/x86_64-unknown-linux-musl/release/check /utils/
ADD target/x86_64-unknown-linux-musl/release/cleanup /utils/
ADD target/x86_64-unknown-linux-musl/release/create /utils/
ADD target/x86_64-unknown-linux-musl/release/info /utils/
ADD target/x86_64-unknown-linux-musl/release/update /utils/
ADD target/x86_64-unknown-linux-musl/release/init /utils/
ADD target/x86_64-unknown-linux-musl/release/tag_check /utils/
ADD target/x86_64-unknown-linux-musl/release/app_overrides /utils/
ADD target/x86_64-unknown-linux-musl/release/app_keys /utils/
ADD target/x86_64-unknown-linux-musl/release/deployment_overrides /utils/
ADD target/x86_64-unknown-linux-musl/release/deployment_keys /utils/
ADD target/x86_64-unknown-linux-musl/release/unarchive /utils/
