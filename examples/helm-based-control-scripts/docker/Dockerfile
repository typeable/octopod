FROM ekidd/rust-musl-builder:1.46.0 as builder

RUN sudo mkdir /app && \
    sudo chown -R rust:rust /app
WORKDIR /app
RUN USER=root cargo new helm-based-control-scripts
COPY Cargo.toml /app/helm-based-control-scripts/
COPY Cargo.lock /app/helm-based-control-scripts/
COPY src /app/helm-based-control-scripts/src
WORKDIR /app/helm-based-control-scripts
RUN cargo build --release

FROM typeable/kube-things as base-utils

FROM busybox

COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/create /utils/create
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/create /utils/update
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/archive /utils/archive
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/check /utils/check
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/cleanup /utils/cleanup
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/archive_check /utils/archive_check
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/tag_check /utils/tag_check
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/init /utils/init
COPY --from=builder /app/helm-based-control-scripts/target/x86_64-unknown-linux-musl/release/info /utils/info

COPY --from=base-utils /utils/kubectl /utils/kubectl
COPY --from=base-utils /utils/helm2 /utils/helm
COPY --from=base-utils /utils/kubedog /utils/kubedog
