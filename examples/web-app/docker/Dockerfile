FROM ekidd/rust-musl-builder:1.48.0 as builder

RUN sudo mkdir /app && \
    sudo chown -R rust:rust /app
WORKDIR /app
RUN USER=root cargo new web-app
WORKDIR /app/web-app
COPY Cargo.toml /app/web-app/
COPY Cargo.lock /app/web-app/
RUN cargo build --release
COPY src /app/web-app/src
COPY templates /app/web-app/templates
RUN rm -r ./target/x86_64-unknown-linux-musl/release/deps/web_app* && \
    cargo build --release --frozen

FROM alpine:3.12.1

COPY --from=builder /app/web-app/target/x86_64-unknown-linux-musl/release/web-app /app/web-app
RUN addgroup -g 1000 user && \
    adduser -D -s /bin/sh -g 1000 -G user user

ENTRYPOINT ["/app/web-app"]
