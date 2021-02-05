#!/bin/bash

(cd migrations && sqitch deploy -t postgresql://octopod:octopod@localhost:5432/octopod)

export MOUNT_DIR=./dev

export PROJECT_NAME="Example Project"
export BASE_DOMAIN=octopod.example.com
export NAMESPACE=deployment
export ARCHIVE_RETENTION=60
export STATUS_UPDATE_TIMEOUT=600
export CREATION_COMMAND=$MOUNT_DIR/echo.sh
export UPDATE_COMMAND=$MOUNT_DIR/echo.sh
export UPDATE_ENVS_COMMAND=$MOUNT_DIR/echo.sh
export ARCHIVE_COMMAND=$MOUNT_DIR/echo.sh
export CHECKING_COMMAND=$MOUNT_DIR/echo.sh
export CLEANUP_COMMAND=$MOUNT_DIR/echo.sh
export ARCHIVE_CHECKING_COMMAND=$MOUNT_DIR/echo.sh
export TAG_CHECKING_COMMAND=$MOUNT_DIR/echo.sh
export INFO_COMMAND=$MOUNT_DIR/info.sh
export NOTIFICATION_COMMAND=$MOUNT_DIR/write.sh
$1/bin/octopod-exe \
    --port 4443 \
    --ui-port 3002 \
    --ws-port 4020 \
    --db "host='127.0.0.1' port=5432 user='octopod' password='octopod'" \
    --db-pool-size 10 \
    --tls-cert-path dev/certs/server_cert.pem \
    --tls-key-path dev/certs/server_key.pem \
    --tls-store-path /tmp/tls_store
