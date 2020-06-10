#! /bin/bash

(cd .. && make)
mkdir tmp
cp ../target/x86_64-unknown-linux-musl/release/create ./tmp
cp ../target/x86_64-unknown-linux-musl/release/update ./tmp
cp ../target/x86_64-unknown-linux-musl/release/delete ./tmp
cp ../target/x86_64-unknown-linux-musl/release/check ./tmp
cp ../target/x86_64-unknown-linux-musl/release/cleanup ./tmp

docker build --no-cache -f Dockerfile -t 560065381221.dkr.ecr.us-east-1.amazonaws.com/dms:b2b-utils .

rm -r tmp
