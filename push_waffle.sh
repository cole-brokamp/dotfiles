#!/bin/bash

# exit on error
set -e

echo "logging into AWS ECR"

$(aws ecr get-login --no-include-email)

echo "pushing waffle to AWS ECR using git HEAD sha1..."

docker tag colebrokamp/waffle:latest 126952269818.dkr.ecr.us-east-1.amazonaws.com/waffle:$(git rev-parse --short --verify HEAD)

docker push 126952269818.dkr.ecr.us-east-1.amazonaws.com/waffle:$(git rev-parse --short --verify HEAD)

echo "pushing waffle up using :latest..."

docker tag colebrokamp/waffle:latest 126952269818.dkr.ecr.us-east-1.amazonaws.com/waffle:latest

docker push 126952269818.dkr.ecr.us-east-1.amazonaws.com/waffle:latest
