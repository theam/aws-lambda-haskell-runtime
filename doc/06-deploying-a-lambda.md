---
title: Deploying a Lambda
---

# Deploying a Lambda

To deploy your project to AWS Lambda, we have provided a `Makefile` to make your life easier.
Run `make` from the root of your project, and you will have a `build/function.zip` generated for you.

Note that this uses [**Docker**](https://www.docker.com) underneath, so make sure you have it installed
and set-up on your system.

## Manual deploy

To build the `function.zip` file manually, you need to run these commands:

```bash
# Create the output `build` directory
mkdir -p build
# Perform a clean build of the project using docker
# (In some cases `stack clean` is not needed, you can try removing it)
stack clean --docker
stack build --docker

# Copy the generated executable to the output path
cp $(stack --docker path --local-install-root)/bin/bootstrap build

# Create a `function.zip` file to upload
cd build && zip function.zip bootstrap && rm bootstrap && cd ..
```