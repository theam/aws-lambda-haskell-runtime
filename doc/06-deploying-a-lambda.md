---
title: Deploying a Lambda
---

## Using the template Makefile

To deploy your project to AWS Lambda, we have provided a `Makefile` to make your life easier.

Run `make` from the root of your project, and you will have a [**Docker**](https://www.docker.com) image generated for you that is ready to be deployed onto AWS Lambda.

## Building the executable manually

You could also build the executable manually, but that is troublesome because it either needs to be static or you need to make sure to ship all library dependencies as well as build it on the same environment it's going to run on.

For simple executables without dependencies, you could just add the following to your `package.yaml`, build the `bootstrap` and ship it to AWS using a `.zip` file.

```yaml
ghc-options:
    .. other options
    - -O2
    - -static
cc-options: -static
ld-options: -static -pthread
```