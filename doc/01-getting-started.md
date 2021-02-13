---
title: Getting Started
---

# Getting Started

This guide assumes that you have the [Stack](https://www.haskellstack.org/) build tool installed.
If not, you can do so by issuing the following command on your terminal:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Haskell compiles to **native code**, which is super efficient. But it has one main drawback: linking changes from machine to machine. It's very hard to make sure that the executable you build will work when deployed to AWS Lambda.

To make sure our projects work consistently, we use AWS Lambda's [docker image](https://aws.amazon.com/blogs/aws/new-for-aws-lambda-container-image-support/) feature. Be sure to install Docker before getting started with the runtime 😄

## Using the template

If you are testing the package, or you are starting a new project, we have provided a Stack template that will scaffold the project for you.
To use it, enter the following command:

```bash
stack new my-haskell-lambda https://github.com/theam/aws-lambda-haskell-runtime/raw/master/stack-template.hsfiles
```

This will create a `my-haskell-lambda` directory with the following structure:

```text
.
├── LICENSE
├── Dockerfile
├── Makefile
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── my-haskell-lambda.cabal
├── package.yaml
├── src
│   └── Lib.hs
└── stack.yaml
```

The project contains a sample handler that you can use as a starting point.

## Adding the dependency to an existing project

If you want to add the runtime to an existing project, you can do so by adding the following `extra-dep` entry to the `stack.yaml` file:

```yaml
extra-deps:
- aws-lambda-haskell-runtime-4.0.0
```

and, to the `package.yaml` file:

```yaml
dependencies:
- ... # other dependencies of your project
- aws-lambda-haskell-runtime >= 4.0.0
```

If you have completed these steps, you should be able to execute `stack build` and see the project build correctly.

## Keep reading!

Let's see how we can add our first handler!
