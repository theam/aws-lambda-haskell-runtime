---
title: Getting Started
---

# Getting Started

This guide assumes that you have the [Stack](https://www.haskellstack.org/) build tool installed.
If not, you can do so by issuing the following command on your terminal:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Haskell compiles to **native code**, which is super efficient. But it has one main drawback: linking changes from machine to machine.

To make sure that our projects always work and are reproducible, we use the Stack [feature](https://docs.haskellstack.org/en/stable/docker_integration/)
for [Docker](https://www.docker.com/) support to build our projects. Be sure to install Docker before getting started with the runtime 😄

## Using the template

If you are testing the package, or you are starting a new project, we have provided a Stack template that will scaffold the project for you.
To use it, enter the following command:

```bash
stack new my-haskell-lambda https://github.com/theam/aws-lambda-haskell-runtime/raw/master/stack-template.hsfiles --resolver=lts-13.25 --omit-packages
```

This will create a `my-haskell-lambda` directory with the following structure:

```text
.
├── LICENSE
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

Now, add the following to your `stack.yaml` file:

```yaml
packages:
- .

extra-deps:
- aws-lambda-haskell-runtime-2.0.1
```

## Adding the dependency to an existing project

If you currently have a project, you can add this package by adding,

to the `stack.yaml` file:

```yaml
extra-deps:
- aws-lambda-haskell-runtime-2.0.1
```

and, to the `package.yaml` file:

```yaml
dependencies:
- ... # other dependencies of your project
- aws-lambda-haskell-runtime >= 2.0
```

## Keep reading!

If you have completed these steps, and type into your terminal:

```bash
stack build
```

you should get a proper build of your project.

Let's see how we can add our first handler!
