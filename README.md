# Haskell Runtime for AWS Lambda
[![Hackage](https://img.shields.io/hackage/v/aws-lambda-haskell-runtime.svg)](https://hackage.haskell.org/package/aws-lambda-haskell-runtime)
[![Linter](https://img.shields.io/badge/code%20style-HLint-brightgreen.svg)](https://github.com/ndmitchell/hlint)
[![CircleCI](https://circleci.com/gh/theam/aws-lambda-haskell-runtime/tree/master.svg?style=svg)](https://circleci.com/gh/theam/aws-lambda-haskell-runtime/tree/master)


This package provides a way of running Haskell projects on AWS Lambda.

# Usage

```
stack new my-haskell-lambda https://github.com/theam/aws-lambda-haskell-runtime/raw/master/stack-template.hsfiles --resolver=lts-12.13 --omit-packages
cd my-haskell-lambda
stack docker pull
```

Then add 

```
packages:
- .

extra-deps:
- aws-lambda-haskell-runtime-1.0.4
```

to your `stack.yaml`

Then run 

```
make
```

Now you should have a `build/function.zip` file that you can uploaded to your lambda.

### Full user guide

Take a look at the [Getting Started with the Haskell AWS Lambda Runtime](https://medium.com/the-theam-journey/getting-started-with-the-haskell-aws-lambda-runtime-951b2322c7a3) guide.
