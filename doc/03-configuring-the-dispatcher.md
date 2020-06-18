---
title: Configuring the Dispatcher
---

# Configuring the Dispatcher

The dispatcher is a special main function that checks which handler it has to run,
and runs it

To make the package generate a dispatcher for you, you have to configure it in
your `app/Main.hs` file to use the `generateLambdaDispatcher` function from `Aws.Lambda`.

The `generateLambdaDispatcher` function has two options: `StandaloneLambda` and `UseWithAPIGateway`.

Use `UseWithAPIGateway` when you'll be exposing your lambda through API Gateway. For more information check the [Use with API Gateway](./04-usage-with-api-gateway.md) page.

Use `StandaloneLambda` when you want to use your lambda as usual. This is what we're going to use for our person age validator function.

To continue the person age validator lambda, activate `TemplateHaskell` for enabling the metaprogramming features of Haskell.
Then, import the `Aws.Lambda` module:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Aws.Lambda
```

After this, add a line with the following statement:

```haskell
generateLambdaDispatcher StandaloneLambda defaultDispatcherOptions
```

This statement will generate something that looks a little bit like this:

```haskell
main :: IO ()
main = do
  handlerName <- getHandlerName
  context <- getContext
  input <- getInput
  case handlerName of
    "src/Lib.handler" -> do
      result <- Lib.handler input context
      publish result
    "..." -> do
      ...
```

In fact, you can check for the generated code by running `stack repl` from
the root of your project, and issuing:

```text
:set -ddump-splices
:l app/Main.hs
```
