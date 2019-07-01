---
title: Configuring the Dispatcher
---

# Configuring the Dispatcher

The dispatcher is a special main function that checks which handler it has to run,
and runs it

To make the package generate a dispatcher for you, you have to configure it in
your `app/Main.hs` file.

First, activate `TemplateHaskell` for enabling the metaprogramming features of Haskell.
Then, import the `Aws.Lambda` module:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Aws.Lambda
```

After this, add a line with the following statement:

```haskell
generateLambdaDispatcher
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
