---
title: Configuring the Dispatcher
---

# Configuring the Dispatcher

The dispatcher is a special main function that checks which handler it has to run, and runs it.

This is very easy to do in `aws-lambda-haskell-runtime`. Just use the `runLambdaHaskellRuntime` function.

```haskell
runLambdaHaskellRuntime ::
  RuntimeContext handlerType m context request response error =>
  DispatcherOptions ->
  IO context ->
  (forall a. m a -> IO a) ->
  HandlersM handlerType m context request response error () ->
  IO ()
```

It may seem like an intimidating type signature, but let's go through each parameter and see what it means.

* `DispatcherOptions` are the configuration options of the dispatcher function. You can just use `defaultDispatcherOptions`.
* The `IO context` action is how you initialize the context object.
* `(forall a. m a -> IO a)` is used when you have your handler in a custom monad. It transforms that custom monadic action into `IO`. If your handlers run in `IO`, just use `id`.
* `HandlersM handlerType m context request response error ()` is the action that registers the handlers under a given name.

For the person validator example we set up in the previous section, it will look like this:

```haskell
import Aws.Lambda
import qualified Lib
import qualified Data.Text as T

main :: IO ()
main =
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id $ do
      -- You could also register multiple handlers
      addStandaloneLambdaHandler (HandlerName $ T.pack "handler") Lib.handler
```
