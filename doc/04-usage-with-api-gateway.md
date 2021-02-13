---
title: Usage with API Gateway or ALB
---

# Usage with API Gateway or ALB

A common use-case is to expose your lambdas through API Gateway or an ALB. Luckily, `aws-lambda-haskell-runtime` has native support for that.

If you want to add such handlers simply use the corresponding `add` function in `runLambdaHaskellRuntime`.

```haskell
import Aws.Lambda

main :: IO ()
main =
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id $ do
      addAPIGatewayHandler "api-gateway" gatewayHandler
      addALBHandler "alb" albHandler
      addStandaloneLambdaHandler "standalone" regularHandler

gatewayHandler ::
  ApiGatewayRequest request ->
  Context context ->
  IO (Either (ApiGatewayResponse error) (ApiGatewayResponse response))
gatewayHandler = doSomething

albHandler ::
  ALBRequest request ->
  Context context ->
  IO (Either (ALBResponse error) (ALBResponse response))
albHandler = doSomething

regularHandler ::
  request ->
  Context context ->
  IO (Either error response)
regularHandler = doSomething
```

You can use the `ApiGatewayRequest` or `ALBRequest` wrapper to access additional request information that API Gateway or ALB provide, such as path, query string parameters, authentication info and much more.

You can find more information about that under the `Input format of a Lambda function for proxy integration` section [here](https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html).

You can use the `ApiGatewayResponse` or `ALBResponse` wrapper to return a custom status code or attach custom headers to the response.

# Supporting both API Gateway and ALB at once

You could use a standalone handler with a dynamic check to support both API Gateway and ALB.

For an example, see how `aws-lambda-haskell-runtime-wai` does it:

```haskell
runWaiAsProxiedHttpLambda ::
  DispatcherOptions ->
  Maybe ALBIgnoredPathPortion ->
  HandlerName ->
  IO Application ->
  IO ()
runWaiAsProxiedHttpLambda options ignoredAlbPath handlerName mkApp =
  runLambdaHaskellRuntime options mkApp id $
    addStandaloneLambdaHandler handlerName $ \(request :: Value) context ->
      case parse parseIsAlb request of
        Success isAlb -> do
          if isAlb
            then case fromJSON @(ALBRequest Text) request of
              Success albRequest ->
                bimap toJSON toJSON <$> albWaiHandler ignoredAlbPath albRequest context
              Error err -> error $ "Could not parse the request as a valid ALB request: " <> err
            else case fromJSON @(ApiGatewayRequest Text) request of
              Success apiGwRequest ->
                bimap toJSON toJSON <$> apiGatewayWaiHandler apiGwRequest context
              Error err -> error $ "Could not parse the request as a valid API Gateway request: " <> err
        Error err ->
          error $
            "Could not parse the request as a valid API Gateway or ALB proxy request: " <> err
```