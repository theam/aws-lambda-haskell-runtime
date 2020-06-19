---
title: Usage with API Gateway
---

# Usage with API Gateway

A common use-case is to expose your lambdas through API Gateway. Luckily, `aws-lambda-haskell-runtime` has native support for that.

If you're using API Gateway, simply pass the `UseWithAPIGateway` option to `generateLambdaDispatcher` in your `app/Main.hs` file.

```haskell
generateLambdaDispatcher UseWithAPIGateway defaultDispatcherOptions 
```

Passing `UseWithAPIGateway` will make the compiler error if you do not have your handlers in the following form:

```haskell
handler :: ApiGatewayRequest request -> Context context -> IO (Either (ApiGatewayResponse error) (ApiGatewayResponse success))
handler = ...
```

You can use the `ApiGatewayRequest` wrapper to access additional request information that API Gateway provides, such as path, query string parameters, authentication info and much more. You can find more information about that under the `Input format of a Lambda function for proxy integration` section [here](https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-proxy-integrations.html).

You can use the `ApiGatewayResponse` wrapper in order to return a custom status code or attach custom headers to the response.

Also note the `defaultDispatcherOptions`. They look like this:

```haskell
-- | Options that the dispatcher generator expects
newtype DispatcherOptions = DispatcherOptions
  { apiGatewayDispatcherOptions :: ApiGatewayDispatcherOptions
  } deriving (Lift)

-- | API Gateway specific dispatcher options
newtype ApiGatewayDispatcherOptions = ApiGatewayDispatcherOptions
  { propagateImpureExceptions :: Bool
  -- ^ Should impure exceptions be propagated through the API Gateway interface
  } deriving (Lift)

defaultDispatcherOptions :: DispatcherOptions
defaultDispatcherOptions =
  DispatcherOptions (ApiGatewayDispatcherOptions True)
```

For production environments, you'd likely want to set `propagateImpureExceptions` to `False` in order to prevent your exception messages from being seen.