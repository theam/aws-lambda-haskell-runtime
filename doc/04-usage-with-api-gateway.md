---
title: Usage with API Gateway
---

# Usage with API Gateway

In order to write a handler for the API Gateway, we will receive a special JSON with
a lot of fields with information from the API Gateway, and we will have to return
a special JSON as the response.

## The request

The full request type looks more or less like this:

```haskell
data APIGatewayRequest = APIGatewayRequest
  { resource :: String
  , path :: ByteString
  , httpMethod :: Method
  , headers :: RequestHeaders
  , queryStringParameters :: [(ByteString, Maybe ByteString)]
  , pathParameters :: HashMap String String
  , stageVariables :: HashMap String String
  , body :: String
  } deriving (Generic, FromJSON)
```

Feel free to copy-paste this data type into your project. Note that you don't have to use all of
it's fields, if you only need, say, the `body` and the `httpMethod` fields, you can use this one
(`resource` is mandatory):

```haskell
data APIGatewayRequest	= APIGatewayRequest
  { resource :: String
  , httpMethod :: Method
  , body :: String
  } deriving (Generic, FromJSON)
```

## The response

The response type looks more or less like this:

```haskell
data APIGatewayResponse = APIGatewayResponse
  { statusCode :: Int
  , headers :: [(HeaderName, ByteString)]
  , body :: String
  } deriving (Generic, ToJSON)
```

Again, you can use this type in your project, and use only the fields you need.
Note that `HeaderName` comes from [`Network.HTTP.Simple`](https://hackage.haskell.org/package/http-conduit-2.3.7.1/docs/Network-HTTP-Simple.html#t:Header).

## An example

```haskell top
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import GHC.Generics
import Aws.Lambda
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString

-- Input
data Event = Event
  { resource :: String
  , body :: String
  } deriving (Generic, FromJSON)

-- Output
data Response = Response
  { statusCode:: Int
  , body :: String
  } deriving (Generic, ToJSON)

-- Type that we decode from the 'body' of 'Event'
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, FromJSON, ToJSON)

greet :: Person -> String
greet person =
  "Hello, " ++ name person ++ "!"

handler :: Event -> Context -> IO (Either String Response)
handler Event{..} context = do
  case decode (ByteString.pack body) of
    Just person ->
      pure $ Right Response
        { statusCode = 200
        , body = greet person
        }
    Nothing ->
      pure $ Right Response
        { statusCode = 200
        , body = "bad person"
        }
```