---
title: Adding a handler
---

# Adding a handler

In this example, we'll create a person age validator.

If you have used the Stack template, you will have a handler that is pre-defined in the `src/Lib.hs` file.

If you are starting from scratch, let's write it bit by bit:

First, we will enable some language extensions in order to work with JSON easier, also, we'll import the required
modules:

```haskell top hide
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
```

```haskell
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where
```

```haskell top
import Aws.Lambda
import GHC.Generics
import Data.Aeson
```

The runtime will decode the JSON input that reaches the AWS Lambda handler, so let's create a type
for persons. We also tell the compiler to derive (auto-implement) the `Generic`, `FromJSON` and `ToJSON` classes
for us.

```haskell top
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Generic, FromJSON, ToJSON)
```

Now, let's write the handler. It **must** be a function that is called `handler` and has a type signature.

The arguments to this function will always go like this:

* The first argument to this handler will always be the input type we expect (note that it has to implement `FromJSON`).
* The second argument is the `Aws.Lambda` `Context` type, which has some information regarding our Lambda execution. The `Context` type also takes a `context` parameter, which we can use if we want to have some state that is shared between Lambda calls. For this example, we don't want to have such state, so we'll just use `()`.

The output will always be an `IO (Either errorType resultType)` where

* `errorType` is whatever custom error type you want to use.
* `resultType` is what your function will return if everything goes well.

Note that both types must implement `ToJSON`, as the runtime will use it to serialize the values.

For example, here we will check if the age of a `Person` is positive, and will return if it is correct. If not, we
will return a `String` error:

```haskell top
handler :: Person -> Context () -> IO (Either String Person)
handler person context =
  if age person > 0 then
    pure (Right person)
  else
    pure (Left "A person's age must be positive")
```

Note how we are using `Right` to return the value in case everything went **right**, and `Left` if something went wrong.