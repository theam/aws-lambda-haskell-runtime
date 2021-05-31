---
title: Adding a handler
---

# Adding a handler

In this example, we'll create a person age validator.

If you have used the Stack template, you will have a handler that is pre-defined in the `src/Lib.hs` file.

First, we need to enable some language extensions in order to make working with JSON easier. We'll also import a few required modules:

```haskell top
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Aws.Lambda
import GHC.Generics
import Data.Aeson
```

We'll create a basic handler that validates a person's age is positive. Let's create a `Person` type to use.

```haskell top
data Person = Person
  { name :: String
  , age  :: Int
  } -- We kindly ask the compiler to autogenerate JSON instances for us
  deriving (Generic, FromJSON, ToJSON)
```

Now, let's implement the actual handler.

A handler is a function with the following type signature:

```haskell
-- Note that request, error and response must all implement ToJSON and FromJSON
handler :: request -> Context context -> IO (Either error response)
```

For our person validator usecase this means the following:

```haskell
handler :: Person -> Context () -> IO (Either String Person)
```

This means we expect to be given a `Person` object and we'll return either some `String` as an error or some other `Person` object (that passed validation).

You can ignore the `Context ()` parameter at this point. This is the Lambda context object which is present in every runtime. By specifying `()` as an inner value, we say we don't want to have anything there.

The implementation of our handler will look like this:

```haskell
handler :: Person -> Context () -> IO (Either String Person)
handler person _context =
  if age person > 0 then
    pure (Right person)
  else
    pure (Left "A person's age must be positive")
```

Note how we are using `Right` to return the value in case everything went **right**, and `Left` if something went wrong.

Now let's see how to register this handler into our runtime.