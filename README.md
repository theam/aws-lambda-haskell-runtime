# Haskell Runtime for AWS Lambda

_It is currently in active development, as it has not been thoroughly tested_

This package provides a set of functions that make easier the usage of Haskell in AWS Lambda, as well as a runtime to deploy it.

## Usage

Using this library you can create two kinds of handlers for your Lambda, an impure one, that allows you to run calls against other API, using other AWS services, as other IO code, and also it provides a pure handler creator variant, which leverages the power of the Glasgow Haskell Compiler to optimize your code, making your processor lambdas much faster.

### Impure handler

```haskell
data CheckoutFinished = CheckoutFinished
  { customerId :: Text
  , cart :: [Product]
  } deriving Generic
instance FromJSON CheckoutFinished
main = impurely handler

handler :: CheckoutFinished -> Context -> IO (Either HandlerError Text)
handler CheckoutFinished {..} ctx = do
  putTextLn ("Checkout finished by " <> customerId)
  return (Right "Everything went correctly")
```

### Pure handler

```haskell
data ItemSelected = ItemSelected
  { itemId :: Text
  , itemName :: Text
  , itemPrice :: Float
  } deriving Generic
instance FromJSON ItemSelected

data Processed = Processed
  { processedName :: Text
  , processedLetters :: Name
  } deriving Generic
instance ToJSON Processed
main = pureLambda handler

handler :: ItemSelected -> Context -> Processed
handler ItemSelected {..} ctx = Processed
  { processedName = "Processed_" <> itemName
  , processedLetters = length itemId + length itemName
  }
```
