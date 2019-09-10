---
title: Home
---

# Haskell Runtime for AWS Lambda

[![CircleCI](https://circleci.com/gh/theam/aws-lambda-haskell-runtime.svg?style=shield)](https://circleci.com/gh/theam/aws-lambda-haskell-runtime)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=shield)](http://makeapullrequest.com)
[![Hackage version](https://img.shields.io/hackage/v/aws-lambda-haskell-runtime.svg)](https://hackage.haskell.org/package/aws-lambda-haskell-runtime)
[![Open Source Love png1](https://badges.frapsoft.com/os/v1/open-source.png?v=103)](https://github.com/ellerbrock/open-source-badges/)
[![Linter](https://img.shields.io/badge/code%20style-HLint-brightgreen.svg)](https://github.com/ndmitchell/hlint)

**aws-lambda-haskell-runtime** allows you to use Haskell as a first-class citizen of AWS Lambda. It allows you to deploy projects built with Haskell, and select the handler as you would do with Node.js. It discovers which modules of the project implement a handler for AWS, and generates a dispatcher dynamically, so you don't have to worry about wiring the lambda calls, as it uses the
handler name specified in the AWS Lambda console.

It makes deploying Haskell code to AWS very easy:

```haskell
module WordCount where

import Aws.Lambda

handler :: String -> Context -> IO (Either String Int)
handler someText _ = do
  let wordsCount = length (words someText)
  if wordsCount > 0 then
    pure (Right wordsCount)
  else
    pure (Left "Sorry, your text was empty")
```

Then, in the `Main` module:

```haskell
import Aws.Lambda
import qualified WordCount
generateLambdaDispatcher
```

# Performance

Performance is overall good, meaning that it faster than Java apps on AWS Lambda, but still not as fast as Node.js:

<style>
table, th, td {
  margin: 0 auto;
  border: 1px solid black;
  padding: 4px;
}
</style>
<table>
<thead>
<tr>
<th><strong>Runtime</strong></th>
<th><strong>Best Cold Start</strong></th>
<th><strong>Worst Cold Start</strong></th>
<th><strong>execution time</strong></th>
<th><strong>Max memory used</strong></th>
</tr>
</thead>
<tbody>
<tr>
<td><strong>Haskell</strong></td>
<td>60.30 ms</td>
<td>98.38 ms</td>
<td>0.86 ms</td>
<td><strong>48 MB</strong></td>
</tr>
<tr>
<td><strong>Java</strong></td>
<td>790 ms</td>
<td>812 ms</td>
<td>0.89 ms</td>
<td>109 MB</td>
</tr>
<tr>
<td><strong>Node.js</strong></td>
<td>3.85 ms</td>
<td>43.8 ms</td>
<td>0.26 ms</td>
<td>66 MB</td>
</tr>
</tbody>
</table>
<br/>

# A bit of background

We were there when [Werner Vogels](https://twitter.com/Werner) announced the new custom lambda runtimes on stage, and we couldn’t have been more excited. It was definitely one of our favorite announcements that morning. We have been trying Haskell (and other flavors of Haskell, like Eta and PureScript) on AWS lambda since we started working on Serverless more than a year ago. From the beginning we felt like Haskell fit like a glove in AWS Lambda — it produces fast and reliable binaries and it’s a pure functional language! There’s nothing like a pure functional language to write Lambda Functions, right?

Well, the reality is that Haskell didn’t work as well as the supported languages did. We had to apply ugly hacks to make it work, like compiling an executable/dynamic library and then wrapping it around in a Node.js module that performed a native call. We always ended up switching to TypeScript or other better supported languages for production projects. But now since AWS started supporting [custom lambda runtimes](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html), that’s all in the past!

Excited as we are? Keep reading! 👇
