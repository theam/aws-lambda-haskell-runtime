---
title: Deploying a Lambda
---

# Deploying a Lambda

To deploy your project to AWS Lambda, we have provided a `Makefile` to make your life easier.
Run `make` from the root of your project, and you will have a `build/function.zip` generated for you.

Note that this uses [**Docker**](https://www.docker.com) underneath, so make sure you have it installed
and set-up on your system.

## Manual deploy

To build the `function.zip` file manually, you need to run these commands:

```bash
# Create the output `build` directory
mkdir -p build
# Perform a clean build of the project using docker
# (In some cases `stack clean` is not needed, you can try removing it)
stack clean --docker
stack build --docker

# Copy the generated executable to the output path
cp $(stack --docker path --local-install-root)/bin/bootstrap build

# Create a `function.zip` file to upload
cd build && zip function.zip bootstrap && rm bootstrap && cd ..
```

## AWS Lambda Console

In the AWS Lambda console select `Create function` and give your function a name,
e.g. `myHaskellLambda`, and for the runtime select `'Provide your own bootstrap'`.

![creating a lambda screenshot](https://imgur.com/Q8vT9ow.png)

![filling aws lambda information](https://imgur.com/OaUL8aA.png)

Inside your function configuration, select `'Upload a .zip file'` for code entry type.
Select the `function.zip` in the Function package.   Change the Handler
to `src/Lib.handler`.

Remember to select `Save` to save your configuration.

![uploading handler](https://imgur.com/fdCmjGS.png)


Next to the Test button select `'Configure test events'` and use the following JSON for
a successful test.

![configuring test events location](https://imgur.com/mYLA3xX.png)
![configuring test events window](https://imgur.com/gpvKMSD.png)

```
{
"personName": "Bobby",
"personAge": 21
}
```

For a test which captures the error and results in a failure use the following JSON.

```
{
"personName": "Bobby",
"personAge": -1
}
```
After selecting the test you wish to run and pressing `Test` you will see the resul in the `Execution result` area.  For successful execution the returned data will be the same as the input.

In the failure case the returned data will be `'A person's age must be positive'`.

![failed execution](https://imgur.com/P4J1AQc.png)

Congratulations you have ran your first AWS Lambda using the Haskell runtime!