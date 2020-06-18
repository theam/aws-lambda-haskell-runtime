FROM lambci/lambda:build-provided

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

# Saving default system libraries before doing anything else
RUN du -a /lib64 /usr/lib64 | cut -f2 > /root/default-libraries

# Installing basic dependencies
RUN yum install -y postgresql-devel \
    git-core \
    tar \
    sudo \
    xz \
    make \
    postgresql-devel \
    libyaml libyaml-devel

# Installing Haskell Stack
RUN sudo curl -sSL https://get.haskellstack.org/ | sh

ARG STACK_RESOLVER=lts-15.16

# Setting up GHC
RUN stack setup --resolver=${STACK_RESOLVER}

# Installing common packages so that docker builds are faster
RUN stack install --resolver=${STACK_RESOLVER} postgresql-simple persistent-template persistent-postgresql persistent
RUN stack install --resolver=${STACK_RESOLVER} text bytestring http-client http-types path template-haskell case-insensitive aeson unordered-containers
RUN stack install --resolver=${STACK_RESOLVER} servant wai servant-server

RUN mkdir /root/lambda-function

ARG PACKAGER_COMMIT_SHA=6404623b59a4189c17cadeb2c5a2eb96f1a76722
RUN cd /tmp && \
    git clone https://github.com/saurabhnanda/aws-lambda-packager.git && \
    cd /tmp/aws-lambda-packager && \
    git checkout ${PACKAGER_COMMIT_SHA} && \
    stack install --resolver=${STACK_RESOLVER}

# Copying the source code of the lambda function into the Docker container
COPY . /root/lambda-function/

RUN pwd

# Building the lambda-function and copying it to the output directory
RUN cd /root/lambda-function
WORKDIR /root/lambda-function/
RUN ls
RUN stack clean --full
RUN stack build --fast

ARG OUTPUT_DIR=/root/output
RUN mkdir ${OUTPUT_DIR} && \
    mkdir ${OUTPUT_DIR}/lib

RUN cp $(stack path --local-install-root)/bin/bootstrap ${OUTPUT_DIR}/bootstrap

# Finally, copying over all custom/extra libraries with the help of aws-lambda-packager
RUN /root/.local/bin/aws-lambda-packager copy-custom-libraries \
    -l /root/default-libraries \
    -f /root/output/bootstrap \
    -o /root/output/lib

ENTRYPOINT sh
