# Base image
FROM ubuntu:17.04

# Install CMake, compilers, GTest and GMock
RUN apt-get update && apt-get install -y cmake make gcc g++ libgtest-dev google-mock

WORKDIR /usr/src/gtest
RUN cmake .
RUN make
RUN mv libg* /usr/lib/

WORKDIR /usr/src/gmock
RUN cmake .
RUN make
RUN mv libg* /usr/lib/

WORKDIR /ff
RUN apt-get update
RUN apt-get -y install build-essential wget pkg-config software-properties-common # Install essential packages
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/zesty/ llvm-toolchain-zesty-4.0 main"
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update

RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update
RUN apt-get install -y gcc-7

RUN apt-get install -y llvm-4.0 llvm-4.0-dev lvm-4.0-runtime

RUN apt-get install -y libc++1 libc++-dev libc++abi1 libc++abi-dev git # Install libc++

ENV CC=gcc-7
ENV CXX=g++-7

COPY . /ff
RUN cmake . -Dtest=ON
RUN make
