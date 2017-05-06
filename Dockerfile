# Base image
FROM ubuntu:17.04

# Install CMake, compilers, GTest and GMock
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update
RUN apt-get install -y cmake make gcc-7 g++ libgtest-dev google-mock

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
RUN apt-get -y install build-essential wget pkg-config  # Install essential packages
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/zesty/ llvm-toolchain-zesty-4.0 main"

RUN apt-get install -y llvm-4.0 llvm-4.0-dev lvm-4.0-runtime

RUN apt-get install -y libc++1 libc++-dev libc++abi1 libc++abi-dev git # Install libc++

COPY . /ff
RUN cmake . -Dtest=ON
RUN make
