# Base image
FROM ubuntu:17.04

# Install CMake, compilers, GTest and GMock
RUN apt-get update
RUN apt-get install -y software-properties-common wget curl

RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN add-apt-repository "deb http://apt.llvm.org/zesty/ llvm-toolchain-zesty-4.0 main"

RUN apt-get update
RUN apt-get install -y cmake make gcc-7 g++ libgtest-dev\
                       google-mock build-essential pkg-config libc++1 libc++-dev libc++abi1 libc++abi-dev git\
                       llvm-4.0 llvm-4.0-dev lvm-4.0-runtime rubygems lcov

RUN gem install coveralls-lcov
WORKDIR /usr/src/gtest
RUN cmake .
RUN cmake --build .
RUN mv libg* /usr/lib/

WORKDIR /ff
COPY . /ff
RUN cmake . -Dcoverage=ON -DCMAKE_BUILD_TYPE=Debug
