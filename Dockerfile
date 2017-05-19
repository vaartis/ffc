# Base image
FROM fedora:rawhide

# Install CMake, compilers, GTest
RUN dnf install -y wget cmake make gcc-c++ pkgconfig git llvm\
                   llvm-libs llvm-devel libedit-devel zlib-devel\
                   elfutils-devel libcurl-devel python2

RUN git clone https://github.com/SimonKagstrom/kcov && cd kcov && git checkout tags/v31 && cmake . && make install

WORKDIR /ff
COPY . /ff

RUN cmake . -Dtest=ON -DCMAKE_BUILD_TYPE=Debug
