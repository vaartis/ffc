# Base image
FROM fedora:rawhide

USER root

RUN dnf install -y ocaml ocamldoc wget llvm llvm-libs llvm-devel unzip m4 which make cmake\
                    patch pkgconfig redhat-rpm-config gcc gcc-c++ sqlite-devel libcurl-devel\
                    fuse-devel zlib-devel ocaml-camlp4-devel libcurl-devel autoconf automake

WORKDIR /ff
COPY . /ff

RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin

RUN opam init -a
RUN opam install -y ctypes llvm ounit ocamlfind ocamlbuild menhir batteries

CMD eval `opam config env` && autoreconf && ./configure --enable-tests && make test && ./run_tests.native
