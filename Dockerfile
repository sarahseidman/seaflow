# Based on 20.04 LTS
FROM ubuntu:focal

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=America/New_York

RUN apt-get -yq update && \
    apt-get -y upgrade && \
    apt-get -yq --no-install-suggests --no-install-recommends install \
    ocaml \
    menhir \
    llvm-10 \
    llvm-10-dev \
    m4 \
    git \
    aspcud \
    ca-certificates \
    python2.7 \
    pkg-config \
    cmake \
    opam

RUN ln -s /usr/bin/lli-10.0 /usr/bin/lli
RUN ln -s /usr/bin/llc-10.0 /usr/bin/llc

RUN opam init --disable-sandboxing
RUN opam install --yes llvm.10.0.0
RUN opam install --yes ocamlfind

RUN apt-get install ocamlbuild

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]