FROM ocaml/opam
SHELL ["/bin/bash", "-c"]

RUN opam switch create 4.14.0
RUN opam switch set 4.14.0
RUN opam pin strictly-annotated https://github.com/ayazhafiz/strictly-annotated.git
RUN opam install -y dune
RUN opam install -y menhir
RUN opam install -y alcotest
RUN opam install -y js_of_ocaml
RUN opam install -y js_of_ocaml-compiler
RUN opam install -y js_of_ocaml-ppx
RUN opam install -y ppx_inline_test
RUN opam install -y ppx_expect
RUN opam install -y ocamlgraph
RUN sudo apt install -y libgmp-dev
RUN opam install -y zarith
RUN opam install -y zarith_stubs_js
RUN opam install -y camomile
RUN opam pin dts_of_ocaml https://github.com/ayazhafiz/dts_of_ocaml.git
RUN opam install -y sedlex
# Tiger
RUN opam pin tigerc_data https://github.com/ayazhafiz/tiger.git
RUN opam pin tigerc_util https://github.com/ayazhafiz/tiger.git
RUN opam pin tigerc_front https://github.com/ayazhafiz/tiger.git
RUN opam pin tigerc_back https://github.com/ayazhafiz/tiger.git
RUN opam pin tiger https://github.com/ayazhafiz/tiger.git

RUN sudo apt-get update && sudo apt-get install -y \
  ca-certificates \
  curl
ARG NODE_VERSION=16.14.0
ARG NODE_PACKAGE=node-v$NODE_VERSION-linux-x64
RUN mkdir $HOME/bin
ARG NODE_HOME=$HOME/bin/$NODE_PACKAGE
ENV NODE_PATH $NODE_HOME/lib/node_modules
ENV PATH $NODE_HOME/bin:$PATH
RUN curl https://nodejs.org/dist/v$NODE_VERSION/$NODE_PACKAGE.tar.gz | tar -xzC $HOME/bin/
