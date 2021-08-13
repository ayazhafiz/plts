FROM ocaml/opam
SHELL ["/bin/bash", "-c"]

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
RUN opam install -y zarith_stubs_js
