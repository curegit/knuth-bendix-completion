#!/bin/sh -e
cd "$(dirname "$0")"
mkdir -p ocamlc
ocamlc -c util.ml
ocamlc -c trs.mli
ocamlc -c trs.ml
ocamlc -c kbc.mli
ocamlc -c kbc.ml
ocamlc -pack -o knuth_bendix.cmo util.cmo trs.cmo kbc.cmo
ocamlc -a -o knuth_bendix.cma knuth_bendix.cmo
#mv -f util.cmi ocamlc/util.cmi
#mv -f util.cmo ocamlc/util.cmo
#mv -f trs.cmi ocamlc/trs.cmi
#mv -f trs.cmo ocamlc/trs.cmo
#mv -f kbc.cmi ocamlc/kbc.cmi
#mv -f kbc.cmo ocamlc/kbc.cmo
mv -f knuth_bendix.cmi ocamlc/knuth_bendix.cmi
#mv -f knuth_bendix.cmo ocamlc/knuth_bendix.cmo
mv -f knuth_bendix.cma ocamlc/knuth_bendix.cma
