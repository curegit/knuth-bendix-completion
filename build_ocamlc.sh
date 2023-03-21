#!/bin/sh
set -e
cd "$(dirname "$0")"
mkdir -p _byte/obj
ocamlc -c util.ml
mv -f util.cmi _byte/obj/util.cmi
mv -f util.cmo _byte/obj/util.cmo
ocamlc -I _byte/obj trs.mli
mv -f trs.cmi _byte/obj/trs.cmi
ocamlc -I _byte/obj -c trs.ml
mv -f trs.cmo _byte/obj/trs.cmo
ocamlc -I _byte/obj kbc.mli
mv -f kbc.cmi _byte/obj/kbc.cmi
ocamlc -I _byte/obj -c kbc.ml
mv -f kbc.cmo _byte/obj/kbc.cmo
cd _byte
ocamlc -pack -o obj/knuth_bendix.cmo obj/util.cmo obj/trs.cmo obj/kbc.cmo
ocamlc -a -o knuth_bendix.cma obj/knuth_bendix.cmo
cp -f obj/knuth_bendix.cmi knuth_bendix.cmi
