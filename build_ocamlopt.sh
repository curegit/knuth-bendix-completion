#!/bin/sh
set -e
cd "$(dirname "$0")"
mkdir -p _native/obj
ocamlopt -for-pack Knuth_bendix -c util.ml
mv -f util.cmi _native/obj/util.cmi
mv -f util.o _native/obj/util.o
mv -f util.cmx _native/obj/util.cmx
ocamlopt -I _native/obj trs.mli
mv -f trs.cmi _native/obj/trs.cmi
ocamlopt -I _native/obj -for-pack Knuth_bendix -c trs.ml
mv -f trs.o _native/obj/trs.o
mv -f trs.cmx _native/obj/trs.cmx
ocamlopt -I _native/obj kbc.mli
mv -f kbc.cmi _native/obj/kbc.cmi
ocamlopt -I _native/obj -for-pack Knuth_bendix -c kbc.ml
mv -f kbc.o _native/obj/kbc.o
mv -f kbc.cmx _native/obj/kbc.cmx
cd _native
ocamlopt -pack -o obj/knuth_bendix.cmx obj/util.cmx obj/trs.cmx obj/kbc.cmx
ocamlopt -a -o knuth_bendix.cmxa obj/knuth_bendix.cmx
cp -f obj/knuth_bendix.cmi knuth_bendix.cmi
cp -f obj/knuth_bendix.cmx knuth_bendix.cmx
