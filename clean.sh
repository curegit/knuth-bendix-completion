#!/bin/sh
cd "$(dirname "$0")" || exit
dune clean
rm -r -f _byte
rm -r -f _native
rm -f ./*.cmi
rm -f ./*.cmo
rm -f ./*.cma
rm -f ./*.o
rm -f ./*.cmx
rm -f ./*.cmxa
