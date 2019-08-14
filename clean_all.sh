#!/bin/sh
cd "$(dirname "$0")" || exit
rm -r -f _byte
rm -r -f _native
rm -f ./*.cmi
rm -f ./*.cmo
rm -f ./*.cma
rm -f ./*.o
rm -f ./*.a
rm -f ./*.cmx
rm -f ./*.cmxa
rm -f ./*.out
which dune > /dev/null && dune clean
