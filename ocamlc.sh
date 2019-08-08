ocamlc -c util.ml
ocamlc -c trs.mli
ocamlc -c trs.ml
ocamlc -c kbc.mli
ocamlc -c kbc.ml
ocamlc -pack -o knuth_bendix.cmo util.cmo trs.cmo kbc.cmo
ocamlc -a -o knuth_bendix.cma knuth_bendix.cmo
