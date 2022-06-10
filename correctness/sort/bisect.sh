#!/bin/sh

rm -f ./bisect*.coverage
dune exec --instrument-with bisect_ppx ./test_sorts.exe
