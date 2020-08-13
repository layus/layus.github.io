#!/bin/bash -e

trap 'rm -rf $dir' EXIT

dir=$(mktemp -d)
[ -d "$dir" ] && cd "$dir"

cat > input.tex
pdflatex -halt-on-error -interaction=nonstopmode -output-directory=$dir input.tex 1>&2

cat *.pdf
