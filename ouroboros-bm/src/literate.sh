#!/bin/sh

lhs2TeX -v -o Cardano_BM.tex Code.lhs \
&& \
pdflatex Cardano_BM.tex && pdflatex Cardano_BM.tex

