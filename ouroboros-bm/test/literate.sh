#!/bin/sh

lhs2TeX -v -o Testing.tex Main.lhs

pdflatex Testing.tex && pdflatex Testing.tex

