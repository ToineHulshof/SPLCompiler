#!/bin/bash

file=$(echo "$1" | cut -f 1 -d '.')
echo $file
cd src &&
ghc Main.hs &&
cd .. &&
./src/Main $1 &&
cd ../ssm &&
sh ssm.sh --cli --file "../SPLCompiler/${file}.ssm"
#tests/t14slides.ssm #