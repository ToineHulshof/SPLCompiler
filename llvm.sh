file=$(echo "$1" | cut -f 1 -d '.')
cd src &&
ghc -odir .out -hidir .out Main.hs &&
cd .. &&
./src/Main $1 -llvm &&
cd ../ssm &&
/usr/local/opt/llvm/bin/lli "${file}.ll"