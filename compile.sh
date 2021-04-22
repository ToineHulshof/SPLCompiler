file=$(echo "$1" | cut -f 1 -d '.')
cd src &&
ghc -odir .out -hidir .out Main.hs &&
cd .. &&
./src/Main $1 &&
cd ../ssm &&
sh ssm.sh --cli --file "../SPLCompiler/${file}.ssm"
