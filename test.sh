dirToine="/Users/toine/Radboud/CompilerConstruction/SPLCompiler/"
for t in test/testfiles/*.spl; do
    sh compile.sh "$dirToine$t"
    echo "\n"
done