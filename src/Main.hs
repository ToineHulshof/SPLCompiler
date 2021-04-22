-- A small script to automatically test all test files

module Main where

import Parser ( parseFile, testP, splP, expP )
import Control.Monad ( (>=>), forM )
import Binding ( check, compile )
import System.Environment

testFiles :: [FilePath]
testFiles = ["2D", "3D", "Example", "SumProduct", "a_bit_of_everything", "a_bit_of_everything_no_types", "arguments", "assignment_to_builtin", "bool", "brainfuck", "comment", "constants", "fac", "fout", "identity", "infinite_type_shouldfail", "integers", "list", "lists", "many_parenthesis", "more_parenthesis", "mutrec", "op", "overloading", "polymorphic_value_again_shouldfail", "polymorphic_value_indirect_shouldfail", "polymorphic_value_shouldfail", "problematic", "problematic_programs", "recursion", "return_ill_typed", "return_in_all_code_paths", "return_well_typed", "self_application_shouldfail", "shadow", "sieve", "stress_test", "sum", "unary_minus", "while", "whitespaces", "x"]

testAllFiles :: IO [()]
testAllFiles = forM testFiles (testFile True)

changeSuffix :: FilePath -> FilePath -> FilePath
changeSuffix c ".spl" = c ++ ".ssm"
changeSuffix c (x:xs) = changeSuffix (c ++ [x]) xs
changeSuffix _ _ = error "File does not have spl as extension"

testFile :: Bool -> FilePath -> IO ()
testFile addPrefix f = do
    let file = if addPrefix then "../test/testfiles/" ++ f ++ ".spl" else f
    s <- readFile file
    -- putStrLn $ "\x1b[3m" ++ f ++ ".spl \x1b[0m"
    compile (changeSuffix "" f) s

test :: IO [()]
test = testAllFiles

main :: IO ()
main = getArgs >>= testFile False . head