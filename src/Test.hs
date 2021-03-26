-- A small script to automatically test all test files

module Test where

import Parser ( parseFile, testP, splP, expP )
import Control.Monad ( (>=>), forM )
import Binding ( test )

testFiles :: [FilePath]
testFiles = ["2D.spl", "3D.spl", "Example.spl", "SumProduct.spl", "a_bit_of_everything.spl", "arguments.spl", "assignment_to_builtin.spl", "bool.spl", "brainfuck.spl", "comment.spl", "constants.spl", "fac.spl", "identity.spl", "infinite_type_shouldfail.spl", "integers.spl", "list.spl", "lists.spl", "many_parenthesis.spl", "more_parenthesis.spl", "mutrec.spl", "op.spl", "overloading.spl", "polymorphic_value_again_shouldfail.spl", "polymorphic_value_indirect_shouldfail.spl", "polymorphic_value_shouldfail.spl", "problematic.spl", "problematic_programs.spl", "recursion.spl", "return_ill_typed.spl", "return_in_all_code_paths.spl", "return_well_typed.spl", "self_application_shouldfail.spl", "shadow.spl", "sieve.spl", "stress_test.spl", "sum.spl", "unary_minus.spl", "while.spl", "whitespaces.spl", "x.spl"]

testAllFiles :: IO [()]
testAllFiles = forM (map ("../test/testfiles/"++) testFiles) (readFile Control.Monad.>=> test)

testFile :: FilePath -> IO ()
testFile = readFile Control.Monad.>=> test

main :: IO [()]
main = testAllFiles