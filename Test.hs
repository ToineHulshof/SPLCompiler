module Test where

import Parse ( parseFile )
import Control.Monad ( forM )

testFiles :: [FilePath]
testFiles = ["2D.spl", "3D.spl", "Example.spl", "SumProduct.spl", "a_bit_of_everything.spl", "arguments.spl", "assignment_to_builtin.spl", "bool.spl", "brainfuck.spl", "code.spl", "comment.spl", "constants.spl", "constants_corner_cases.spl", "empty.spl", "higher_order_functions.spl", "identity.spl", "infinite_type_shouldfail.spl", "integers.spl", "list.spl", "list_ops.spl", "lists.spl", "many_parenthesis.spl", "monomorph.spl", "more_parenthesis.spl", "multiple_recursion.spl", "multiple_recursion_values.spl", "mutrec.spl", "op.spl", "overloading.spl", "polymorphic_value_again_shouldfail.spl", "polymorphic_value_indirect_shouldfail.spl", "polymorphic_value_shouldfail.spl", "problematic.spl", "problematic_programs.spl", "recursion.spl", "return_ill_typed.spl", "return_in_all_code_paths.spl", "return_well_typed.spl", "self_application_shouldfail.spl", "shadow.spl", "sieve.spl", "stress.spl", "stress_test.spl", "sum.spl", "unary_minus.spl", "unbalanced_parenthesis.spl", "unbalanced_parenthesis2.spl", "while.spl", "whitespaces.spl", "x.spl"]

testAllFiles :: IO [()]
testAllFiles = forM (map ("tests/"++) testFiles) parseFile

-- Known errors:
-- Op de manier waarop wij expRec doen, kun je niet (a + b) + c parsen.
-- Multiline comments kunnen blijkbaar ook op 1 regel