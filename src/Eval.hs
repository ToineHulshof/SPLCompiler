-- In this file the evaluation of Terms and Factors is handed (namely the problem with associativity)
-- The suggested method in the lectures is used. More details can be found in the report

module Eval where

import Grammar
import Parser ( termP, code )

-- Puts an integer (often an evaluated integer) at the front of a Term datatype
intToTerm :: [(TermOp, Factor)] -> Integer -> Term
intToTerm fs i = Term (Factor (ExpInt i) []) fs

-- Puts an integer at the front of a Factor datatype
intToFactor :: [(FactorOp, BottomExp)] -> Integer -> Factor
intToFactor bs i = Factor (ExpInt i) bs

-- Evaluates the Term dataType to an integer
-- Note that + and - are both operands for the Term and thus, this function is left-associative
termE :: Term -> Integer
termE (Term f []) = factorE f
termE (Term f1 ((o, f2) : fs)) = termE $ intToTerm fs $ termOp o (factorE f1) (factorE f2)

-- Evaluates the Factor dataType to an integer
-- Note that * and / are both operands for the Factor and thus, this function is left-associative
factorE :: Factor -> Integer
factorE (Factor b []) = bottomE b
factorE (Factor b1 ((o, b2) : bs)) = factorE $ intToFactor bs $ factorOp o (bottomE b1) (bottomE b2)

-- Evaluates a BottemExp to an Integer
-- The evaluation is only needed for integers, so only a case for an ExpInt is implemented
bottomE :: BottomExp -> Integer
bottomE (ExpInt i) = i

-- Maps the TermOp to the actual operator
termOp :: TermOp -> Integer -> Integer -> Integer
termOp Add = (+)
termOp Subtract = (-)

-- Maps the FactorOp to the actual operator
factorOp :: FactorOp -> Integer -> Integer -> Integer
factorOp Times = (*)
factorOp Divides = div