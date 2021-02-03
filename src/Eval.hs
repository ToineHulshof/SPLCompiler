module Eval where

import Grammar
import Parser ( termP, code )

intToTerm :: [(TermOp, Factor)] -> Int -> Term
intToTerm fs i = Term (Factor (ExpInt i) []) fs

intToFactor :: [(FactorOp, BottomExp)] -> Int -> Factor
intToFactor bs i = Factor (ExpInt i) bs

termE :: Term -> Int
termE (Term f []) = factorE f
termE (Term f1 ((o, f2) : fs)) = termE $ intToTerm fs $ termOp o (factorE f1) (factorE f2)

factorE :: Factor -> Int
factorE (Factor b []) = bottomE b
factorE (Factor b1 ((o, b2) : bs)) = factorE $ intToFactor bs $ factorOp o (bottomE b1) (bottomE b2)

bottomE :: BottomExp -> Int 
bottomE (ExpInt i) = i

termOp :: TermOp -> Int -> Int -> Int
termOp Add = (+)
termOp Subtract = (-)

factorOp :: FactorOp -> Int -> Int -> Int
factorOp Times = (*)
factorOp Divides = div