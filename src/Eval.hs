module Eval where

import Grammar
import Parser

eval :: Exp -> Integer
eval (Exp o e1 e2) = op2 o (eval e1) (eval e2)
eval (ExpOp1 o e) = op1 o (eval e)
eval (ExpBrackets e) = eval e
eval (ExpInt i) = i

op2 :: Op2 -> Integer -> Integer -> Integer
op2 Plus = (+)
op2 Minus = (-)
op2 Product = (*)
op2 Division = div
op2 Modulo = mod

op1 :: Op1 -> Integer -> Integer 
op1 Min x = -x