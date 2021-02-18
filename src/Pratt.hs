module Pratt where

import Grammar
import Parser ( fst3 )

data Associativity = LeftA | RightA | NonA deriving (Show)
type Operator = (String, (Int, Associativity))

bp :: String -> Either Error (Int, Associativity)
bp s | s `elem` ["+", "-"] = Right (6, LeftA)
     | s `elem` ["*", "/", "%"] = Right (7, LeftA)
     | s `elem` ["!=", "<", "<=", "==", ">", ">="] = Right (4, NonA)
     | s == "&&" = Right (3, RightA)
     | s == "||" = Right (2, RightA)
     | s == ":" = Right (5, RightA)
     | otherwise = Left ("Unknown token", 0, 0)

bp' :: Code -> Either Error (Int, Int)
bp' code | s `elem` ["+", "-"] = Right (9, 10)
     | s `elem` ["*", "/", "%"] = Right (11, 12)
     | s `elem` ["!=", "<", "<=", "==", ">", ">="] = Right (5, 6)
     | s == "&&" = Right (3, 4)
     | s == "||" = Right (1, 2)
     | s == ":" = Right (7, 8)
     | otherwise = Left ("Unknown token", l, c)
     where s = map fst3 code
           (_, l, c) = head code
