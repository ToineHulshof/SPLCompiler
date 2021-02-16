module Pratt where

import Grammar

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
