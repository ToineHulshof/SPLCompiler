module Pratt where

import Grammar ( Exp, Parser(Parser), Error, Code, code )
import Data.Maybe ( isJust )
import Parser
import Data.Char ( isAlpha, isAlphaNum, isDigit, isSpace )
import Control.Applicative (Alternative ((<|>)))

data Token
    = Plus
    | Minus 
    | Times
    | Divide
    | Modulo
    | NotEquals
    | Less
    | Greater 
    | LessEquals
    | Equals
    | GreaterEquals
    | And
    | Or
    | Cons
    | LeftBracket
    | RightBracket
    | EmptyList
    | TokenID String
    | TokenInt Integer
    deriving Show

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

expP' :: Parser Exp
expP' = Parser $ \input -> do
    (rest, tokens) <- tokenize 0 input
    exp <- pratt tokens
    return (rest, exp)

pratt :: [Token] -> Either Error Exp
pratt = undefined

tokenize :: Int -> Code -> Either Error (Code, [Token])
-- tokenize d []
--     | d == 0 = Right ([], [])
--     | otherwise = Left ("Uneven brackets", 0, 0)
-- tokenize d [(x, l, c)]
--     | d < 0 || d > 1 = Left ("Uneven brackets", l, c)
--     | d == 0 = Right ([], [])
--     | d == 1 && x == ')' = Right ([], [RightBracket])
tokenize d i@((x1, l1, c1) : x@(x2, l2, c2) : xs) 
    | x1 == '-' && isDigit x2 = let (num, rest) = span (isDigit . fst3) (x : xs) in ((TokenInt ((*(-1)) $ read $ map fst3 num):) <$>) <$> tokenize d rest
    | Just token <- t = do
        (c, ts) <- tokenize d (if isJust t2 then xs else x:xs)
        return (c, token : ts)
    | x1 == ';' = Right (i, [])
    | x1 == ')' && d == 0 = Right (i, [])
    | x1 == '(' = ((LeftBracket:) <$>) <$> tokenize (d + 1) (x : xs)
    | x1 == ')' = ((RightBracket:) <$>) <$> tokenize (d - 1) (x : xs)
    | isSpace x1 = tokenize d (x : xs)
    | isAlpha x1 = let (iden, rest) = span isIden i in ((TokenID (map fst3 iden):) <$>) <$> tokenize d rest
    | isDigit x1 = let (num, rest) = span (isDigit . fst3) i in ((TokenInt (read $ map fst3 num):) <$>) <$> tokenize d rest
    where
        t1 = stringToToken [x1]
        t2 = stringToToken [x1, x2]
        t = t2 <|> t1
        s = [x1, x2]

isIden :: (Char, Int, Int) -> Bool
isIden (c, _, _) = c == '_' || isAlphaNum c

stringToToken :: String -> Maybe Token
stringToToken "!=" = Just NotEquals
stringToToken "<=" = Just LessEquals
stringToToken "==" = Just Equals
stringToToken ">=" = Just GreaterEquals
stringToToken "&&" = Just And
stringToToken "||" = Just Or
stringToToken "[]" = Just EmptyList
stringToToken "+" = Just Plus
stringToToken "-" = Just Minus
stringToToken "*" = Just Times
stringToToken "/" = Just Divide
stringToToken "%" = Just Modulo
stringToToken "<" = Just Less 
stringToToken ">" = Just Greater
stringToToken ":" = Just Cons
stringToToken t = Nothing
