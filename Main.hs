{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char ( isDigit, isSpace, isAlphaNum )
import Control.Applicative ( Alternative((<|>), empty) )

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

type Goal = Expr
data Expr = ExprExpr Expr Op Term | ExprTerm Term deriving (Show)
data Term = TermID String | TermNumber Int deriving (Show)
data Op = Plus | Minus deriving (Show)

type Code = [(Char, Int, Int)]
type Error = (String, Int, Int)

newtype Parser a = Parser { parse :: Code -> Either Error (Code, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (f <$>) . p

instance Applicative Parser where
    pure x = Parser $ \code -> Right (code, x)
    (Parser p1) <*> (Parser p2) = Parser $ \code -> do
        (code', f) <- p1 code
        (code'', a) <- p2 code'
        Right (code'', f a)

instance Alternative Parser where
    empty = Parser . const $ Left ("Failed", 0, 0)
    (Parser p1) <|> (Parser p2) = Parser $ \code -> p1 code <> p2 code

ws :: Parser String
ws = spanP isSpace

charP :: Char -> Parser Char 
charP x = Parser $ \case
    (y, l, c) : xs
        | y == x -> Right (xs, x)
        | otherwise -> Left ("Found " ++ [y] ++ ", but expected " ++ [x], l, c)
    [] -> Left ("Unexpected EOF", 0, 0)

stringP :: String -> Parser String
stringP = traverse charP

opP :: Parser Op
opP = f <$> (charP '+' <|> charP '-') where
    f '+' = Plus
    f '-' = Minus

idP :: Parser Term
idP = TermID <$> notNull (spanP isAlphaNum)

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \code -> let (token, rest) = span (p . fst3) code in Right (rest, map fst3 token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \code -> do
    (code', xs) <- p code
    if null xs then Left ("Error", 0, 0) else Right (code', xs) 

numberP :: Parser Term
numberP = TermNumber . read <$> notNull (spanP isDigit)

termP :: Parser Term
termP = numberP <|> idP 

exprExprP :: Parser Expr
exprExprP = ExprExpr <$> exprP <*> (ws *> opP <* ws) <*> termP

exprTermP :: Parser Expr
exprTermP = ExprTerm <$> termP 

exprP :: Parser Expr
exprP = exprTermP <|> exprExprP

goalP :: Parser Goal
goalP = exprP

result :: Either Error (Code, a) -> String
result (Right _) = "Parsed succesfully"
result (Left (e, l, c)) = "Error: \"" ++ e ++ "\". Line: " ++ show l ++ ", Character: " ++ show c ++ "."

code :: String -> Code
code s = [(a,b,c) | (b,d) <- zip [1..] $ lines s, (c,a) <- zip [1..] d]

main :: IO String
main = getLine >>= readFile . result . parse goalP . code
