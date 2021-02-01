module Lexer where

import Grammar
import Prelude hiding ( lex )
import Parser hiding ( result )
import Data.Char

data Token
    = TokenVar
    | TokenEquals
    | TokenSemicolon
    | TokenLeftBracket
    | TokenRightBracket
    | TokenDoubleColon
    | TokenLeftBrace
    | TokenRightBrace
    | TokenLeftSquareBracket
    | TokenRightSquareBracket
    | TokenArrow
    | TokenVoid
    | TokenInt 
    | TokenBool 
    | TokenChar 
    | TokenIf
    | TokenElse
    | TokenWhile
    | TokenReturn
    | TokenFalse
    | TokenTrue
    | TokenEmptyArray
    | TokenDot
    | TokenPlus 
    | TokenMinus 
    | TokenTimes
    | TokenDivide
    | TokenModulo 
    | TokenDoubleEquals
    | TokenLess
    | TokenGreater 
    | TokenLessEquals
    | TokenGreaterEquals
    | TokenNotEquals
    | TokenAnd 
    | TokenOr 
    | TokenCons 
    | TokenNot 
    | TokenInteger Int 
    | TokenID String

lex :: Code -> Either Error [Token]
lex = undefined 

comments :: Bool -> Int -> Code -> Either Error Code 
comments _ d []
    | d == 0 = Right []
    | otherwise = Left ("Did not close all comments", 0, 0)
comments s d [(x, l, c)]
    | d /= 0 = Left ("Did not close all comments", l, c)
    | s = Right []
    | otherwise = Right [(x, l, c)]
comments s d ((x1, l1, c1) : (x2, l2, c2) : xs)
    | t == "//" = comments True d xs
    | t == "/*" = comments s (d + 1) xs
    | t == "*/" && (d /= 0) = comments s (d - 1) xs
    | t == "*/" && (d == 0) = Left ("Trying to close comment that doesn't exist", l2, c2)
    | l2 > l1 && s = comments False d ((x2, l2, c2) : xs)
    | s || (d > 0) = comments s d ((x2, l2, c2) : xs)
    | otherwise = (:) (x1, l1, c1) <$> comments s d ((x2, l2, c2) : xs)
    where t = [x1, x2]

prepare :: String -> Either Error [Token]
prepare s = comments False 0 (code s) >>= lex

result :: Either Error [Token] -> String
result = undefined

lexFile :: FilePath -> IO String
lexFile f = result . prepare <$> readFile f

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib = (!!) fibs