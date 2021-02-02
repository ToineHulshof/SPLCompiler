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
    | TokenIntType
    | TokenBoolType
    | TokenCharType
    | TokenIf
    | TokenElse
    | TokenWhile
    | TokenReturn
    | TokenEmptyArray
    | TokenDot
    | TokenNot 
    | TokenOp2 Op2Token
    | TokenBool Bool
    | TokenInteger Int 
    | TokenID String

data Op2Token
    = Op2Plus 
    | Op2Minus 
    | Op2Times
    | Op2Divide
    | Op2Modulo 
    | Op2DoubleEquals
    | Op2Less
    | Op2Greater 
    | Op2LessEquals
    | Op2GreaterEquals
    | Op2NotEquals
    | Op2And 
    | Op2Or 
    | Op2Cons


lex :: Code -> Either Error [Token]
lex [] = Right []
lex ((x, l, c) : xs)
    | x == ';' = (:) <$> Right TokenSemicolon <*> lex xs
    | x == '(' = (:) <$> Right TokenLeftBrace <*> lex xs
    | x == ')' = (:) <$> Right TokenLeftBrace <*> lex xs
    | x == '{' = (:) <$> Right TokenLeftBracket <*> lex xs
    | x == '}' = (:) <$> Right TokenLeftBracket <*> lex xs
    | x == '[' = (:) <$> Right TokenLeftSquareBracket <*> lex xs
    | x == ']' = (:) <$> Right TokenRightSquareBracket <*> lex xs
    | x == '.' = (:) <$> Right TokenDot <*> lex xs
    | x == '!' = (:) <$> Right TokenNot <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs
    | x == '+' = (:) <$> Right (TokenOp2 Op2Plus) <*> lex xs

prepare :: String -> Either Error [Token]
prepare s = comments False 0 (code s) >>= lex

result :: Either Error [Token] -> String
result = undefined

lexFile :: FilePath -> IO String
lexFile f = result . prepare <$> readFile f
