{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative (empty, (<|>), many))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

type Code = [(Char, Int, Int)]
type Error = (String, Int, Int)

newtype Parser a = Parser {parse :: Code -> Either Error (Code, a)}

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

newtype SPL
  = SPL [Decl]
  deriving (Show)

data Decl
  = DeclVarDecl VarDecl
  | DeclFunDecl FunDecl
  deriving (Show)

data VarDecl
  = VarDeclVar String ExpRec
  | VarDeclType Type String ExpRec
  deriving (Show)

data FunDecl
  = FunDecl String (Maybe FArgs) (Maybe FunType) [VarDecl] [Stmt]
  deriving (Show)

data RetType
  = RetTypeType Type
  | Void
  deriving (Show)

data FunType
  = FunType (Maybe FTypes) RetType
  deriving (Show)

data FTypes
  = FTypes Type (Maybe FTypes)
  deriving (Show)

data Type
  = TypeBasic BasicType
  | TypeTuple (Type, Type)
  | TypeArray [Type]
  deriving (Show)

data BasicType
  = IntType
  | BoolType
  | CharType
  deriving (Show)

data FArgs
  = FArgs (Maybe FArgs) String
  deriving (Show)

data Stmt
  = StmtIf ExpRec [Stmt] (Maybe [Stmt])
  | StmtWhile Exp [Stmt]
  | StmtField String FieldRec ExpRec
  | StmtFunCall FunCall
  | StmtReturn (Maybe ExpRec)
  deriving (Show)

data ExpRec
  = Exp
  | ExpRecOp2 Exp Op2 ExpRec
  | ExpRecOp1 Op1 ExpRec
  | ExpRecBrackets ExpRec
  | ExpRecTuple (ExpRec, ExpRec)
  deriving (Show)

data Exp
  = ExpField String FieldRec
  | ExpChar Char
  | ExpBool Bool
  | ExpFunCall FunCall
  | ExpEmptyList
  deriving (Show)

newtype FieldRec
  = FieldRec (Maybe Field)
  deriving (Show)

data Field
  = Field FieldRec FieldFun
  deriving (Show)

data FieldFun
  = Hd
  | Tl
  | Fst
  | Snd
  deriving (Show)

data FunCall
  = FunCall String (Maybe ActArgs)
  deriving (Show)

data ActArgs
  = ActArgs ExpRec (Maybe ActArgs)
  deriving (Show)

data Op2
  = Plus
  | Minus
  | Product
  | Division
  | Modulo
  | Eq
  | Smaller
  | Greater
  | Leq
  | Geq
  | Neq
  | And
  | Or
  | Cons
  deriving (Show)

data Op1
  = Not
  | Min
  deriving (Show)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

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

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \code -> let (token, rest) = span (p . fst3) code in Right (rest, map fst3 token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \code -> do
  (code', xs) <- p code
  if null xs then Left ("Error", 0, 0) else Right (code', xs)

-- Parses at least 1 element
sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep e = (:) <$> e <*> many (sep *> e)

-- Parses 0 or more elements
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep e = sepBy1 sep e <|> pure []

splP :: Parser SPL
splP = undefined

declP :: Parser Decl
declP = declVarDeclP <|> declFunDeclP

declVarDeclP :: Parser Decl
declVarDeclP = DeclVarDecl <$> varDeclP

declFunDeclP :: Parser Decl
declFunDeclP = DeclFunDecl <$> funDeclP

funDeclP :: Parser FunDecl
funDeclP = FunDecl <$> idP <*> fArgsP <*> funTypeP <*> v <*> s
  where
    v = sepBy sep varDeclP
    s = sepBy1 sep stmtP
    sep = ws *> charP ';' <* ws

varDeclP :: Parser VarDecl
varDeclP = varDeclVarP <|> varDeclTypeP

-- Not sure if ws before strinP "var" is required
varDeclVarP :: Parser VarDecl
varDeclVarP = VarDeclVar <$> (stringP "var" *> ws *> idP <* ws <* charP '=' <* ws) <*> expRecP

varDeclTypeP :: Parser VarDecl
varDeclTypeP = VarDeclType <$> typeP <*> (ws *> idP <* ws <* charP '=' <* ws) <*> expRecP

retTypeP :: Parser RetType
retTypeP = voidP <|> retTypeTypeP

voidP :: Parser RetType
voidP = Void <$ stringP "Void"

retTypeTypeP :: Parser RetType
retTypeTypeP = RetTypeType <$> typeP

expRecP :: Parser ExpRec
expRecP = undefined

typeP :: Parser Type
typeP = undefined

stmtP :: Parser Stmt
stmtP = undefined

fArgsP :: Parser (Maybe FArgs)
fArgsP = undefined

funTypeP :: Parser (Maybe FunType)
funTypeP = undefined

idP :: Parser String
idP = (++) <$> spanP isAlpha <*> spanP (\c -> isAlphaNum c || c == '_')

intP :: Parser Int
intP = (read <$> digitP) <|> ((*(-1)) . read <$> (charP '-' *> digitP)) 

digitP :: Parser String
digitP = notNull $ spanP isDigit

op1P :: Parser Op1
op1P = (Not <$ charP '!') <|> (Min <$ charP '-')

op2P :: Parser Op2
op2P = (Plus <$ charP '+') <|> (Minus <$ charP '-') <|> (Product <$ charP '*') <|> (Division <$ charP '/') <|> (Modulo <$ charP '%') <|> (Eq <$ stringP "==") <|> (Smaller <$ charP '<') <|> (Greater <$ charP '>') <|> (Plus <$ charP '+') <|> (Leq <$ stringP "<=") <|> (Geq <$ stringP ">=") <|> (Plus <$ charP '+') <|> (Neq <$ stringP "!=") <|> (And <$ stringP "&&") <|> (Or <$ stringP "||") <|> (Cons <$ charP ':')

result :: Either Error (Code, a) -> String
result (Right _) = "Parsed succesfully"
result (Left (e, l, c)) = "Error: \"" ++ e ++ "\". Line: " ++ show l ++ ", Character: " ++ show c ++ "."

code :: String -> Code
code s = [(a, b, c) | (b, d) <- zip [1 ..] $ lines s, (c, a) <- zip [1 ..] d]

main :: IO String
main = getLine >>= readFile . result . parse splP . code
