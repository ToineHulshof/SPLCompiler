{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative (empty, (<|>), many, some), optional)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

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
  = FunDecl String [String] [Type] [VarDecl] [Stmt]
  deriving (Show)

data RetType
  = RetTypeType Type
  | Void
  deriving (Show)

-- data FTypes
--   = FTypes Type (Maybe FTypes)
--   deriving (Show)

data Type
  = TypeBasic BasicType
  | TypeTuple Type Type
  | TypeArray Type
  | TypeID String
  deriving (Show)

data BasicType
  = IntType
  | BoolType
  | CharType
  deriving (Show)

data Stmt
  = StmtIf ExpRec [Stmt] (Maybe [Stmt])
  | StmtWhile ExpRec [Stmt]
  | StmtField String [Field] ExpRec
  | StmtFunCall FunCall
  | StmtReturn (Maybe ExpRec)
  deriving (Show)

data ExpRec
  = ExpRecExp Exp
  | ExpRecOp2 Exp Op2 ExpRec
  | ExpRecOp1 Op1 ExpRec
  | ExpRecBrackets ExpRec
  | ExpRecTuple (ExpRec, ExpRec)
  deriving (Show)

data Exp
  = ExpField String [Field]
  | ExpInt Int
  | ExpChar Char
  | ExpBool Bool
  | ExpFunCall FunCall
  | ExpEmptyList
  deriving (Show)

data Field
  = Head
  | Tail
  | First
  | Second
  deriving (Show)

data FunCall
  = FunCall String [ExpRec]
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

eitherToMaybe :: Either Error (Code, a) -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right (_, a)) = Just a

ws :: Parser String
ws = spanP isSpace

w :: Parser a -> Parser a
w p = ws *> p <* ws

c :: Char -> Parser Char
c x = w (charP x)

charP :: Char -> Parser Char
charP x = charP' (==x)

stringP :: String -> Parser String
stringP = traverse charP

charP' :: (Char -> Bool) -> Parser Char
charP' p = Parser $ \case
  (y, l, c) : xs
    | p y -> Right (xs, y)
    | otherwise -> Left ([y], l, c)
  [] -> Left ("Unexpected EOF", 0, 0)

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
sepBy sep e = sepBy1 (ws *> sep <* ws) e <|> pure []

splP :: Parser SPL
splP = SPL <$> sepBy1 (charP ';') declP

declP :: Parser Decl
declP = declVarDeclP <|> declFunDeclP

declVarDeclP :: Parser Decl
declVarDeclP = DeclVarDecl <$> varDeclP

declFunDeclP :: Parser Decl
declFunDeclP = DeclFunDecl <$> funDeclP

funDeclP :: Parser FunDecl
funDeclP = FunDecl <$> idP <*> (c '(' *> sepBy (charP ',') idP <* c ')') <*> funTypeP <*> (c '{' *> many varDeclP) <*> (some stmtP <* c '}')

varDeclP :: Parser VarDecl
varDeclP = (varDeclVarP <|> varDeclTypeP) <* c ';'

-- Not sure if ws before strinP "var" is required
varDeclVarP :: Parser VarDecl
varDeclVarP = VarDeclVar <$> (stringP "var" *> w idP <* charP '=' <* ws) <*> expRecP

varDeclTypeP :: Parser VarDecl
varDeclTypeP = VarDeclType <$> typeP <*> (w idP <* charP '=' <* ws) <*> expRecP

retTypeP :: Parser RetType
retTypeP = voidP <|> retTypeTypeP

voidP :: Parser RetType
voidP = Void <$ stringP "Void"

retTypeTypeP :: Parser RetType
retTypeTypeP = RetTypeType <$> typeP

idP :: Parser String
idP = (:) <$> charP' isAlpha <*> spanP (\c -> isAlphaNum c || c == '_')

intP :: Parser Int
intP = read <$> digitP <|> (*(-1)) . read <$> (charP '-' *> digitP)

digitP :: Parser String
digitP = notNull $ spanP isDigit

op1P :: Parser Op1
op1P = Not <$ charP '!' <|> Min <$ charP '-'

op2P :: Parser Op2
op2P =  Plus <$ charP '+'
    <|> Minus <$ charP '-'
    <|> Product <$ charP '*'
    <|> Division <$ charP '/'
    <|> Modulo <$ charP '%'
    <|> Eq <$ stringP "=="
    <|> Leq <$ stringP "<="
    <|> Geq <$ stringP ">="
    <|> Smaller <$ charP '<'
    <|> Greater <$ charP '>'
    <|> Neq <$ stringP "!="
    <|> And <$ stringP "&&"
    <|> Or <$ stringP "||"
    <|> Cons <$ charP ':'

funCallP :: Parser FunCall
funCallP = FunCall <$> idP <*> (c '(' *> sepBy (charP ',') expRecP <* c ')')

fieldFunP :: Parser Field
fieldFunP = Head <$ stringP "hd" <|> Tail <$ stringP "tl" <|> First <$ stringP "fst" <|> Second <$ stringP "snd"

fieldP :: Parser [Field]
fieldP = many (c '.' *> fieldFunP)

expCharP :: Parser Exp
expCharP = ExpChar <$> (charP '\'' *> charP' isAlpha <* charP '\'')

expBoolP :: Parser Exp
expBoolP = ExpBool True <$ stringP "True" <|> ExpBool False <$ stringP "False"

expIntP :: Parser Exp
expIntP = ExpInt <$> intP

expFunCall :: Parser Exp
expFunCall = ExpFunCall <$> funCallP

expEmptyListP :: Parser Exp
expEmptyListP = ExpEmptyList <$ stringP "[]"

expP :: Parser Exp
expP = expIntP <|> expCharP <|> expBoolP <|> expFunCall <|> expEmptyListP <|> ExpField <$> idP <*> fieldP

expRecOp2P :: Parser ExpRec
expRecOp2P = ExpRecOp2 <$> expP <*> w op2P <*> expRecP

expRecOp1P :: Parser ExpRec
expRecOp1P = ExpRecOp1 <$> (op1P <* ws) <*> expRecP 

expRecBracketsP :: Parser ExpRec
expRecBracketsP = ExpRecBrackets <$> (charP '(' *> w expRecP <* charP ')')

expRecTupleP :: Parser ExpRec
expRecTupleP = curry ExpRecTuple <$> (charP '(' *> w expRecP <* charP ',') <*> (w expRecP <* charP ')')

expRecP :: Parser ExpRec
expRecP = expRecOp2P <|> expRecOp1P <|> expRecBracketsP <|> expRecTupleP <|> ExpRecExp <$> expP

stmtIfP :: Parser Stmt
stmtIfP = (\ex i e -> StmtIf ex i (Just e)) <$> conditionP "if" <*> stmtsP <*> (w (stringP "else") *> stmtsP) <|> (\ex i -> StmtIf ex i Nothing) <$> conditionP "if" <*> stmtsP

conditionP :: String -> Parser ExpRec
conditionP s = stringP s *> c '(' *> expRecP <* c ')'

stmtsP :: Parser [Stmt]
stmtsP = c '{' *> many stmtP <* c '}'

stmtWhileP :: Parser Stmt
stmtWhileP = StmtWhile <$> conditionP "while" <*> stmtsP 

stmtFieldP :: Parser Stmt
stmtFieldP = StmtField <$> idP <*> fieldP <*> (c '=' *> expRecP <* c ';')

stmtFunCallP :: Parser Stmt
stmtFunCallP = StmtFunCall <$> funCallP <* c ';'

stmtReturnP :: Parser Stmt
stmtReturnP = StmtReturn . pure <$> (stringP "return" *> ws *> expRecP <* c ';') <|> StmtReturn Nothing <$ stringP "return" <* c ';'

stmtP :: Parser Stmt
stmtP = stmtIfP <|> stmtWhileP <|> stmtFieldP <|> stmtFunCallP <|> stmtReturnP

basicTypeP :: Parser BasicType
basicTypeP = IntType <$ stringP "Int" <|> BoolType <$ stringP "Bool" <|> CharType <$ stringP "Char"

funTypeP :: Parser [Type]
funTypeP = w (stringP "::") *> sepBy (stringP "->") typeP <|> pure []

typeTupleP :: Parser Type
typeTupleP = TypeTuple <$> (c '(' *> typeP <* c ',') <*> typeP <* c ')'  

typeArrayP :: Parser Type
typeArrayP = TypeArray <$> (c '[' *> typeP <* c ']')

typeP :: Parser Type
typeP = typeTupleP <|> typeArrayP <|> TypeBasic <$> basicTypeP <|> TypeID <$> idP

result :: Show a => Either Error (Code, a) -> String
result (Right (_, a)) = "Parsed succesfully\n" ++ show a
result (Left (e, l, c)) = "Error: " ++ e ++ ". Line: " ++ show l ++ ", Character: " ++ show c ++ "."

code :: String -> Code
code s = [(a, b, c) | (b, d) <- zip [1 ..] $ lines s, (c, a) <- zip [1 ..] d]

parseFileP :: Show a => Parser a -> FilePath -> IO ()
parseFileP p f = readFile f >>= putStrLn . result . parse p . code

parseFile :: FilePath -> IO ()
parseFile = parseFileP splP

main :: IO String
main = getLine >>= readFile . result . parse splP . code
