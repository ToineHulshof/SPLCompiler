-- Our implemented Parser with the help of the other files

{-# LANGUAGE LambdaCase #-}

module Parser where

import Grammar
-- import Types
import Control.Applicative (Alternative ((<|>), many, some))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (isNothing)
import Data.List (isPrefixOf)

-- Several definitions of helper functions which are used in the "real" parsers

-- Returns the first element of a 3-tuple
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- Parses all consecutive whitespace
ws :: Parser String
ws = spanP isSpace

-- Ignores all surrounded whitespaces for the given parser
w :: Parser a -> Parser a
w p = ws *> p <* ws

-- Parses the given Char while ignoring whitespaces
c :: Char -> Parser Char
c x = w (charP x)

-- Creates a Parser that parses the next Char that is equal to the Char in the Parser
charP :: Char -> Parser Char
charP x = satisfy (==x)

-- Creates a Parser that parses the next sequence of Chars that is equal to the String in the Parser
stringP :: String -> Parser String
stringP = traverse charP

-- Creates a Parser that parses a Char with a specific requirement
-- If the Char is fulfilling the requirement, return a (Code, Char)
-- If the Char doesnt fulfill the requirement, return an Error with the Char and its position
-- If none of the above is the case, return an Error with "Unexpected EOF"
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  (y, l, c) : xs
    | p y -> Right (xs, y)
    | otherwise -> Left [Error [y] l c]
  [] -> Left [Error "Unexpected EOF" 0 0]

-- Creates a Parser that parses all the consecutive Chars that satisfy the given requirement
spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \code -> let (token, rest) = span (p . fst3) code in Right (rest, map fst3 token)

-- Extends the given Parser with the functionality to return an Error when zero characters are parsed
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \code -> do
  (code', xs) <- p code
  if null xs then Left [Error "Error" 0 0] else Right (code', xs)

-- Parses at least 1 element of Parser b seperated by Parser a
sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep e = (:) <$> e <*> many (sep *> e)

-- Parses 0 or more elements of Parser b seperated by Parser a
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep e = sepBy1 (ws *> sep <* ws) e <|> pure []

-- The definitions that correspond with the implemented Grammar
-- These are self-explanatory, but the general idea is elaborated in the report

splP :: Parser SPL
splP = SPL <$> some (w declP)

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

varDeclVarP :: Parser VarDecl
varDeclVarP = VarDeclVar <$> (stringP "var" *> w idP <* charP '=' <* ws) <*> expP

varDeclTypeP :: Parser VarDecl
varDeclTypeP = VarDeclType <$> typeP <*> (w idP <* charP '=' <* ws) <*> expP

retTypeP :: Parser Type
retTypeP = voidP <|> retTypeTypeP

voidP :: Parser Type
voidP = Void <$ stringP "Void"

retTypeTypeP :: Parser Type
retTypeTypeP = typeP

idP :: Parser String
idP = (:) <$> satisfy isAlpha <*> spanP (\c -> isAlphaNum c || c == '_')

intP :: Parser Integer
intP = read <$> digitP <|> (*(-1)) . read <$> (charP '-' *> digitP)

digitP :: Parser String
digitP = notNull $ spanP isDigit

funCallP :: Parser FunCall
funCallP = FunCall <$> idP <*> (c '(' *> sepBy (charP ',') expP <* c ')')

fieldFunP :: Parser Field
fieldFunP = Head <$ stringP "hd" <|> Tail <$ stringP "tl" <|> First <$ stringP "fst" <|> Second <$ stringP "snd"

fieldP :: Parser [Field]
fieldP = many (c '.' *> fieldFunP)

op1P :: Parser Op1
op1P = Not <$ c '!' <|> Min <$ c '-'

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

expP :: Parser Exp
expP = expOp2P <|> expNOp2P

expNOp2P :: Parser Exp 
expNOp2P = ExpInt <$> intP <|> expBoolP <|> ExpOp1 <$> op1P <*> expP <|> ExpFunCall <$> funCallP <|> ExpField <$> idP <*> fieldP <|> expCharP <|> ExpEmptyList <$ w (stringP "[]")

expOp2P :: Parser Exp 
expOp2P = Parser $ expBP 0

expBP :: Int -> Code -> Either [Error] (Code, Exp)
expBP minBP c = do
  (c', lhs) <- parse (expBracketsP <|> expTupleP <|> expNOp2P) c
  (c'', lhs') <- lhsP 0 minBP lhs c'
  return (c'', lhs')

lhsP :: Int -> Int -> Exp -> Code -> Either [Error] (Code, Exp)
lhsP l m e c = do
  (c', o') <- help $ parse (w op2P) c
  case o' of
    Nothing -> Right (c, e)
    Just o -> do
      let (lBP, rBP) = bp o
      if lBP < m then
        Right (c, e)
      else do
        (c'', rhs) <- expBP rBP c'
        (c''', lhs) <- lhsP lBP m (Exp o e rhs) c''
        return (c''', lhs)
  where
    help :: Either [Error] (Code, Op2) -> Either [Error] (Code, Maybe Op2)
    help (Left _) = Right ([], Nothing)
    help (Right (c, o)) = Right (c, Just o)

bp :: Op2 -> (Int, Int)
bp o
  | o `elem` [Plus, Minus] = (9, 10)
  | o `elem` [Product, Division, Modulo] = (11, 12)
  | o `elem` [Neq, Smaller, Leq, Greater, Eq, Greater, Geq] = (5, 6)
  | o == And = (4, 3)
  | o == Or = (2, 1)
  | o == Cons = (8, 7)

expBoolP :: Parser Exp
expBoolP = ExpBool True <$ stringP "True" <|> ExpBool False <$ stringP "False"

expCharP :: Parser Exp
expCharP = ExpChar <$> (charP '\'' *> satisfy (const True) <* charP '\'')

expTupleP :: Parser Exp
expTupleP = curry ExpTuple <$> (c '(' *> expP <* c ',') <*> expP <* c ')'

expBracketsP :: Parser Exp
expBracketsP = ExpBrackets <$> (c '(' *> expP <* c ')')

stmtIfP :: Parser Stmt
stmtIfP = (\ex i e -> StmtIf ex i (Just e)) <$> conditionP "if" <*> stmtsP <*> (w (stringP "else") *> stmtsP) <|> (\ex i -> StmtIf ex i Nothing) <$> conditionP "if" <*> stmtsP

conditionP :: String -> Parser Exp
conditionP s = stringP s *> c '(' *> expP <* c ')'

stmtsP :: Parser [Stmt]
stmtsP = c '{' *> many stmtP <* c '}'

stmtWhileP :: Parser Stmt
stmtWhileP = StmtWhile <$> conditionP "while" <*> stmtsP 

stmtFieldP :: Parser Stmt
stmtFieldP = StmtField <$> idP <*> fieldP <*> (c '=' *> expP <* c ';')

stmtFunCallP :: Parser Stmt
stmtFunCallP = StmtFunCall <$> funCallP <* c ';'

stmtReturnP :: Parser Stmt
stmtReturnP = StmtReturn . pure <$> (stringP "return" *> ws *> expP <* c ';') <|> StmtReturn Nothing <$ stringP "return" <* c ';'

stmtP :: Parser Stmt
stmtP = stmtIfP <|> stmtWhileP <|> stmtFieldP <|> stmtReturnP <|> stmtFunCallP

basicTypeP :: Parser BasicType
basicTypeP = IntType <$ stringP "Int" <|> BoolType <$ stringP "Bool" <|> CharType <$ stringP "Char"

funTypeP :: Parser (Maybe Type)
funTypeP = (\args ret -> Just $ foldr1 TypeFun $ args ++ [ret]) <$> (w (stringP "::") *> many (w typeP)) <*> (w (stringP "->") *> retTypeP) <|> pure Nothing

typeTupleP :: Parser Type
typeTupleP = TypeTuple <$> (c '(' *> typeP <* c ',') <*> typeP <* c ')'  

typeArrayP :: Parser Type
typeArrayP = TypeArray <$> (c '[' *> typeP <* c ']')

typeP :: Parser Type
typeP = typeTupleP <|> typeArrayP <|> TypeBasic <$> basicTypeP <|> TypeID <$> idP

-- Several functions to easily apply the parser to certain programs

-- Joins a list of strings with a given seperator
join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join s (x:xs) = x ++ s ++ join s xs

-- Maps the result of the parsed program to a string which describes the result
result :: Show a => Either [Error] (Code, a) -> String
result (Right (c, a))
  | null c = "Parsed succesfully" -- ++ show a
  | otherwise = show $ Error ("Did not complete parsing: " ++ map fst3 c) 0 0
result (Left es) = join "\n" $ map show es

-- A funtion to parse (recursive) comments and returns either an Error or the Code without the comments
-- The boolean argument is true when the parser is currently in a line comment and false otherwise
-- The integer argument represents the current "level" of recursive block comments, e.g. 0 when you are not in a /* */ comment, 1 if you are and 2 if you are in a comment inside a comment
-- comments :: Bool -> Int -> Code -> Either [Error] Code
-- comments _ d []
--   | d == 0 = Right [] -- Parser is done and the parser is not currently in a block comment 
--   | otherwise = Left ("Did not close all comments", 0, 0) -- Parser is done, but is currently in a block comment
-- comments s d (('/', l1, c1) : ('/', l2, c2) : xs) = comments True d xs
-- comments s d (('/', l1, c1) : ('*', l2, c2) : xs) = comments s (d + 1) xs
-- comments s d (('*', l1, c1) : ('/', l2, c2) : xs)
--   | d /= 0 = comments s (d - 1) xs
--   | otherwise = Left ("Trying to close comment that doesn't exist", l2, c2)
-- comments s d ((x1, l1, c1) : (x2, l2, c2) : xs)
--   | l2 > l1 && s = comments False d ((x2, l2, c2) : xs)
--   | s || (d > 0) = comments s d ((x2, l2, c2) : xs)
-- comments s d ((x1, l1, c1) : xs) = (:) (x1, l1, c1) <$> comments s d xs

-- A funtion to parse (recursive) comments and returns either an Error or the Code without the comments
-- The boolean argument is true when the parser is currently in a line comment and false otherwise
-- The integer argument represents the current "level" of recursive block comments, e.g. 0 when you are not in a /* */ comment, 1 if you are and 2 if you are in a comment inside a comment
comments :: Bool -> Int -> Code -> Either [Error] Code 
comments _ d []
    | d == 0 = Right [] -- Parser is done and the parser is not currently in a block comment 
    | otherwise = Left [Error "Did not close all comments" 0 0] -- Parser is done, but is currently in a block comment
comments s d [(x, l, c)]
    | d /= 0 = Left [Error "Did not close all comments" l c] -- Parser only has one character left, but it is still in a block comment, so this can't be closed
    | s = Right [] -- Parser only has one character left and is currently in a line comment
    | otherwise = Right [(x, l, c)] -- Parser has only one character left and isn't in a comment
comments s d ((x1, l1, c1) : (x2, l2, c2) : xs)
    | t == "//" = comments True d xs -- Parser recognizes it is in a line comment
    | t == "/*" = comments s (d + 1) xs -- Parser starts a new recursive block comment
    | t == "*/" && (d /= 0) = comments s (d - 1) xs -- Parser closes a valid recursive block comment 
    | t == "*/" && (d == 0) = Left [Error "Trying to close comment that doesn't exist" l2 c2] -- Parser closes an invalid recursive block comment
    | l2 > l1 && s = comments False d ((x2, l2, c2) : xs) -- The end of line is reached, so the line comment is reset if it was active
    | s || (d > 0) = comments s d ((x2, l2, c2) : xs) -- Parser is currently in a comment and ignores the next character
    | otherwise = (:) (x1, l1, c1) <$> comments s d ((x2, l2, c2) : xs) -- Parser isn't in a comment currently and adds the current character to the returned Code
    where t = [x1, x2]

-- A function that parses a file for a given parser
parseFileP :: Show a => Parser a -> (Either [Error] (Code, a) -> String) -> FilePath -> IO ()
parseFileP p r f = readFile f >>= putStrLn . help where
  help :: String -> String
  help s = r $ comments False 0 (code s) >>= parse p --(++ " " ++ f) $ 

-- A function that parses the program in the given FilePath
parseFile :: FilePath -> IO ()
parseFile = parseFileP splP result

-- A helper function to test if a parser behaves correctly on a given input.
testP :: Parser a -> String -> Either [Error] (Code, a)
testP p s = comments False 0 (code s) >>= parse p

p :: String -> Either [Error] (Code, SPL)
p = parse splP . code

-- A function that transforms a string to a list of tuples with (character, line, column)
code :: String -> Code
code s = [(a, b, c) | (b, d) <- zip [1..] $ lines s, (c, a) <- zip [1 ..] d]

-- Parses the file provided in the IO
main :: IO ()
main = getLine >>= parseFile
