-- Our implemented Parser with the help of the other files

{-# LANGUAGE LambdaCase #-}

module Parser where

import Grammar
import Errors
import Control.Applicative ( Alternative((<|>), some, many), optional )
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (isNothing, listToMaybe)
import Data.List (isPrefixOf)
import Debug.Trace ( trace )

-- Several definitions of helper functions which are used in the "real" parsers

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
charP x = satisfy (==x) True

-- Does not consume the character
charP' :: Char -> Parser (Positioned Char)
charP' x = satisfy' (==x) False

-- Creates a Parser that parses the next sequence of Chars that is equal to the String in the Parser
stringP :: String -> Parser String
stringP = traverse charP

-- Creates a Parser that parses a Char with a specific requirement
-- If the Char is fulfilling the requirement, return a (Code, Char)
-- If the Char doesnt fulfill the requirement, return an Error with the Char and its position
-- If none of the above is the case, return an Error with "Unexpected EOF"
satisfy' :: (Char -> Bool) -> Bool -> Parser (Positioned Char)
satisfy' p consume = Parser $ \case
  (y, (l, c)) : xs
    | p y -> ([], Just (if consume then xs else (y, (l, c)) : xs, (y, (l, c))))
    | otherwise -> ([Error ParseError [y] ([y], (l, c))], Nothing)
  [] -> ([Error ParseError "Unexpected EOF" ("", (1, 1))], Nothing)

satisfy :: (Char -> Bool) -> Bool -> Parser Char
satisfy p consume = fst <$> satisfy' p consume

-- Creates a Parser that parses all the consecutive Chars that satisfy the given requirement
spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \code -> let (token, rest) = span (p . fst) code in ([], Just (rest, map fst token))

-- Extends the given Parser with the functionality to return an Error when zero characters are parsed
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser help
  where
    help c = case r of
      Nothing -> (e, Nothing)
      Just (c', xs) -> if null xs then ([Error ParseError "Found 0, while at least 1 is expected." ("", snd $ head c')], Nothing) else (e, Just (c', xs))
      where
        (e, r) = p c

-- Parses at least 1 element of Parser b seperated by Parser a
sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep e = (:) <$> e <*> many (sep *> e)

-- Parses 0 or more elements of Parser b seperated by Parser a
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep e = sepBy1 (ws *> sep <* ws) e <|> pure []

-- The definitions that correspond with the implemented Grammar
-- These are self-explanatory, but the general idea is elaborated in the report

splP :: Parser SPL
splP = some (w declP)

declP :: Parser Decl
declP = declVarDeclP <|> declFunDeclP <|> DeclError <$> errorP "\n"

declVarDeclP :: Parser Decl
declVarDeclP = DeclVarDecl <$> varDeclP

declFunDeclP :: Parser Decl
declFunDeclP = DeclFunDecl <$> funDeclP

funDeclP :: Parser FunDecl
funDeclP = FunDecl <$> idP <*> (c '(' *> sepBy (charP ',') idP <* c ')') <*> funTypeP <*> (c '{' *> many varDeclP) <*> (some stmtP <* c '}')

optP :: Char -> Parser Char
optP ch = Parser $ \case
  ((x, (l, c)):xs)
    | x == ch -> ([], Just (xs, ch))
    | otherwise -> ([Error ParseError ("Missing \"" ++ [ch] ++ "\" inserted") (" ", (l, c))], Just ((x, (l, c)):xs, ch))
  [] -> ([Error ParseError ("Missing \"" ++ [ch] ++ "\" inserted") (" ", (1, 1))], Just ([], ch))

varDeclP :: Parser VarDecl
varDeclP = (varDeclVarP <|> varDeclTypeP) <* optP ';'

varDeclVarP :: Parser VarDecl
varDeclVarP = VarDecl Nothing <$> (stringP "var" *> w idP <* charP '=' <* ws) <*> expP

varDeclTypeP :: Parser VarDecl
varDeclTypeP = VarDecl . Just <$> typeP <*> (w idP <* charP '=' <* ws) <*> expP

retTypeP :: Parser Type
retTypeP = voidP <|> retTypeTypeP

voidP :: Parser Type
voidP = Void <$ stringP "Void"

retTypeTypeP :: Parser Type
retTypeTypeP = typeP

idP :: Parser String
idP = (:) <$> satisfy isAlpha True <*> spanP (\c -> isAlphaNum c || c == '_')

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
    <|> Equals <$ stringP "=="
    <|> Leq <$ stringP "<="
    <|> Geq <$ stringP ">="
    <|> Smaller <$ charP '<'
    <|> Greater <$ charP '>'
    <|> Neq <$ stringP "!="
    <|> And <$ stringP "&&"
    <|> Or <$ stringP "||"
    <|> Cons <$ charP ':'

errorP :: [Char] -> Parser (Positioned String)
errorP cs = (\s (_, (l, c)) -> (s, (l, c - length s))) <$> spanP (`notElem` cs) <*> charsP cs

charsP :: [Char] -> Parser (Positioned Char)
charsP [c] = charP' c
charsP (c:cs) = charP' c <|> charsP cs

expP :: Parser Exp
expP = expOp2P <|> expNOp2P

expNOp2P :: Parser Exp 
expNOp2P = ExpInt <$> intP <|> expBoolP <|> ExpOp1 <$> op1P <*> expP <|> ExpFunCall <$> funCallP <|> ExpField <$> idP <*> fieldP <|> expCharP <|> ExpEmptyList <$ w (stringP "[]") <|> (ExpError <$> errorP ";\n")

expOp2P :: Parser Exp 
expOp2P = Parser $ expBP 0

expBP :: Int -> Code -> ([Error], Maybe (Code, Exp))
expBP minBP c = case r1 of
  Nothing -> (e1, Nothing)
  Just (c', lhs) -> case r2 of
    Nothing -> (e2, Nothing)
    Just (c'', lhs') -> ([], Just (c'', lhs'))
    where
      (e2, r2) = lhsP 0 minBP lhs c'
  where
    (e1, r1) = parse (expBracketsP <|> expTupleP <|> expNOp2P) c

lhsP :: Int -> Int -> Exp -> Code -> ([Error], Maybe (Code, Exp))
lhsP l m e c = case r of
  Nothing -> (e1, Just (c, e))
  Just (c', o) -> if lBP < m then (e1, Just (c, e)) else case r2 of
    Nothing -> (e2, Nothing)
    Just (c'', rhs) -> case r3 of
      Nothing -> (e3, Nothing)
      Just (c''', lhs) -> ([], Just (c''', lhs))
      where
        (e3, r3) = lhsP lBP m (Exp o e rhs) c''
    where
      (lBP, rBP) = bp o
      (e2, r2) = expBP rBP c'
  where
    (e1, r) = parse (w op2P) c

bp :: Op2 -> (Int, Int)
bp o
  | o `elem` [Plus, Minus] = (9, 10)
  | o `elem` [Product, Division, Modulo] = (11, 12)
  | o `elem` [Neq, Smaller, Leq, Greater, Equals, Greater, Geq] = (5, 6)
  | o == And = (4, 3)
  | o == Or = (2, 1)
  | o == Cons = (8, 7)

expBoolP :: Parser Exp
expBoolP = ExpBool True <$ stringP "True" <|> ExpBool False <$ stringP "False"

expCharP :: Parser Exp
expCharP = ExpChar <$> (charP '\'' *> satisfy (const True) True <* charP '\'')

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
typeP = typeTupleP <|> typeArrayP <|> TypeBasic <$> basicTypeP <|> TypeID Nothing <$> idP

-- Several functions to easily apply the parser to certain programs

-- Maps the result of the parsed program to a string which describes the result
result :: Show a => ([Error], Maybe (Code, a)) -> String
result ([], Just (c, a))
  | null c = "Parsed succesfully" -- ++ show a
  | otherwise = show $ Error ParseError "Did not complete parsing" (map fst c, snd $ head c)
result (es, _) = join "\n" $ map show es

-- A funtion to parse (recursive) comments and returns either an Error or the Code without the comments
-- The boolean argument is true when the parser is currently in a line comment and false otherwise
-- The integer argument represents the current "level" of recursive block comments, e.g. 0 when you are not in a /* */ comment, 1 if you are and 2 if you are in a comment inside a comment
comments :: Bool -> Int -> Code -> ([Error], Code)
comments _ d []
    | d == 0 = ([], []) -- Parser is done and the parser is not currently in a block comment 
    | otherwise = ([Error ParseError "Did not close all comments" ("", (1, 1))], []) -- Parser is done, but is currently in a block comment
comments s d [(x, (l, c))]
    | d /= 0 = ([Error ParseError "Did not close all comments" ("", (l, c))], []) -- Parser only has one character left, but it is still in a block comment, so this can't be closed
    | s = ([], []) -- Parser only has one character left and is currently in a line comment
    | otherwise = ([], [(x, (l, c))]) -- Parser has only one character left and isn't in a comment
comments s d ((x1, (l1, c1)) : (x2, (l2, c2)) : xs)
    | t == "//" = comments True d xs -- Parser recognizes it is in a line comment
    | t == "/*" = comments s (d + 1) xs -- Parser starts a new recursive block comment
    | t == "*/" && (d /= 0) = comments s (d - 1) xs -- Parser closes a valid recursive block comment 
    | t == "*/" && (d == 0) = ([Error ParseError "Trying to close comment that doesn't exist" ("*/", (l2, c2))], xs) -- Parser closes an invalid recursive block comment
    | l2 > l1 && s = comments False d ((x2, (l2, c2)) : xs) -- The end of line is reached, so the line comment is reset if it was active
    | s || (d > 0) = comments s d ((x2, (l2, c2)) : xs) -- Parser is currently in a comment and ignores the next character
    | otherwise = (:) (x1, (l1, c1)) <$> comments s d ((x2, (l2, c2)) : xs) -- Parser isn't in a comment currently and adds the current character to the returned Code
    where t = [x1, x2]

-- A function that parses a file for a given parser
parseFileP :: Show a => Parser a -> (([Error], Maybe (Code, a)) -> String) -> FilePath -> IO ()
parseFileP p r f = readFile f >>= putStrLn . help where
  help :: String -> String
  help s = r $ comments False 0 (code s) >>= parse p

-- A function that parses the program in the given FilePath
parseFile :: FilePath -> IO ()
parseFile = parseFileP splP result

-- A helper function to test if a parser behaves correctly on a given input.
testP :: Parser a -> String -> ([Error], Maybe (Code, a))
testP p s = comments False 0 (code s) >>= parse p

p :: String -> ([Error], Maybe (Code, SPL))
p s = case r of
    Nothing -> (e, Nothing)
    Just (c, spl) -> (e ++ e2 ++ ([Error ParseError "Did not finish parsing" (map fst c, snd $ head c) | not $ null c]), Just (c, spl))
      where
        e2 = extractErrors spl
  where
    (e, r) = testP splP s

extractErrors :: SPL -> [Error]
extractErrors = concatMap erDecl

erDecl :: Decl -> [Error]
erDecl (DeclVarDecl (VarDecl _ _ e)) = []
erDecl (DeclFunDecl (FunDecl _ _ _ vs stmts)) = []
erDecl (DeclError s) = [Error ParseError "Unknown declaration" s]

erExp :: Exp -> [Error]
erExp = undefined

erStmts :: [Stmt] -> [Error]
erStmts = concatMap erStmt

erStmt :: Stmt -> [Error]
erStmt = undefined

-- A function that transforms a string to a list of tuples with (character, line, column)
code :: String -> Code
code s = [(a, (b, c)) | (b, d) <- zip [1..] $ map (++ "\n") (lines s), (c, a) <- zip [1 ..] d]

-- Parses the file provided in the IO
main :: IO ()
main = getLine >>= parseFile
