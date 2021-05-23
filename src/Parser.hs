-- Our implemented Parser with the help of the other files

{-# LANGUAGE LambdaCase #-}

module Parser where

import Grammar
import Errors
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (isNothing, listToMaybe, fromMaybe)
import Data.List (isPrefixOf)
import Data.Bifunctor ( Bifunctor(first) )
import Debug.Trace ( trace )

-- Several definitions of helper functions which are used in the "real" parsers

-- Parses all consecutive whitespace
ws :: Parser String
ws = spanP isSpace

ws' :: Parser String
ws' = spanP (\c -> isSpace c && c /= '\n')

-- Ignores all surrounded whitespaces for the given parser
w :: Parser a -> Parser a
w p = ws *> p <* ws

-- Parses the given Char while ignoring whitespaces
c :: Char -> Parser Char
c x = w (charP x)

c' :: Char -> Parser Char
c' x = ws *> charP x

-- Creates a Parser that parses the next Char that is equal to the Char in the Parser
charP :: Char -> Parser Char
charP x = satisfy (==x) True

-- Creates a Parser that parses the next sequence of Chars that is equal to the String in the Parser
stringP :: String -> Parser String
stringP = traverse charP

-- This function takes 2 strings and determines which part of the first string need to be cut of
-- To make it equal to the second string.
stringDev :: String -> String -> String 
stringDev s1 s2 = take (length s1 - length s2) s1

-- Parser that determines the position and string corresponding to the given parser
pp :: Parser (P -> a) -> Parser a
pp p = (\(p1, s1) f (p2, s2) -> f (p1, stringDev s1 s2)) <$> pP <*> p <*> pP

pp' :: Parser a -> Parser (a, P)
pp' p = (\(p1, s1) a (p2, s2) -> (a, (p1, stringDev s1 s2))) <$> pP <*> p <*> pP

ppE :: Parser Exp -> Parser Exp
ppE p = (\(p1, s1) e (p2, s2) -> posE (p1, stringDev s1 s2) e) <$> pP <*> p <*> pP

posE :: P -> Exp -> Exp
posE p (Exp t o e1 e2 _) = Exp t o e1 e2 p
posE p (ExpOp1 o e _) = ExpOp1 o e p
posE p (ExpTuple e _) = ExpTuple e p
posE p (ExpBrackets e _) = ExpBrackets e p
posE p (ExpField s fs _) = ExpField s fs p
posE p (ExpInt i _) = ExpInt i p
posE p (ExpChar c _) = ExpChar c p
posE p (ExpBool b _) = ExpBool b p
posE p (ExpFunCall f _) = ExpFunCall f p
posE p (ExpEmptyList _) = ExpEmptyList p
posE p (ExpError _) = ExpError p

pP :: Parser P
pP = Parser $ \case
  c@((p, _) : _) -> ([], Just (c, (p, map snd c)))
  [] -> ([], Just ([((1, 1), '\n')], ((1, 1), "")))

-- Creates a Parser that parses a Char with a specific requirement
-- If the Char is fulfilling the requirement, return a (Code, Char)
-- If the Char doesnt fulfill the requirement, return an Error with the Char and its position
-- If none of the above is the case, return an Error with "Unexpected EOF"
satisfy' :: (Char -> Bool) -> Bool -> Parser (Positioned Char)
satisfy' p consume = Parser $ \case
  ((l, c), y) : xs
    | p y -> ([], Just (if consume then xs else ((l, c), y) : xs, ((l, c), y)))
    | otherwise -> ([Error ParseError (nes $ "Unknown token '" ++ [y] ++ "'") (Just ((l, c), [y]))], Nothing)
  [] -> ([Error ParseError (nes "Unexpected EOF") Nothing], Nothing)

satisfy :: (Char -> Bool) -> Bool -> Parser Char
satisfy p consume = snd <$> satisfy' p consume

-- Creates a Parser that parses all the consecutive Chars that satisfy the given requirement
spanP :: (Char -> Bool) -> Parser String
spanP p = map snd <$> spanP' p

spanP' :: (Char -> Bool) -> Parser Code
spanP' p = Parser $ \code -> let (token, rest) = span (p . snd) code in ([], Just (rest, token))

-- Extends the given Parser with the functionality to return an Error when zero characters are parsed
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser help
  where
    help c = case r of
      Nothing -> (e, Nothing)
      Just (c', xs) -> if null xs then ([Error ParseError (nes "Found 0, while at least 1 is expected.") ((\h -> (fst h, "")) <$> listToMaybe c')], Nothing) else (e, Just (c', xs))
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
declP = declVarDeclP <|> declFunDeclP <|> DeclError <$> errorP (== '\n') False

declVarDeclP :: Parser Decl
declVarDeclP = DeclVarDecl <$> varDeclP

declFunDeclP :: Parser Decl
declFunDeclP = DeclFunDecl <$> funDeclP

funDeclP :: Parser FunDecl
funDeclP = (\(a, f) b c d e -> FunDecl a b c d e f) <$> pp' idP <*> (c '(' *> sepBy (optP ',') idP <* c' ')') <*> funTypeP <*> (c '{' *> many (w varDeclP)) <*> (some (w stmtP) <* c' '}')

adjustPosition :: Position -> Char -> Position
adjustPosition p '\n' = p
adjustPosition (l, c) _ = (l, c - 1)

optP :: Char -> Parser Char
optP ch = ws' *> optP' ch <* ws where
  optP' ch = Parser $ \case
    ((p, x):xs)
      | x == ch -> ([], Just (xs, ch))
      | otherwise -> ([Error ParseError (nes $ "Missing '" ++ [ch] ++ "' inserted") (Just (adjustPosition p x, " "))], Just ((p, x):xs, ch))
    [] -> ([Error ParseError (nes $ "Missing \"" ++ [ch] ++ "\" inserted") Nothing], Just ([], ch))

varDeclP :: Parser VarDecl
varDeclP = (varDeclVarP <|> varDeclTypeP) <* optP ';'

varDeclVarP :: Parser VarDecl
varDeclVarP = VarDecl Nothing <$> (stringP "var" *> w idP <* charP '=' <* ws) <*> expP

varDeclTypeP :: Parser VarDecl
varDeclTypeP = VarDecl . Just <$> typeP <*> (w idP <* charP '=' <* ws) <*> exp'P

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
funCallP = pp (FunCall Nothing <$> idP <*> (c '(' *> sepBy (charP ',') exp'P <* c' ')'))

fieldFunP :: Parser Field
fieldFunP = pp (Head <$ stringP "hd") <|> pp (Tail <$ stringP "tl") <|> pp (First <$ stringP "fst") <|> pp (Second <$ stringP "snd")

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
    -- <|> Op2Error <$> f (\c -> not (isAlpha c) && not (isSpace c) && c /= ';')
    <|> Op2Error <$> errorP (\c -> isAlpha c && not (isSpace c) && c `elem` "()[].,{}") True

f :: (Char -> Bool) -> Parser P
f p = (\c -> (fromMaybe (1, 1) (listToMaybe (map fst c)), map snd c)) <$> notNull (spanP' p)

errorP :: (Char -> Bool) -> Bool -> Parser P
errorP p consume = (\s ((l, c), _) -> ((l, c - length s), s)) <$> notNull (spanP (not . p)) <*> satisfy' p consume

exp'P :: Parser Exp
exp'P = expOp2P False <|> expNOp2P'

expP :: Parser Exp
expP = expOp2P True <|> expNOp2P

expNOp2P :: Parser Exp 
expNOp2P = expNOp2P' <|> (ExpError <$> errorP (`elem` "\n;") False)

expNOp2P' :: Parser Exp 
expNOp2P' = ppE expListP <|> ppE expStringP <|> pp (ExpInt <$> intP) <|> expBoolP <|> pp (ExpOp1 <$> op1P <*> expP) <|> pp (ExpFunCall <$> funCallP) <|> pp (ExpField <$> idP <*> fieldP) <|> expCharP

expOp2P :: Bool -> Parser Exp
expOp2P b = ppE (Parser $ expBP b 0)

expBP :: Bool -> Int -> Code -> ([Error], Maybe (Code, Exp))
expBP b minBP c = case r1 of
  Nothing -> (e1, Nothing)
  Just (c', lhs) -> case r2 of
    Nothing -> (e2, Nothing)
    Just (c'', lhs') -> ([], Just (c'', lhs'))
    where
      (e2, r2) = lhsP b 0 minBP lhs c'
  where
    (e1, r1) = parse (expBracketsP <|> expTupleP <|> (if b then expNOp2P else expNOp2P')) c

lhsP :: Bool -> Int -> Int -> Exp -> Code -> ([Error], Maybe (Code, Exp))
lhsP b l m e c = case r of
  Nothing -> (e1, Just (c, e))
  Just (c', o) -> if lBP < m then (e1, Just (c, e)) else case r2 of
    Nothing -> (e2, Nothing)
    Just (c'', rhs) -> case r3 of
      Nothing -> (e3, Nothing)
      Just (c''', lhs) -> ([], Just (c''', lhs))
      where
        (e3, r3) = lhsP b lBP m (Exp Nothing o e rhs ((1, 1), "")) c''
    where
      (lBP, rBP) = bp o
      (e2, r2) = expBP b rBP c'
  where
    (e1, r) = parse (w op2P) c

bp :: Op2 -> (Int, Int)
bp (Op2Error _) = (9, 10)
bp o
  | o `elem` [Plus, Minus] = (9, 10)
  | o `elem` [Product, Division, Modulo] = (11, 12)
  | o `elem` [Neq, Smaller, Leq, Greater, Equals, Greater, Geq] = (5, 6)
  | o == And = (4, 3)
  | o == Or = (2, 1)
  | o == Cons = (8, 7)

expBoolP :: Parser Exp
expBoolP = pp $ ExpBool True <$ stringP "True" <|> ExpBool False <$ stringP "False"

expCharP :: Parser Exp
expCharP = pp $ ExpChar <$> (charP '\'' *> (escapedCharP <|> anyCharP) <* charP '\'')

escapedCharP :: Parser Char
escapedCharP = charP '\\' *> anyCharP

anyCharP :: Parser Char
anyCharP = satisfy (const True) True

expTupleP :: Parser Exp
expTupleP = pp $ curry ExpTuple <$> (c '(' *> expP <* c ',') <*> expP <* c' ')'

expBracketsP :: Parser Exp
expBracketsP = pp $ ExpBrackets <$> (c '(' *> expP <* c' ')')

expStringP :: Parser Exp
expStringP = (\p -> foldr (foldCons . (`ExpChar` p)) (ExpEmptyList p)) <$> pP <*> (c '"' *> many (escapedCharP <|> satisfy (/='"') True) <* c' '"')

expListP :: Parser Exp
expListP = (\(es, p) -> let ps = map expToP es in foldr foldCons (ExpEmptyList (fromMaybe p (listToMaybe ps))) (zipWith posE ps es)) <$> pp' (c '[' *> sepBy (c ',') exp'P <* c' ']')

foldCons :: Exp -> Exp -> Exp 
foldCons e1 e2 = Exp Nothing Cons e1 e2 (expToP e1)

expToP :: Exp -> P
expToP (Exp _ _ _ _ p) = p
expToP (ExpOp1 _ _ p) = p
expToP (ExpTuple _ p) = p
expToP (ExpBrackets _ p) = p
expToP (ExpField _ _ p) = p
expToP (ExpInt _ p) = p
expToP (ExpChar _ p) = p
expToP (ExpBool _ p) = p
expToP (ExpFunCall _ p) = p
expToP (ExpEmptyList p) = p
expToP (ExpError p) = p

stmtIfP :: Parser Stmt
stmtIfP = (\ex i e -> StmtIf ex i (Just e)) <$> conditionP "if" <*> stmtsP <*> (w (stringP "else") *> stmtsP) <|> (\ex i -> StmtIf ex i Nothing) <$> conditionP "if" <*> stmtsP

conditionP :: String -> Parser Exp
conditionP s = stringP s *> c '(' *> expP <* c' ')'

stmtsP :: Parser [Stmt]
stmtsP = c '{' *> many stmtP <* c '}'

stmtWhileP :: Parser Stmt
stmtWhileP = StmtWhile <$> conditionP "while" <*> stmtsP 

stmtFieldP :: Parser Stmt
stmtFieldP = pp $ StmtField <$> idP <*> fieldP <*> (c '=' *> expP <* optP ';')

stmtFunCallP :: Parser Stmt
stmtFunCallP = StmtFunCall <$> funCallP <* optP ';'

stmtReturnP :: Parser Stmt
stmtReturnP = pp (StmtReturn . pure <$> (stringP "return" *> ws *> expP <* optP ';')) <|> pp (StmtReturn Nothing <$ stringP "return") <* optP ';'

stmtP :: Parser Stmt
stmtP = stmtIfP <|> stmtWhileP <|> stmtFieldP <|> stmtReturnP <|> stmtFunCallP <|> stmtVarDeclP <|> StmtError Nothing <$> errorP (`elem` "\n}") False

stmtVarDeclP :: Parser Stmt
stmtVarDeclP = (\(_, p) -> StmtError (Just "Variable declarations are only allowed at the start of a function.") p) <$> pp' declVarDeclP

basicTypeP :: Parser BasicType
basicTypeP = IntType <$ stringP "Int" <|> BoolType <$ stringP "Bool" <|> CharType <$ stringP "Char"

funTypeP :: Parser (Maybe Type)
funTypeP = (\args ret -> Just $ foldr1 TypeFun $ args ++ [ret]) <$> (w (stringP "::") *> many (w typeP)) <*> (w (stringP "->") *> retTypeP) <|> pure Nothing

typeTupleP :: Parser Type
typeTupleP = TypeTuple <$> (c '(' *> typeP <* c ',') <*> typeP <* c' ')'  

typeListP :: Parser Type
typeListP = TypeList <$> (c '[' *> typeP <* c' ']')

typeP :: Parser Type
typeP = typeTupleP <|> typeListP <|> TypeBasic <$> basicTypeP <|> TypeID Nothing <$> idP

-- Several functions to easily apply the parser to certain programs

-- Maps the result of the parsed program to a string which describes the result
result :: ([Error], Maybe (Code, SPL)) -> String
result ([], Just (c, a))
  | null c = "Parsed succesfully" -- ++ show a
  | otherwise = show $ Error ParseError (nes "Did not complete parsing") ((\h -> (fst h, map snd c)) <$> listToMaybe c)
result (es, _) = join "\n" $ map show es

-- A funtion to parse (recursive) comments and returns either an Error or the Code without the comments
-- The boolean argument is true when the parser is currently in a line comment and false otherwise
-- The integer argument represents the current "level" of recursive block comments, e.g. 0 when you are not in a /* */ comment, 1 if you are and 2 if you are in a comment inside a comment
comments :: Bool -> Int -> Code -> ([Error], Code)
comments _ d []
    | d == 0 = ([], []) -- Parser is done and the parser is not currently in a block comment 
    | otherwise = ([Error ParseError (nes "Did not close all comments") Nothing], []) -- Parser is done, but is currently in a block comment
comments s d [((l, c), x)]
    | d /= 0 = ([Error ParseError (nes "Did not close all comments") (Just ((l, c), ""))], []) -- Parser only has one character left, but it is still in a block comment, so this can't be closed
    | s = ([], []) -- Parser only has one character left and is currently in a line comment
    | otherwise = ([], [((l, c), x)]) -- Parser has only one character left and isn't in a comment
comments s d (((l1, c1), x1) : ((l2, c2), x2) : xs)
    | t == "//" = comments True d xs -- Parser recognizes it is in a line comment
    | t == "/*" = comments s (d + 1) xs -- Parser starts a new recursive block comment
    | t == "*/" && (d /= 0) = comments s (d - 1) xs -- Parser closes a valid recursive block comment 
    | t == "*/" && (d == 0) = ([Error ParseError (nes "Trying to close comment that doesn't exist") (Just ((l2, c2), "*/"))], xs) -- Parser closes an invalid recursive block comment
    | l2 > l1 && s = comments False d (((l2, c2), x2) : xs) -- The end of line is reached, so the line comment is reset if it was active
    | s || (d > 0) = comments s d (((l2, c2), x2) : xs) -- Parser is currently in a comment and ignores the next character
    | otherwise = (:) ((l1, c1), x1) <$> comments s d (((l2, c2), x2) : xs) -- Parser isn't in a comment currently and adds the current character to the returned Code
    where t = [x1, x2]

-- A function that parses a file for a given parser
parseFileP :: (([Error], Maybe (Code, SPL)) -> String) -> FilePath -> IO ()
parseFileP r f = readFile f >>= putStrLn . help where
  help :: String -> String
  help s = r $ comments False 0 (code s) >>= parse splP

-- A function that parses the program in the given FilePath
parseFile :: FilePath -> IO ()
parseFile = parseFileP result

-- A helper function to test if a parser behaves correctly on a given input.
testP :: Parser a -> String -> ([Error], Maybe (Code, a))
testP p s = comments False 0 (code s) >>= parse p

t :: Parser a -> String -> ([Error], Maybe (String, a))
t p s = fmap (first (map snd)) <$> testP p s

p :: String -> ([Error], Maybe (Code, SPL))
p s = case r of
    Nothing -> (e, Nothing)
    Just (c, spl) -> (e ++ e2 ++ ([Error ParseError (nes "Did not finish parsing") ((\h -> (fst h, map snd c)) <$> listToMaybe c) | not $ null c]), Just (c, spl))
      where
        e2 = extractErrors spl
  where
    (e, r) = testP splP s

extractErrors :: SPL -> [Error]
extractErrors = concatMap erDecl

erDecl :: Decl -> [Error]
erDecl (DeclVarDecl (VarDecl _ _ e)) = erExp e
erDecl (DeclFunDecl (FunDecl _ _ _ vs stmts _)) = concatMap (erDecl . DeclVarDecl) vs ++ erStmts stmts
erDecl (DeclError s) = [Error ParseError (nes "Unknown declaration") (Just s)]

erExp :: Exp -> [Error]
erExp (Exp _ o e1 e2 _) = erOp2 o ++ erExp e1 ++ erExp e2
erExp (ExpOp1 _ e _) = erExp e
erExp (ExpTuple (e1, e2) _) = erExp e1 ++ erExp e2
erExp (ExpBrackets e _) = erExp e
erExp (ExpError s) = [Error ParseError (nes "Incorrect expression") (Just s)]
erExp _ = []

erOp2 :: Op2 -> [Error]
erOp2 (Op2Error s) = [Error ParseError (nes "Unknown binary operator") (Just s)]
erOp2 _ = []

erStmts :: [Stmt] -> [Error]
erStmts = concatMap erStmt

erStmt :: Stmt -> [Error]
erStmt (StmtIf e ss1 ss2) = erExp e ++ erStmts ss1 ++ erStmts (fromMaybe [] ss2)
erStmt (StmtWhile e ss) = erExp e ++ erStmts ss
erStmt (StmtField _ _ e _) = erExp e
erStmt (StmtReturn (Just e) _) = erExp e
erStmt (StmtError s p) = [Error ParseError (nes (fromMaybe "Unknown statement" s)) (Just p)]
erStmt _ = []

-- A function that transforms a string to a list of tuples with (character, line, column)
code :: String -> Code
code s = [((b, c), a) | (b, d) <- zip [1..] $ map (++ "\n") (lines s), (c, a) <- zip [1 ..] d]

-- Parses the file provided in the IO
main :: IO ()
main = getLine >>= parseFile
