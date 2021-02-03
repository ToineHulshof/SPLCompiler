{-# LANGUAGE LambdaCase #-}

module Parser where

import Grammar
import Control.Applicative (Alternative ((<|>), many, some))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

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
charP x = satisfy (==x)

stringP :: String -> Parser String
stringP = traverse charP

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
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

retTypeP :: Parser RetType
retTypeP = voidP <|> retTypeTypeP

voidP :: Parser RetType
voidP = Void <$ stringP "Void"

retTypeTypeP :: Parser RetType
retTypeTypeP = RetTypeType <$> typeP

idP :: Parser String
idP = (:) <$> satisfy isAlpha <*> spanP (\c -> isAlphaNum c || c == '_')

intP :: Parser Int
intP = read <$> digitP <|> (*(-1)) . read <$> (charP '-' *> digitP)

digitP :: Parser String
digitP = notNull $ spanP isDigit

-- op1P :: Parser Op1
-- op1P = Not <$ charP '!' <|> Min <$ charP '-'

-- op2P :: Parser Op2
-- op2P =  Plus <$ charP '+'
--     <|> Minus <$ charP '-'
--     <|> Product <$ charP '*'
--     <|> Division <$ charP '/'
--     <|> Modulo <$ charP '%'
--     <|> Eq <$ stringP "=="
--     <|> Leq <$ stringP "<="
--     <|> Geq <$ stringP ">="
--     <|> Smaller <$ charP '<'
--     <|> Greater <$ charP '>'
--     <|> Neq <$ stringP "!="
--     <|> And <$ stringP "&&"
--     <|> Or <$ stringP "||"
--     <|> Cons <$ charP ':'

funCallP :: Parser FunCall
funCallP = FunCall <$> idP <*> (c '(' *> sepBy (charP ',') expP <* c ')')

fieldFunP :: Parser Field
fieldFunP = Head <$ stringP "hd" <|> Tail <$ stringP "tl" <|> First <$ stringP "fst" <|> Second <$ stringP "snd"

fieldP :: Parser [Field]
fieldP = many (c '.' *> fieldFunP)

expP :: Parser Exp 
expP = ExpOp1 <$> w op1P <*> expP <|> ExpOrRec <$> orExpP <* w (stringP "||") <*> expP <|> ExpOr <$> orExpP

op1P :: Parser Op1
op1P = Not <$ charP '!' <|> Min <$ charP '-'

orExpP :: Parser OrExp
orExpP = ExpAndRec <$> andExpP <* w (stringP "&&") <*> orExpP <|> ExpAnd <$> andExpP

andExpP :: Parser AndExp 
andExpP =  ExpCompareRec <$> compareExpP <*> compareOpP <*> andExpP <|> ExpCompare <$> compareExpP

compareOpP :: Parser CompareOp 
compareOpP =  Equals <$ stringP "=="
          <|> LessEquals <$ stringP "<="
          <|> GreaterEquals  <$ stringP ">="
          <|> Less <$ charP '<'
          <|> Greater <$ charP '>'
          <|> NotEquals <$ stringP "!="

compareExpP :: Parser CompareExp 
compareExpP = ExpConsRec <$> termP <* c ':' <*> compareExpP <|> ExpCons <$> termP

termOpP :: Parser TermOp
termOpP = Add <$ charP '+' <|> Subtract <$ charP '-'

factorOpP :: Parser FactorOp 
factorOpP = Times <$ charP '*' <|> Divides <$ charP '/'

termP :: Parser Term 
termP = Term <$> factorP <*> many ((,) <$> w termOpP <*> factorP)

factorP :: Parser Factor
factorP = Factor <$> bottomExpP <*> many ((,) <$> w factorOpP <*> bottomExpP)

bottomExpP :: Parser BottomExp
bottomExpP = expRecP <|> expTupleP <|> ExpFunCall <$> funCallP <|> ExpEmptyList <$ stringP "[]" <|> ExpInt <$> intP <|> expCharP <|> expBoolP <|> ExpField <$> idP <*> fieldP

expBoolP :: Parser BottomExp
expBoolP = ExpBool True <$ stringP "True" <|> ExpBool False <$ stringP "False"

expCharP :: Parser BottomExp
expCharP = ExpChar <$> (charP '\'' *> satisfy isAlpha <* charP '\'')

expTupleP :: Parser BottomExp
expTupleP = curry ExpTuple <$> (c '(' *> expP <* c ',') <*> expP <* c ')'

expRecP :: Parser BottomExp
expRecP = ExpRec <$> (c '(' *> expP <* c ')')

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

funTypeP :: Parser (Maybe ([Type], RetType))
funTypeP = curry Just <$> (w (stringP "::") *> many (w typeP)) <*> (w (stringP "->") *> retTypeP) <|> pure Nothing

typeTupleP :: Parser Type
typeTupleP = TypeTuple <$> (c '(' *> typeP <* c ',') <*> typeP <* c ')'  

typeArrayP :: Parser Type
typeArrayP = TypeArray <$> (c '[' *> typeP <* c ']')

typeP :: Parser Type
typeP = typeTupleP <|> typeArrayP <|> TypeBasic <$> basicTypeP <|> TypeID <$> idP

result :: Show a => Either Error (Code, a) -> String
result (Right (c, a))
  | null c = "Parsed succesfully" ++ show a
  | otherwise = "Error: did not complete parsing"
result (Left (e, l, c)) = "Error: " ++ e ++ ". Line: " ++ show l ++ ", Character: " ++ show c ++ "."

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

codeLines :: String -> [(Int, String)]
codeLines = zip [1..] . lines

parseFileP :: Show a => Parser a -> (Either Error (Code, a) -> String) -> FilePath -> IO ()
parseFileP p r f = readFile f >>= putStrLn . help where
  help :: String -> String
  help s = (++ " " ++ f) $ r $ comments False 0 (code s) >>= parse p

parseFile :: FilePath -> IO ()
parseFile = parseFileP splP result

testP :: Parser a -> String -> Either Error (Code, a)
testP p = parse p . code

code :: String -> Code
code s = [(a, b, c) | (b, d) <- zip [1..] $ lines s, (c, a) <- zip [1 ..] d]

main :: IO ()
main = getLine >>= parseFile
