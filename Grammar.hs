module Grammar where

import Control.Applicative (Alternative (empty, (<|>)))

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
  = FunDecl String [String] (Maybe ([Type], RetType)) [VarDecl] [Stmt]
  deriving (Show)

data RetType
  = RetTypeType Type
  | Void
  deriving (Show)
  
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
  | ExpString String
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