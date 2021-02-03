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
  = VarDeclVar String Exp
  | VarDeclType Type String Exp
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
  = StmtIf Exp [Stmt] (Maybe [Stmt])
  | StmtWhile Exp [Stmt]
  | StmtField String [Field] Exp
  | StmtFunCall FunCall
  | StmtReturn (Maybe Exp)
  deriving (Show)

data Exp
  = ExpOp1 Op1 Exp
  | ExpOr OrExp
  | ExpOrRec OrExp Exp
  deriving (Show)

data OrExp
  = ExpAnd AndExp
  | ExpAndRec AndExp OrExp
  deriving (Show)

data AndExp
  = ExpCompare CompareExp
  | ExpCompareRec CompareExp CompareOp AndExp
  deriving (Show)

data CompareExp
  = ExpCons Term
  | ExpConsRec Term CompareExp
  deriving (Show)

data Term
  = Term Factor [(TermOp, Factor)]
  deriving (Show)

data Factor
  = Factor BottomExp [(FactorOp, BottomExp)]
  deriving (Show)

data FactorOp
  = Times
  | Divides
  deriving (Show)

data BottomExp
  = ExpRec Exp
  | ExpTuple (Exp, Exp)
  | ExpField String [Field]
  | ExpInt Int
  | ExpChar Char
  | ExpBool Bool
  | ExpFunCall FunCall
  | ExpEmptyList
  deriving (Show)

data TermOp
  = Add
  | Subtract
  deriving (Show)

data CompareOp
  = NotEquals
  | Less
  | LessEquals
  | Equals
  | Greater
  | GreaterEquals
  deriving (Show)

data Op1
  = Not
  | Min
  deriving (Show)

data Field
  = Head
  | Tail
  | First
  | Second
  deriving (Show)

data FunCall
  = FunCall String [Exp]
  deriving (Show)
