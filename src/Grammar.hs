-- In this file the whole grammar is defined which is used in the parser

module Grammar where

import Errors
import Control.Applicative (Alternative (empty, (<|>)))

-- Our defined Parser type, which takes a Code object and parses it and returns either and Error or a parsed tuple, with Code that was not parsed yet
newtype Parser a = Parser {parse :: Code -> Either [Error] (Code, a)}

-- Proof that our Parser is a Functor
instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (f <$>) . p

-- Proof that our Parser is an Applicative
instance Applicative Parser where
  pure x = Parser $ \code -> Right (code, x)
  (Parser p1) <*> (Parser p2) = Parser $ \code -> do
    (code', f) <- p1 code
    (code'', a) <- p2 code'
    Right (code'', f a)

-- Proof that our Parser is an Alternative
instance Alternative Parser where
  empty = Parser . const $ Left []
  (Parser p1) <|> (Parser p2) = Parser $ \code -> p1 code <> p2 code

-- Our defined types in the Grammar (pretty similar to the given grammar; implementation details are mentioned in the code or in the report)
-- Our naming convention for the constructors: if there is only one, simply use the same name;
-- If there are more, the name is AB, where A is the current datatype and B is the next, recursive datatype (DeclVarDecl -> current = Decl and next = VarDecl)

newtype SPL
  = SPL [Decl]
  deriving (Show)

data Decl
  = DeclVarDecl VarDecl
  | DeclFunDecl FunDecl
  deriving (Show)

data VarDecl
  = VarDecl (Maybe Type) String Exp
  deriving (Show)

data FunDecl
  = FunDecl String [String] (Maybe Type) [VarDecl] [Stmt]
  deriving (Show)

data Condition = Eq | Ord deriving (Eq, Ord, Show)

data Type
  = TypeBasic BasicType
  | TypeTuple Type Type
  | TypeArray Type
  | TypeID (Maybe Condition) String
  | TypeFun Type Type
  | Void

instance Eq Type where
  TypeBasic b1 == TypeBasic b2 = b1 == b2
  TypeTuple l1 r1 == TypeTuple l2 r2 = l1 == l2 && r1 == r2
  TypeArray t1 == TypeArray t2 = t1 == t2
  TypeID {} == TypeID {} = True
  TypeFun l1 r1 == TypeFun l2 r2 = l1 == l2 && r1 == r2
  Void == Void = True
  _ == _ = False

instance Show Type where
  show (TypeBasic b) = show b
  show (TypeTuple t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TypeArray t) = "[" ++ show t ++ "]"
  show (TypeID c s) = "\x1b[36m" ++ s ++ "\x1b[0m"
  show (TypeFun t1 t2) = show t1 ++ " -> " ++ show t2
  show Void = "\x1b[34mVoid\x1b[0m"

data BasicType
  = IntType
  | BoolType
  | CharType
  deriving (Eq)

instance Show BasicType where
  show IntType = "\x1b[34mInt\x1b[0m"
  show BoolType = "\x1b[34mBool\x1b[0m"
  show CharType = "\x1b[34mChar\x1b[0m"

-- Recursive definition of Field is already defined here, not in Field
data Stmt
  = StmtIf Exp [Stmt] (Maybe [Stmt])
  | StmtWhile Exp [Stmt]
  | StmtField String [Field] Exp
  | StmtFunCall FunCall
  | StmtReturn (Maybe Exp)
  deriving (Show)

-- Here we used the provided hints to ensure precendence
-- More details can be found in the report
data Exp
  = Exp Op2 Exp Exp
  | ExpOp1 Op1 Exp
  | ExpTuple (Exp, Exp)
  | ExpBrackets Exp
  | ExpField String [Field]
  | ExpInt Integer
  | ExpChar Char
  | ExpBool Bool
  | ExpFunCall FunCall
  | ExpEmptyList
  deriving (Show)

data Op2
  = Plus
  | Minus
  | Product
  | Division
  | Modulo
  | Equals
  | Smaller
  | Greater
  | Leq
  | Geq
  | Neq
  | And
  | Or
  | Cons
  deriving (Eq, Show)

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
