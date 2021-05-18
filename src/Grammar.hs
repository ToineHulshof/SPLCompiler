-- In this file the whole grammar is defined which is used in the parser

module Grammar where

import Errors
import Control.Applicative (Alternative (empty, (<|>)))

-- Our defined Parser type, which takes a Code object and parses it and returns either and Error or a parsed tuple, with Code that was not parsed yet
newtype Parser a = Parser { parse :: Code -> ([Error], Maybe (Code, a)) }

-- Proof that our Parser is a Functor
instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap (fmap f)) . p

-- Proof that our Parser is an Applicative
instance Applicative Parser where
  pure x = Parser $ \code -> ([], Just (code, x))
  (Parser p1) <*> (Parser p2) = Parser help
    where
      help c = case r1 of
        Nothing -> (e1, Nothing)
        Just (c', f) -> case r2 of
          Nothing -> (e1 ++ e2, Nothing)
          Just (c'', x) -> (e1 ++ e2, Just (c'', f x))
          where
            (e2, r2) = p2 c'
        where
          (e1, r1) = p1 c


-- Proof that our Parser is an Alternative
instance Alternative Parser where
  empty = Parser . const $ ([], Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \code -> case p1 code of
    (_, Nothing) -> p2 code
    res -> res


-- Our defined types in the Grammar (pretty similar to the given grammar; implementation details are mentioned in the code or in the report)
-- Our naming convention for the constructors: if there is only one, simply use the same name;
-- If there are more, the name is AB, where A is the current datatype and B is the next, recursive datatype (DeclVarDecl -> current = Decl and next = VarDecl)

type SPL = [Decl]

data Decl
  = DeclVarDecl VarDecl
  | DeclFunDecl FunDecl
  | DeclError P
  deriving (Show)

data VarDecl
  = VarDecl (Maybe Type) String Exp
  deriving (Show)

data FunDecl
  = FunDecl String [String] (Maybe Type) [VarDecl] [Stmt] P
  deriving (Show)

data Condition = Eq | Ord deriving (Eq, Ord, Show)

data Type
  = TypeBasic BasicType
  | TypeTuple Type Type
  | TypeList Type
  | TypeID (Maybe Condition) String
  | TypeFun Type Type
  | Void

instance Eq Type where
  TypeBasic b1 == TypeBasic b2 = b1 == b2
  TypeTuple l1 r1 == TypeTuple l2 r2 = l1 == l2 && r1 == r2
  TypeList t1 == TypeList t2 = t1 == t2
  TypeID {} == TypeID {} = True
  TypeFun l1 r1 == TypeFun l2 r2 = l1 == l2 && r1 == r2
  Void == Void = True
  _ == _ = False

instance Show Type where
  show (TypeBasic b) = show b
  show (TypeTuple t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TypeList t) = "[" ++ show t ++ "]"
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
  | StmtField String [Field] Exp P
  | StmtFunCall FunCall
  | StmtReturn (Maybe Exp) P
  | StmtError (Maybe String) P
  deriving (Show)

-- Here we used the provided hints to ensure precendence
-- More details can be found in the report
data Exp
  = Exp (Maybe Type) Op2 Exp Exp P
  | ExpOp1 Op1 Exp P
  | ExpTuple (Exp, Exp) P
  | ExpBrackets Exp P
  | ExpField (Maybe Type) String [Field] P
  | ExpInt Integer P
  | ExpChar Char P
  | ExpBool Bool P
  | ExpFunCall FunCall P
  | ExpEmptyList P
  | ExpError P
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
  | Op2Error P
  deriving (Eq, Show)

data Op1
  = Not
  | Min
  deriving (Show)

data Field
  = Head P
  | Tail P
  | First P
  | Second P
  deriving (Show)

data FunCall
  = FunCall (Maybe Type) String [Exp] P
  deriving (Show)
