-- In this file the whole grammar is defined which is used in the parser

module Grammar where

import Control.Applicative (Alternative (empty, (<|>)))

-- Code is the list of chars in a program including its position, where the integers are the line and column respectively
type Code = [(Char, Int, Int)]
-- Error is a datatype to store an error message as a String with its position, where the integers are the line and column respectively
data Error
  = Error String Int Int

instance Show Error where
  show (Error e l c) = "\x1b[31mError\x1b[0m " ++ e ++ " " ++ show l ++ ":" ++ show c

-- Our defined Parser type, which takes a Code object and parses it and returns either and Error or a parsed tuple, with Code that was not parsed yet
newtype Parser a = Parser { parse :: Code -> Either [Error] (Code, a) }

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
  empty = Parser . const $ Left [Error "" 0 0]--("Failed", 0, 0)
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
  = VarDeclVar String Exp
  | VarDeclType Type String Exp
  deriving (Show)

-- FArgs and FunType are immediately defined here, not separate in the Grammar
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
  | Eq
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
