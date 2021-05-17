-- The implemented pretty printer for printing the output of the parser

module Printer where

import Grammar
import Errors
import Parser (code, expP, parseFileP, splP, testP, funDeclP)
import Text.Printf (printf)

type Depth = Int

-- A function that either prints the Error or pretty prints the parsed code
result' :: ([Error], Maybe (Code, SPL)) -> String
result' ([], Just (_, a)) = ppSPL a
result' (e, _) = show e

-- Pretty prints the program in the provided filepath
pp :: FilePath -> IO ()
pp = parseFileP splP result'

-- Pretty prints n tabs
tab :: Int -> String
tab n = replicate n '\t'

-- All the pretty print functions for the implemented grammer
-- These are all pretty self-explanatory

ppSPL :: SPL -> String
ppSPL a = join "\n\n" $ map (ppDecl 0) a

ppDecl :: Depth -> Decl -> String
ppDecl d (DeclVarDecl vd) = printf "%s%s" (tab d) (ppVarDecl vd)
ppDecl d (DeclFunDecl fd) = printf "%s" (ppFunDecl d fd)

ppVarDecl :: VarDecl -> String
ppVarDecl (VarDecl Nothing n e) = printf "var %s = %s;" n (ppExp e)
ppVarDecl (VarDecl (Just t) n e) = printf "%s %s = %s;" (ppType t) n (ppExp e)

ppFunDecl :: Depth -> FunDecl -> String
ppFunDecl d (FunDecl n a t v s _) = printf "%s%s(%s) %s{\n%s%s%s%s}" (tab d) n (join ", " a) (ppFunType t) (unlines $ map (printf "%s%s" (tab (d + 1)) . ppVarDecl) v) (if null v then "" else "\n") (unlines $ map (ppStmt (d + 1)) s) (tab d)

ppFunType :: Maybe Type -> String
ppFunType Nothing = ""
ppFunType (Just t) = ":: " ++ ppType t

ppType :: Type -> String
ppType (TypeBasic t) = ppBasicType t
ppType (TypeTuple t1 t2) = printf "(%s, %s)" (ppType t1) (ppType t2)
ppType (TypeList t) = printf "[%s]" (ppType t)
ppType (TypeID _ s) = s
ppType (TypeFun t1 Void) = printf "-> %s " (ppType t1)
ppType (TypeFun t1 t2) = printf "%s %s" (ppType t1) (ppType t2)
ppType Void = "Void"

ppBasicType :: BasicType -> String
ppBasicType IntType = "Int"
ppBasicType BoolType = "Bool"
ppBasicType CharType = "Char"

ppStmt :: Depth -> Stmt -> String
ppStmt d (StmtIf e s es) = printf "%sif (%s) {\n%s%s}%s" (tab d) (ppExp e) (unlines $ map (ppStmt (d + 1)) s) (tab d) (ppStmtElse d es)
ppStmt d (StmtWhile e s) = printf "%swhile (%s) {\n%s%s}" (tab d) (ppExp e) (unlines $ map (ppStmt (d + 1)) s) (tab d)
ppStmt d (StmtField n f e _) = printf "%s%s%s = %s;" (tab d) n (concatMap ppField f) (ppExp e)
ppStmt d (StmtFunCall f) = printf "%s%s;" (tab d) (ppFunCall f)
ppStmt d (StmtReturn e _) = printf "%sreturn%s;" (tab d) (ppMExp e)

ppMExp :: Maybe Exp -> String
ppMExp Nothing = ""
ppMExp (Just e) = " " ++ ppExp e

ppFunCall :: FunCall -> String
ppFunCall (FunCall _ n a _) = printf "%s(%s)" n (join ", " (map ppExp a))

ppField :: Field -> String
ppField (Head _) = ".hd"
ppField (Tail _) = ".tl"
ppField (First _) = ".fst"
ppField (Second _) = ".snd"

ppStmtElse :: Depth -> Maybe [Stmt] -> String
ppStmtElse _ Nothing = ""
ppStmtElse d (Just s) = printf " else {\n%s%s}" (unlines $ map (ppStmt (d + 1)) s) (tab d)

ppExp :: Exp -> String
ppExp (Exp _ o e1 e2 _) = ppExp e1 ++ " " ++ ppOp2 o ++ " " ++ ppExp e2
ppExp (ExpOp1 o e _) = ppOp1 o ++ ppExp e
ppExp (ExpTuple (e1, e2) _) = printf "(%s, %s)" (ppExp e1) (ppExp e2)
ppExp (ExpBrackets e _) = printf "(%s)" (ppExp e)
ppExp (ExpField _ s f _) = s ++ concatMap ppField f
ppExp (ExpInt i _) = show i
ppExp (ExpChar c _) = printf "'%s'" [c]
ppExp (ExpBool b _) = if b then "True" else "False"
ppExp (ExpFunCall f _) = ppFunCall f
ppExp ExpEmptyList {} = "[]"

ppOp1 :: Op1 -> String
ppOp1 Not = "!"
ppOp1 Min = "-"

ppOp2 :: Op2 -> String
ppOp2 Plus = "+"
ppOp2 Minus = "-"
ppOp2 Product = "*"
ppOp2 Division = "/"
ppOp2 Modulo = "%"
ppOp2 Equals = "=="
ppOp2 Smaller = "<"
ppOp2 Greater = ">"
ppOp2 Leq = "<="
ppOp2 Geq = ">="
ppOp2 Neq = "!="
ppOp2 And = "&&"
ppOp2 Or = "||"
ppOp2 Cons = ":"