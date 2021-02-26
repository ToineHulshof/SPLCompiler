-- The implemented pretty printer for printing the output of the parser

module Printer where

import Grammar
import Parser ( expP, parseFileP, splP, code )
import Text.Printf ( printf )

type Depth = Int

-- A function that either prints the Error or pretty prints the parsed code
result' :: Either [Error] (Code, SPL) -> String
result' (Left es) = join "\n" $ map show es
result' (Right (_, a)) = ppSPL a

-- Pretty prints the program in the provided filepath
pp :: FilePath -> IO ()
pp = parseFileP splP result'

-- Pretty prints n tabs
tab :: Int -> String
tab n = replicate n '\t'

-- Joins a list of strings with a given seperator
join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join s (x:xs) = x ++ s ++ join s xs

-- All the pretty print functions for the implemented grammer
-- These are all pretty self-explanatory

ppSPL :: SPL -> String
ppSPL (SPL a) = join "\n\n" $ map (ppDecl 0) a

ppDecl :: Depth -> Decl -> String 
ppDecl d (DeclVarDecl vd) = printf "%s%s" (tab d) (ppVarDecl vd)
ppDecl d (DeclFunDecl fd) = printf "%s" (ppFunDecl d fd)

ppVarDecl :: VarDecl -> String
ppVarDecl (VarDeclVar n e) = printf "var %s = %s;" n (ppExp e)
ppVarDecl (VarDeclType t n e) = printf "%s %s = %s;" (ppType t) n (ppExp e)

ppFunDecl :: Depth -> FunDecl -> String 
ppFunDecl d (FunDecl n a t v s) = printf "%s%s(%s) %s{\n%s%s%s%s}" (tab d) n (join ", " a) (ppFunType t) (unlines $ map (printf "%s%s" (tab (d + 1)) . ppVarDecl) v) (if null v then "" else "\n") (unlines $ map (ppStmt (d + 1)) s) (tab d)

ppFunType :: Maybe ([Type], RetType) -> String 
ppFunType Nothing = ""
ppFunType (Just (t, r)) = printf ":: %s-> %s " (join "" (map ((++" ") . ppType) t)) (ppRetType r)

ppRetType :: RetType -> String 
ppRetType (RetTypeType t) = ppType t
ppRetType Void = "Void"

ppType :: Type -> String 
ppType (TypeBasic t) = ppBasicType t
ppType (TypeTuple t1 t2) = printf "(%s, %s)" (ppType t1) (ppType t2)
ppType (TypeArray t) = printf "[%s]" (ppType t)
ppType (TypeID s) = s

ppBasicType :: BasicType -> String
ppBasicType IntType = "Int"
ppBasicType BoolType = "Bool"
ppBasicType CharType = "Char"

ppStmt :: Depth -> Stmt -> String 
ppStmt d (StmtIf e s es) = printf "%sif (%s) {\n%s%s}%s" (tab d) (ppExp e) (unlines $ map (ppStmt (d + 1)) s) (tab d) (ppStmtElse d es) 
ppStmt d (StmtWhile e s) = printf "%swhile (%s) {\n%s%s}" (tab d) (ppExp e) (unlines $ map (ppStmt (d + 1)) s) (tab d)
ppStmt d (StmtField n f e) = printf "%s%s%s = %s;" (tab d) n (concatMap ppField f) (ppExp e)
ppStmt d (StmtFunCall f) = printf "%s%s;" (tab d) (ppFunCall f)
ppStmt d (StmtReturn e) = printf "%sreturn%s;" (tab d) (ppMExp e)

ppMExp :: Maybe Exp -> String
ppMExp Nothing = ""
ppMExp (Just e) = " " ++ ppExp e

ppFunCall :: FunCall -> String 
ppFunCall (FunCall n a) = printf "%s(%s)" n (join ", " (map ppExp a))

ppField :: Field -> String 
ppField Head = ".hd"
ppField Tail = ".tl"
ppField First = ".fst"
ppField Second = ".snd"

ppStmtElse :: Depth -> Maybe [Stmt] -> String
ppStmtElse _ Nothing = ""
ppStmtElse d (Just s) = printf " else {\n%s%s}" (unlines $ map (ppStmt (d + 1)) s) (tab d)

ppExp :: Exp -> String 
ppExp (Exp o e1 e2) = ppExp e1 ++ " " ++ ppOp2 o ++ " " ++ ppExp e2
ppExp (ExpOp1 o e) = ppOp1 o ++ ppExp e
ppExp (ExpTuple (e1, e2)) = printf "(%s, %s)" (ppExp e1) (ppExp e2)
ppExp (ExpBrackets e) = printf "(%s)" (ppExp e)
ppExp (ExpField s f) = s ++ concatMap ppField f
ppExp (ExpInt i) = show i
ppExp (ExpChar c) = printf "'%s'" [c]
ppExp (ExpBool b) = if b then "True" else "False"
ppExp (ExpFunCall f) = ppFunCall f
ppExp ExpEmptyList = "[]"

ppOp1 :: Op1 -> String 
ppOp1 Not = "!"
ppOp1 Min = "-"

ppOp2 :: Op2 -> String 
ppOp2 Plus = "+"
ppOp2 Minus = "-"
ppOp2 Product = "*"
ppOp2 Division = "/"
ppOp2 Modulo = "%"
ppOp2 Eq = "=="
ppOp2 Smaller = "<"
ppOp2 Greater = ">"
ppOp2 Leq = "<="
ppOp2 Geq = ">="
ppOp2 Neq = "!="
ppOp2 And = "&&"
ppOp2 Or = "||"
ppOp2 Cons = ":"