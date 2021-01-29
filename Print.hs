module Print where

import Grammar
import Text.Printf ( printf )

type Depth = Int

tab :: Int -> String
tab n = replicate n '\t'

join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join s (x:xs) = x ++ s ++ join x xs

pp :: SPL -> String
pp (SPL a) = join "\n\n" $ map (ppDecl 0) a

ppDecl :: Depth -> Decl -> String 
ppDecl d (DeclVarDecl vd) = printf "%s%s" (tab d) (ppVarDecl vd)
ppDecl d (DeclFunDecl fd) = printf "%s" (ppFunDecl d fd)

ppVarDecl :: VarDecl -> String
ppVarDecl (VarDeclVar n e) = printf "var %s = %s;" n (ppExpRec e)
ppVarDecl (VarDeclType t n e) = printf "%s %s = %s;" (ppType t) n (ppExpRec e)

ppFunDecl :: Depth -> FunDecl -> String 
ppFunDecl d (FunDecl n a t v s) = printf "%s%s(%s) %s{%s\n%s\n%s}" (tab d) n (join ", " a) (ppFunType t) (join "\n" (map (ppStmt $ d + 1) s)) (join ("\n" ++ tab (d + 1)) (map ppVarDecl v)) (tab d)

ppExpRec :: ExpRec -> String
ppExpRec = error "not implemented"

ppFunType :: Maybe ([Type], RetType) -> String 
ppFunType Nothing = ""
ppFunType (Just (t, r)) = printf ":: %s-> %s " (join "" (map ((++" ") . ppType) t)) (ppRetType r)

ppRetType :: RetType -> String 
ppRetType (RetTypeType t) = ppType t
ppRetType Void = "Void"

ppType :: Type -> String 
ppType (TypeBasic t) = ppBasicType t
ppType (TypeTuple t1 t2) = printf "(%s, %s)" (ppType t1) (ppType t2)

ppBasicType :: BasicType -> String
ppBasicType IntType = "Int"
ppBasicType BoolType = "Bool"
ppBasicType CharType = "Char"

ppStmt :: Depth -> Stmt -> String 
ppStmt d (StmtIf e s es) = printf "if" (ppStmtElse d es) 

ppStmtElse :: Depth -> Maybe [Stmt] -> String
ppStmtElse = undefined