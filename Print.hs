module Print where

import Grammar
import Parser
import Text.Printf ( printf )

type Depth = Int

result' :: Either Error (Code, SPL) -> String
result' (Left (e, l, c)) = "Error " ++ e ++ " line " ++ show l ++ " column " ++ show c
result' (Right (_, a)) = ppSPL a

pp :: FilePath -> IO ()
pp = parseFileP splP result'

test :: String -> String
test s = case parse expP $ code s of
    Left (e, l, c) -> e
    Right (_, e) -> ppExp e

tab :: Int -> String
tab n = replicate n '\t'

join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join s (x:xs) = x ++ s ++ join s xs

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
ppExp (ExpTerm t) = ppTerm t
ppExp (ExpTermExp t o e) = printf "%s %s %s" (ppTerm t) (ppTermOp o) (ppExp e)

ppTerm :: Term -> String
ppTerm (TermFactor f) = ppFactor f
ppTerm (TermFactorTerm f o t) = printf "(%s %s %s)" (ppFactor f) (ppFactorOp o) (ppTerm t)

ppFactor :: Factor -> String 
ppFactor (FactorInt i) = show i 
ppFactor (FactorExp e) = printf "(%s)" (ppExp e)

ppFactorOp :: FactorOp -> String
ppFactorOp Times = "*"
ppFactorOp Divides = "/"

ppTermOp :: TermOp -> String
ppTermOp Add = "+"
ppTermOp Subtract = "-"

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

ppOp1 :: Op1 -> String 
ppOp1 Not = "!"
ppOp1 Min = "-"