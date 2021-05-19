module Codegen where

import Grammar
import Data.Char
import Data.Maybe
import Control.Monad.State hiding ( join )
import Errors
import Control.Applicative ( Alternative((<|>)) )
import Debug.Trace ( trace )
import Types ( funTypeToList, Subst, subst, apply )
import qualified Data.Map as M

data Instruction
    = LoadConstant Int
    | BranchAlways String
    | BranchFalse String
    | BranchSubroutine String
    | BranchTrue String
    | AdjustStack Int
    | LoadRegister Register
    | StoreRegister Register
    | StoreStack Int
    | StoreLocal Int
    | StoreAddress Int
    | StoreHeap
    | StoreMultipleHeap Int
    | LoadStackAddress Int
    | LoadLocal Int
    | LoadStack Int
    | LoadAddress (Either Int Register)
    | LoadHeap Int
    | LoadMultipleHeap Int Int
    | LoadRegisterFromRegister Register Register
    | ChangeAddress Int
    | Link Int
    | Unlink
    | Swap
    | Label String
    | Return
    | Add
    | Multiply
    | Subtract
    | Divide
    | Mod
    | AndI
    | OrI
    | XOr
    | EqualsI
    | NotEquals
    | Less
    | LessEqual
    | GreaterI
    | GreaterEqual
    | Negation
    | NotI
    | Trap TrapCode
    | Halt

data TrapCode
    = Int
    | Char

instance Show TrapCode where
    show Int = "0"
    show Char = "1"

data Register
    = ReturnRegister
    | GlobalOffset
    | GlobalTemp
    | StackPointer
    | HeapTemp

instance Show Register where
    show ReturnRegister = "RR"
    show GlobalOffset = "R5"
    show StackPointer = "R1"
    show GlobalTemp = "R6"
    show HeapTemp = "R7"

instance Show Instruction where
    show (LoadConstant i) = "ldc " ++ show i
    show (LoadStackAddress i) = "ldsa " ++ show i
    show (BranchAlways s) = "bra " ++ s
    show (BranchSubroutine s) = "bsr " ++ s
    show (BranchTrue s) = "brt " ++ s
    show (BranchFalse s) = "brf " ++ s
    show (LoadRegister r) = "ldr " ++ show r
    show (LoadStack i) = "lds " ++ show i
    show (LoadAddress (Left i)) = "lda " ++ show i
    show (LoadAddress (Right r)) = "lda " ++ show r
    show (LoadHeap i) = "ldh " ++ show i
    show (LoadMultipleHeap o l) = "ldmh " ++ show o ++ " " ++ show l
    show (LoadRegisterFromRegister to from) = "ldrr " ++ show to ++ " " ++ show from
    show (ChangeAddress i) = "ldaa " ++ show i
    show (StoreRegister r) = "str " ++ show r
    show (StoreStack i) = "sts " ++ show i
    show (StoreLocal i) = "stl " ++ show i
    show (StoreMultipleHeap i) = "stmh " ++ show i
    show StoreHeap = "sth"
    show (StoreAddress i) = "sta " ++ show i
    show (LoadLocal i) = "ldl " ++ show i
    show (AdjustStack i) = "ajs " ++ show i
    show (Link i) = "link " ++ show i
    show Unlink = "unlink"
    show (Label s) = s ++ ":"
    show Swap = "swp"
    show Return = "ret"

    show Add = "add"
    show (Trap c) = "trap " ++ show c
    show Multiply = "mul"
    show Subtract = "sub"
    show Divide = "div"
    show Mod = "mod"
    show AndI = "and"
    show OrI = "or"
    show XOr = "xor"
    show EqualsI = "eq"
    show NotEquals = "ne"
    show Less = "lt"
    show LessEqual = "le"
    show GreaterI = "gt"
    show GreaterEqual = "ge"
    show Negation = "neg"
    show NotI = "not"
    show Halt = "halt"

data GenEnv = GenEnv { ifCounter :: Int, funName :: String, localMap :: M.Map String Int, globalMap :: M.Map String Int, functions :: [[Instruction]], labels :: [String], spl :: SPL }
type CG a = StateT GenEnv IO a

new :: CG Int
new = do
    e <- get
    put e { ifCounter = ifCounter e + 1 }
    return $ ifCounter e

setFunName :: String -> CG ()
setFunName n = do
    e <- get
    put e { funName = n }

setGlobalMap :: M.Map String Int -> CG ()
setGlobalMap m = do
    e <- get
    put e { globalMap = m }

setLocalMap :: M.Map String Int -> CG ()
setLocalMap m = do
    e <- get
    put e { localMap = m }

addFunction :: [Instruction] -> CG ()
addFunction f = do
    e <- get
    put e { functions = f : functions e }

addLabel :: String -> CG ()
addLabel l = do
    e <- get
    put e { labels = l : labels e }

genCode :: Bool -> FilePath -> SPL -> IO ()
genCode False = genCodeSSM
genCode True = genCodeLLVM

changeSuffix :: Bool -> FilePath -> FilePath -> FilePath
changeSuffix llvm c ".spl" = c ++ (if llvm then ".ll" else ".ssm")
changeSuffix llvm c (x:xs) = changeSuffix llvm (c ++ [x]) xs
changeSuffix _ _ _ = error "File does not have spl as extension"

genCodeSSM :: FilePath -> SPL -> IO ()
genCodeSSM f spl = do
    (instructions, _) <- runStateT (genSPL spl) (GenEnv { ifCounter = 0, funName = "", localMap = M.empty, globalMap = M.empty, functions = [], labels = [], spl = spl })
    writeFile (changeSuffix False [] f) (unlines $ map show instructions)

genCodeLLVM :: FilePath -> SPL -> IO ()
genCodeLLVM f spl = writeFile (changeSuffix True [] f) "; ModuleID = 'multiply.c'\nsource_filename = \"multiply.c\"\ntarget datalayout = \"e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\ntarget triple = \"x86_64-apple-macosx11.0.0\"\n\n@.str = private unnamed_addr constant [3 x i8] c\"%d\00\", align 1\n\n; Function Attrs: noinline nounwind optnone ssp uwtable\ndefine i32 @fib(i32 %0) #0 {\n  %2 = alloca i32, align 4\n  %3 = alloca i32, align 4\n  store i32 %0, i32* %3, align 4\n  %4 = load i32, i32* %3, align 4\n  %5 = icmp sle i32 %4, 1\n  br i1 %5, label %6, label %8\n\n6:                                                ; preds = %1\n  %7 = load i32, i32* %3, align 4\n  store i32 %7, i32* %2, align 4\n  br label %16\n\n8:                                                ; preds = %1\n  %9 = load i32, i32* %3, align 4\n  %10 = sub nsw i32 %9, 1\n  %11 = call i32 @fib(i32 %10)\n  %12 = load i32, i32* %3, align 4\n  %13 = sub nsw i32 %12, 2\n  %14 = call i32 @fib(i32 %13)\n  %15 = add nsw i32 %11, %14\n  store i32 %15, i32* %2, align 4\n  br label %16\n\n16:                                               ; preds = %8, %6\n  %17 = load i32, i32* %2, align 4\n  ret i32 %17\n}\n\n; Function Attrs: noinline nounwind optnone ssp uwtable\ndefine i32 @main() #0 {\n  %1 = alloca i32, align 4\n  store i32 0, i32* %1, align 4\n  %2 = call i32 @fib(i32 18)\n  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), i32 %2)\n  ret i32 0\n}\n\ndeclare i32 @printf(i8*, ...) #1\n\nattributes #0 = { noinline nounwind optnone ssp uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"darwin-stkchk-strong-link\" \"disable-tail-calls\"=\"false\" \"frame-pointer\"=\"all\" \"less-precise-fpmad\"=\"false\" \"min-legal-vector-width\"=\"0\" \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"true\" \"probe-stack\"=\"___chkstk_darwin\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"penryn\" \"target-features\"=\"+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }\nattributes #1 = { \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"darwin-stkchk-strong-link\" \"disable-tail-calls\"=\"false\" \"frame-pointer\"=\"all\" \"less-precise-fpmad\"=\"false\" \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"true\" \"probe-stack\"=\"___chkstk_darwin\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"penryn\" \"target-features\"=\"+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }\n\n!llvm.module.flags = !{!0, !1, !2}\n!llvm.ident = !{!3}\n\n!0 = !{i32 2, !\"SDK Version\", [2 x i32] [i32 11, i32 3]}\n!1 = !{i32 1, !\"wchar_size\", i32 4}\n!2 = !{i32 7, !\"PIC Level\", i32 2}\n!3 = !{!\"Apple clang version 12.0.5 (clang-1205.0.22.9)\"}"

genSPL :: SPL -> CG [Instruction]
genSPL ds = do
    let vardecls = [(\(DeclVarDecl v) -> v) x | x@DeclVarDecl {} <- ds]
    (i1, m) <- genGlobalVars 1 vardecls
    setGlobalMap m
    i2 <- concat <$> mapM genFunDecl [(\(DeclFunDecl f) -> f) x | x@DeclFunDecl {} <- ds]
    functions <- gets functions
    return $ LoadRegisterFromRegister GlobalOffset StackPointer : i1 ++ [BranchAlways "main"] ++ i2 ++ concat functions

genGlobalVars :: Int -> [VarDecl] -> CG ([Instruction], M.Map String Int)
genGlobalVars _ [] = return ([], M.empty)
genGlobalVars i ((VarDecl _ n e):xs) = do
    i1 <- genExp e
    (i2, m) <- genGlobalVars (i + 1) xs
    return (i1 ++ i2, M.singleton n i `M.union` m)

genFunDecl :: FunDecl -> CG [Instruction]
genFunDecl (FunDecl n args (Just t) vars stmts _)
    | isPoly t = return []
    | otherwise = do
        m <- argsMap (-1 - length args) args
        setLocalMap m
        i1 <- genLocalVars 1 args vars
        setFunName n
        i2 <- genStmts stmts
        setLocalMap M.empty
        return $ Label n : Link (length vars) : i1 ++ i2 ++ [Label $ n ++ "End", Unlink, StoreStack (-1)] ++ [if n == "main" then Halt else Return]

argsMap :: Int -> [String] -> CG (M.Map String Int)
argsMap _ [] = return M.empty
argsMap i (x:xs) = do
    m <- argsMap (i + 1) xs 
    return $ m `M.union` M.singleton x i

genLocalVars :: Int -> [String] -> [VarDecl] -> CG [Instruction]
genLocalVars _ args [] = return []
genLocalVars i args ((VarDecl _ n e):vs) = do
    i1 <- genExp e
    m <- gets localMap
    setLocalMap $ M.singleton n i `M.union` m
    i2 <- genLocalVars (i + 1) args vs
    return $ i1 ++ [StoreLocal i] ++ i2

genStmts :: [Stmt] -> CG [Instruction]
genStmts ss = concat <$> mapM genStmt ss

genStmt :: Stmt -> CG [Instruction]
genStmt (StmtFunCall f) = genFunCall f
genStmt (StmtIf e ss1 ss2) = do
    i1 <- genExp e
    i2 <- genStmts (fromMaybe [] ss2)
    i3 <- genStmts ss1
    i <- show <$> new
    return $ i1 ++ [BranchTrue $ "Then" ++ i] ++ i2 ++ [BranchAlways $ "EndIf" ++ i, Label $ "Then" ++ i] ++ i3 ++ [Label $ "EndIf" ++ i]
genStmt (StmtWhile e ss) = do
    i1 <- genExp e
    i2 <- genStmts ss
    i <- show <$> new
    return $ Label ("While" ++ i) : i1 ++ [BranchFalse $ "EndWhile" ++ i] ++ i2 ++ [BranchAlways $ "While" ++ i, Label $ "EndWhile" ++ i]
genStmt (StmtField n [] e _) = do
    lm <- gets localMap
    gm <- gets globalMap
    i1 <- genExp e
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> error ""
            Just i -> do
                return $ i1 ++ [LoadConstant i, StoreAddress 0]
        Just i -> return $ i1 ++ [StoreLocal i]
genStmt (StmtField n fs e _) = do
    lm <- gets localMap
    gm <- gets globalMap
    i1 <- genExp e
    let i2 = map (LoadHeap . genField) (init fs)
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> error ""
            Just i -> do
                return $ i1 ++ [LoadRegister GlobalOffset, LoadAddress (Left i)] ++ i2 ++ [StoreAddress $ genField (last fs)]
        Just i -> return $ i1 ++ [LoadLocal i] ++ i2 ++ [StoreAddress $ genField (last fs)]
genStmt (StmtReturn Nothing _) = do 
    funName <- gets funName
    return [BranchAlways $ funName ++ "End"]
genStmt (StmtReturn (Just e) _) = do 
    i1 <- genExp e
    funName <- gets funName
    return $ i1 ++ [StoreRegister ReturnRegister, BranchAlways $ funName ++ "End"]

genField :: Field -> Int
genField (First _) = -1
genField (Second _) = 0
genField (Head _) = 0
genField (Tail _) = -1

genFunCall :: FunCall -> CG [Instruction]
genFunCall (FunCall (Just (TypeFun t Void)) "print" [arg] _) = do
    i1 <- genExp arg
    i2 <- genPrint t
    return $ i1 ++ i2 ++ printString "\n"
genFunCall (FunCall (Just (TypeFun (TypeList _) _)) "isEmpty" [arg] _) = (++ [LoadConstant 0, EqualsI]) <$> genExp arg
genFunCall (FunCall (Just t') n args _) = do
    ds <- gets spl
    let f@(FunDecl _ _ (Just t) _ _ _) = fromJust $ findFunction ds n
    if not $ isPoly t
        then funCallInstructions n args
        else do
            let name = n ++ join "-" (map typeName (init (funTypeToList t')))
            labels <- gets labels
            when (name `notElem` labels) $ genPolyFunDecl f t' name
            funCallInstructions name args

monoStmts :: Subst -> [Stmt] -> [Stmt]
monoStmts s = map $ monoStmt s

monoStmt :: Subst -> Stmt -> Stmt
monoStmt s (StmtIf e ss1 ss2) = StmtIf (monoExp s e) (monoStmts s ss1) (monoStmts s <$> ss2)
monoStmt s (StmtWhile e ss) = StmtWhile (monoExp s e) (monoStmts s ss)
monoStmt s (StmtField n fs e p) = StmtField n fs (monoExp s e) p
monoStmt s (StmtFunCall f) = StmtFunCall (monoFunCall s f)
monoStmt s (StmtReturn Nothing p) = StmtReturn Nothing p
monoStmt s (StmtReturn (Just e) p) = StmtReturn (Just $ monoExp s e) p

monoFunCall :: Subst -> FunCall -> FunCall
monoFunCall s (FunCall t n es p) = FunCall (apply s t) n (map (monoExp s) es) p

monoVarDecl :: Subst -> VarDecl -> VarDecl
monoVarDecl s (VarDecl t n e) = VarDecl (apply s t) n (monoExp s e)

monoExp :: Subst -> Exp -> Exp
monoExp s (Exp t o e1 e2 p) = Exp (apply s t) o (monoExp s e1) (monoExp s e2) p
monoExp s (ExpOp1 o e p) = ExpOp1 o (monoExp s e) p
monoExp s (ExpTuple (e1, e2) p) = ExpTuple (monoExp s e1, monoExp s e2) p
monoExp s (ExpBrackets e p) = ExpBrackets (monoExp s e) p
monoExp s (ExpField t n fs p) = ExpField (apply s t) n fs p
monoExp s (ExpFunCall f p) = ExpFunCall (monoFunCall s f) p
monoExp _ e = e

genPolyFunDecl :: FunDecl -> Type -> String -> CG ()
genPolyFunDecl f@(FunDecl n args (Just ft) vars stmts p) t l = do
    let s = subst ft t
    is <- genFunDecl (FunDecl l args (Just t) (map (monoVarDecl s) vars) (monoStmts s stmts) p)
    addFunction is
    addLabel l

funCallInstructions :: String -> [Exp] -> CG [Instruction]
funCallInstructions n args = (++ [BranchSubroutine n, AdjustStack (-length args + 1), LoadRegister ReturnRegister]) . concat <$> mapM genExp args

findFunction :: [Decl] -> String -> Maybe FunDecl
findFunction [] _ = Nothing 
findFunction ((DeclFunDecl f@(FunDecl n _ _ _ _ _)) : ds) s
    | n == s = Just f
    | otherwise = findFunction ds s
findFunction (d : ds) s = findFunction ds s

isPoly :: Type -> Bool
isPoly (TypeTuple t1 t2) = isPoly t1 || isPoly t2
isPoly (TypeList t) = isPoly t
isPoly (TypeID _ _) = True
isPoly (TypeFun t1 t2) = isPoly t1 || isPoly t2
isPoly _ = False

printString :: String -> [Instruction]
printString = concatMap (\c -> [LoadConstant (ord c), Trap Char])

genPrint :: Type -> CG [Instruction]
genPrint (TypeBasic IntType) = return [Trap Int]
genPrint (TypeBasic CharType) = return $ printString "'" ++ [Trap Char] ++ printString "'"
genPrint (TypeID _ _) = return []
genPrint t = do
    let name = "print" ++ typeName t
    labels <- gets labels
    when (name `notElem` labels) $ genPrint' name t
    return [BranchSubroutine name]

genPrint' :: String -> Type -> CG ()
genPrint' _ (TypeBasic BoolType) = do
    let f = [Label "printBool", Link 0, LoadLocal (-2), BranchTrue "printTrue"] ++ printString "False" ++ [BranchAlways "printEnd", Label "printTrue"] ++ printString "True" ++ [Label "printEnd", Unlink, Return]
    addFunction f
    addLabel "printBool"
genPrint' name (TypeTuple t1 t2) = do
    i1 <- genPrint t1
    i2 <- genPrint t2
    let f = [Label name, Link 0, LoadLocal (-2)] ++ (printString "(" ++ [LoadStack 0, LoadHeap (-1)] ++ i1 ++ printString ", " ++ [LoadHeap 0] ++ i2 ++ printString ")") ++ [Unlink, StoreStack (-1), Return]
    addFunction f
    addLabel name
genPrint' name (TypeList (TypeBasic CharType)) = do
    i <- show <$> new
    let f = [Label name, Link 0] ++ printString "\"" ++ [Label $ "While" ++ i, LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchFalse $ "EndWhile" ++ i, LoadLocal (-2), LoadHeap 0, Trap Char, LoadLocal (-2), LoadHeap (-1), StoreLocal (-2), LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchTrue $ "Then" ++ i, BranchAlways $ "EndIf" ++ i, Label $ "Then" ++ i] ++ [Label $ "EndIf" ++ i, BranchAlways $ "While" ++ i, Label $ "EndWhile" ++ i] ++ printString "\"" ++ [Unlink, StoreStack (-1), Return]
    addFunction f
    addLabel name
genPrint' name (TypeList t) = do
    i1 <- genPrint t
    i <- show <$> new
    let f = [Label name, Link 0] ++ printString "[" ++ [Label $ "While" ++ i, LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchFalse $ "EndWhile" ++ i, LoadLocal (-2), LoadHeap 0] ++ i1 ++ [LoadLocal (-2), LoadHeap (-1), StoreLocal (-2), LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchTrue $ "Then" ++ i, BranchAlways $ "EndIf" ++ i, Label $ "Then" ++ i] ++ printString ", " ++ [Label $ "EndIf" ++ i, BranchAlways $ "While" ++ i, Label $ "EndWhile" ++ i] ++ printString "]" ++ [Unlink, StoreStack (-1), Return]
    addFunction f
    addLabel name
genPrint' _ t = trace (show t) undefined

typeName :: Type -> String
typeName (TypeBasic IntType) = "Int"
typeName (TypeBasic BoolType) = "Bool"
typeName (TypeBasic CharType) = "Char"
typeName (TypeTuple t1 t2) = "Tuple" ++ typeName t1 ++ typeName t2
typeName (TypeList t) = "List" ++ typeName t
typeName _ = "Int"

genEq :: Type -> CG [Instruction]
genEq (TypeBasic _) = return []
genEq (TypeID _ _) = return [EqualsI]
genEq t = do
    let name = "equal" ++ typeName t
    labels <- gets labels
    when (name `notElem` labels) $ genEq' name t
    return [BranchSubroutine name, LoadRegister ReturnRegister]

genEq' :: String -> Type -> CG ()
genEq' name (TypeTuple t1 t2) = do
    i1 <- genEq t1
    i2 <- genEq t2
    let f = [Label name, Link 0, LoadLocal (-3), LoadHeap (-1), LoadLocal (-2), LoadHeap (-1)] ++ i1 ++ [LoadLocal (-3), LoadHeap 0, LoadLocal (-2), LoadHeap 0] ++ i2 ++ [AndI, StoreRegister ReturnRegister, Unlink, StoreStack (-1), Return]
    addFunction f
    addLabel name
genEq' name (TypeList t) = do
    i1 <- genEq t
    i <- show <$> new
    let f = [Label name, Link 2, LoadLocal (-3), LoadConstant 0, EqualsI, StoreLocal 1, LoadLocal (-2), LoadConstant 0, EqualsI, StoreLocal 2, LoadLocal 1, LoadLocal 2, OrI, BranchTrue ("Then" ++ i), BranchAlways ("EndIf" ++ i), Label ("Then" ++ i), LoadLocal 1, LoadLocal 2, AndI, StoreRegister ReturnRegister, BranchAlways $ name ++ "End", Label $ "EndIf" ++ i, LoadLocal (-3), LoadHeap 0, LoadLocal (-2), LoadHeap 0] ++ i1 ++ [LoadLocal (-3), LoadHeap (-1), LoadLocal (-2), LoadHeap (-1), BranchSubroutine name, AdjustStack (-1), LoadRegister ReturnRegister, AndI, StoreRegister ReturnRegister, Label $ name ++ "End", Unlink, StoreStack (-1), Return]
    addFunction f
    addLabel name

genExp :: Exp -> CG [Instruction]
genExp (Exp (Just t) o e1 e2 _) = do
    i1 <- genExp e1
    i2 <- genExp e2
    let i3 = genOp2 o
    if o == Cons
        then return $ i2 ++ i1 ++ [i3] else if o `elem` [Equals, Neq] 
        then do
            i4 <- genEq t
            return $ i1 ++ i2 ++ i4 ++ [NotI | o /= Equals]
    else return $ i1 ++ i2 ++ [i3]
genExp (ExpOp1 o e _) = do
    i <- genExp e
    return $ i ++ [genOp1 o]
genExp (ExpBrackets e _) = genExp e
genExp (ExpFunCall f _) = genFunCall f
genExp (ExpField _ n fs _) = do
    let i1 = map (LoadHeap . genField) fs
    lm <- gets localMap
    gm <- gets globalMap
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> trace (show lm) (error "")
            Just i -> return $ [LoadRegister GlobalOffset, LoadAddress (Left i)] ++ i1
        Just i -> return $ LoadLocal i : i1
genExp (ExpInt i _) = return [LoadConstant $ fromInteger i]
genExp (ExpBool b _) = return [LoadConstant $ if b then 1 else 0]
genExp (ExpChar c _) = return [LoadConstant $ ord c]
genExp (ExpTuple (e1, e2) _) = do
    i1 <- genExp e1
    i2 <- genExp e2
    return $ i1 ++ i2 ++ [StoreMultipleHeap 2]
genExp ExpEmptyList {} = return [LoadConstant 0]

genOp2 :: Op2 -> Instruction
genOp2 Plus = Add
genOp2 Minus = Subtract
genOp2 Product = Multiply
genOp2 Division = Divide
genOp2 Modulo = Mod
genOp2 Equals = EqualsI
genOp2 Smaller = Less
genOp2 Greater = GreaterI
genOp2 Leq = LessEqual
genOp2 Geq = GreaterEqual
genOp2 Neq = NotEquals
genOp2 And = AndI
genOp2 Or = OrI
genOp2 Cons = StoreMultipleHeap 2

genOp1 :: Op1 -> Instruction
genOp1 Min = Negation
genOp1 Not = NotI
