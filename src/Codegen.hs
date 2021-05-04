module Codegen where

import Grammar
import Data.Char
import Data.Maybe
import Control.Monad.State
import Control.Monad
import Control.Applicative ( Alternative((<|>)) )
import Debug.Trace ( trace )
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
    show Return = "ret"

    show Add = "add"
    show (Trap c) = "trap " ++ show c-- ++ "\nldc 0x20\ntrap 1"
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

data GenEnv = GenEnv { ifCounter :: Int, funName :: String, boolPrint :: Bool, localMap :: M.Map String Int, globalMap :: M.Map String Int }
type CG a = StateT GenEnv IO a

new :: CG Int
new = do
    e <- get
    put e { ifCounter = ifCounter e + 1 }
    return $ ifCounter e

updateBoolPrint :: CG ()
updateBoolPrint = do
    e <- get
    put e { boolPrint = True }

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

genCode :: FilePath -> SPL -> IO ()
genCode f spl = do
    (instructions, _) <- runStateT (genSPL spl) (GenEnv { ifCounter = 0, funName = "", boolPrint = False, localMap = M.empty, globalMap = M.empty })
    writeFile f (unlines $ map show instructions)
    --putStrLn "\x1b[32mCompilation successful\x1b[0m"

genSPL :: SPL -> CG [Instruction]
genSPL ds = do
    let vardecls = [(\(DeclVarDecl v) -> v) x | x@DeclVarDecl {} <- ds]
    (i1, m) <- genGlobalVars 1 vardecls
    setGlobalMap m
    i2 <- concat <$> mapM genFunDecl [(\(DeclFunDecl f) -> f) x | x@DeclFunDecl {} <- ds]
    i3 <- genExtra
    return $ LoadRegisterFromRegister GlobalOffset StackPointer : i1 ++ [BranchAlways "main"] ++ i3 ++ i2

genExtra :: CG [Instruction]
genExtra = do
    boolPrint <- gets boolPrint
    let i = if boolPrint then [Label "printBool", Link 1, LoadLocal (-2), BranchTrue "printTrue"] ++ printString "False" ++ [BranchAlways "printEnd", Label "printTrue"] ++ printString "True" ++ [Label "printEnd", Unlink, Return] else []
    return i

genGlobalVars :: Int -> [VarDecl] -> CG ([Instruction], M.Map String Int)
genGlobalVars _ [] = return ([], M.empty)
genGlobalVars i ((VarDecl _ n e):xs) = do
    i1 <- genExp e
    (i2, m) <- genGlobalVars (i + 1) xs
    return (i1 ++ i2, M.singleton n i `M.union` m)

genFunDecl :: FunDecl -> CG [Instruction]
genFunDecl (FunDecl n args _ vars stmts) = do
    i1 <- genLocalVars 1 args vars
    setFunName n
    i2 <- genStmts stmts
    setLocalMap M.empty
    return $ Label n : Link (length vars + length args) : i1 ++ i2 ++ [Unlink, StoreStack (-1)] ++ [if n == "main" then Halt else Return]

argsMap :: Int -> [String] -> CG (M.Map String Int)
argsMap _ [] = return M.empty
argsMap i (x:xs) = do
    m <- argsMap (i + 1) xs 
    return $ m `M.union` M.singleton x i

genLocalVars :: Int -> [String] -> [VarDecl] -> CG [Instruction]
genLocalVars _ args [] = do
    m1 <- argsMap (-1 - length args) args
    m2 <- gets localMap
    setLocalMap $ m1 `M.union` m2
    return []
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
genStmt (StmtField n [] e) = do
    lm <- gets localMap
    gm <- gets globalMap
    i1 <- genExp e
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> error ""
            Just i -> do
                return $ i1 ++ [LoadConstant i, StoreAddress 0]
        Just i -> return $ i1 ++ [StoreLocal i]
genStmt (StmtField n _ e) = undefined
genStmt (StmtReturn Nothing) = do 
    funName <- gets funName
    if funName == "main"
        then return [Halt]
        else return []
genStmt (StmtReturn (Just e)) = do 
    i1 <- genExp e
    funName <- gets funName
    if funName == "main"
        then return [StoreRegister ReturnRegister, Halt]
        else return $ i1 ++ [StoreRegister ReturnRegister]

genFunCall :: FunCall -> CG [Instruction]
-- genFunCall (FunCall _ n args) = concatMap genExp args ++ [BranchSubroutine n, AdjustStack (-length args), LoadRegister ReturnRegister]
genFunCall (FunCall (Just t) "print" args) = do
    i1 <- concat <$> mapM genExp args
    let (TypeFun t' _) = t
    genPrint i1 t'
genFunCall (FunCall _ n args) = do
    i <- concat <$> mapM genExp args
    return $ i ++ [BranchSubroutine n, LoadRegister ReturnRegister]

printString :: String -> [Instruction]
printString s = map (LoadConstant . ord) (reverse s) ++ replicate (length s) (Trap Char)

genPrint :: [Instruction] -> Type -> CG [Instruction]
genPrint i1 (TypeBasic IntType) = return $ i1 ++ [Trap Int]
genPrint i1 (TypeBasic BoolType) = updateBoolPrint >> return (i1 ++ [BranchSubroutine "printBool", AdjustStack (-1)])
genPrint i1 (TypeBasic CharType) = return $ i1 ++ [Trap Char]
genPrint i1 (TypeArray t) = do
    i <- show <$> new
    i2 <- genPrint [] t -- ?
    return $ printString "[" ++ i1 ++ [Label $ "print" ++ i, StoreRegister HeapTemp, LoadRegister HeapTemp, LoadConstant 0, EqualsI, BranchTrue $ "end" ++ i, LoadRegister HeapTemp, LoadMultipleHeap 0 2] ++ i2 ++ printString ", " ++ [BranchAlways $ "print" ++ i, Label $ "end" ++ i] ++ printString "]"
genPrint i1 (TypeTuple t1 t2) = do
    i2 <- genPrint (i1 ++ [LoadHeap (-1)]) t1
    i3 <- genPrint (i1 ++ [LoadHeap 0]) t2
    return $ printString "(" ++ i2 ++ printString ", " ++ i3 ++ printString ")"
genPrint _ _ = undefined -- TypeID, TypeFun, Void

genGetArray :: [Instruction]
genGetArray = undefined

genExp :: Exp -> CG [Instruction]
genExp (Exp t Cons e1 e2) = do
    i2 <- genExp e2 -- address of heap
    i1 <- genExp e1 -- value of head
    return $ i2 ++ i1 ++ [StoreMultipleHeap 2]
genExp (Exp _ o e1 e2) = do
    i1 <- genExp e1
    i2 <- genExp e2
    return $ i1 ++ i2 ++ [genOp2 o]
genExp (ExpOp1 o e) = do
    i <- genExp e
    return $ i ++ [genOp1 o]
genExp (ExpBrackets e) = genExp e
genExp (ExpFunCall f) = genFunCall f
genExp (ExpField _ n []) = do
    lm <- gets localMap
    gm <- gets globalMap
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> trace (show lm) (error "")
            Just i -> return [LoadRegister GlobalOffset, LoadAddress (Left i)]
        Just i -> return [LoadLocal i]
genExp (ExpField (Just (TypeArray t)) n (x:xs)) = do 
    i1 <- genExp (ExpField (Just t) n xs)
    case x of
        Head -> return $ LoadHeap 0 : i1
        Tail -> return $ LoadHeap 1 : i1
        _ -> error ""
genExp (ExpField t n fs) = trace (show t) (error "")
genExp (ExpInt i) = return [LoadConstant $ fromInteger i]
genExp (ExpBool b) = return [LoadConstant $ if b then 1 else 0]
genExp (ExpChar c) = return [LoadConstant $ ord c]
genExp (ExpTuple (e1, e2)) = do
    i1 <- genExp e1
    i2 <- genExp e2
    return $ i1 ++ i2 ++ [StoreMultipleHeap 2]
genExp ExpEmptyList = return [LoadConstant 0]

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
genOp2 Cons = undefined

genOp1 :: Op1 -> Instruction
genOp1 Min = Negation
genOp1 Not = NotI
