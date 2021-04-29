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
    | LoadLocal Int
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

instance Show TrapCode where
    show Int = "0"

data Register
    = ReturnRegister

instance Show Register where
    show ReturnRegister = "RR"

instance Show Instruction where
    show (LoadConstant i) = "ldc " ++ show i
    show (BranchAlways s) = "bra " ++ s
    show (BranchSubroutine s) = "bsr " ++ s
    show (BranchTrue s) = "brt " ++ s
    show (BranchFalse s) = "brf " ++ s
    show (LoadRegister r) = "ldr " ++ show r
    show (StoreRegister r) = "str " ++ show r
    show (StoreStack i) = "sts " ++ show i
    show (StoreLocal i) = "stl " ++ show i
    show (LoadLocal i) = "ldl " ++ show i
    show (AdjustStack i) = "ajs " ++ show i
    show (Link i) = "link " ++ show i
    show Unlink = "unlink"
    show (Label s) = s ++ ":"
    show Return = "ret"

    show Add = "add"
    show (Trap c) = "trap " ++ show c ++ "\nsts 0x85\ntrap 1"
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

type GenEnv = (Int, String, M.Map String Int, M.Map String Int)
type CG a = StateT GenEnv IO a

new :: CG Int
new = do
    (s, n, g, l) <- get
    put (s + 1, n, g, l)
    return s

getFunName :: CG String
getFunName = gets (\(_, n, _, _) -> n)

setFunName :: String -> CG ()
setFunName n = do
    (s, _, g, l) <- get
    put (s, n, g, l)

getGlobalMap :: CG (M.Map String Int)
getGlobalMap = gets (\(_, _, g, _) -> g)

setGlobalMap :: M.Map String Int -> CG ()
setGlobalMap m = do
    (s, n, _, l) <- get
    put (s, n, m, l)

getLocalMap :: CG (M.Map String Int)
getLocalMap = gets (\(_, _, _, l) -> l)

setLocalMap :: M.Map String Int -> CG ()
setLocalMap m = do
    (s, n, g, _) <- get
    put (s, n, g, m)

genCode :: FilePath -> SPL -> IO ()
genCode f spl = do
    (instructions, _) <- runStateT (genSPL spl) (0, "", M.empty, M.empty)
    writeFile f (unlines $ map show (BranchAlways "main" : instructions)) --"ldc 60\nldc 9\nadd\ntrap 0\nhalt"
    --putStrLn "\x1b[32mCompilation successful\x1b[0m"

genSPL :: SPL -> CG [Instruction]
genSPL ds = concat <$> mapM genDecl ds

genDecl :: Decl -> CG [Instruction]
genDecl (DeclVarDecl (VarDecl _ _ e)) = genExp e
-- genDecl (DeclFunDecl (FunDecl "main" args _ vars stmts)) = do
--     (i1, m) <- genLocalVarDecls 0 vars
--     setLocalMap m
--     i2 <- genStmts stmts
--     return $ Label "main" : i1 ++ i2
genDecl (DeclFunDecl (FunDecl n args _ vars stmts)) = do
    (i1, m) <- genLocalVarDecls 1 args vars
    setLocalMap m
    setFunName n
    i2 <- genStmts stmts
    return $ Label n : Link (length vars + length args) : i1 ++ i2 ++ [Unlink, StoreStack (-1)] ++ [Return | n /= "main"]

argsMap :: Int -> [String] -> CG (M.Map String Int)
argsMap _ [] = return M.empty
argsMap i (x:xs) = do
    m <- argsMap (i + 1) xs 
    return $ m `M.union` M.singleton x i

genLocalVarDecls :: Int -> [String] -> [VarDecl] -> CG ([Instruction], M.Map String Int)
genLocalVarDecls i args [] = do
    m <- argsMap i args
    return ([], m)
genLocalVarDecls i args ((VarDecl _ n e):vs) = do
    i1 <- genExp e
    (i2, m) <- genLocalVarDecls (i + 1) args vs
    return (i1 ++ [StoreLocal i] ++ i2, M.singleton n i `M.union` m)

genStmts :: [Stmt] -> CG [Instruction]
genStmts ss = concat <$> mapM genStmt ss

-- link aantal vars
-- sp naar r0
-- acces naar var = r0 - index van var

-- x = 3;

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
    lm <- getLocalMap
    gm <- getGlobalMap
    i1 <- genExp e
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> error ""
            Just i -> undefined
        Just i -> return $ i1 ++ [StoreLocal i]
genStmt (StmtField n _ e) = undefined
genStmt (StmtReturn Nothing) = do 
    funName <- getFunName
    if funName == "main"
        then return [Halt]
        else return []
genStmt (StmtReturn (Just e)) = do 
    i1 <- genExp e
    funName <- getFunName
    if funName == "main"
    then return [StoreRegister ReturnRegister, Halt]
        else return $ i1 ++ [StoreRegister ReturnRegister]

genFunCall :: FunCall -> CG [Instruction]
-- genFunCall (FunCall _ n args) = concatMap genExp args ++ [BranchSubroutine n, AdjustStack (-length args), LoadRegister ReturnRegister]
genFunCall (FunCall _ "print" args) = do
    i <- concat <$> mapM genExp args
    return $ i ++ [Trap Int]
genFunCall (FunCall _ n args) = do
    i <- concat <$> mapM genExp args
    return $ i ++ [BranchSubroutine n, LoadRegister ReturnRegister]

genExp :: Exp -> CG [Instruction]
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
    lm <- getLocalMap
    gm <- getGlobalMap
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> trace (show lm) (error "")
            Just i -> undefined
        Just i -> return [LoadLocal i]
genExp (ExpField _ n fs) = return [LoadLocal (-2)]
genExp (ExpInt i) = return [LoadConstant $ fromInteger i]
genExp (ExpBool b) = return [LoadConstant $ if b then 1 else 0]
genExp (ExpChar c) = return [LoadConstant $ ord c]

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
