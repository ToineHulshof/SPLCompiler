module Codegen where

import Grammar
import Data.Char
import Data.Maybe
import Control.Monad.State
import Control.Monad

data Instruction
    = LoadConstant Int
    | BranchAlways String
    | BranchSubroutine String
    | BranchTrue String
    | AdjustStack Int
    | LoadRegister Register
    | StoreRegister Register
    | StoreStack Int
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
    show (LoadRegister r) = "ldr " ++ show r
    show (StoreRegister r) = "str " ++ show r
    show (StoreStack i) = "sts " ++ show i
    show (LoadLocal i) = "ldl " ++ show i
    show (AdjustStack i) = "ajs " ++ show i
    show (Link i) = "link " ++ show i
    show Unlink = "unlink"
    show (Label s) = s ++ ":"
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

type CG a = StateT Int IO a

new :: CG Int
new = do
    s <- get
    put (s + 1)
    return s

genCode :: FilePath -> SPL -> IO ()
genCode f spl = do
    (instructions, _) <- runStateT (genSPL spl) 0
    writeFile f (unlines $ map show (BranchAlways "main" : instructions ++ [Trap Int, Halt])) --"ldc 60\nldc 9\nadd\ntrap 0\nhalt"
    --putStrLn "\x1b[32mCompilation successful\x1b[0m"

genSPL :: SPL -> CG [Instruction]
genSPL ds = concat <$> mapM genDecl ds

genDecl :: Decl -> CG [Instruction]
genDecl (DeclVarDecl (VarDecl _ _ e)) = genExp e
genDecl (DeclFunDecl (FunDecl "main" args _ vars stmts)) = do
    i <- genStmts stmts
    return $ Label "main" : i
genDecl (DeclFunDecl (FunDecl n args _ vars stmts)) = do 
    i <- genStmts stmts
    return $ Label n : Link 0 : i ++ [Unlink, StoreStack (-1), Return]

genStmts :: [Stmt] -> CG [Instruction]
genStmts ss = concat <$> mapM genStmt ss



genStmt :: Stmt -> CG [Instruction]
genStmt (StmtFunCall f) = genFunCall f
genStmt (StmtIf e ss1 ss2) = do
    i1 <- genExp e
    i2 <- genStmts (fromMaybe [] ss2)
    i3 <- genStmts ss1
    i <- show <$> new
    return $ i1 ++ [BranchTrue $ "Then" ++ i] ++ i2 ++ [BranchAlways $ "End" ++ i, Label $ "Then" ++ i] ++ i3 ++ [Label $ "End" ++ i, StoreRegister ReturnRegister]
genStmt (StmtReturn Nothing) = return []--[Return]
genStmt (StmtReturn (Just e)) = genExp e -- ++ [StoreRegister ReturnRegister]--, Return]

genFunCall :: FunCall -> CG [Instruction]
-- genFunCall (FunCall _ n args) = concatMap genExp args ++ [BranchSubroutine n, AdjustStack (-length args), LoadRegister ReturnRegister]
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
