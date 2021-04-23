module Codegen where

import Grammar
import Data.Char
import Data.Maybe

data Instruction
    = LoadConstant Int
    | BranchAlways String
    | BranchSubroutine String
    | BranchTrue String
    | AdjustStack Int
    | LoadRegister Register
    | StoreRegister Register
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
    
genCode :: FilePath -> SPL -> IO ()
genCode f spl = do
    writeFile f (unlines $ map show (BranchAlways "main" : genSPL spl ++ [Trap Int, Halt])) --"ldc 60\nldc 9\nadd\ntrap 0\nhalt"
    --putStrLn "\x1b[32mCompilation successful\x1b[0m"

genSPL :: SPL -> [Instruction]
genSPL = concatMap genDecl

genDecl :: Decl -> [Instruction]
genDecl (DeclVarDecl (VarDecl _ _ e)) = genExp e
genDecl (DeclFunDecl (FunDecl "main" args _ vars stmts)) = Label "main" : genStmts stmts
genDecl (DeclFunDecl (FunDecl n args _ vars stmts)) = Label n : Link 0 : genStmts stmts ++ [Unlink, Return]

genStmts :: [Stmt] -> [Instruction]
genStmts = concatMap genStmt

genStmt :: Stmt -> [Instruction]
genStmt (StmtFunCall f) = genFunCall f
genStmt (StmtIf e ss1 ss2) = genExp e ++ [BranchTrue "Then"] ++ genStmts (fromMaybe [] ss2) ++ [BranchAlways "End", Label "Then"] ++ genStmts ss1 ++ [Label "End"]
genStmt (StmtReturn Nothing) = []--[Return]
genStmt (StmtReturn (Just e)) = genExp e ++ [StoreRegister ReturnRegister]--, Return]

genFunCall :: FunCall -> [Instruction]
-- genFunCall (FunCall _ n args) = concatMap genExp args ++ [BranchSubroutine n, AdjustStack (-length args), LoadRegister ReturnRegister]
genFunCall (FunCall _ n args) = concatMap genExp args ++ [BranchSubroutine n, LoadRegister ReturnRegister]

genExp :: Exp -> [Instruction]
genExp (Exp _ o e1 e2) = genExp e1 ++ genExp e2 ++ [genOp2 o]
genExp (ExpOp1 o e) = genExp e ++ [genOp1 o]
genExp (ExpBrackets e) = genExp e
genExp (ExpFunCall f) = genFunCall f
genExp (ExpField _ n fs) = [LoadLocal (-2)]
genExp (ExpInt i) = [LoadConstant $ fromInteger i]
genExp (ExpBool b) = [LoadConstant $ if b then 1 else 0]
genExp (ExpChar c) = [LoadConstant $ ord c]

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
