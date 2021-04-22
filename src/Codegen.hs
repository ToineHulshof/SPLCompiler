module Codegen where

import Grammar

data Instruction
    = LoadConstant Int
    | BranchAlways String
    | Link Int
    | Add
    | Trap TrapCode
    | Halt

data TrapCode
    = Int

instance Show TrapCode where
    show Int = "0"

instance Show Instruction where
    show (LoadConstant i) = "ldc " ++ show i
    show (BranchAlways s) = "bra " ++ s
    show (Link i) = "link " ++ show i
    show Add = "add"
    show (Trap c) = "trap " ++ show c
    show Halt = "halt"
    
genCode :: FilePath -> SPL -> IO ()
genCode f spl = do
    writeFile f (unlines $ map show (genSPL spl ++ [Trap Int, Halt])) --"ldc 60\nldc 9\nadd\ntrap 0\nhalt"
    putStrLn "\x1b[32mCompilation successful\x1b[0m"

genSPL :: SPL -> [Instruction]
genSPL = concatMap genDecl

genDecl :: Decl -> [Instruction]
genDecl (DeclVarDecl (VarDecl _ _ e)) = genExp e

genExp :: Exp -> [Instruction]
genExp (Exp _ o e1 e2) = genExp e1 ++ genExp e2 ++ [genOp2 o]
genExp (ExpInt i) = [LoadConstant $ fromInteger i]

genOp2 :: Op2 -> Instruction
genOp2 Plus = Add

-- data Exp
--   = Exp (Maybe Type) Op2 Exp Exp
--   | ExpOp1 Op1 Exp
--   | ExpTuple (Exp, Exp)
--   | ExpBrackets Exp
--   | ExpField (Maybe Type) String [Field]
--   | ExpInt Integer
--   | ExpChar Char
--   | ExpBool Bool
--   | ExpFunCall FunCall
--   | ExpEmptyList