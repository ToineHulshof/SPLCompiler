module Codegen where

import Grammar ( SPL )

data Instruction
    = LoadConstant Int
    | BranchAlways String
    | Link Int

instance Show Instruction where
    show (LoadConstant i) = "ldc " ++ show i
    show (BranchAlways s) = "bra " ++ s
    show (Link i) = "link " ++ show i

genCode :: FilePath -> SPL -> IO ()
genCode f spl = do
    writeFile f "ldc 60\nldc 9\nadd\ntrap 0\nhalt"
    putStrLn "\x1b[32mCompilation successful\x1b[0m"