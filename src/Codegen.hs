module Codegen where

data Instruction
    = LoadConstant Int
    | BranchAlways String
    | Link Int
