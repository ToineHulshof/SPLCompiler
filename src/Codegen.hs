module Codegen where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.State hiding (join)
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace (trace)
import Errors
import Extension
import Grammar
import Types (Subst, apply, funTypeToList, subst)

data RuntimeIssue = RuntimeError | RuntimeWarning deriving Eq

instance Show RuntimeIssue where
  show RuntimeError = "\x1b[1m\x1b[31m❌ Runtime error:\x1b[0m"
  show RuntimeWarning = "\x1b[1m\x1b[33m🟡 Warning:\x1b[0m"

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
  = IntT
  | Char

instance Show TrapCode where
  show IntT = "0"
  show Char = "1"

data Register
  = ReturnRegister
  | GlobalOffset
  | GlobalTemp
  | StackPointer
  | HeapTemp
  | HeapPointer

instance Show Register where
  show ReturnRegister = "RR"
  show GlobalOffset = "R5"
  show StackPointer = "R1"
  show GlobalTemp = "R6"
  show HeapTemp = "R7"
  show HeapPointer = "R3"

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

data GenEnv = GenEnv {ifCounter :: Int, funName :: String, localMap :: M.Map String Int, globalMap :: M.Map String Int, functions :: [[Instruction]], labels :: [String], spl :: SPL}

type CG a = StateT GenEnv IO a

new :: CG Int
new = do
  e <- get
  put e {ifCounter = ifCounter e + 1}
  return $ ifCounter e

setFunName :: String -> CG ()
setFunName n = do
  e <- get
  put e {funName = n}

addGlobal :: String -> Int -> CG ()
addGlobal s i = do
  e <- get
  put e {globalMap = M.insert s i (globalMap e)}

setLocalMap :: M.Map String Int -> CG ()
setLocalMap m = do
  e <- get
  put e {localMap = m}

addFunction :: [Instruction] -> CG ()
addFunction f = do
  e <- get
  put e {functions = f : functions e}

addLabel :: String -> CG ()
addLabel l = do
  e <- get
  put e {labels = l : labels e}

genCode :: Bool -> FilePath -> FunDecl -> SPL -> IO ()
genCode False = genCodeSSM
genCode True = genCodeLLVM

changeSuffix :: Bool -> FilePath -> FilePath -> FilePath
changeSuffix llvm c ".spl" = c ++ (if llvm then ".ll" else ".ssm")
changeSuffix llvm c (x : xs) = changeSuffix llvm (c ++ [x]) xs
changeSuffix _ _ _ = error "File does not have spl as extension"

genCodeSSM :: FilePath -> FunDecl -> SPL -> IO ()
genCodeSSM f main spl = do
  (instructions, _) <- runStateT (genSPL main spl) (GenEnv {ifCounter = 0, funName = "", localMap = M.empty, globalMap = M.empty, functions = [], labels = [], spl = spl})
  writeFile (changeSuffix False [] f) (unlines $ map show instructions)

genCodeLLVM :: FilePath -> FunDecl -> SPL -> IO ()
genCodeLLVM f main spl = do
  (llvmcode, _) <- runStateT (genSPLLLVM spl) (GenEnvLLVM {uniqueInt = 0, regCount = 0, regMap = M.empty, llvmlocalmap = M.empty, retType = Void})
  writeFile (changeSuffix True [] f) (unlines llvmcode)

genSPL :: FunDecl -> SPL -> CG [Instruction]
genSPL main ds = do
    let vardecls = [(\(DeclVarDecl v) -> v) x | x@DeclVarDecl {} <- ds]
    i1 <- genGlobalVars 1 vardecls
    i2 <- genFunDecl main
    functions <- gets functions
    return $ LoadRegisterFromRegister GlobalOffset StackPointer : LoadConstant 4096 : StoreRegister HeapPointer : i1 ++ [BranchAlways "main"] ++ i2 ++ concat functions

genGlobalVars :: Int -> [VarDecl] -> CG [Instruction]
genGlobalVars _ [] = return []
genGlobalVars i ((VarDecl _ n e) : xs) = do
  addGlobal n i
  i1 <- genExp e
  i2 <- genGlobalVars (i + 1) xs
  return (i1 ++ i2)

genFunDecl :: FunDecl -> CG [Instruction]
genFunDecl (FunDecl _ n args (Just t) vars stmts _) = do
  localMapTemp <- gets localMap
  m <- argsMap (-1 - length args) args
  setLocalMap m
  i1 <- genLocalVars 1 args vars
  setFunName n
  i2 <- genStmts stmts
  setLocalMap localMapTemp
  return $ Label n : Link (length vars) : i1 ++ i2 ++ [Label $ n ++ "-End", Unlink] ++ [if n == "main" then Halt else Return]

argsMap :: Int -> [String] -> CG (M.Map String Int)
argsMap _ [] = return M.empty
argsMap i (x : xs) = do
  m <- argsMap (i + 1) xs
  return $ m `M.union` M.singleton x i

genLocalVars :: Int -> [String] -> [VarDecl] -> CG [Instruction]
genLocalVars _ args [] = return []
genLocalVars i args ((VarDecl _ n e) : vs) = do
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
  return $ i1 ++ [BranchTrue $ "then-" ++ i] ++ i2 ++ [BranchAlways $ "endIf-" ++ i, Label $ "then-" ++ i] ++ i3 ++ [Label $ "endIf-" ++ i]
genStmt (StmtWhile e ss) = do
  i1 <- genExp e
  i2 <- genStmts ss
  i <- show <$> new
  return $ Label ("while-" ++ i) : i1 ++ [BranchFalse $ "endWhile-" ++ i] ++ i2 ++ [BranchAlways $ "while-" ++ i, Label $ "endWhile-" ++ i]
genStmt (StmtField n [] e _) = do
  lm <- gets localMap
  gm <- gets globalMap
  i1 <- genExp e
  case M.lookup n lm of
    Nothing -> case M.lookup n gm of
      Nothing -> error ""
      Just i -> do
        return $ i1 ++ [LoadRegister GlobalOffset, StoreAddress i]
    Just i -> return $ i1 ++ [StoreLocal i]
genStmt (StmtField n fs e _) = do
  lm <- gets localMap
  gm <- gets globalMap
  i1 <- genExp e
  i2 <- concat <$> mapM genField fs
  case M.lookup n lm of
    Nothing -> case M.lookup n gm of
      Nothing -> error ""
      Just i -> do
        return $ i1 ++ [LoadRegister GlobalOffset, LoadAddress (Left i)] ++ init i2 ++ [StoreAddress $ fieldToInt (last fs)]
    Just i -> return $ i1 ++ [LoadLocal i] ++ init i2 ++ [StoreAddress $ fieldToInt (last fs)]
genStmt (StmtReturn Nothing _) = do
  funName <- gets funName
  return [BranchAlways $ funName ++ "-End"]
genStmt (StmtReturn (Just e) _) = do
  i1 <- genExp e
  funName <- gets funName
  return $ i1 ++ [StoreRegister ReturnRegister, BranchAlways $ funName ++ "-End"]

fieldToInt :: Field -> Int
fieldToInt (First _) = -1
fieldToInt (Second _) = 0
fieldToInt (Head _) = 0
fieldToInt (Tail _) = -1

genField :: Field -> CG [Instruction]
genField (First _) = return [LoadHeap (-1)]
genField (Second _) = return [LoadHeap 0]
genField (Head _) = do
  genRuntimeError "empty-Head" ([Link 0, LoadLocal (-2), LoadConstant 0, EqualsI, BranchTrue "empty-Head-if", LoadLocal (-2), LoadHeap 0, StoreRegister ReturnRegister, Unlink, Return, Label "empty-Head-if"] ++ runtimeIssue RuntimeError "Cannot take head of empty list")
  return [BranchSubroutine "empty-Head", AdjustStack (-1), LoadRegister ReturnRegister]
genField (Tail _) = do
  genRuntimeError "empty-Tail" ([Link 0, LoadLocal (-2), LoadConstant 0, EqualsI, BranchTrue "empty-Tail-if", LoadLocal (-2), LoadHeap (-1), StoreRegister ReturnRegister, Unlink, Return, Label "empty-Tail-if"]  ++ runtimeIssue RuntimeError "Cannot take tail of empty list")
  return [BranchSubroutine "empty-Tail", AdjustStack (-1), LoadRegister ReturnRegister]

runtimeIssue :: RuntimeIssue -> String -> [Instruction]
runtimeIssue i e = printString (show i ++ "\x1b[1m " ++ e ++ "\x1b[0m\n") ++ [Halt | i == RuntimeError]

genRuntimeError :: String -> [Instruction] -> CG ()
genRuntimeError l i = do
  labels <- gets labels
  when (l `notElem` labels) $ do
    addFunction $ Label l : i
    addLabel l

genFunCall :: FunCall -> CG [Instruction]
genFunCall (FunCall (Just (TypeFun t Void)) "print" [arg] _) = do
  i1 <- genExp arg
  i2 <- genPrint t
  return $ i1 ++ i2 ++ printString "\n"
genFunCall (FunCall (Just (TypeFun (TypeList _) _)) "isEmpty" [arg] _) = (++ [LoadConstant 0, EqualsI]) <$> genExp arg
genFunCall (FunCall (Just t') n args _) = do
    ds <- gets spl
    let f@(FunDecl oa _ _ (Just t) _ _ _) = fromJust $ findFunction ds n
    let argTypes = funTypeToList t'
    let oas = map (argTypes !!) oa
    let name = funLabel n oas
    labels <- gets labels
    when (name `notElem` labels) $ genFunCall' (null oa) f t' name
    funCallInstructions name args

isOverLoaded :: FunDecl -> Maybe [Int]
isOverLoaded f = Nothing

funLabel :: String -> [Type] -> String
funLabel n t = n ++ concatMap (("-" ++) . typeName) t

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
monoExp s (ExpFunCall f p) = ExpFunCall (monoFunCall s f) p
monoExp _ e = e

genFunCall' :: Bool -> FunDecl -> Type -> String -> CG ()
genFunCall' b f@(FunDecl o n args (Just ft) vars stmts p) t l = do
  addLabel l
  let f' = if b then f else let s = subst ft t in FunDecl o l args (Just t) (map (monoVarDecl s) vars) (monoStmts s stmts) p
  is <- genFunDecl f'
  addFunction is

funCallInstructions :: String -> [Exp] -> CG [Instruction]
funCallInstructions n args = (++ [BranchSubroutine n, AdjustStack (- length args), LoadRegister ReturnRegister]) . concat <$> mapM genExp args

findFunction :: [Decl] -> String -> Maybe FunDecl
findFunction [] _ = Nothing
findFunction ((DeclFunDecl f@(FunDecl _ n _ _ _ _ _)) : ds) s
  | n == s = Just f
  | otherwise = findFunction ds s
findFunction (d : ds) s = findFunction ds s

printString :: String -> [Instruction]
printString = concatMap (\c -> [LoadConstant (fromEnum c), Trap Char])

genPrint :: Type -> CG [Instruction]
genPrint (TypeBasic IntType) = return $ printString "\x1b[36m" ++ [Trap IntT] ++ printString "\x1b[0m"
genPrint (TypeBasic CharType) = return $ printString "\x1b[35m'" ++ [Trap Char] ++ printString "'\x1b[0m"
genPrint (TypeID _ _) = return []
genPrint t = do
  let name = "print-" ++ typeName t
  labels <- gets labels
  when (name `notElem` labels) $ genPrint' name t
  return [BranchSubroutine name]

genPrint' :: String -> Type -> CG ()
genPrint' _ (TypeBasic BoolType) = do
  let f = [Label "print-Bool", Link 0, LoadLocal (-2), BranchTrue "print-True"] ++ printString "\x1b[34mFalse\x1b[0m" ++ [BranchAlways "print-End", Label "print-True"] ++ printString "\x1b[34mTrue\x1b[0m" ++ [Label "print-End", Unlink, Return]
  addFunction f
  addLabel "print-Bool"
genPrint' name (TypeTuple t1 t2) = do
  i1 <- genPrint t1
  i2 <- genPrint t2
  let f = [Label name, Link 0, LoadLocal (-2)] ++ (printString "(" ++ [LoadStack 0, LoadHeap (-1)] ++ i1 ++ printString ", " ++ [LoadHeap 0] ++ i2 ++ printString ")") ++ [Unlink, Return]
  addFunction f
  addLabel name
genPrint' name (TypeList (TypeBasic CharType)) = do
  i <- show <$> new
  let f = [Label name, Link 0] ++ printString "\x1b[31m\"" ++ [Label $ "while-" ++ i, LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchFalse $ "endWhile-" ++ i, LoadLocal (-2), LoadHeap 0, Trap Char, LoadLocal (-2), LoadHeap (-1), StoreLocal (-2), LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchTrue $ "then-" ++ i, BranchAlways $ "endIf-" ++ i, Label $ "then-" ++ i] ++ [Label $ "endIf-" ++ i, BranchAlways $ "while-" ++ i, Label $ "endWhile-" ++ i] ++ printString "\"\x1b[0m" ++ [Unlink, Return]
  addFunction f
  addLabel name
genPrint' name (TypeList t) = do
  i1 <- genPrint t
  i <- show <$> new
  let f = [Label name, Link 0] ++ printString "[" ++ [Label $ "while-" ++ i, LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchFalse $ "endWhile-" ++ i, LoadLocal (-2), LoadHeap 0] ++ i1 ++ [LoadLocal (-2), LoadHeap (-1), StoreLocal (-2), LoadLocal (-2), LoadConstant 0, EqualsI, NotI, BranchTrue $ "then-" ++ i, BranchAlways $ "endIf-" ++ i, Label $ "then-" ++ i] ++ printString ", " ++ [Label $ "endIf-" ++ i, BranchAlways $ "while-" ++ i, Label $ "endWhile-" ++ i] ++ printString "]" ++ [Unlink, Return]
  addFunction f
  addLabel name
genPrint' _ t = undefined

typeName :: Type -> String
typeName (TypeBasic IntType) = "Int"
typeName (TypeBasic BoolType) = "Bool"
typeName (TypeBasic CharType) = "Char"
typeName (TypeTuple t1 t2) = "Tuple" ++ typeName t1 ++ typeName t2
typeName (TypeList t) = "List" ++ typeName t
typeName _ = "Int"

genEq :: Type -> CG [Instruction]
genEq (TypeBasic _) = return [EqualsI]
genEq t = do
  let name = "equal" ++ typeName t
  labels <- gets labels
  when (name `notElem` labels) $ genEq' name t
  return [BranchSubroutine name, LoadRegister ReturnRegister]

genEq' :: String -> Type -> CG ()
genEq' name (TypeTuple t1 t2) = do
  i1 <- genEq t1
  i2 <- genEq t2
  let f = [Label name, Link 0, LoadLocal (-3), LoadHeap (-1), LoadLocal (-2), LoadHeap (-1)] ++ i1 ++ [LoadLocal (-3), LoadHeap 0, LoadLocal (-2), LoadHeap 0] ++ i2 ++ [AndI, StoreRegister ReturnRegister, Unlink, Return]
  addFunction f
  addLabel name
genEq' name (TypeList t) = do
  i1 <- genEq t
  i <- show <$> new
  let f = [Label name, Link 2, LoadLocal (-3), LoadConstant 0, EqualsI, StoreLocal 1, LoadLocal (-2), LoadConstant 0, EqualsI, StoreLocal 2, LoadLocal 1, LoadLocal 2, OrI, BranchTrue ("then-" ++ i), BranchAlways ("endIf-" ++ i), Label ("then-" ++ i), LoadLocal 1, LoadLocal 2, AndI, StoreRegister ReturnRegister, BranchAlways $ name ++ "-End", Label $ "endIf-" ++ i, LoadLocal (-3), LoadHeap 0, LoadLocal (-2), LoadHeap 0] ++ i1 ++ [LoadLocal (-3), LoadHeap (-1), LoadLocal (-2), LoadHeap (-1), BranchSubroutine name, AdjustStack (-1), LoadRegister ReturnRegister, AndI, StoreRegister ReturnRegister, Label $ name ++ "-End", Unlink, Return]
  addFunction f
  addLabel name

genExp :: Exp -> CG [Instruction]
genExp (Exp (Just t) o e1 e2 _) = do
  i1 <- genExp e1
  i2 <- genExp e2
  i3 <- genOp2 o
  if o == Cons
    then return $ i2 ++ i1 ++ i3
    else
      if o `elem` [Equals, Neq]
        then do
          i4 <- genEq t
          return $ i1 ++ i2 ++ i4 ++ [NotI | o /= Equals]
        else return $ i1 ++ i2 ++ i3
genExp (ExpOp1 o e _) = do
  i <- genExp e
  return $ i ++ [genOp1 o]
genExp (ExpBrackets e _) = genExp e
genExp (ExpFunCall f _) = genFunCall f
genExp (ExpField n fs _) = do
    i1 <- concat <$> mapM genField fs
    lm <- gets localMap
    gm <- gets globalMap
    case M.lookup n lm of
        Nothing -> case M.lookup n gm of
            Nothing -> do
                f <- gets funName
                trace (f ++ ", " ++ n ++ " " ++ show lm) (error "")
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

maxInt :: Int
maxInt = 2147483647

minInt :: Int
minInt = -2147483648

genOp2 :: Op2 -> CG [Instruction]
genOp2 Plus = do
  genRuntimeError "add-fun" $ [Link 0, LoadLocal (-2), LoadConstant 0, GreaterI, LoadLocal (-3), LoadConstant maxInt, LoadLocal (-2), Subtract, GreaterI, AndI, BranchFalse "add-fun-if-1"] ++ runtimeIssue RuntimeWarning "integer overflow caused by + operation" ++ [Label "add-fun-if-1", LoadLocal (-2), LoadConstant 0, Less, LoadLocal (-3), LoadConstant minInt, LoadLocal (-2), Subtract, Less, AndI, BranchFalse "add-fun-if-2"] ++ runtimeIssue RuntimeWarning "integer underflow caused by + operation" ++ [Label "add-fun-if-2", LoadLocal (-2), LoadLocal (-3), Add, StoreRegister ReturnRegister, Unlink, Return]
  return [BranchSubroutine "add-fun", AdjustStack (-2), LoadRegister ReturnRegister]
genOp2 Minus = do
  genRuntimeError "sub-fun" $ [Link 0, LoadLocal (-2), LoadConstant 0, Less, LoadLocal (-3), LoadConstant maxInt, LoadLocal (-2), Add, GreaterI, AndI, BranchFalse "sub-fun-if-1"] ++ runtimeIssue RuntimeWarning "integer overflow caused by - operation" ++ [Label "sub-fun-if-1", LoadLocal (-2), LoadConstant 0, GreaterI, LoadLocal (-3), LoadConstant minInt, LoadLocal (-2), Add, Less, AndI, BranchFalse "sub-fun-if-2"] ++ runtimeIssue RuntimeWarning "integer underflow caused by - operation" ++ [Label "sub-fun-if-2", LoadLocal (-3), LoadLocal (-2), Subtract, StoreRegister ReturnRegister, Unlink, Return]
  return [BranchSubroutine "sub-fun", AdjustStack (-2), LoadRegister ReturnRegister]
genOp2 Product = do
  genRuntimeError "mul-fun" $ [Link 1, LoadLocal (-3), LoadLocal (-2), Multiply, StoreLocal 1, LoadLocal (-3), LoadConstant 0, EqualsI, NotI, LoadLocal 1, LoadLocal (-3), Divide, LoadLocal (-2), EqualsI, NotI, AndI, BranchFalse "mul-fun-if"] ++ runtimeIssue RuntimeWarning "integer overflow caused by * operation" ++ [Label "mul-fun-if", LoadLocal 1, StoreRegister ReturnRegister, Unlink, Return]
  return [BranchSubroutine "mul-fun", AdjustStack (-2), LoadRegister ReturnRegister]
genOp2 Division = do
  genRuntimeError "div-fun" ([Link 0, LoadLocal (-2), LoadConstant 0, EqualsI, BranchTrue "div-fun-if", LoadLocal (-3), LoadLocal (-2), Divide, StoreRegister ReturnRegister, Unlink, Return, Label "div-fun-if"] ++ runtimeIssue RuntimeError "Divide by 0")
  return [BranchSubroutine "div-fun", AdjustStack (-2), LoadRegister ReturnRegister]
genOp2 Modulo = return [Mod]
genOp2 Equals = return [EqualsI]
genOp2 Smaller = return [Less]
genOp2 Greater = return [GreaterI]
genOp2 Leq = return [LessEqual]
genOp2 Geq = return [GreaterEqual]
genOp2 Neq = return [NotEquals]
genOp2 And = return [AndI]
genOp2 Or = return [OrI]
genOp2 Cons = return [StoreMultipleHeap 2]

genOp1 :: Op1 -> Instruction
genOp1 Min = Negation
genOp1 Not = NotI
