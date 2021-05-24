module Extension where

import Control.Monad.State hiding (join)
import Data.Char
import qualified Data.Map as M
import Debug.Trace
import Grammar
import Types (Subst, apply, funTypeToList, subst)

data RegType = Pointer | Value
  deriving (Show)

data GenEnvLLVM = GenEnvLLVM {uniqueInt :: Int, regCount :: Int, regMap :: M.Map Int (RegType, Type), llvmlocalmap :: M.Map String Int, retType :: Type}

type CGLL a = StateT GenEnvLLVM IO a

data StrdType = Int

newInt :: CGLL Int
newInt = do
  e <- get
  put e {uniqueInt = uniqueInt e + 1}
  return $ uniqueInt e

newReg :: CGLL Int
newReg = do
  e <- get
  let ret = regCount e
  put e {regCount = regCount e + 1}
  return ret

setRegCount :: Int -> CGLL ()
setRegCount i = do
  e <- get
  put e {regCount = i}

setRegMap :: M.Map Int (RegType, Type) -> CGLL ()
setRegMap m = do
  e <- get
  put e {regMap = m}

insertRegMap :: Int -> RegType -> Type -> CGLL ()
insertRegMap i rt t = do
  e <- get
  let m = regMap e
  put e {regMap = M.insert i (rt, t) m}

setllvmlocalmap :: M.Map String Int -> CGLL ()
setllvmlocalmap m = do
  e <- get
  put e {llvmlocalmap = m}

insertllvmlocalmap :: String -> Int -> CGLL ()
insertllvmlocalmap i rt = do
  e <- get
  let m = llvmlocalmap e
  put e {llvmlocalmap = M.insert i rt m}

setReturnType :: Type -> CGLL ()
setReturnType t = do
  e <- get
  put e {retType = t}

genStoreValueInst :: String -> String -> Int -> String
genStoreValueInst t val toBeStored = "store " ++ t ++ " " ++ val ++ ", " ++ t ++ "* %" ++ show toBeStored

genStoreAddrInst :: String -> Int -> Int -> String
genStoreAddrInst t addr toBeStored = "store " ++ t ++ " %" ++ show addr ++ ", " ++ t ++ "* %" ++ show toBeStored

genLoadInst :: String -> Int -> Int -> String
genLoadInst t loadedFrom loadedTo = "%" ++ show loadedTo ++ " = load " ++ t ++ ", " ++ t ++ "* %" ++ show loadedFrom

genAllocaInst :: String -> Int -> String
genAllocaInst t addr = "%" ++ show addr ++ " = alloca " ++ t

genSPLLLVM :: SPL -> CGLL [String]
genSPLLLVM spl = do
  genDecls spl

genDecls :: [Decl] -> CGLL [String]
genDecls [] = return []
genDecls (x : xs) = do
  s1 <- genDeclLLVM x
  s2 <- genDecls xs
  return $ s1 ++ s2

genDeclLLVM :: Decl -> CGLL [String]
genDeclLLVM (DeclVarDecl vd) = return []
genDeclLLVM (DeclFunDecl fd) = genFunDeclLLVM fd

genFunDeclLLVM :: FunDecl -> CGLL [String]
genFunDeclLLVM (FunDecl n args (Just t) vars stmts _) = do
  setllvmlocalmap M.empty
  setRegCount 0
  setRegMap M.empty
  let retType = getRetFromFunType t
  let argType = init (funTypeToList t)
  setReturnType retType
  let funDeclCode = strFunName n t
  putArgsInMap args argType
  _ <- newReg
  varscode <- genLocalVarsLLVM vars
  stmtscode <- genStmtsLLVM stmts
  let extraRet = case last stmts of
        (StmtReturn _ _) -> []
        _ ->
          ( case retType of
              Void -> ["ret void"]
              _ -> []
          )
  return $ funDeclCode : varscode ++ stmtscode ++ extraRet ++ ["}"]

putArgsInMap :: [String] -> [Type] -> CGLL ()
putArgsInMap [] _ = return ()
putArgsInMap (x : xs) (t : ts) = do
  i <- newReg
  insertllvmlocalmap x i
  insertRegMap i Value t
  putArgsInMap xs ts
  return ()

getRetFromFunType :: Type -> Type
getRetFromFunType (TypeFun _ t2) = getRetFromFunType t2
getRetFromFunType t = t

typeToStr :: Type -> String
typeToStr (TypeBasic IntType) = "i32"
typeToStr (TypeBasic CharType) = "i8"
typeToStr (TypeBasic BoolType) = "i1"
typeToStr Void = "void"
typeToStr _ = ""

strToType :: String -> Type
strToType "i32" = TypeBasic IntType
strToType "i8" = TypeBasic CharType
strToType "i1" = TypeBasic BoolType
strToType "Void" = Void
strToType _ = undefined

putCommas :: [String] -> String
putCommas [] = ""
putCommas [x] = x
putCommas (x : xs) = x ++ ", " ++ putCommas xs

strFunName :: String -> Type -> String
strFunName name t =
  let typeNames = map typeToStr (funTypeToList t)
      argNames = init typeNames
      retName = last typeNames
      args = putCommas argNames
   in "define " ++ retName ++ " @" ++ name ++ "(" ++ args ++ ") {"

allocateBasicValue :: String -> String -> CGLL ([String], Int) -- lines of code, the int address where it is stored
allocateBasicValue val typestr = do
  i <- newReg
  let code =
        [ genAllocaInst typestr i,
          genStoreValueInst typestr val i
        ]
  insertRegMap i Pointer (strToType typestr)
  return (code, i)

allocateAddr :: Int -> String -> CGLL ([String], Int) -- lines of code, the int address where it is stored
allocateAddr addr typestr = do
  i <- newReg
  let code =
        [ genAllocaInst typestr i,
          genStoreAddrInst typestr addr i
        ]
  insertRegMap i Pointer (strToType typestr)
  return (code, i)

allocateLines :: [String] -> Int -> String -> CGLL ([String], Int)
allocateLines lines addr typestr = do
  i <- newReg
  let code =
        lines
          ++ [ genAllocaInst typestr i,
               genStoreAddrInst typestr addr i
             ]
  insertRegMap i Pointer (strToType typestr)
  return (code, i)

-- String is the type, int is here the value is stored
allocateExp :: ExpRet -> String -> CGLL ([String], Int)
allocateExp (BasicValue val) = allocateBasicValue val
allocateExp (Addr addr) = allocateAddr addr
allocateExp (Lines cmds addr) = allocateLines cmds addr

-- Gets the register to transform and returns the code and the new register
codeAddrToValue :: Int -> CGLL ([String], Int)
codeAddrToValue reg = do
  rmap <- gets regMap
  let (rt, t) = case M.lookup reg rmap of
        (Just (rt, t)) -> (rt, t)
        Nothing -> error ""
  (code, addr) <- case rt of
    Pointer -> do
      i <- newReg
      let inst = genLoadInst (typeToStr t) reg i
      return (inst, i)
    Value -> return ("", reg)
  return ([code], addr)

genLocalVarsLLVM :: [VarDecl] -> CGLL [String]
genLocalVarsLLVM [] = return []
genLocalVarsLLVM ((VarDecl (Just t) name exp) : xs) = do
  let typestr = typeToStr t
  expStr <- genExpLLVM exp
  lmap <- gets llvmlocalmap
  (code, addr) <- allocateExp expStr typestr
  setllvmlocalmap $ M.singleton name addr `M.union` lmap
  others <- genLocalVarsLLVM xs
  return (code ++ others)

data ExpRet = BasicValue String | Addr Int | Lines [String] Int
  deriving (Show)

genStmtsLLVM :: [Stmt] -> CGLL [String]
genStmtsLLVM [] = return []
genStmtsLLVM (s : ss) = do
  codes <- genStmtLLVM s
  codess <- genStmtsLLVM ss
  return (codes ++ codess)

genStmtLLVM :: Stmt -> CGLL [String]
-- genStmtLLVM (StmtFunCall fc) = genFunCallLLVM fc
genStmtLLVM (StmtReturn me _) =
  case me of
    (Just e) -> do
      expStr <- genExpLLVM e
      retType <- gets retType
      case expStr of
        (BasicValue val) -> return ["ret " ++ typeToStr retType ++ " " ++ val]
        (Addr addr) -> do
          (cd, reg) <- codeAddrToValue addr
          return $ cd ++ ["ret " ++ typeToStr retType ++ " %" ++ show reg]
        (Lines strs addr) -> do
          (cd, reg) <- codeAddrToValue addr
          return $ strs ++ cd ++ ["ret " ++ typeToStr retType ++ " %" ++ show reg]
    Nothing -> return ["ret void"]
genStmtLLVM _ = undefined

genExpLLVM :: Exp -> CGLL ExpRet
-- genExpLLVM (ExpField (Just t) name [] _) c = do
--   lmap <- gets llvmlocalmap
--   let (code, addr, newi) =
--         ( case M.lookup name lmap of
--             (Just (Ptr addr)) -> generateLoadCode [addr] [t] c
--             (Just (Val addr)) -> generateStoreCode addr t c
--             Nothing -> error ""
--         )
--   return (Lines code (show (head addr)), newi)
-- genExpLLVM (ExpField (Just t) name fs _) c = undefined
genExpLLVM (ExpInt i _) = return (BasicValue (show i))
genExpLLVM (ExpChar char _) = return (BasicValue (show (ord char)))
genExpLLVM (ExpBool b _) = return (BasicValue bool) where bool = if b then "true" else "false"
genExpLLVM (ExpFunCall fc _) = do
  (code, addr) <- genFunCallLLVM fc
  return $ Lines code addr
genExpLLVM _ = undefined

-- Generates all necessary evaluation of expressions for the function call
-- [String] is the complete code, [Int] are the addresses for the exps
genExpsLLVM :: [Exp] -> [Type] -> CGLL ([String], [Int])
genExpsLLVM [] [] = return ([], [])
genExpsLLVM (e : es) (t : ts) = do
  expstr <- genExpLLVM e
  (code, addr) <- allocateExp expstr (typeToStr t)
  (otherCode, otherAddr) <- genExpsLLVM es ts
  return (code ++ otherCode, addr : otherAddr)

-- Generates all necessary load instructions for a function call
generateLoadCode :: [Int] -> CGLL ([String], [Int])
generateLoadCode [] = return ([], [])
generateLoadCode (addr : addrs) = do
  i <- newReg
  rmap <- gets regMap
  let (rt, t) = case M.lookup addr rmap of
        (Just (rt, t)) -> (rt, t)
        Nothing -> error ""
  let code = trace (show rmap) genLoadInst (typeToStr t) addr i
  (othercode, newpnts) <- generateLoadCode addrs
  return (code : othercode, i : newpnts)

genFunCallLLVM :: FunCall -> CGLL ([String], Int) -- Int is the place where the result is stored
genFunCallLLVM (FunCall (Just t) name exps _) = do
  let retType = getRetFromFunType t
  let argTypes = init (funTypeToList t)
  (argcode, addrs) <- genExpsLLVM exps argTypes
  (loadcode, pnts) <- generateLoadCode addrs
  i <- newReg
  let code =
        argcode ++ loadcode ++ ["%" ++ show i ++ " = call " ++ typeToStr retType ++ " @" ++ name ++ "(" ++ putCommas (zipWith (\t p -> typeToStr t ++ " %" ++ show p) argTypes pnts) ++ ")"]
  return (code, i)