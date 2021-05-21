module Extension where

import Control.Monad.State hiding (join)
import Data.Char
import qualified Data.Map as M
import Debug.Trace
import Grammar
import Types (Subst, apply, funTypeToList, subst)

data RegType = Pointer | Value

data GenEnvLLVM = GenEnvLLVM {uniqueInt :: Int, regCount :: Int, regMap :: M.Map Int RegType, llvmlocalmap :: M.Map String Int, retType :: Type}

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

setRegMap :: M.Map Int RegType -> CGLL ()
setRegMap m = do
  e <- get
  put e {regMap = m}

insertRegMap :: Int -> RegType -> CGLL ()
insertRegMap i rt = do
  e <- get
  let m = regMap e
  put e {regMap = M.insert i rt m}

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

genStoreInst :: String -> String -> Int -> String -- where val can be either a value of an address
genStoreInst t val toBeStored = "store " ++ t ++ " " ++ val ++ ", " ++ t ++ "* %" ++ show toBeStored

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
  insertRegMap i Value
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

allocateBasic :: String -> String -> CGLL ([String], Int) -- lines of code, the int address where it is stored
allocateBasic val typestr = do
  i <- newReg
  let code =
        [ genAllocaInst typestr i,
          genStoreInst typestr val i
        ]
  insertRegMap i Pointer
  return (code, i)

allocateComplex :: [String] -> String -> String -> CGLL ([String], Int)
allocateComplex lines addr typestr = do
  i <- newReg
  let code =
        lines
          ++ [ genAllocaInst typestr i,
               genStoreInst typestr addr i
             ]
  insertRegMap i Pointer
  return (code, i)

-- String is the type, int is the fresh address
allocateExp :: ExpRet -> String -> CGLL ([String], Int)
allocateExp (Basic val) = allocateBasic val
allocateExp (Lines cmds addr) = allocateComplex cmds addr

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

data ExpRet = Basic String | Lines [String] String

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
      let code = case expStr of
            (Basic str) -> ["ret " ++ typeToStr retType ++ " " ++ str]
            (Lines strs str) -> strs ++ ["ret " ++ typeToStr retType ++ " " ++ str]
      return code
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
genExpLLVM (ExpInt i _) = return (Basic (show i))
genExpLLVM (ExpChar char _) = return (Basic (show (ord char)))
genExpLLVM (ExpBool b _) = return (Basic bool) where bool = if b then "true" else "false"
-- genExpLLVM (ExpFunCall fc _) c = do
--   (code, newi) <- genFunCallLLVM fc c
--   return (Lines code (show (newi -1)), newi)
genExpLLVM _ = undefined

-- [String] is the complete code, [Int] are the addresses for the exps, Int is the new fresh int
-- genExpsLLVM :: [Exp] -> [Type] -> Int -> CGLL ([String], [String], Int)
-- genExpsLLVM [] [] i = return ([], [], i)
-- genExpsLLVM (e : es) (t : ts) i = do
--   (expstr, newi) <- genExpLLVM e i
--   let (code, addr, newi') = allocateExp expstr (typeToStr t) newi
--   (otherCode, otherAddr, newi) <- genExpsLLVM es ts newi'
--   return (code ++ otherCode, show addr : otherAddr, newi)

-- generateLoadCode :: [String] -> [Type] -> Int -> ([String], [Int], Int)
-- generateLoadCode [] [] i = ([], [], i)
-- generateLoadCode (addr : addrs) (t : ts) i =
--   let code = ["%" ++ show i ++ " = load " ++ typeToStr t ++ ", " ++ typeToStr t ++ "* %" ++ addr]
--       (othercode, newpnts, newi) = generateLoadCode addrs ts (i + 1)
--    in (code ++ othercode, i : newpnts, newi)

-- genFunCallLLVM :: FunCall -> Int -> CGLL ([String], Int)
-- genFunCallLLVM (FunCall (Just t) name exps _) i = do
--   let retType = getRetFromFunType t
--   let argTypes = init (funTypeToList t)
--   (argcode, addrs, newi) <- genExpsLLVM exps argTypes i
--   let (loadcode, pnts, newi') = generateLoadCode addrs argTypes newi
--   let code =
--         argcode ++ loadcode ++ ["%" ++ show newi' ++ " = call " ++ typeToStr retType ++ " @" ++ name ++ "(" ++ putCommas (zipWith (\t p -> typeToStr t ++ " %" ++ show p) argTypes pnts) ++ ")"]
--   return (code, newi' + 1)