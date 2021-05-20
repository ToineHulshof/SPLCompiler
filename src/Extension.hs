module Extension where

import Control.Monad.State hiding (join)
import Data.Char
import qualified Data.Map as M
import Debug.Trace
import Grammar
import Types (Subst, apply, funTypeToList, subst)

data GenEnvLLVM = GenEnvLLVM {uniqueInt :: Int, llvmlocalmap :: M.Map String String, retType :: Type}

type CGLL a = StateT GenEnvLLVM IO a

newInt :: CGLL Int
newInt = do
  e <- get
  put e {uniqueInt = uniqueInt e + 1}
  return $ uniqueInt e

setReturnType :: Type -> CGLL ()
setReturnType t = do
  e <- get
  put e {retType = t}

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
  let i = 0
  let retType = getRetFromFunType t
  setReturnType retType
  let funDeclCode = strFunName n t
  (updMap, i) <- putArgsInMap args i
  let j = i
  let i = j + 1
  (updMap', varscode, i') <- genLocalVarsLLVM vars i
  (stmtscode, i'') <- genStmtsLLVM stmts i'
  let extraRet = case last stmts of
        (StmtReturn _ _) -> []
        _ ->
          ( case retType of
              Void -> ["ret void"]
              _ -> []
          )
  return $ funDeclCode : varscode ++ stmtscode ++ extraRet ++ ["}"]

putArgsInMap :: [String] -> Int -> CGLL (M.Map String String, Int)
putArgsInMap [] i = return (M.empty, i)
putArgsInMap (x : xs) i = do
  (m, i') <- putArgsInMap xs (i + 1)
  return (m `M.union` M.singleton x (show i), i')

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

genLocalVarsLLVM :: [VarDecl] -> Int -> CGLL (M.Map String String, [String], Int)
genLocalVarsLLVM [] i = return (M.empty, [], i)
genLocalVarsLLVM ((VarDecl (Just t) name exp) : xs) i = do
  let typestr = typeToStr t
  (expStr, iexp) <- genExpLLVM exp (i + 1)
  (m, others, i') <- genLocalVarsLLVM xs iexp
  let code = case expStr of
        Basic str ->
          [ "%" ++ show i ++ " = alloca " ++ typestr,
            "store " ++ typestr ++ " " ++ str ++ ", " ++ typestr ++ "* %" ++ show i
          ]
        Lines strs str ->
          ["%" ++ show i ++ " = alloca " ++ typestr]
            ++ strs
            ++ [ "store " ++ typestr ++ " %" ++ show str ++ ", " ++ typestr ++ "* %" ++ show i
               ]
  return (m `M.union` M.singleton name (show i), code ++ others, i')

data ExpRet = Basic String | Lines [String] String

genStmtsLLVM :: [Stmt] -> Int -> CGLL ([String], Int)
genStmtsLLVM [] i = return ([], i)
genStmtsLLVM (s : ss) i = do
  (codes, i') <- genStmtLLVM s i
  (codess, i'') <- genStmtsLLVM ss i'
  return (codes ++ codess, i'')

genStmtLLVM :: Stmt -> Int -> CGLL ([String], Int)
genStmtLLVM (StmtReturn me _) i =
  case me of
    (Just e) -> do
      (expStr, i') <- genExpLLVM e i
      retType <- gets retType
      let code = case expStr of
            (Basic str) -> ["ret " ++ typeToStr retType ++ " " ++ str]
            (Lines strs str) -> strs ++ ["ret " ++ typeToStr retType ++ " " ++ str]
      return (code, i')
    Nothing -> return (["ret void"], i)
genStmtLLVM _ _ = undefined

genExpLLVM :: Exp -> Int -> CGLL (ExpRet, Int)
genExpLLVM (ExpInt i _) c = return (Basic (show i), c)
genExpLLVM (ExpChar char _) c = return (Basic (show (ord char)), c)
genExpLLVM (ExpBool b _) c = return (Basic bool, c) where bool = if b then "true" else "false"
genExpLLVM _ _ = undefined