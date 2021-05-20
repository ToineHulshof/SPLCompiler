module Extension where

import Control.Monad.State hiding (join)
import qualified Data.Map as M
import Grammar
import Types (Subst, apply, funTypeToList, subst)

data GenEnvLLVM = GenEnvLLVM {uniqueInt :: Int, llvmlocalmap :: M.Map String String}

type CGLL a = StateT GenEnvLLVM IO a

newInt :: CGLL Int
newInt = do
  e <- get
  put e {uniqueInt = uniqueInt e + 1}
  return $ uniqueInt e

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
  let s1 = strFunName n t
  (updMap, i) <- putArgsInMap args i
  let i = i + 1
  -- process vars
  -- process stmts
  return $ s1 : ["}"]

putArgsInMap :: [String] -> Int -> CGLL (M.Map String String, Int)
putArgsInMap [] i = return (M.empty, i)
putArgsInMap (x : xs) i = do
  (m, i') <- putArgsInMap xs (i + 1)
  return (m `M.union` M.singleton x (show i), i')

typeToStr :: Type -> String
typeToStr (TypeBasic IntType) = "i32"
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
