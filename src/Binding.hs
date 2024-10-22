{-# LANGUAGE TupleSections #-}

module Binding where

import Codegen (genCode)
import Control.Monad (foldM, forM)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Array (listArray)
import Data.Graph (SCC (..), stronglyConnCompR)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.Tuple (swap)
import Debug.Trace (trace)
import Errors
import Grammar
import Parser (p, splP, testP)
import System.Exit (exitFailure)
import Types

components :: SPL -> [SCC Decl]
components ds = map ((\(a, _, _) -> a) <$>) $ stronglyConnCompR $ map (\d -> let (a, b) = ctDecl d in (d, a, b)) ds

ctDecl :: Decl -> ((Kind, String), [(Kind, String)])
ctDecl (DeclVarDecl (VarDecl _ n e)) = ((Var, n), ctExp [] e)
ctDecl (DeclFunDecl (FunDecl _ n args _ vars stmts _)) = ((Fun, n), ctStmts args stmts ++ concatMap (\(VarDecl _ _ e) -> ctExp args e) vars)

ctStmts :: [String] -> [Stmt] -> [(Kind, String)]
ctStmts args = concatMap (ctStmt args)

ctStmt :: [String] -> Stmt -> [(Kind, String)]
ctStmt args (StmtIf e ss1 ss2) = ctExp args e ++ ctStmts args ss1 ++ ctStmts args (fromMaybe [] ss2)
ctStmt args (StmtWhile e ss) = ctExp args e ++ ctStmts args ss
ctStmt args (StmtField n _ e _)
  | n `elem` args = []
  | otherwise = [(Var, n)]
ctStmt args (StmtFunCall (FunCall _ n es _)) = (Fun, n) : concatMap (ctExp args) es
ctStmt args (StmtReturn Nothing _) = []
ctStmt args (StmtReturn (Just e) _) = ctExp args e

ctExp :: [String] -> Exp -> [(Kind, String)]
ctExp args (Exp _ _ e1 e2 _) = ctExp args e1 ++ ctExp args e2
ctExp args (ExpOp1 _ e _) = ctExp args e
ctExp args (ExpTuple (e1, e2) _) = ctExp args e1 ++ ctExp args e2
ctExp args (ExpBrackets e _) = ctExp args e
ctExp args (ExpField n _ _)
    | n `elem` args = []
    | otherwise = [(Var, n)]
ctExp args (ExpFunCall (FunCall _ n es _) _) = (Fun, n) : concatMap (ctExp args) es
ctExp _ _ = []

stdlib :: TypeEnv
stdlib =
  TypeEnv $
    M.fromList
      [ ((Fun, "print"), Scheme ["t"] $ TypeFun (TypeID Nothing "t") Void),
        ((Fun, "isEmpty"), Scheme ["t"] $ TypeFun (TypeList $ TypeID Nothing "t") (TypeBasic BoolType))
      ]

btSPL :: TypeEnv -> SPL -> TI TypeEnv
btSPL env [] = return env
btSPL env (d : ds) = do
  env1 <- btDecl env d
  btSPL (env `combine` env1) ds

btDecl :: TypeEnv -> Decl -> TI TypeEnv
btDecl _ (DeclVarDecl v) = btVarDecl v
btDecl env (DeclFunDecl f) = btFunDecl env f

btVarDecl :: VarDecl -> TI TypeEnv
btVarDecl (VarDecl Nothing s _) = TypeEnv . M.singleton (Var, s) . Scheme [] <$> newTyVar Nothing "a"
btVarDecl (VarDecl (Just t) s _) = return $ TypeEnv $ M.singleton (Var, s) (Scheme [] t)

btFunDecl :: TypeEnv -> FunDecl -> TI TypeEnv
btFunDecl (TypeEnv env) (FunDecl o s args Nothing _ _ p) = do
  case M.lookup (Fun, s) env of
    Nothing -> do
      nvars <- mapM (newTyVar Nothing) args
      ret <- newTyVar Nothing "r"
      let t = foldr1 TypeFun $ nvars ++ [ret]
      return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
    Just _ -> tell [Error TypeError (nes $ "Function \x1b[33m" ++ s ++ "\x1b[0m\x1b[1m is already defined.") (Just p)] >> return (TypeEnv env)
btFunDecl (TypeEnv env) (FunDecl o s _ (Just t) _ _ p) = do
  case M.lookup (Fun, s) env of
    Nothing -> return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
    Just _ -> tell [Error TypeError (nes $ "Function \x1b[33m" ++ s ++ "\x1b[0m\x1b[1m is already defined.") (Just p)] >> return (TypeEnv env)

hasEffect :: (String, Type) -> Bool
hasEffect (s, TypeID _ n) = n /= s
hasEffect _ = True

effect :: Subst -> Bool
effect s = any hasEffect $ M.toList s

repeatDecl :: Int -> TypeEnv -> [Decl] -> TI (TypeEnv, [Decl])
repeatDecl 0 env ds = return (env, ds)
repeatDecl i env ds = do
  (_, env1, ds') <- tiDecls env ds
  repeatDecl (i - 1) env1 ds'

tiComp :: TypeEnv -> SCC Decl -> TI (TypeEnv, [Decl])
tiComp env (AcyclicSCC d) = (\(_, e, dc) -> (e, [dc])) <$> tiDecl env d
tiComp env (CyclicSCC ds) = repeatDecl (length ds) env ds

tiComps :: TypeEnv -> [SCC Decl] -> TI (TypeEnv, [Decl])
tiComps env [] = return (env, [])
tiComps env (d : ds) = do
  (env1, ds1) <- tiComp env d
  (env2, ds2) <- tiComps env1 ds
  return (env2 `combine` env1, ds1 ++ ds2)

varCycle :: SCC Decl -> Bool
varCycle (AcyclicSCC _) = False
varCycle (CyclicSCC ds) = any isVarDecl ds
  where
    isVarDecl DeclFunDecl {} = False
    isVarDecl DeclVarDecl {} = True

ti' :: SPL -> TypeEnv -> TI (TypeEnv, SPL)
ti' spl e = do
  bt <- btSPL emptyEnv spl
  let comps = components spl
  if any varCycle comps
    then tell [Error TypeError (nes "Cycle found in global variables") Nothing] >> return (e, spl)
    else tiComps (stdlib `combine` e `combine` bt) comps

tiResult :: Bool -> String -> Maybe FilePath -> SPL -> TypeEnv -> IO ()
tiResult llvm s f spl e = do
    (((env, spl'), e), _) <- runTI $ ti' spl e
    if null e
        then case f of
            Nothing -> putStr $ "\x1b[32m✅ Program is correctly typed\x1b[0m\n" ++ show env ++ "\n"
            Just filePath -> case getMain spl' of
                Just main -> putStrLn "\x1b[32m✅ Program is correctly typed\x1b[0m\n" >> genCode llvm filePath main spl'
                Nothing -> do
                    print $ Errors (fromMaybe "<interactive>" f) (listArray (1, length l) l) [Error CodegenError (nes "\x1b[31mNo main function\x1b[0m\n") Nothing]
                    putStrLn "\x1b[32m✅ Program is correctly typed\x1b[0m\n"
                    putStr $ show env
                    exitFailure
        else print (Errors (fromMaybe "<interactive>" f) (listArray (1, length l) l) (removeDuplicates e)) >> exitFailure
    where
        l = lines s

getMain :: SPL -> Maybe FunDecl
getMain [] = Nothing
getMain (DeclFunDecl f@(FunDecl _ n _ _ _ _ _) : ds)
  | n == "main" = Just f
  | otherwise = getMain ds
getMain (DeclVarDecl _ : ds) = getMain ds

replaceTab :: Char -> String
replaceTab '\t' = "    "
replaceTab c = [c]

testEnv :: Bool -> Maybe FilePath -> TypeEnv -> String -> IO ()
testEnv llvm f env s'
  | not $ null e = print (Errors (fromMaybe "<interactive>" f) (listArray (1, length l) l) e) >> exitFailure
  | otherwise = case r of
    Nothing -> print e
    Just (_, spl) -> tiResult llvm s f spl env
  where
    s = concatMap replaceTab s'
    (e, r) = p s
    l = lines s

check :: String -> IO ()
check = testEnv False Nothing emptyEnv

compile :: Bool -> FilePath -> String -> IO ()
compile llvm f = testEnv llvm (Just f) emptyEnv
