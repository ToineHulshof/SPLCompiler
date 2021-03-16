module Binding where

import Parser
import Grammar
import Types
import Control.Monad (foldM)
import qualified Data.Map as M

emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

combine :: TypeEnv -> TypeEnv -> TypeEnv
combine (TypeEnv env1) (TypeEnv env2) = TypeEnv $ env1 `M.union` env2

btSPL :: SPL -> TI TypeEnv
btSPL (SPL ds) = do
    x <- mapM btDecl ds
    return $ foldl combine emptyEnv x

btDecl :: Decl -> TI TypeEnv
btDecl (DeclVarDecl v) = btVarDecl v
btDecl (DeclFunDecl f) = btFunDecl f

btVarDecl :: VarDecl -> TI TypeEnv
btVarDecl (VarDeclVar s _) = do TypeEnv . M.singleton (Var, s) . Scheme [] <$> newTyVar "a"
btVarDecl (VarDeclType t s _) = return $ TypeEnv $ M.singleton (Var, s) (Scheme [] t)

btFunDecl :: FunDecl -> TI TypeEnv
btFunDecl (FunDecl s args Nothing _ _) = do
    nvars <- mapM newTyVar args
    ret <- newTyVar "r"
    let t = foldr TypeFun Void $ nvars ++ [ret]
    return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
btFunDecl (FunDecl s _ (Just t) _ _) = return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)

showEnv :: SPL -> IO ()
showEnv s = do
    (res, _) <- runTI (btSPL s)
    case res of
        Left err -> putStrLn $ "error: " ++ err
        Right t  -> putStrLn $ "typenv: " ++ show t