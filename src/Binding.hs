module Binding where

import Parser
import Grammar
import Types
import Control.Monad (foldM, forM)
import qualified Data.Map as M

btSPL :: SPL -> TI TypeEnv
btSPL (SPL ds) = do
    x <- mapM btDecl ds
    return $ foldl combine emptyEnv x

btDecl :: Decl -> TI TypeEnv
btDecl (DeclVarDecl v) = return (TypeEnv M.empty) -- (volgens mij gebeurt dit al in het typing) btVarDecl v
btDecl (DeclFunDecl f) = btFunDecl f

btVarDecl :: VarDecl -> TI TypeEnv
btVarDecl (VarDeclVar s _) = TypeEnv . M.singleton (Var, s) . Scheme [] <$> newTyVar "a"
btVarDecl (VarDeclType t s _) = return $ TypeEnv $ M.singleton (Var, s) (Scheme [] t)

btFunDecl :: FunDecl -> TI TypeEnv
btFunDecl (FunDecl s args Nothing _ _) = do
    nvars <- mapM newTyVar args
    ret <- newTyVar "r"
    let t = foldr1 TypeFun $ nvars ++ [ret]
    return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
btFunDecl (FunDecl s _ (Just t) _ _) = return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)

ti :: SPL -> TypeEnv -> IO ()
ti spl e = do
    (bt, _) <- runTI $ btSPL spl
    print bt
    case bt of
        Left err -> putStrLn err
        Right env -> do
            (res, _) <- runTI $ tiSPL (e `combine` env) spl
            case res of
                Left err -> putStrLn err
                Right (s, e) -> putStrLn $ "Program is correctly typed\nsubst: " ++ show s ++ "\nenv: " ++ show e

testEnv :: TypeEnv -> String -> IO ()
testEnv env s = case testP splP s of
    Left e -> print e
    Right (c, s) -> if not $ null c then putStrLn "Did not finish parsing" else ti s env

test :: String -> IO ()
test = testEnv emptyEnv