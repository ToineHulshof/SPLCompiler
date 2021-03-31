module Binding where

import Parser
import Grammar
import Types
import Control.Monad (foldM, forM)
import qualified Data.Map as M

stdlib :: TypeEnv
stdlib = TypeEnv $ M.fromList [
    ((Fun, "print"), Scheme [] (TypeFun (TypeID Nothing "t") Void)),
    ((Fun, "isEmpty"), Scheme [] (TypeFun (TypeArray $ TypeID Nothing "t") (TypeBasic BoolType)))
    ]

btSPL :: SPL -> TI TypeEnv
btSPL (SPL ds) = do
    x <- mapM btDecl ds
    return $ foldl combine emptyEnv x

btDecl :: Decl -> TI TypeEnv
btDecl (DeclVarDecl v) = btVarDecl v
btDecl (DeclFunDecl f) = btFunDecl f

btVarDecl :: VarDecl -> TI TypeEnv
btVarDecl (VarDeclVar s _) = TypeEnv . M.singleton (Var, s) . Scheme [] <$> newTyVar Nothing "a"
btVarDecl (VarDeclType t s _) = return $ TypeEnv $ M.singleton (Var, s) (Scheme [] t)

btFunDecl :: FunDecl -> TI TypeEnv
btFunDecl (FunDecl s args Nothing _ _) = do
    nvars <- mapM (newTyVar Nothing) args
    ret <- newTyVar Nothing "r"
    let t = foldr1 TypeFun $ nvars ++ [ret]
    return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
btFunDecl (FunDecl s _ (Just t) _ _) = return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)

ti :: SPL -> TypeEnv -> IO ()
ti spl e = do
    (bt, _) <- runTI $ btSPL spl
    case bt of
        Left err -> putStrLn err
        Right env -> do
            (res, _) <- runTI $ tiSPL (stdlib `combine` e `combine` env) spl
            case res of
                Left err -> putStrLn err
                Right e -> putStr $ "Program is correctly typed\n" ++ show e

testEnv :: TypeEnv -> String -> IO ()
testEnv env s = case testP splP s of
    Left e -> print e
    Right (c, s) -> if not $ null c then putStrLn ("Did not finish parsing" ++ " " ++ map fst3 c) else ti s env

test :: String -> IO ()
test = testEnv emptyEnv
