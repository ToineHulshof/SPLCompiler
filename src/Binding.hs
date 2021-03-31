module Binding where

import Parser
import Grammar
import Types
import Control.Monad (foldM, forM)
import qualified Data.Map as M
import Debug.Trace ( trace )

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

hasEffect :: (String, Type) -> Bool
hasEffect (s, TypeID _ n) = n /= s
hasEffect _ = True

effect :: Subst -> Bool
effect s = any hasEffect $ M.toList s

finalEnv :: SPL -> TypeEnv -> TI TypeEnv
finalEnv spl env = do
    (s, env') <- tiSPL env spl
    if env == env' then return env' else finalEnv spl env'

ti :: SPL -> TypeEnv -> TI TypeEnv
ti spl e = do
    bt <- btSPL spl
    let env = stdlib `combine` e `combine` bt
    (_, env1) <- tiSPL env spl
    (_, env2) <- tiSPL env1 spl
    return env2
    -- finalEnv spl $ stdlib `combine` e `combine` bt

tiResult :: SPL -> TypeEnv -> IO ()
tiResult spl e = do
    (bt, _) <- runTI $ ti spl e
    case bt of
        Left err -> putStrLn $ "\x1b[31mError:\x1b[0m " ++ err ++ "\n"
        Right env -> putStr $ "\x1b[32mProgram is correctly typed\x1b[0m\n" ++ show env ++ "\n"

testEnv :: TypeEnv -> String -> IO ()
testEnv env s = case testP splP s of
    Left e -> print e
    Right (c, s) -> if not $ null c then putStrLn ("Did not finish parsing" ++ " " ++ map fst3 c) else tiResult s env

check :: String -> IO ()
check = testEnv emptyEnv
