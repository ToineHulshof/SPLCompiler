module Binding where

import Parser
import Grammar
import Control.Monad.Except ( MonadError(throwError) )
import Types
import Data.Maybe ( fromMaybe )
import Control.Monad (foldM, forM)
import qualified Data.Map as M
import Debug.Trace ( trace )
import Data.Graph

graph :: SPL -> Graph
graph spl = buildG (0, 0) []
    where
        m = idMap 0 spl

idMap :: Int -> SPL -> M.Map (Kind, String) Int
idMap i (SPL []) = M.empty
idMap i (SPL (d:ds)) = help i d `M.union` idMap (i + 1) (SPL ds)
    where
        help :: Int -> Decl -> M.Map (Kind, String) Int
        help i (DeclVarDecl (VarDecl _ n _)) = M.singleton (Var, n) i
        help i (DeclFunDecl (FunDecl n _ _ _ _)) = M.singleton (Fun, n) i

stdlib :: TypeEnv
stdlib = TypeEnv $ M.fromList [
    ((Fun, "print"), TypeFun (TypeID Nothing "t") Void),
    ((Fun, "isEmpty"), TypeFun (TypeArray $ TypeID Nothing "t") (TypeBasic BoolType))
    ]

btSPL :: TypeEnv -> SPL -> TI TypeEnv
btSPL env (SPL []) = return env
btSPL env (SPL (d:ds)) = do
    env1 <- btDecl env d
    env2 <- btSPL env1 (SPL ds)
    return (env1 `combine` env2)

btDecl :: TypeEnv -> Decl -> TI TypeEnv
btDecl _ (DeclVarDecl v) = btVarDecl v
btDecl env (DeclFunDecl f) = btFunDecl env f

btVarDecl :: VarDecl -> TI TypeEnv
btVarDecl (VarDecl Nothing s _) = TypeEnv . M.singleton (Var, s) <$> newTyVar Nothing "a"
btVarDecl (VarDecl (Just t) s _) = return $ TypeEnv $ M.singleton (Var, s) t

btFunDecl :: TypeEnv -> FunDecl -> TI TypeEnv
btFunDecl env (FunDecl s args Nothing _ _) = do
    nvars <- mapM (newTyVar Nothing) args
    ret <- newTyVar Nothing "r"
    let t = foldr1 TypeFun $ nvars ++ [ret]
    return $ TypeEnv $ M.singleton (Fun, s) t
btFunDecl (TypeEnv env) (FunDecl s _ (Just t) _ _) = return $ TypeEnv $ M.singleton (Fun, s) t

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
    bt <- btSPL emptyEnv spl
    -- let env = stdlib `combine` e `combine` bt
    -- (_, env1) <- tiSPL env spl
    -- (_, env2) <- tiSPL env1 spl
    -- (_, env3) <- tiSPL env2 spl
    -- return env3
    finalEnv spl $ stdlib `combine` e `combine` bt

tiResult :: SPL -> TypeEnv -> IO ()
tiResult spl e = do
    (bt, _) <- runTI $ ti spl e
    case bt of
        Left err -> putStrLn $ "\x1b[31mTypeError:\x1b[0m " ++ err ++ "\n"
        Right env -> putStr $ "\x1b[32mProgram is correctly typed\x1b[0m\n" ++ show env ++ "\n"

testEnv :: TypeEnv -> String -> IO ()
testEnv env s = case testP splP s of
    Left e -> putStrLn $ "\x1b[31mParseError:\x1b[0m" ++ show e ++ "\n"
    Right (c, s) -> if not $ null c then putStrLn ("\x1b[31mParseError:\x1b[0m Did not finish parsing \"\x1b[3m" ++ map fst3 c ++ "\x1b[0m\"\n") else tiResult s env

check :: String -> IO ()
check = testEnv emptyEnv
