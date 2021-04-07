{-# LANGUAGE TupleSections #-}

module Binding where

import Parser
import Grammar
import Control.Monad.Except ( MonadError(throwError) )
import Types
import Data.Maybe ( fromMaybe, fromJust, maybeToList )
import Control.Monad (foldM, forM)
import qualified Data.Map as M
import Debug.Trace ( trace )
import Data.Graph ( stronglyConnCompR, SCC, Forest )
import Data.Tuple ( swap )

components :: SPL -> [SCC (Decl, (Kind, String), [(Kind, String)])]
components (SPL ds) = stronglyConnCompR $ map (\d -> let (a, b) = ctDecl d in (d, a, b)) ds

ctDecl :: Decl -> ((Kind, String), [(Kind, String)])
ctDecl (DeclVarDecl (VarDecl _ n e)) = ((Var, n), ctExp [] e)
ctDecl (DeclFunDecl (FunDecl n args _ vars stmts)) = ((Fun, n), ctStmts args stmts ++ concatMap (\(VarDecl _ _ e) -> ctExp args e) vars)

ctStmts :: [String] -> [Stmt] -> [(Kind, String)]
ctStmts args = concatMap (ctStmt args)

ctStmt :: [String] -> Stmt -> [(Kind, String)]
ctStmt args (StmtIf e ss1 ss2) = ctExp args e ++ ctStmts args ss1 ++ ctStmts args (fromMaybe [] ss2)
ctStmt args (StmtWhile e ss) = ctExp args e ++ ctStmts args ss
ctStmt args (StmtField n _ e)
    | n `elem` args = []
    | otherwise = [(Var, n)]
ctStmt args (StmtFunCall (FunCall n es)) = [(Fun, n)]
ctStmt args (StmtReturn Nothing) = []
ctStmt args (StmtReturn (Just e)) = ctExp args e

ctExp :: [String] -> Exp -> [(Kind, String)]
ctExp args (Exp _ e1 e2) = ctExp args e1 ++ ctExp args e2
ctExp args (ExpOp1 _ e) = ctExp args e
ctExp args (ExpTuple (e1, e2)) = ctExp args e1 ++ ctExp args e2
ctExp args (ExpBrackets e) = ctExp args e
ctExp args (ExpField n _)
    | n `elem` args = []
    | otherwise = [(Var, n)]
ctExp args (ExpFunCall (FunCall n es)) = [(Fun, n)]
ctExp _ _ = []

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

tix :: Forest Decl
tix = undefined

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
        Left err -> putStrLn $ "\x1b[31mTypeError:\x1b[0ct " ++ err ++ "\n"
        Right env -> putStr $ "\x1b[32mProgract is correctly typed\x1b[0m\n" ++ show env ++ "\n"

testEnv :: TypeEnv -> String -> IO ()
testEnv env s = case testP splP s of
    Left e -> putStrLn $ "\x1b[31mParseError:\x1b[0m" ++ show e ++ "\n"
    Right (c, s) -> if not $ null c then putStrLn ("\x1b[31mParseError:\x1b[0ct Did not finish parsing \"\x1b[3m" ++ map fst3 c ++ "\x1b[0m\"\n") else tiResult s env

check :: String -> IO ()
check = testEnv emptyEnv
