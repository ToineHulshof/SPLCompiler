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
import Data.Graph ( buildG, Vertex, scc, Edge, Forest )
import Data.Tuple ( swap )

type CallTree = M.Map (Kind, String) Int
type DeclMap = M.Map Int Decl

components :: SPL -> Forest Decl
components spl = (fmap . fmap) (\v -> fromJust $ M.lookup v dm) <$> scc $ buildG (0, M.size ct - 1) e
    where
        (ct, dm) = idMap 0 spl
        e = callTree ct spl

callTree :: CallTree -> SPL -> [Edge]
callTree ct (SPL ds) = concatMap (ctDecl ct) ds

ctDecl :: CallTree -> Decl -> [Edge]
ctDecl ct (DeclVarDecl (VarDecl _ n e)) = map (fromJust $ M.lookup (Var, n) ct,) $ ctExp ct [] e
ctDecl ct (DeclFunDecl (FunDecl n args _ vars stmts)) = map (fromJust $ M.lookup (Fun, n) ct,) (ctStmts ct args stmts ++ concatMap (\(VarDecl _ _ e) -> ctExp ct args e) vars)

ctStmts :: CallTree -> [String] -> [Stmt] -> [Vertex]
ctStmts ct args = concatMap $ ctStmt ct args

ctStmt :: CallTree -> [String] -> Stmt -> [Vertex]
ctStmt ct args (StmtIf e ss1 ss2) = ctExp ct args e ++ ctStmts ct args ss1 ++ ctStmts ct args (fromMaybe [] ss2)
ctStmt ct args (StmtWhile e ss) = ctExp ct args e ++ ctStmts ct args ss
ctStmt ct args (StmtField n _ e)
    | n `elem` args = []
    | otherwise = maybeToList $ M.lookup (Var, n) ct
ctStmt ct args (StmtFunCall (FunCall n es)) = maybeToList $ M.lookup (Fun, n) ct
ctStmt _ _ (StmtReturn Nothing) = []
ctStmt ct args (StmtReturn (Just e)) = ctExp ct args e

ctExp :: CallTree -> [String] -> Exp -> [Vertex]
ctExp ct args (Exp _ e1 e2) = ctExp ct args e1 ++ ctExp ct args e2
ctExp ct args (ExpOp1 _ e) = ctExp ct args e
ctExp ct args (ExpTuple (e1, e2)) = ctExp ct args e1 ++ ctExp ct args e2
ctExp ct args (ExpBrackets e) = ctExp ct args e
ctExp ct args (ExpField n _)
    | n `elem` args = []
    | otherwise = maybeToList $ M.lookup (Var, n) ct
ctExp ct args (ExpFunCall (FunCall n es)) = maybeToList $ M.lookup (Fun, n) ct
ctExp _ _ _ = []

idMap :: Int -> SPL -> (CallTree, DeclMap)
idMap i (SPL []) = (M.empty, M.empty)
idMap i (SPL (d:ds)) = help i d `concatTuple` idMap (i + 1) (SPL ds)
    where
        help :: Int -> Decl -> (CallTree, DeclMap)
        help i d@(DeclVarDecl (VarDecl _ n _)) = (M.singleton (Var, n) i, M.singleton i d)
        help i d@(DeclFunDecl (FunDecl n _ _ _ _)) = (M.singleton (Fun, n) i, M.singleton i d)
        concatTuple (ct1, dm1) (ct2, dm2) = (ct1 `M.union` ct2, dm1 `M.union` dm2)

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
