{-# LANGUAGE TupleSections #-}

module Binding where

import Parser ( p, testP )
import Grammar
import Errors
import Control.Monad.Except ( MonadError(throwError) )
import Types
import Data.Maybe ( fromMaybe, fromJust, maybeToList )
import Data.Array
import Control.Monad (foldM, forM)
import qualified Data.Map as M
import Debug.Trace ( trace )
import Data.Graph ( stronglyConnCompR, SCC(..) )
import Data.Tuple ( swap )

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

components :: SPL -> [SCC Decl]
components ds = map (fst3 <$>) $ stronglyConnCompR $ map (\d -> let (a, b) = ctDecl d in (d, a, b)) ds

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
    ((Fun, "print"), Scheme ["t"] $ TypeFun (TypeID Nothing "t") Void),
    ((Fun, "isEmpty"), Scheme ["t"] $ TypeFun (TypeArray $ TypeID Nothing "t") (TypeBasic BoolType))
    ]

btSPL :: TypeEnv -> SPL -> TI TypeEnv
btSPL env [] = return env
btSPL env (d:ds) = do
    env1 <- btDecl env d
    env2 <- btSPL env1 ds
    return (env1 `combine` env2)

btDecl :: TypeEnv -> Decl -> TI TypeEnv
btDecl _ (DeclVarDecl v) = btVarDecl v
btDecl env (DeclFunDecl f) = btFunDecl env f

btVarDecl :: VarDecl -> TI TypeEnv
btVarDecl (VarDecl Nothing s _) = TypeEnv . M.singleton (Var, s) . Scheme [] <$> newTyVar Nothing "a"
btVarDecl (VarDecl (Just t) s _) = return $ TypeEnv $ M.singleton (Var, s) (Scheme [] t)

btFunDecl :: TypeEnv -> FunDecl -> TI TypeEnv
btFunDecl env (FunDecl s args Nothing _ _) = do
    nvars <- mapM (newTyVar Nothing) args
    ret <- newTyVar Nothing "r"
    let t = foldr1 TypeFun $ nvars ++ [ret]
    return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
btFunDecl (TypeEnv env) (FunDecl s _ (Just t) _ _) = return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)

hasEffect :: (String, Type) -> Bool
hasEffect (s, TypeID _ n) = n /= s
hasEffect _ = True

effect :: Subst -> Bool
effect s = any hasEffect $ M.toList s

repeatDecl :: Int -> TypeEnv -> [Decl] -> TI TypeEnv
repeatDecl 0 env _ = return env
repeatDecl i env ds = do
    env1 <- snd <$> tiDecls env ds
    repeatDecl (i - 1) env1 ds

tiComp :: TypeEnv -> SCC Decl -> TI TypeEnv
tiComp env (AcyclicSCC d) = snd <$> tiDecl env d
tiComp env (CyclicSCC ds) = repeatDecl (length ds) env ds

tiComps :: TypeEnv -> [SCC Decl] -> TI TypeEnv
tiComps = foldM tiComp

varCycle :: SCC Decl -> Bool
varCycle (AcyclicSCC _) = False
varCycle (CyclicSCC ds) = any isVarDecl ds where
    isVarDecl DeclFunDecl {} = False
    isVarDecl DeclVarDecl {} = True

ti' :: SPL -> TypeEnv -> TI TypeEnv
ti' spl e = do
    bt <- btSPL emptyEnv spl
    let comps = components spl
    if any varCycle comps then throwError "Stuk" else
        tiComps (stdlib `combine` e `combine` bt) comps

tiResult :: SPL -> TypeEnv -> IO ()
tiResult spl e = do
    (bt, _) <- runTI $ ti' spl e
    case bt of
        Left err -> putStrLn $ "\x1b[31mTypeError:\x1b[0m " ++ err
        Right env -> putStr $ "\x1b[32mProgram is correctly typed\x1b[0m\n" ++ show env

testEnv :: TypeEnv -> FilePath -> String -> IO ()
testEnv env f s
    | not $ null e = print (Errors f (listArray (1, length l) l) e)
    | otherwise = case r of
        Nothing -> print e
        Just (_, spl) -> tiResult spl env
    where
        (e, r) = p s
        l = lines s

checkFile :: FilePath -> String -> IO ()
checkFile = testEnv emptyEnv

check :: String -> IO ()
check = checkFile "<interactive>"
