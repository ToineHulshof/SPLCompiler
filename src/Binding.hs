{-# LANGUAGE TupleSections #-}

module Binding where

import Parser ( splP, testP, p )
import Grammar
import Control.Monad.Writer ( MonadWriter(tell) )
import Codegen ( genCode )
import Types
import Errors
import Data.Maybe ( fromMaybe, fromJust, maybeToList )
import Control.Monad (foldM, forM)
import qualified Data.Map as M
import Debug.Trace ( trace )
import Data.Graph ( stronglyConnCompR, SCC(..) )
import Data.Tuple ( swap )
import Data.Array

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
ctStmt args (StmtFunCall (FunCall _ n es)) = (Fun, n) : concatMap (ctExp args) es
ctStmt args (StmtReturn Nothing) = []
ctStmt args (StmtReturn (Just e)) = ctExp args e

ctExp :: [String] -> Exp -> [(Kind, String)]
ctExp args (Exp _ _ e1 e2) = ctExp args e1 ++ ctExp args e2
ctExp args (ExpOp1 _ e) = ctExp args e
ctExp args (ExpTuple (e1, e2)) = ctExp args e1 ++ ctExp args e2
ctExp args (ExpBrackets e) = ctExp args e
ctExp args (ExpField _ n _)
    | n `elem` args = []
    | otherwise = [(Var, n)]
ctExp args (ExpFunCall (FunCall _ n es)) = (Fun, n) : concatMap (ctExp args) es
ctExp _ _ = []

stdlib :: TypeEnv
stdlib = TypeEnv $ M.fromList [
    ((Fun, "print"), Scheme ["t"] $ TypeFun (TypeID Nothing "t") Void),
    ((Fun, "isEmpty"), Scheme ["t"] $ TypeFun (TypeList $ TypeID Nothing "t") (TypeBasic BoolType))
    ]

btSPL :: TypeEnv -> SPL -> TI TypeEnv
btSPL env [] = return env
btSPL env (d:ds) = do
    env1 <- btDecl env d
    btSPL (env `combine` env1) ds

btDecl :: TypeEnv -> Decl -> TI TypeEnv
btDecl _ (DeclVarDecl v) = btVarDecl v
btDecl env (DeclFunDecl f) = btFunDecl env f

btVarDecl :: VarDecl -> TI TypeEnv
btVarDecl (VarDecl Nothing s _) = TypeEnv . M.singleton (Var, s) . Scheme [] <$> newTyVar Nothing "a"
btVarDecl (VarDecl (Just t) s _) = return $ TypeEnv $ M.singleton (Var, s) (Scheme [] t)

btFunDecl :: TypeEnv -> FunDecl -> TI TypeEnv
btFunDecl (TypeEnv env) (FunDecl s args Nothing _ _) = do
    case M.lookup (Fun, s) env of
        Nothing -> do
            nvars <- mapM (newTyVar Nothing) args
            ret <- newTyVar Nothing "r"
            let t = foldr1 TypeFun $ nvars ++ [ret]
            return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
        Just _ -> tell [Error TypeError ("Function " ++ s ++ " is already defined.") x] >> return (TypeEnv env)
btFunDecl (TypeEnv env) (FunDecl s _ (Just t) _ _) = do
    case M.lookup (Fun, s) env of
        Nothing -> return $ TypeEnv $ M.singleton (Fun, s) (Scheme [] t)
        Just _ -> tell [Error TypeError ("Function " ++ s ++ " is already defined.") x] >> return (TypeEnv env)
    
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
tiComps env (d:ds) = do
    (env1, ds1) <- tiComp env d
    (env2, ds2) <- tiComps env1 ds
    return (env2 `combine` env1, ds1 ++ ds2)

varCycle :: SCC Decl -> Bool
varCycle (AcyclicSCC _) = False
varCycle (CyclicSCC ds) = any isVarDecl ds where
    isVarDecl DeclFunDecl {} = False
    isVarDecl DeclVarDecl {} = True

ti' :: SPL -> TypeEnv -> TI (TypeEnv, SPL)
ti' spl e = do
    bt <- btSPL emptyEnv spl
    let comps = components spl
    if any varCycle comps then tell [Error TypeError "Cycle found in global variables" x] >> return (e, spl) else
        tiComps (stdlib `combine` e `combine` bt) comps

tiResult :: Bool -> String -> Maybe FilePath -> SPL -> TypeEnv -> IO ()
tiResult llvm s f spl e = do
    (((env, spl'), e), _) <- runTI $ ti' spl e
    if null e
        then case f of
            Nothing -> putStr $ "\x1b[32mProgram is correctly typed\x1b[0m\n" ++ show env ++ "\n"
            Just filePath -> if containsMain spl' then genCode llvm filePath spl' else putStrLn "\x1b[31mNo main function\x1b[0m"
        else print (Errors (fromMaybe "<interactive>" f) (listArray (1, length l) l) (removeDuplicates e))
    where
        l = lines s
-- tiResult :: Bool -> Maybe FilePath -> SPL -> TypeEnv -> IO ()
-- tiResult llvm f spl e = do
--     (bt, _) <- runTI $ ti' spl e
--     case bt of
--         Left err -> putStrLn $ "\x1b[31mTypeError:\x1b[0m " ++ err ++ "\n"
--         Right (env, spl') -> case f of
--             Nothing -> putStr $ "\x1b[32mProgram is correctly typed\x1b[0m\n" ++ show env ++ "\n"
--             Just filePath -> if containsMain spl' then genCode llvm filePath spl' else putStrLn "\x1b[31mNo main function\x1b[0m"

containsMain :: SPL -> Bool
containsMain = any isMain
    where 
        isMain :: Decl -> Bool
        isMain (DeclFunDecl (FunDecl "main" _ _ _ _)) = True
        isMain _ = False

testEnv :: Bool -> Maybe FilePath -> TypeEnv -> String -> IO ()
testEnv llvm f env s
    | not $ null e = print (Errors (fromMaybe "<interactive>" f) (listArray (1, length l) l) e)
    | otherwise = case r of
        Nothing -> print e
        Just (_, spl) -> tiResult llvm s f spl env
    where
        (e, r) = p s
        l = lines s

check :: String -> IO ()
check = testEnv False Nothing emptyEnv

compile :: Bool -> FilePath -> String -> IO ()
compile llvm f = testEnv llvm (Just f) emptyEnv
