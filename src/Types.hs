{-# LANGUAGE TupleSections #-}

module Types where

import Control.Monad.Except ( zipWithM, runExceptT, MonadError(throwError), ExceptT )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( MonadState(put, get), StateT(runStateT) )
import Data.Maybe ( fromMaybe, listToMaybe, isJust, isNothing )
import Data.List ( group, sort )
import qualified Data.Map as M
import qualified Data.Set as S
import Grammar
import Debug.Trace ( trace )

data Scheme = Scheme [String] Type deriving (Show)
type Subst = M.Map String Type
data Kind = Var | Fun deriving (Eq, Ord)

instance Show Kind where
    show Var = "\x1b[35mVar\x1b[0m"
    show Fun = "\x1b[1m\x1b[35mFun\x1b[0m"

class Types a where
  ftv :: a -> S.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TypeID _ n) = S.singleton n
  ftv (TypeBasic _)  = S.empty
  ftv (TypeList t) = ftv t
  ftv (TypeFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TypeTuple t1 t2)  = ftv t1 `S.union` ftv t2
  ftv Void = S.empty

  apply s (TypeID c n) = case M.lookup n s of
    Nothing -> TypeID c n
    Just t -> checkConditions t
        where
            checkConditions (TypeID c1 n) = TypeID (composeConditions c c1) n
            checkConditions t = t
  apply _ (TypeBasic t) = TypeBasic t
  apply s (TypeList t) = TypeList (apply s t)
  apply s (TypeFun t1 t2) = TypeFun (apply s t1) (apply s t2)
  apply s (TypeTuple t1 t2) = TypeTuple (apply s t1) (apply s t2)
  apply _ Void = Void

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv l = foldr (S.union . ftv) S.empty l

nullSubst :: Subst
nullSubst = M.empty

combine :: TypeEnv -> TypeEnv -> TypeEnv
combine (TypeEnv env1) (TypeEnv env2) = TypeEnv $ env1 `M.union` env2

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (apply s1) s2 `M.union` s1

newtype TypeEnv = TypeEnv (M.Map (Kind, String) Scheme)

instance Eq TypeEnv where
    e1 == e2 = show e1 == show e2

conditions :: Type -> [(String, Condition)]
conditions (TypeBasic _) = []
conditions (TypeTuple t1 t2) = conditions t1 ++ conditions t2
conditions (TypeList t) = conditions t
conditions (TypeID Nothing _) = []
conditions (TypeID (Just c) n) = [(n, c)]
conditions (TypeFun t1 t2) = conditions t1 ++ conditions t2
conditions Void = []

showConditions :: M.Map String String -> [(String, Condition)] -> String
showConditions m [] = ""
showConditions m [(s, c)] = "\x1b[34m" ++ show c ++ "\x1b[0m \x1b[36m" ++ fromMaybe s (M.lookup s m) ++ "\x1b[0m => "
showConditions m ((s, c):cs) = "\x1b[34m" ++ show c ++ "\x1b[0m \x1b[36m" ++ fromMaybe s (M.lookup s m) ++ "\x1b[0m, " ++ showConditions m cs 

varStrings :: Type -> [String]
varStrings (TypeID _ s) = [s]
varStrings (TypeTuple t1 t2) = varStrings t1 ++ varStrings t2
varStrings (TypeList t) = varStrings t
varStrings (TypeFun t1 t2) = varStrings t1 ++ varStrings t2
varStrings _ = []

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

varsMap :: Type -> M.Map String String
varsMap t = M.fromList $ zip (reverse $ removeDuplicates $ reverse $ varStrings t) (map (: []) ['a' .. 'z'])

showType :: M.Map String String -> Type -> String
showType m (TypeBasic b) = show b
showType m (TypeTuple t1 t2) = "(" ++ showType m t1 ++ ", " ++ showType m t2 ++ ")"
showType m (TypeList t) = "[" ++ showType m t ++ "]"
showType m (TypeID _ s) = "\x1b[36m" ++ fromMaybe s (M.lookup s m) ++ "\x1b[0m"
showType m (TypeFun t1 t2) = showType m t1 ++ " -> " ++ showType m t2
showType m Void = "\x1b[34mVoid\x1b[0m"

instance Show TypeEnv where
    show env = help funs ++ help vars
        where
            help (TypeEnv e) = unlines $ map (\((k, n), Scheme _ t) -> show k ++ " " ++ "\x1b[33m" ++ n ++ "\x1b[0m" ++ " :: " ++ showConditions (varsMap t) (map head $ group $ sort $ conditions t) ++ showType (varsMap t) t) $ filter (\((_, n), _) -> n `notElem` ["print", "isEmpty"]) $ M.toList e
            (funs, vars) = woFun env

emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

remove :: TypeEnv -> Kind -> String -> TypeEnv
remove (TypeEnv env) k var = TypeEnv $ M.delete (k, var) env

removeAll :: TypeEnv -> Kind -> [String] -> TypeEnv
removeAll env _ [] = env
removeAll env k (v:vars) = removeAll (remove env k v) k vars

woFun :: TypeEnv -> (TypeEnv, TypeEnv)
woFun (TypeEnv env) = (help Fun, help Var)
    where
        help :: Kind -> TypeEnv
        help k = TypeEnv $ M.fromList $ filter (\((k1, _), _) -> k1 == k) (M.toList env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `S.difference` S.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

data TIEnv = TIEnv { }
data TIState = TIState { tiSupply :: Int, tiSubst :: Subst }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    where initTIEnv = TIEnv { }
          initTIState = TIState { tiSupply = 1, tiSubst = M.empty }

newTyVar :: Maybe Condition -> String -> TI Type
newTyVar c prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    return (TypeID c ("a" ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (newTyVar Nothing) vars
    let s = M.fromList (zip vars nvars)
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList (ftv t `S.difference` ftv env)

mgu :: Type -> Type -> TI Subst
mgu (TypeFun l1 r1) (TypeFun l2 r2) = do
    s1 <- mgu l1 l2
    s2 <- mgu (apply s1 r1) (apply s1 r2)
    return $ s2 `composeSubst` s1
mgu (TypeList t1) (TypeList t2) = mgu t1 t2
mgu (TypeTuple l1 r1) (TypeTuple l2 r2) = do
    s1 <- mgu l1 l2
    s2 <- mgu (apply s1 r1) (apply s1 r2)
    return $ s2 `composeSubst` s1
mgu (TypeID c1 u1) (TypeID c2 u2) = return $ M.singleton u1 (TypeID (composeConditions c1 c2) u2)
mgu (TypeID c u) t = varBind u c t
mgu t (TypeID c u) = varBind u c t
mgu (TypeBasic t1) (TypeBasic t2)
    | t1 == t2 = return nullSubst
    | otherwise = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
mgu Void Void = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ showType (varsMap t1) t1 ++ " vs. " ++ showType (varsMap t2) t2

condition :: Type -> String -> (Maybe Condition, Bool)
condition (TypeID c n) s = (c, n == s)
condition _ _ = (Nothing, False)

composeConditions :: Maybe Condition -> Maybe Condition -> Maybe Condition
composeConditions Nothing c = c
composeConditions (Just Ord) _ = Just Ord
composeConditions c Nothing = c
composeConditions _ c = c
     
varBind :: String -> Maybe Condition -> Type -> TI Subst
varBind u (Just Eq) t = return $ M.singleton u t
varBind u (Just Ord) t
    | isOrd t = return $ M.singleton u t
    | otherwise = throwError $ showType (varsMap t) t ++ " is not Ord"
    where
        isOrd (TypeBasic IntType) = True
        isOrd (TypeBasic CharType) = True
        isOrd _ = False
varBind u c t
    | u `S.member` ftv t = throwError $ "occur check fails: \x1b[36m" ++ u ++ "\x1b[0m vs. " ++ showType (varsMap t) t
    | otherwise = return $ M.singleton u t

tiSPL :: TypeEnv -> SPL -> TI (Subst, TypeEnv, SPL)
tiSPL = tiDecls

tiDecl :: TypeEnv -> Decl -> TI (Subst, TypeEnv, Decl)
tiDecl env (DeclVarDecl v) = fmap DeclVarDecl <$> tiVarDecl env v
tiDecl e (DeclFunDecl f) = do
    (s, _, e, d) <- tiFunDecl e f
    return (s, e, DeclFunDecl d)

tiVarDecl :: TypeEnv -> VarDecl -> TI (Subst, TypeEnv, VarDecl)
tiVarDecl env (VarDecl Nothing s ex) = do
    (s1, t1, ex') <- tiExp' True env ex
    let env1@(TypeEnv e) = apply s1 env
    let d = VarDecl (Just t1) s ex'
    let TypeEnv env2 = remove env1 Var s
    return (s1, TypeEnv (M.insert (Var, s) (Scheme [] t1) env2), d)
tiVarDecl env (VarDecl (Just t) s e) = do
    (s1, t1, e') <- tiExp' True env e
    s2 <- mgu t1 t
    let cs1 = s2 `composeSubst` s1
    let TypeEnv env1 = remove env Var s
    let env2 = TypeEnv (M.insert (Var, s) (Scheme [] t1) env1)
    return (cs1, apply cs1 env2, VarDecl (Just t1) s e')

tiDecls :: TypeEnv -> [Decl] -> TI (Subst, TypeEnv, [Decl])
tiDecls env [] = return (nullSubst, env, [])
tiDecls env (d:ds) = do
    (s1, env1, d1) <- tiDecl env d
    (s2, env2, d2) <- tiDecls env1 ds
    return (s2 `composeSubst` s1, env2, d1:d2)

tiVarDecls :: TypeEnv -> [VarDecl] -> TI (Subst, TypeEnv, [VarDecl])
tiVarDecls env [] = return (nullSubst, env, [])
tiVarDecls env (v:vs) = do
    (s1, env1, d1) <- tiVarDecl env v
    (s2, env2, d2) <- tiVarDecls env1 vs
    return (s2 `composeSubst` s1, env2, d1:d2)

checkReturn :: TypeEnv -> Type -> Stmt -> TI Subst
checkReturn _ t (StmtReturn Nothing) = mgu t Void
checkReturn env t (StmtReturn (Just e)) = do
    (s1, t1, _) <- tiExp env e
    s2 <- mgu t t1
    return $ s2 `composeSubst` s1

getReturns :: Stmt -> [Stmt]
getReturns (StmtIf _ s1 s2) = allReturns s1 ++ allReturns (fromMaybe [] s2)
getReturns (StmtWhile _ s) = allReturns s
getReturns StmtField {} = []
getReturns StmtFunCall {} = []
getReturns r = [r]

allReturns :: [Stmt] -> [Stmt]
allReturns = concatMap getReturns

hasReturn :: Stmt -> Bool
hasReturn (StmtIf _ ss1 ss2) = correctReturn ss1 && correctReturn (fromMaybe [] ss2)
hasReturn (StmtWhile _ ss) = correctReturn ss
hasReturn StmtReturn {} = True
hasReturn _ = False

correctReturn :: [Stmt] -> Bool
correctReturn = any hasReturn

returnType :: TypeEnv -> Stmt -> TI (Subst, Type)
returnType _ (StmtReturn Nothing) = return (nullSubst, Void)
returnType env (StmtReturn (Just e)) = do
    (s, t, _) <- tiExp env e
    return (s, t)

tiFunDecl :: TypeEnv -> FunDecl -> TI (Subst, Type, TypeEnv, FunDecl)
tiFunDecl env (FunDecl n args (Just t) vars stmts)
    | l1 /= l2 = throwError $ show n ++ " got " ++ show l1  ++ " arguments, but expected " ++ show l2 ++ " arguments"
    | otherwise = do
        (s1, t1, env1, FunDecl _ _ _ vars' stmts') <- tiFunDecl env (FunDecl n args Nothing vars stmts)
        s2 <- mgu t1 t
        let t2 = apply s2 t
        let env2 = remove env1 Fun n
        let env3 = env2 `combine` TypeEnv (M.singleton (Fun, n) (generalize env2 t2))
        return (s2 `composeSubst` s1, t2, env3, FunDecl n args (Just t2) vars' stmts')
    where
        l1 = length (funTypeToList t) - 1
        l2 = length args
tiFunDecl env@(TypeEnv envt) (FunDecl n args Nothing vars stmts) = case M.lookup (Fun, n) envt of
    Nothing -> throwError $ "function " ++ n ++ " was not found in the environment, while it should be present."
    Just _ -> do
        tvs <- mapM (newTyVar Nothing) args
        let env1 = remove env Fun n
        let TypeEnv env2 = removeAll env Var args
        let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme [] t)) args tvs
        let env3 = TypeEnv $ env2 `M.union` argsTvMap
        (s1, env4, vars') <- tiVarDecls env3 vars
        (s2, stmts') <- tiStmts env4 stmts
        let cs1 = s2 `composeSubst` s1
        let returns = allReturns stmts
        let x = FunDecl
        case listToMaybe returns of
            Nothing -> do
                let t = foldr1 TypeFun (apply cs1 tvs ++ [Void])
                let env5 = env1 `combine` TypeEnv (M.singleton (Fun, n) (Scheme [] t))
                return (cs1, t, apply cs1 env5, FunDecl n args (Just t) vars' stmts')
            Just r -> do
                if not $ correctReturn stmts then throwError "Not every path has a return statment" else do
                (s3, t2) <- returnType (apply cs1 env4) r
                let cs2 = s3 `composeSubst` cs1
                ss <- mapM (checkReturn (apply cs2 env4) t2) returns
                let s4 = foldr1 composeSubst ss
                let cs3 = s4 `composeSubst` cs2
                let t = foldr1 TypeFun $ apply cs3 (tvs ++ [t2])
                let env5 = env1 `combine` TypeEnv (M.singleton (Fun, n) (generalize env1 t))
                return (cs3, t, apply cs3 env5, FunDecl n args (Just t) vars' stmts')

tiStmts :: TypeEnv -> [Stmt] -> TI (Subst, [Stmt])
tiStmts _ [] = return (nullSubst, [])
tiStmts env (s:ss) = do
    (s1, st1) <- tiStmt env s
    (s2, st2) <- tiStmts (apply s1 env) ss
    return (s2 `composeSubst` s1, st1:st2)

tiField :: Type -> Field -> TI (Subst, Type)
tiField t Head = do
    t1 <- newTyVar Nothing "f"
    s <- mgu (TypeList t1) t
    return (s, apply s t1)
tiField t Tail = do
    t1 <- newTyVar Nothing "f"
    s <- mgu (TypeList t1) t
    return (s, apply s $ TypeList t1)
tiField t First = do
    t1 <- newTyVar Nothing "f"
    t2 <- newTyVar Nothing "f"
    s <- mgu (TypeTuple t1 t2) t
    return (s, apply s t1)
tiField t Second = do
    t1 <- newTyVar Nothing "f"
    t2 <- newTyVar Nothing "f"
    s <- mgu (TypeTuple t1 t2) t  
    return (s, apply s t2)

tiFields :: Type -> [Field] -> TI (Subst, Type)
tiFields t [] = return (nullSubst, t)
tiFields t (f:fs) = do
    (s1, t1) <- tiField t f
    (s2, t2) <- tiFields t1 fs
    return (s2 `composeSubst` s1, t2)

tiStmt :: TypeEnv -> Stmt -> TI (Subst, Stmt)
tiStmt env (StmtIf e ss1 ss2) = do
    (s1, t1, e') <- tiExp env e
    s2 <- mgu (TypeBasic BoolType) t1
    let cs1 = s2 `composeSubst` s1
    (s3, ss1') <- tiStmts (apply cs1 env) ss1
    let cs2 = s3 `composeSubst` cs1
    (s4, ss2') <- tiStmts (apply cs2 env) (fromMaybe [] ss2)
    return (s4 `composeSubst` cs2, StmtIf e' ss1' (if isNothing ss2 then Nothing else Just ss2'))
tiStmt env (StmtWhile e ss) = do
    (s1, t1, e') <- tiExp env e
    s2 <- mgu (TypeBasic BoolType) t1
    let cs1 = s2 `composeSubst` s1
    (s3, stmts') <- tiStmts (apply cs1 env) ss
    return (s3 `composeSubst` cs1, StmtWhile e' stmts')
tiStmt e (StmtField n fs ex) = do
    (s1, t1, e') <- tiExp' False e ex
    let TypeEnv env1 = apply s1 e
    case M.lookup (Var, n) env1 of
        Nothing -> throwError $ n ++ " is not defined"
        Just sigma -> do
            t <- instantiate sigma
            (s2, t') <- tiFields t fs
            s3 <- mgu t1 t'
            return (s3 `composeSubst` s2 `composeSubst` s1, StmtField n fs e')
tiStmt env (StmtFunCall f) = do
    (s, _, f) <- tiFunCall env f
    return (s, StmtFunCall f)
tiStmt env (StmtReturn sr) = do
    case sr of
        Nothing -> return (nullSubst, StmtReturn Nothing)
        Just e -> do
            (s, t, e') <- tiExp env e
            return (s, StmtReturn $ Just e')
 
retType :: Type -> Type
retType (TypeFun t1 t2) = retType t2
retType t = t

funType :: Type -> Maybe Type
funType (TypeFun t1 r@(TypeFun t2 t3)) = TypeFun t1 <$> funType r
funType (TypeFun t1 t2) = Just t1
funType t = Nothing

funTypeToList :: Type -> [Type]
funTypeToList (TypeFun t1 t2) = t1 : funTypeToList t2
funTypeToList t = [t]

tiExps :: TypeEnv -> [Exp] -> TI (Subst, [Type], [Exp])
tiExps _ [] = return (nullSubst, [], [])
tiExps env (e:es) = do
    (s1, t1, e1) <- tiExp env e
    (s2, t2, e2) <- tiExps (apply s1 env) es
    let cs1 = s2 `composeSubst` s1
    return (cs1, t1 : t2, e1 : e2)

mguList :: [Type] -> [Type] -> TI Subst
mguList [] _ = return nullSubst
mguList _ [] = return nullSubst
mguList (a:as) (b:bs) = do
    s1 <- mgu a b
    s2 <- mguList as bs
    return $ s2 `composeSubst` s1

tiFunCall :: TypeEnv -> FunCall -> TI (Subst, Type, FunCall)
tiFunCall e@(TypeEnv env) f@(FunCall _ n es) = case M.lookup (Fun, n) env of
    Nothing -> throwError $ "function " ++ n ++ " doesn't exist"
    Just sigma -> do
        t <- instantiate sigma
        case funType t of 
            Nothing -> if null es then return (nullSubst, retType t, FunCall (Just t) n es) else throwError $ "Number of arguments of " ++ show f ++ " does not correspond with its type"
            Just funT -> if length es /= length (funTypeToList funT) then throwError $ show n ++ " got " ++ show (length es)  ++ " arguments, but expected " ++ show (length (funTypeToList funT)) ++ " arguments" else do
                (s1, ts, es') <- tiExps e es
                s2 <- mguList ts (apply s1 $ funTypeToList funT)
                let cs1 = s2 `composeSubst` s1
                let t' = foldr1 TypeFun $ apply cs1 (ts ++ [retType t])
                return (cs1, apply cs1 $ retType t, FunCall (Just t') n es')

tiOp1 :: Op1 -> (Type, Type)
tiOp1 Min = (TypeBasic IntType, TypeBasic IntType)
tiOp1 Not = (TypeBasic BoolType, TypeBasic BoolType)

tiOp2 :: Op2 -> TI (Type, Type, Type)
tiOp2 o
    | o `elem` [Plus, Minus, Product, Division, Modulo] = return (TypeBasic IntType, TypeBasic IntType, TypeBasic IntType)
    | o `elem` [And, Or] = return (TypeBasic BoolType, TypeBasic BoolType, TypeBasic BoolType)
    | o == Cons = newTyVar Nothing "c" >>= (\t -> return (t, TypeList t, TypeList t))
    | o `elem` [Equals, Neq] = newTyVar (Just Eq) "e" >>= (\t -> return (t, t, TypeBasic BoolType))
    | o `elem` [Leq, Geq, Smaller, Greater] = newTyVar (Just Ord) "e" >>= (\t -> return (t, t, TypeBasic BoolType))

tiExp :: TypeEnv -> Exp -> TI (Subst, Type, Exp)
tiExp = tiExp' False

refresh :: Bool -> Type -> TI Type
refresh False t = return t
refresh True t = instantiate $ generalize emptyEnv t

tiExp' :: Bool -> TypeEnv -> Exp -> TI (Subst, Type, Exp)
tiExp' b env (Exp _ o e1 e2) = do
    (t1, t2, t3) <- tiOp2 o
    (s1, t1', e1') <- tiExp' b env e1
    s2 <- mgu t1' (apply s1 t1)
    let cs1 = s2 `composeSubst` s1
    (s3, t2', e2') <- tiExp' b (apply cs1 env) e2
    let cs2 = s3 `composeSubst` cs1
    s4 <- mgu (apply cs2 t2') (apply cs2 t2)
    let cs3 = s4 `composeSubst` cs2
    return (cs3, apply cs3 t3, Exp (Just t1') o e1' e2')
tiExp' b env (ExpOp1 o e) = do
    let (t1, t2) = tiOp1 o
    (s1, t1', e') <- tiExp' b env e
    s2 <- mgu t1 t1'
    return (s2 `composeSubst` s1, t2, ExpOp1 o e')
tiExp' b env (ExpTuple (e1, e2)) = do
    (s1, t1, e1') <- tiExp' b env e1
    (s2, t2, e2') <- tiExp' b (apply s1 env) e2
    return (s2 `composeSubst` s1, TypeTuple t1 t2, ExpTuple (e1', e2'))
tiExp' b env (ExpBrackets e) = tiExp' b env e
tiExp' b (TypeEnv env) e@(ExpField _ n fs) = case M.lookup (Var, n) env of
    Nothing -> throwError $ n ++ " is not defined"
    Just sigma -> do
        t <- instantiate sigma
        t' <- refresh b t
        (\(s, ty) -> (s, ty, ExpField Nothing n fs)) <$> tiFields t' fs
tiExp' _ _ e@(ExpInt _) = return (nullSubst, TypeBasic IntType, e)
tiExp' _ _ e@(ExpBool _) = return (nullSubst, TypeBasic BoolType, e)
tiExp' _ _ e@(ExpChar _) = return (nullSubst, TypeBasic CharType, e)
tiExp' _ env (ExpFunCall f) = fmap ExpFunCall <$> tiFunCall env f
tiExp' _ _ e@ExpEmptyList = do
    t <- newTyVar Nothing "l"
    return (nullSubst, TypeList t, e)
