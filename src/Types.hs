module Types where

import Control.Monad.Except ( zipWithM, foldM, runExceptT, MonadError(throwError), ExceptT )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( MonadState(put, get), StateT(runStateT) )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.List ( group, sort )
import qualified Data.Map as M
import qualified Data.Set as S
import Grammar
import Debug.Trace ( trace )

data Scheme = Scheme [String] Type deriving (Show)
type Subst = M.Map String Type
data Kind = Var | Fun deriving (Eq, Ord, Show)

class Types a where
  ftv :: a -> S.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TypeID _ n) = S.singleton n
  ftv (TypeBasic _)  = S.empty
  ftv (TypeArray t) = ftv t
  ftv (TypeFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TypeTuple t1 t2)  = ftv t1 `S.union` ftv t2
  ftv Void = S.empty

  apply s (TypeID c n) = case M.lookup n s of
    Nothing -> TypeID c n
    Just t -> t
  apply _ (TypeBasic t) = TypeBasic t
  apply s (TypeArray t) = TypeArray (apply s t)
  apply s (TypeFun t1 t2) = TypeFun (apply s t1) (apply s t2)
  apply s (TypeTuple t1 t2) = TypeTuple (apply s t1) (apply s t2)
  apply _ Void = Void

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `S.intersection` S.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

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

conditions :: Type -> [(String, Condition)]
conditions (TypeBasic _) = []
conditions (TypeTuple t1 t2) = conditions t1 ++ conditions t2
conditions (TypeArray t) = conditions t
conditions (TypeID Nothing _) = []
conditions (TypeID (Just c) n) = [(n, c)]
conditions (TypeFun t1 t2) = conditions t1 ++ conditions t2
conditions Void = []

showConditions :: [(String, Condition)] -> String
showConditions [] = ""
showConditions [(s, c)] = show c ++ " " ++ s ++ " => "
showConditions ((s, c):cs) = show c ++ " " ++ s ++ ", " ++ showConditions cs 

instance Show TypeEnv where
    show env = help funs ++ "\n" ++ help vars
        where
            help (TypeEnv e) = unlines $ map (\((k, n), Scheme c t) -> show k ++ " " ++ n ++ " :: " ++ showConditions (map head $ group $ sort $ conditions t) ++ show t) $ M.toList e
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
    apply s env = fun `combine` TypeEnv (M.map (apply s) var)
        where (fun, TypeEnv var) = woFun env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = S.toList (ftv t `S.intersection` ftv env)

data TIEnv = TIEnv { }
data TIState = TIState { tiSupply :: Int, tiSubst :: Subst }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    where initTIEnv = TIEnv { }
          initTIState = TIState { tiSupply = 0, tiSubst = M.empty }

newTyVar :: Maybe Condition -> String -> TI Type
newTyVar c prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    return (TypeID c (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (newTyVar Nothing) vars
    let s = M.fromList (zip vars nvars)
    return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (TypeFun l1 r1) (TypeFun l2 r2) = do
    s1 <- mgu l1 l2
    s2 <- mgu (apply s1 r1) (apply s1 r2)
    return $ s1 `composeSubst` s2
mgu (TypeArray t1) (TypeArray t2) = mgu t1 t2
mgu (TypeTuple l1 r1) (TypeTuple l2 r2) = do
    s1 <- mgu l1 l2
    s2 <- mgu (apply s1 r1) (apply s1 r2)
    return $ s1 `composeSubst` s2
mgu (TypeID c u) t = varBind u c t
mgu t (TypeID c u) = varBind u c t
mgu (TypeBasic t1) (TypeBasic t2)
    | t1 == t2 = return nullSubst
    | otherwise = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
mgu Void Void = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

condition :: Type -> String -> (Maybe Condition, Bool)
condition (TypeID c n) s = (c, n == s)
condition _ _ = (Nothing, False)

composeConditions :: Maybe Condition -> Maybe Condition -> Maybe Condition
composeConditions Nothing c = c
composeConditions (Just Ord) _ = Just Ord
composeConditions c Nothing = c
composeConditions _ c = c

varBind :: String -> Maybe Condition -> Type -> TI Subst
varBind u c t
    | b = return $ M.singleton u (TypeID (composeConditions c c1) u)
    | u `S.member` ftv t = trace (show u ++ ", " ++ show c ++ ", " ++ show t) throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
    | otherwise = return (M.singleton u t)
    where (c1, b) = condition t u

tiSPL :: TypeEnv -> SPL -> TI TypeEnv
tiSPL env (SPL ds) = tiDecls env ds

tiDecl :: TypeEnv -> Decl -> TI TypeEnv
tiDecl env (DeclVarDecl v) = do
    (_ ,env1) <- tiVarDecl env v
    return env1
tiDecl e (DeclFunDecl f) = tiFunDecl e f

tiVarDecl :: TypeEnv -> VarDecl -> TI (Subst, TypeEnv)
tiVarDecl env (VarDeclVar s ex) = do
    (s1, t1) <- tiExp env ex
    let env1@(TypeEnv e) = apply s1 env
    case M.lookup (Var, s) e of 
        Nothing -> do
            let t' = generalize env1 t1
            let TypeEnv env2 = remove env1 Var s
            return (s1, TypeEnv (M.insert (Var, s) t' env2))
        Just v -> do
            vt <- instantiate v
            s2 <- mgu vt t1
            return (s1, apply (s1 `composeSubst` s2) env1)
tiVarDecl env (VarDeclType t s e) = do
    (s1, t1) <- tiExp env e
    s2 <- mgu t1 t
    let t' = generalize (apply (s1 `composeSubst` s2) env) t1
    let TypeEnv env1 = remove env Var s
    let env2 = TypeEnv (M.insert (Var, s) t' env1)
    return (s1 `composeSubst` s2, apply (s1 `composeSubst` s2) env2)

tiDecls :: TypeEnv -> [Decl] -> TI TypeEnv
tiDecls = foldM tiDecl

nameOfVarDecl :: VarDecl -> String
nameOfVarDecl (VarDeclVar n _) = n
nameOfVarDecl (VarDeclType _ n _) = n

tiVarDecls :: TypeEnv -> [VarDecl] -> TI (Subst, TypeEnv)
tiVarDecls env [] = return (nullSubst, env)
tiVarDecls env (v:vs) = do
    (s1, env1) <- tiVarDecl env v
    (s2, env2) <- tiVarDecls env1 vs
    return (s1 `composeSubst` s2, env2)

checkReturn :: TypeEnv -> Type -> Stmt -> TI Subst
checkReturn _ t (StmtReturn Nothing) = mgu t Void
checkReturn env t (StmtReturn (Just e)) = do
    (s1, t1) <- tiExp env e
    s2 <- mgu t t1
    return $ s1 `composeSubst` s2

getReturns :: Stmt -> [Stmt]
getReturns (StmtIf _ s1 s2) = allReturns s1 ++ allReturns (fromMaybe [] s2)
getReturns (StmtWhile _ s) = allReturns s
getReturns StmtField {} = []
getReturns StmtFunCall {} = []
getReturns r = [r]

allReturns :: [Stmt] -> [Stmt]
allReturns = concatMap getReturns

returnType :: TypeEnv -> Stmt -> TI (Subst, Type)
returnType _ (StmtReturn Nothing) = return (nullSubst, Void)
returnType env (StmtReturn (Just e)) = tiExp env e

tiFunDecl :: TypeEnv -> FunDecl -> TI TypeEnv
tiFunDecl env (FunDecl n args (Just t) vars stmts)
    | l1 /= l2 = throwError $ show n ++ " got " ++ show l1  ++ " arguments, but expected " ++ show l2 ++ " arguments"
    | otherwise = do
        let TypeEnv env1 = removeAll env Var args
        let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme [] t)) args (funTypeToList t)
        let env2 = TypeEnv (env1 `M.union` argsTvMap)
        (s1, env3) <- tiVarDecls env2 vars
        s2 <- tiStmts env3 stmts
        let returns = allReturns stmts
        if null returns then do
            s3 <- mgu (retType t) Void 
            let substFull = s1 `composeSubst` s2 `composeSubst` s3 
            return $ apply substFull env
        else do
            ss <- mapM (checkReturn (apply s2 env3) $ retType t) returns
            let substFull = foldl composeSubst nullSubst ss
            return $ apply substFull env
    where
        l1 = length (funTypeToList t) - 1
        l2 = length args
tiFunDecl env@(TypeEnv envt) (FunDecl n args Nothing vars stmts) = case M.lookup (Fun, n) envt of
    Nothing -> throwError "wtf"
    Just s -> do
        tvs <- mapM (newTyVar Nothing) args
        let env1 = remove env Fun n
        let TypeEnv env2 = removeAll env Var args
        let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme [] t)) args tvs
        let env3 = TypeEnv $ env2 `M.union` argsTvMap
        (s1, env4) <- tiVarDecls env3 vars
        s2 <- tiStmts env4 stmts
        let cs1 = s1 `composeSubst` s2
        let returns = allReturns stmts
        let ts = map (apply cs1) tvs
        case listToMaybe returns of
            Nothing -> do
                let t = foldr1 TypeFun (ts ++ [Void])
                let env5 = env1 `combine` TypeEnv (M.singleton (Fun, n) (Scheme [] t))
                return $ apply cs1 env5
            Just r -> do
                (s3, t2) <- returnType (apply cs1 env4) r
                let cs2 = s3 `composeSubst` cs1
                ss <- mapM (checkReturn (apply cs2 env4) t2) returns
                let s4 = foldl composeSubst nullSubst ss
                let cs3 = s4 `composeSubst` cs2
                let t = foldr1 TypeFun $ apply cs3 (ts ++ [t2])
                let env5 = env1 `combine` TypeEnv (M.singleton (Fun, n) (Scheme [] t))
                return $ apply cs3 env5

tiStmts :: TypeEnv -> [Stmt] -> TI Subst
tiStmts _ [] = return nullSubst
tiStmts env (s:ss) = do
    s1 <- tiStmt env s
    s2 <- tiStmts (apply s1 env) ss
    return (s1 `composeSubst` s2)

tiField :: Type -> Field -> TI (Subst, Type)
tiField t Head = do
    t1 <- newTyVar Nothing "f"
    s <- mgu (TypeArray t1) t
    return (s, apply s t1)
tiField t Tail = do
    t1 <- newTyVar Nothing "f"
    s <- mgu (TypeArray t1) t
    return (s, apply s $ TypeArray t1)
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
    return (s1 `composeSubst` s2, t2)

tiStmt :: TypeEnv -> Stmt -> TI Subst
tiStmt env (StmtIf e ss1 ss2) = do
    (s1, t1) <- tiExp env e
    s1 <- mgu (TypeBasic BoolType) t1
    s2 <- tiStmts env ss1
    s3 <- tiStmts (apply s1 env) (fromMaybe [] ss2)
    return (s1 `composeSubst` s2 `composeSubst` s3)
tiStmt env (StmtWhile e ss) = do
    (s1, t1) <- tiExp env e
    s1 <- mgu (TypeBasic BoolType) t1
    s2 <- tiStmts env ss
    return (s1 `composeSubst` s2)
tiStmt e@(TypeEnv env) (StmtField n fs ex) = do
    (s1, t1) <- tiExp e ex
    case M.lookup (Var, n) env of
        Nothing -> throwError $ n ++ " is not defined"
        Just sigma -> do
            t <- instantiate sigma
            (s2, t') <- tiFields t fs
            s3 <- mgu t1 t'
            return (s1 `composeSubst` s2 `composeSubst` s3)
tiStmt env (StmtFunCall f) = do
    (s, _) <- tiFunCall env f
    return s
tiStmt env (StmtReturn sr) = do
    case sr of
        Nothing -> return nullSubst
        Just e -> do
            (s, t) <- tiExp env e
            return s
 
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

tiFunCall :: TypeEnv -> FunCall -> TI (Subst, Type)
tiFunCall e@(TypeEnv env) f@(FunCall n es) = case M.lookup (Fun, n) env of
    Nothing -> throwError $ "function " ++ n ++ " doesn't exist"
    Just sigma -> do
        t <- instantiate sigma
        case funType t of 
            Nothing -> if null es then return (nullSubst, retType t) else throwError $ "Number of arguments of " ++ show f ++ " does not correspond with its type"
            Just funT -> if length es /= length (funTypeToList funT) then throwError $ show n ++ " got " ++ show (length es)  ++ " arguments, but expected " ++ show (length (funTypeToList funT)) ++ " arguments" else do
                ts <- mapM (tiExp e) es
                s <- zipWithM mgu (map snd ts) (funTypeToList funT)
                let s1 = foldr1 composeSubst $ map fst ts ++ s
                return (s1, retType t)

tiOp1 :: Op1 -> (Type, Type)
tiOp1 Min = (TypeBasic IntType, TypeBasic IntType)
tiOp1 Not = (TypeBasic BoolType, TypeBasic BoolType)

tiOp2 :: Op2 -> TI (Type, Type, Type)
tiOp2 o
    | o `elem` [Plus, Minus, Product, Division, Modulo] = return (TypeBasic IntType, TypeBasic IntType, TypeBasic IntType)
    | o `elem` [And, Or] = return (TypeBasic BoolType, TypeBasic BoolType, TypeBasic BoolType)
    | o == Cons = newTyVar Nothing "c" >>= (\t -> return (t, TypeArray t, TypeArray t))
    | o `elem` [Equals, Neq] = newTyVar (Just Eq) "e" >>= (\t -> return (t, t, TypeBasic BoolType))
    | o `elem` [Leq, Geq, Smaller, Greater] = newTyVar (Just Ord) "e" >>= (\t -> return (t, t, TypeBasic BoolType))

tiExp :: TypeEnv -> Exp -> TI (Subst, Type)
tiExp env (Exp o e1 e2) = do
    (t1, t2, t3) <- tiOp2 o
    (s1, t1') <- tiExp env e1
    s2 <- mgu t1' (apply s1 t1)
    let cs1 = s2 `composeSubst` s1
    (s3, t2') <- tiExp (apply cs1 env) e2
    let cs2 = s3 `composeSubst` cs1
    s4 <- mgu (apply cs2 t2') (apply cs2 t2)
    let cs3 = s4 `composeSubst` cs2
    return (cs3, apply cs3 t3)
tiExp env (ExpOp1 o e) = do
    let (t1, t2) = tiOp1 o
    (s1, t1') <- tiExp env e
    s2 <- mgu t1 t1'
    return (s1 `composeSubst` s2, t2)
tiExp env (ExpTuple (e1, e2)) = do
    (s1, t1) <- tiExp env e1
    (s2, t2) <- tiExp (apply s1 env) e2
    return (s1 `composeSubst` s2, TypeTuple t1 t2)
tiExp env (ExpBrackets e) = tiExp env e
tiExp (TypeEnv env) (ExpField s fs) = case M.lookup (Var, s) env of
    Nothing -> throwError $ s ++ " is not defined"
    Just sigma -> do
        t <- instantiate sigma
        tiFields t fs
tiExp _ (ExpInt _) = return (nullSubst, TypeBasic IntType)
tiExp _ (ExpBool _) = return (nullSubst, TypeBasic BoolType)
tiExp _ (ExpChar _) = return (nullSubst, TypeBasic CharType)
tiExp env (ExpFunCall f) = tiFunCall env f
tiExp _ ExpEmptyList = do
    t <- newTyVar Nothing "l"
    return (nullSubst, TypeArray t)

testExp :: Exp -> IO ()
testExp e = do
    (res, _) <- runTI $ tiExp (TypeEnv M.empty) e
    case res of
        Left err -> putStrLn $ "error: " ++ err
        Right t -> putStrLn $ show e ++ " :: " ++ show t
