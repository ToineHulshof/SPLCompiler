module Types where

import Control.Monad.Except ( zipWithM, foldM, runExceptT, MonadError(throwError), ExceptT )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( MonadState(put, get), StateT(runStateT) )
import Data.Functor ( (<&>) )
import Data.Maybe ( fromMaybe, listToMaybe )
import qualified Data.Map as M
import qualified Data.Set as S
import Grammar
import Debug.Trace ( trace )

data Scheme = Scheme (Maybe Condition) [String] Type deriving (Show)
type Subst = M.Map String Type
data Kind = Var | Fun deriving (Eq, Ord, Show)
data Condition = Eq | Ord deriving (Eq, Ord, Show)

class Types a where
  ftv :: a -> S.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TypeID n) = S.singleton n
  ftv (TypeBasic _)  = S.empty
  ftv (TypeArray t) = ftv t
  ftv (TypeFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TypeTuple t1 t2)  = ftv t1 `S.union` ftv t2
  ftv Void = S.empty

  apply s (TypeID n) = case M.lookup n s of
    Nothing -> TypeID n
    Just t -> t
  apply _ (TypeBasic t) = TypeBasic t
  apply s (TypeArray t) = TypeArray (apply s t)
  apply s (TypeFun t1 t2) = TypeFun (apply s t1) (apply s t2)
  apply s (TypeTuple t1 t2) = TypeTuple (apply s t1) (apply s t2)
  apply _ Void = Void

instance Types Scheme where
  ftv (Scheme _ vars t) = ftv t `S.intersection` S.fromList vars
  apply s (Scheme c vars t) = Scheme c vars (apply (foldr M.delete s vars) t)

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

instance Show TypeEnv where
    show (TypeEnv env) = unlines $ map (\((k, n), Scheme c _ t) -> show k ++ " " ++ n ++ " :: " ++ cString c ++ show t) $ M.toList env
        where
            cString Nothing = ""
            cString (Just c) = show c ++ " => "

emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

remove :: TypeEnv -> Kind -> String -> TypeEnv
remove (TypeEnv env) k var = TypeEnv $ M.delete (k, var) env

removeAll :: TypeEnv -> Kind -> [String] -> TypeEnv
removeAll env _ [] = env
removeAll env k (v:vars) = removeAll (remove env k v) k vars 

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme Nothing vars t
    where vars = S.toList (ftv t `S.intersection` ftv env)

data TIEnv = TIEnv { }
data TIState = TIState { tiSupply :: Int, tiSubst :: Subst }

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    where initTIEnv = TIEnv { }
          initTIState = TIState { tiSupply = 0, tiSubst = M.empty }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    return (TypeID (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme _ vars t) = do
    nvars <- mapM newTyVar vars
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
mgu (TypeID u) t = varBind u t
mgu t (TypeID u) = varBind u t
mgu (TypeBasic t1) (TypeBasic t2)
    | t1 == t2 = return nullSubst
    | otherwise = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
mgu Void Void = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t
    | TypeID u == t = return nullSubst
    | u `S.member` ftv t = throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
    | otherwise = return (M.singleton u t)

tiSPL :: TypeEnv -> SPL -> TI TypeEnv
tiSPL env (SPL ds) = tiDecls env ds

tiDecl :: TypeEnv -> Decl -> TI TypeEnv
tiDecl env (DeclVarDecl v) = tiVarDecl env v
tiDecl e (DeclFunDecl f) = tiFunDecl e f

tiVarDecl :: TypeEnv -> VarDecl -> TI TypeEnv
tiVarDecl env@(TypeEnv e) (VarDeclVar s ex) = do
    (s1, t1) <- tiExp env ex
    case M.lookup (Var, s) e of 
        Nothing -> do
            let t' = generalize env t1
            let TypeEnv env1 = remove env Var s
            let env2 = TypeEnv (M.insert (Var, s) t' env1)
            return $ apply s1 env2
        Just v -> do
            vt <- instantiate v
            s2 <- mgu vt t1
            return $ apply (s1 `composeSubst` s2) env
tiVarDecl env (VarDeclType t s e) = do
    (s1, t1) <- tiExp env e
    s2 <- mgu t1 t
    let t' = generalize (apply (s1 `composeSubst` s2) env) t1
    let TypeEnv env1 = remove env Var s
    let env2 = TypeEnv (M.insert (Var, s) t' env1)
    return $ apply (s1 `composeSubst` s2) env2

tiDecls :: TypeEnv -> [Decl] -> TI TypeEnv
tiDecls = foldM tiDecl

nameOfVarDecl :: VarDecl -> String
nameOfVarDecl (VarDeclVar n _) = n
nameOfVarDecl (VarDeclType _ n _) = n

tiVarDecls :: TypeEnv -> [VarDecl] -> TI TypeEnv
tiVarDecls = foldM tiVarDecl
-- tiVarDecls env vs = tiDecls env $ map DeclVarDecl vs

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
        let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme Nothing [] t)) args (funTypeToList t)
        let env2 = TypeEnv (env1 `M.union` argsTvMap)
        env3 <- tiVarDecls env2 vars
        s2 <- tiStmts env3 stmts
        let returns = allReturns stmts
        if null returns then do
            s3 <- mgu (retType t) Void 
            let substFull = s2 `composeSubst` s3 
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
        -- x <- instantiate s
        -- let tvs = init $ funTypeToList x
        tvs <- mapM newTyVar args
        let env1 = remove env Fun n
        let TypeEnv env2 = removeAll env Var args
        let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme Nothing [] t)) args tvs
        let env3 = TypeEnv $ env2 `M.union` argsTvMap
        env4 <- tiVarDecls env3 vars
        s1 <- tiStmts env3 stmts
        let returns = allReturns stmts
        let ts = map (apply s1) tvs
        case listToMaybe returns of
            Nothing -> do
                let t = foldr1 TypeFun (ts ++ [Void])
                let env5 = env1 `combine` TypeEnv (M.singleton (Fun, n) (Scheme Nothing [] t))
                return $ apply s1 env5
            Just r -> do
                (_, t2) <- returnType env4 r
                mapM_ (checkReturn env3 t2) returns
                let t = foldr1 TypeFun (ts ++ [t2])
                let env5 = env1 `combine` TypeEnv (M.singleton (Fun, n) (Scheme Nothing [] t))
                return $ apply s1 env5

tiStmts :: TypeEnv -> [Stmt] -> TI Subst
tiStmts _ [] = return nullSubst
tiStmts env (s:ss) = do
    s1 <- tiStmt env s
    s2 <- tiStmts (apply s1 env) ss
    return (s1 `composeSubst` s2)

tiField :: Type -> Field -> TI (Subst, Type)
tiField t Head = do
    t1 <- newTyVar "f"
    s <- mgu t (TypeArray t1)
    return (s, apply s t1)
tiField t Tail = do
    t1 <- newTyVar "f"
    s <- mgu t (TypeArray t1)
    return (s, apply s $ TypeArray t1)
tiField t First = do
    t1 <- newTyVar "f"
    t2 <- newTyVar "f"
    s <- mgu t (TypeTuple t1 t2)
    return (s, apply s t1)
tiField t Second = do
    t1 <- newTyVar "f"
    t2 <- newTyVar "f"
    s <- mgu t (TypeTuple t1 t2)
    return (s, apply s t2)

tiFields :: Type -> [Field] -> TI (Subst, Type)
tiFields t [] = return (nullSubst, t)
tiFields t (f:fs) = do
    (s1, t1) <- tiField t f
    (s2, t2) <- tiFields t1 fs
    return (s2 `composeSubst` s1, t2)

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
        Just e -> tiExp env e <&> fst

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

tiOp2 :: TypeEnv -> Op2 -> TI (Subst, Type, Type, Type, Maybe Condition)
tiOp2 env o
    | o `elem` [Plus, Minus, Product, Division, Modulo] = return (nullSubst, TypeBasic IntType, TypeBasic IntType, TypeBasic IntType, Nothing)
    | o `elem` [And, Or] = return (nullSubst, TypeBasic BoolType, TypeBasic BoolType, TypeBasic BoolType, Nothing)
    | o == Cons = newTyVar "c" >>= (\t -> return (nullSubst, t, TypeArray t, TypeArray t, Nothing))
    | o `elem` [Equals, Neq] = newTyVar "e" >>= (\t -> return (nullSubst, t, t, TypeBasic BoolType, Just Eq))
    | o `elem` [Leq, Geq, Smaller, Greater] = newTyVar "e" >>= (\t -> return (nullSubst, t, t, TypeBasic BoolType, Just Ord))

tiExp :: TypeEnv -> Exp -> TI (Subst, Type)
tiExp env (Exp o e1 e2) = do
    (s0, t1, t2, t3, l) <- tiOp2 env o
    (s1, t1') <- tiExp (apply s0 env) e1
    (s2, t2') <- tiExp (apply (s0 `composeSubst` s1) env) e2
    s3 <- mgu t1 t1'
    s4 <- mgu t2 t2'
    let substFull = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1 `composeSubst` s0
    case l of
        Nothing -> return (substFull, t3)
        Just ts -> do
            s5 <- mgu t1' t2'
            -- mapM_ (mgu t1') ts
            -- mapM_ (mgu t2') ts
            return (s5 `composeSubst` substFull, t3)
            -- if t1' `elem` ts then return (s5 `composeSubst` substFull, t3) else throwError $ "cannot apply " ++ show o ++ " on types " ++ show t1' ++ " and " ++ show t2'
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
    t <- newTyVar "l"
    return (nullSubst, TypeArray t)

testExp :: Exp -> IO ()
testExp e = do
    (res, _) <- runTI $ tiExp (TypeEnv M.empty) e
    case res of
        Left err -> putStrLn $ "error: " ++ err
        Right t -> putStrLn $ show e ++ " :: " ++ show t
