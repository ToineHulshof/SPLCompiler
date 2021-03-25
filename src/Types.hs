module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor ( (<&>) )
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Grammar
import Parser

-- import qualified Text.PrettyPrint as PP

data Scheme = Scheme [String] Type deriving (Show)
type Subst = M.Map String Type
data Kind = Var | Fun deriving (Eq, Ord, Show)

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
  apply s (TypeBasic t) = TypeBasic t
  apply s (TypeArray t) = TypeArray (apply s t)
  apply s (TypeFun t1 t2) = TypeFun (apply s t1) (apply s t2)
  apply s (TypeTuple t1 t2) = TypeTuple (apply s t1) (apply s t2)
  apply s Void = Void

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

instance Show TypeEnv where
    show (TypeEnv env) = show env

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
generalize env t = Scheme vars t
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
instantiate (Scheme vars t) = do
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
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t
    | TypeID u == t = return nullSubst
    | u `S.member` ftv t = throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
    | otherwise = return (M.singleton u t)

tiSPL :: TypeEnv -> SPL -> TI (Subst, TypeEnv)
tiSPL env (SPL ds) = tiDecls env ds

tiDecl :: TypeEnv -> Decl -> TI (Subst, TypeEnv)
tiDecl env (DeclVarDecl v) = tiVarDecl env v
tiDecl e (DeclFunDecl f) = tiFunDecl e f

tiVarDecl :: TypeEnv -> VarDecl -> TI (Subst, TypeEnv)
tiVarDecl env (VarDeclVar s e) = do
    (s1, t1) <- tiExp env e
    let t' = generalize (apply s1 env) t1
    let TypeEnv env' = remove env Var s
    let env'' = TypeEnv (M.insert (Var, s) t' env')
    return (s1, apply s1 env'')
tiVarDecl env (VarDeclType t s e) = do
    (s1, t1) <- tiExp env e
    s2 <- mgu t1 t
    let t' = generalize (apply (s1 `composeSubst` s2) env) t1
    let TypeEnv env' = remove env Var s
    let env'' = TypeEnv (M.insert (Var, s) t' env')
    return (s1 `composeSubst` s2, apply (s1 `composeSubst` s2) env'')

tiDecls :: TypeEnv -> [Decl] -> TI (Subst, TypeEnv)
tiDecls env [] = return (nullSubst, env)
tiDecls env (d:ds) = do
    (s1, e1) <- tiDecl env d
    (s2, e2) <- tiDecls (apply s1 e1) ds
    return (s1 `composeSubst` s2, apply (s1 `composeSubst` s2) e2)

nameOfVarDecl :: VarDecl -> String
nameOfVarDecl (VarDeclVar n _) = n
nameOfVarDecl (VarDeclType _ n _) = n

tiVarDecls :: TypeEnv -> [VarDecl] -> TI (Subst, TypeEnv)
tiVarDecls env vs = tiDecls env $ map DeclVarDecl vs

tiFunDecl :: TypeEnv -> FunDecl -> TI (Subst, TypeEnv)
tiFunDecl = undefined
-- tiFunDecl env (FunDecl n args (Just t) vars stmts) = do
--     argsTv <- mapM newTyVar args
--     retTv <- newTyVar "r"
--     let TypeEnv env' = removeAll env Var args
--     let argsType = foldr TypeFun End $ argsTv ++ [retTv]
--     s1 <- mgu argsType t
--     let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme [] t)) args argsTv
--     let env'' = apply s1 $ TypeEnv (env' `M.union` argsTvMap)
--     (s2, env''') <- tiVarDecls env'' vars
--     s3 <- tiStmts (apply s2 env''') stmts
--     let returns = [ rs | rs@StmtReturn {} <- stmts]
--     return _a
-- tiFunDecl env (FunDecl _ args Nothing vars stmts) = do
--     argsTv <- mapM newTyVar args
--     retTv <- newTyVar "r"
--     let TypeEnv env' = removeAll env Var args
--     let argsTvMap = M.fromList $ zipWith (\a t -> ((Var, a), Scheme [] t)) args argsTv
--     let env'' = TypeEnv (env' `M.union` argsTvMap)
--     -- check return
--     -- check var decl
--     -- check statms

--     return _a

-- FunDecl zonder type

tiStmts :: TypeEnv -> [Stmt] -> TI Subst
tiStmts _ [] = return nullSubst
tiStmts env (s:ss) = do
    s1 <- tiStmt env s
    s2 <- tiStmts (apply s1 env) ss
    return (s1 `composeSubst` s2)

checkField :: Type -> Field -> TI Type
checkField (TypeArray t) Head = return t
checkField t@(TypeArray ty) Tail = return t
checkField (TypeTuple t1 t2) First = return t1
checkField (TypeTuple t1 t2) Second = return t2
checkField n f = throwError $ show f ++ " cannot be applied on type " ++ show n

checkFields :: Type -> [Field] -> TI Type
checkFields = foldM checkField

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
            t' <- checkFields t fs
            s2 <- mgu t1 t'
            return (s1 `composeSubst` s2)
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

funType :: Type -> Type
funType (TypeFun t1 r@(TypeFun t2 t3)) = TypeFun t1 (funType r)
funType (TypeFun t1 t2) = t1

tiFunCall :: TypeEnv -> FunCall -> TI (Subst, Type)
tiFunCall e@(TypeEnv env) (FunCall n es) = case M.lookup (Fun, n) env of
    Nothing -> throwError $ "function " ++ n ++ " doesn't exist"
    Just sigma -> do
        t <- instantiate sigma
        ts <- mapM (tiExp e) es
        let t1 = foldr1 TypeFun (map snd ts)
        let s1 = foldr1 composeSubst (map fst ts)
        let ret = retType t
        let fun = funType t
        s <- mgu t1 fun
        return (s, ret)

tiOp1 :: Op1 -> (Type, Type)
tiOp1 Min = (TypeBasic IntType, TypeBasic IntType)
tiOp1 Not = (TypeBasic BoolType, TypeBasic BoolType)

tiOp2 :: TypeEnv -> Op2 -> TI (Subst, Type, Type, Type, Maybe [Type])
tiOp2 env o
    | o `elem` [Plus, Minus, Product, Division, Modulo] = return (nullSubst, TypeBasic IntType, TypeBasic IntType, TypeBasic IntType, Nothing)
    | o `elem` [And, Or] = return (nullSubst, TypeBasic BoolType, TypeBasic BoolType, TypeBasic BoolType, Nothing)
    | o == Cons = newTyVar "c" >>= (\t -> return (nullSubst, t, TypeArray t, TypeArray t, Nothing))
    | o `elem` [Eq, Neq] = newTyVar "e" >>= (\t -> return (nullSubst, t, t, TypeBasic BoolType, Just [TypeBasic BoolType, TypeBasic IntType, TypeBasic CharType]))
    | o `elem` [Leq, Geq, Smaller, Greater] = newTyVar "e" >>= (\t -> return (nullSubst, t, t, TypeBasic BoolType, Just [TypeBasic IntType, TypeBasic CharType]))

tiExp :: TypeEnv -> Exp -> TI (Subst, Type)
tiExp env (Exp o e1 e2) = do
    (s0, t1, t2, t3, l) <- tiOp2 env o
    (s1, t1') <- tiExp (apply s0 env) e1
    (s2, t2') <- tiExp (apply (s0 `composeSubst` s1) env) e2
    s3 <- mgu t1 t1'
    s4 <- mgu t2 t2'
    let substFull = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1 `composeSubst` s0
    let env' = apply substFull env
    case l of
        Nothing -> return (substFull, t3)
        Just ts -> do
            s5 <- mgu t1' t2'
            if t1' `elem` ts then return (s5 `composeSubst` substFull, t3) else throwError $ "cannot apply " ++ show o ++ " on types " ++ show t1' ++ " and " ++ show t2'
tiExp env (ExpOp1 o e) = do
    let (t1, t2) = tiOp1 o
    (s1, t1') <- tiExp env e
    s2 <- mgu t1 t1'
    return (s1 `composeSubst` s2, t2)
tiExp env (ExpTuple (e1, e2)) = do
    (s1, t1) <- tiExp env e1
    (s2, t2) <- tiExp (apply s1 env) e1
    return (s1 `composeSubst` s2, TypeTuple t1 t2)
tiExp env (ExpBrackets e) = tiExp env e
tiExp (TypeEnv env) (ExpField s fs) = case M.lookup (Var, s) env of
    Nothing -> throwError $ s ++ " is not defined"
    Just sigma -> do 
        t <- instantiate sigma
        t' <- checkFields t fs
        s <- mgu t t'
        return (s, t')
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

-- test' :: [VarDecl] -> IO ()
-- test' ds = do
--     (res, _) <- runTI $ tiVarDecls (TypeEnv M.empty) ds
--     case res of
--         Left err -> putStrLn $ "error: " ++ err
--         Right t -> putStrLn $ show ds ++ " :: " ++ show t

-- showSubst :: TI (Subst, Type) -> IO ()
-- showSubst ti = do
--     (a, b) <- runTI ti
--     case a of
--         Left err -> putStrLn  err
--         Right (s, t) -> print s

-- showSubst' :: TI Subst -> IO ()
-- showSubst' ti = do
--     (a, b) <- runTI ti
--     case a of
--         Left err -> putStrLn  err
--         Right s -> print s

-- test :: Decl -> IO ()
-- test d = do
--     (res, _) <- runTI (ti (TypeEnv M.empty) d)
--     case res of
--         Left err -> putStrLn $ "error: " ++ err
--         Right t -> putStrLn $ show d ++ " :: " ++ show t